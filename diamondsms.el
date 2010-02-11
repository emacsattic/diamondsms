;;; diamondsms.el --- Send SMS' via GNU/Emacs

;; Copyright (C) 2008, 2009 Yoni Rabkin
;; Copyright (C) 2002 Edward O'Connor
;;
;; Author: Yoni Rabkin <yonirabkin@member.fsf.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;

;;; Installation:
;;
;; Add the following to your .emacs file:
;;
;; (add-to-list 'load-path X)
;;
;; ...where X is the directory path where diamondsms.el is stored.
;;
;; (require 'diamondsms)
;;
;; (setq diamondsms-account-id A
;;       diamondsms-pin-code B
;;       diamondsms-default-from-number C)
;;
;; ...where A is your DiamonCard account ID number, B your DiamondCard
;; pin code and C is the number you wish the messages to be seen as
;; originating (A, B, C should be strings)
;;
;; Set the variable `diamondsms-phonebook-alist' to contain the names
;; and numbers of the people you want to contact. For example:
;;
;; (setq diamondsms-phonebook-alist
;;       '(("whitehouse" . "0012024571111")
;; 	   ("ASPCA"      . "0018882762210")))
;;
;; To send a message:
;; M-x diamondsms-send-sms
;;
;; To send a message without using the phonebook:
;; M-x diamondsms-send-sms-to-number

;;; History:
;;
;; Originally written in September of 2008.

;;; Code:

(require 'url)
(require 'xml)

(defvar diamondsms-account-id nil
  "Accound ID for the Diamondcard service.")
(defvar diamondsms-pin-code nil
  "Pin code for the Diamondcard service.")
(defvar diamondsms-default-from-number nil
  "Phone number of SMS sender.")
(defvar diamondsms-log nil
  "Log of messages.")

(defvar diamondsms-response-debug "")
(defvar diamondsms-request-debug  "")

(defvar diamondsms-phonebook-alist nil
  "Alist of (person-name-string . number-string) pairs.")

(defun diamondsms-sub-e-name (e) (car e))
(defun diamondsms-sub-e-type (e) (cdr (nth 0 (nth 1 e))))
(defun diamondsms-sub-e-value (e) (nth 2 e))
(defun diamondsms-parse-response-out (response-sexp)
  "Return the \"out\" element of xml RESPONSE-SEXP."
  (let ((subelement-list
	 (cddr (nth 2 (nth 2 (nth 2 (nth 0 response-sexp))))))
	subelement-alist)
    (when (not subelement-list) (error "nil response"))
    (dolist (subelement subelement-list)
      (setq subelement-alist
	    (append subelement-alist
		    (list
		     (cons (diamondsms-sub-e-name subelement)
			   (diamondsms-sub-e-value subelement))))))
    subelement-alist))

(defun diamondsms-user-response-string (response-sexp)
  "Display a message to the user based on RESPONSE-SEXP."
  (let ((subelement-alist (diamondsms-parse-response-out response-sexp))
	error-code sending-id)
    (setq error-code (cdr (assoc 'ErrCode subelement-alist))
	  sending-id (cdr (assoc 'SendingId subelement-alist)))
    (add-to-list 'diamondsms-log (cons sending-id error-code))
    (if error-code
	(error "SMS server response: sending failed!")
      (message "sending succeeded, message id: %s" sending-id))))

(defun diamondsms-soap-process-response (response-buffer)
  "Process the SOAP response in RESPONSE-BUFFER."
  (let ((retval "nodata"))
    (with-current-buffer response-buffer
      (goto-char (point-min))
      (when (looking-at "^HTTP/1.* 200 OK$")
	(re-search-forward "^$" nil t 1)
	(setq retval (buffer-substring-no-properties (point) (point-max))))
      (kill-buffer response-buffer))
    (with-temp-buffer
      (insert "\n" retval "\n")
      (goto-char (point-min))
      (while (re-search-forward "\r" nil t)
	(replace-match ""))
      (diamondsms-user-response-string
       (xml-parse-region (point-min) (point-max))))))

(defun diamondsms-soap-request (url action data)
  "Send and process SOAP request to URL with DATA."
  (let ((request-data
	 (encode-coding-string
	  (concat "<?xml version=\"1.0\" encoding=\"utf-8\" ?>"
		  data) 'utf-8)))
    (let* ((url-request-extra-headers
	    `(("Content-type" . "text/xml; charset=\"utf-8\"")
	      ("SOAPAction" . ,(format "%s%s" url action))))
	   (url-request-method "POST")
	   (url-request-data request-data))
      (let ((response (url-retrieve-synchronously url)))
	;; for debugging
	;;
	(setq diamondsms-response-debug
	      (copy-sequence (with-current-buffer response
			       (buffer-substring-no-properties (point-min) (point-max)))))
	(diamondsms-soap-process-response response)))))

(defun diamondsms-send-request-string (accountid pincode messagetext fromnum tonum)
  "Fill in the SMS send SOAP envelope."
  (format
   "<SOAP-ENV:Envelope
  xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"
   xmlns:impl=\"http://sms.diamondcard.us/SMSapi\"
   xmlns:wsdlsoap=\"http://schemas.xmlsoap.org/wsdl/soap/\"
   xmlns:wsdl=\"http://schemas.xmlsoap.org/wsdl/\"
   xmlns:soapenc=\"http://schemas.xmlsoap.org/soap/encoding/\"
   xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
   xmlns:tns1=\"http://sms.diamondcard.us/SMSapi\"
   xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
  <SOAP-ENV:Body>
    <tns1:send xmlns:tns1=\"http://sms.diamondcard.us/SMSapi\">
      <tns1:inParams>
	<tns1:AccId>%s</tns1:AccId>
	<tns1:PinCode>%s</tns1:PinCode>
	<tns1:MsgTxt>%s</tns1:MsgTxt>
	<tns1:SendFrom>%s</tns1:SendFrom>
	<tns1:Destination>%s</tns1:Destination>
      </tns1:inParams>
    </tns1:send>
  </SOAP-ENV:Body>
</SOAP-ENV:Envelope>"
   accountid pincode messagetext fromnum tonum))

(defun diamondsms-sanity-check ()
  "Throw an error if certain conditions are not met."
  (when (not diamondsms-account-id)
    (error "accound ID not set"))
  (when (not diamondsms-pin-code)
    (error "pin code not set"))
  (when (not diamondsms-default-from-number)
    (error "default sender number not set")))

(defun diamondsms-send-sms-to-number (number text)
  "Send TEXT to NUMBER using Short Message Service."
  (interactive "sPhone number: \nsSMS text: \n")
  (let ((cont t))
    (diamondsms-sanity-check)
    (when (= (length text) 0)
      (error "attempt to send an empty SMS"))
    (when (not number)
      (error "can't send an SMS without a valid phone number"))
    (when (>= (length text) 120)
      (setq cont
	    (y-or-n-p "over 120 chars; provider might split and charge extra; continue?")))
    (if cont
	(diamondsms-soap-request "http://sms.diamondcard.us"
				 "/SMSapi#send"
				 (diamondsms-send-request-string
				  diamondsms-account-id
				  diamondsms-pin-code
				  text
				  diamondsms-default-from-number
				  number))
      (message "message sending canceled"))))

(defun diamondsms-send-sms (text)
  "Send TEXT using Short Message Service."
  (interactive "stext: \n")
  (let ((number (cdr (assoc (completing-read "number: " diamondsms-phonebook-alist)
			    diamondsms-phonebook-alist)))
	(cont t))
    (diamondsms-sanity-check)
    (when (= (length text) 0)
      (error "attempt to send an empty SMS"))
    (when (not number)
      (error "can't send an SMS without a valid phone number"))
    (when (>= (length text) 120)
      (setq cont
	    (y-or-n-p "over 120 chars; provider might split and charge extra; continue?")))
    (if cont
	(diamondsms-soap-request "http://sms.diamondcard.us"
				 "/SMSapi#send"
				 (diamondsms-send-request-string
				  diamondsms-account-id
				  diamondsms-pin-code
				  text
				  diamondsms-default-from-number
				  number))
      (message "message sending canceled"))))

(provide 'diamondsms)

;;; diamondsms.el ends here
