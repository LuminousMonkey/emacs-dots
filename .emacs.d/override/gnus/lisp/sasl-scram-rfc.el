;;; sasl-scram-rfc.el --- SCRAM-SHA-1 module for the SASL client framework  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015 Free Software Foundation, Inc.

;; Author: Magnus Henoch <magnus.henoch@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program is implemented from RFC 5802.  It implements the
;; SCRAM-SHA-1 SASL mechanism.
;;
;; RFC 5802 foresees "hash agility", i.e. new mechanisms based on the
;; same protocol but using a different hash function.  Likewise, this
;; module attempts to separate generic and specific functions, which
;; should make it easy to implement any future SCRAM-* SASL mechanism.
;; It should be as simple as copying the SCRAM-SHA-1 section below and
;; replacing all SHA-1 references.
;;
;; This module does not yet implement the variants with channel
;; binding, i.e. SCRAM-*-PLUS.  That would require cooperation from
;; the TLS library.

;;; Code:

(ignore-errors (require 'cl-lib))

(require 'sasl)
(require 'hex-util)
(require 'rfc2104)

;;; Generic for SCRAM-*

(defun sasl-scram-client-first-message (client _step)
  (let ((c-nonce (sasl-unique-id)))
    (sasl-client-set-property client 'c-nonce c-nonce))
  (concat
   ;; n = client doesn't support channel binding
   "n,"
   ;; TODO: where would we get authorization id from?
   ","
   (sasl-scram--client-first-message-bare client)))

(defun sasl-scram--client-first-message-bare (client)
  (let ((c-nonce (sasl-client-property client 'c-nonce)))
    (concat
     ;; TODO: saslprep username or disallow non-ASCII characters
     "n=" (sasl-client-name client) ","
     "r=" c-nonce)))

(eval-and-compile
  (declare-function sasl-cl-coerce "sasl-scram-rfc")
  (declare-function sasl-cl-mapcar-many "sasl-scram-rfc")
  (if (fboundp 'cl-map)
      (defalias 'sasl-cl-map 'cl-map)
    (defun sasl-cl-mapcar-many (func seqs)
      (if (cdr (cdr seqs))
	  (let* ((res nil)
		 (n (apply 'min (mapcar 'length seqs)))
		 (i 0)
		 (args (copy-sequence seqs))
		 p1 p2)
	    (setq seqs (copy-sequence seqs))
	    (while (< i n)
	      (setq p1 seqs p2 args)
	      (while p1
		(setcar p2
			(if (consp (car p1))
			    (prog1 (car (car p1))
			      (setcar p1 (cdr (car p1))))
			  (aref (car p1) i)))
		(setq p1 (cdr p1) p2 (cdr p2)))
	      (push (apply func args) res)
	      (setq i (1+ i)))
	    (nreverse res))
	(let ((res nil)
	      (x (car seqs))
	      (y (nth 1 seqs)))
	  (let ((n (min (length x) (length y)))
		(i -1))
	    (while (< (setq i (1+ i)) n)
	      (push (funcall func
			     (if (consp x) (pop x) (aref x i))
			     (if (consp y) (pop y) (aref y i)))
		    res)))
	  (nreverse res))))

    (defun sasl-cl-coerce (x type)
      "Coerce OBJECT to type TYPE.
TYPE is a Common Lisp type specifier.
\n(fn OBJECT TYPE)"
      (cond ((eq type 'list) (if (listp x) x (append x nil)))
	    ((eq type 'vector) (if (vectorp x) x (vconcat x)))
	    ((eq type 'string) (if (stringp x) x (concat x)))
	    ((eq type 'array) (if (arrayp x) x (vconcat x)))
	    ((and (eq type 'character) (stringp x) (= (length x) 1)) (aref x 0))
	    ((and (eq type 'character) (symbolp x))
	     (sasl-cl-coerce (symbol-name x) type))
	    ((eq type 'float) (float x))
	    ;;((cl-typep x type) x)
	    (t (error "Can't coerce %s to type %s" x type))))

    (defun sasl-cl-map (type func seq &rest rest)
      "Map a FUNCTION across one or more SEQUENCEs, returning a sequence.
TYPE is the sequence type to return.
\n(fn TYPE FUNCTION SEQUENCE...)"
      (let (res y)
	(if rest
	    (if (or (cdr rest) (nlistp seq) (nlistp (car rest)))
		(setq res (sasl-cl-mapcar-many func (cons seq rest)))
	      (setq y (car rest))
	      (while (and seq y)
		(push (funcall func (pop seq) (pop y)) res))
	      (setq res (nreverse res)))
	  (setq res (mapcar func seq)))
	(and type (sasl-cl-coerce res type)))))

  (if (fboundp 'string-prefix-p)
      (defalias 'sasl-string-prefix-p 'string-prefix-p)
    (defun sasl-string-prefix-p (prefix string &optional ignore-case)
      "Return non-nil if PREFIX is a prefix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying attention
to case differences."
      (let ((prefix-length (length prefix)))
	(cond ((> prefix-length (length string)) nil)
	      (ignore-case
	       (string-equal (downcase prefix)
			     (downcase (substring string 0 prefix-length))))
	      (t
	       (string-equal prefix (substring string 0 prefix-length))))))))

(defun sasl-scram--client-final-message (hash-fun block-length hash-length client step)
  (unless (string-match
	   "^r=\\([^,]+\\),s=\\([^,]+\\),i=\\([0-9]+\\)\\(?:$\\|,\\)"
	   (sasl-step-data step))
    (sasl-error "Unexpected server response"))
  (let* ((hmac-fun (lambda (text key)
		     (decode-hex-string
		      (rfc2104-hash hash-fun block-length hash-length key text))))
	 (step-data (sasl-step-data step))
	 (nonce (match-string 1 step-data))
	 (salt-base64 (match-string 2 step-data))
	 (iteration-count (string-to-number (match-string 3 step-data)))

	 (c-nonce (sasl-client-property client 'c-nonce))
	 ;; no channel binding, no authorization id
	 (cbind-input "n,,"))
    (unless (sasl-string-prefix-p c-nonce nonce)
      (sasl-error "Invalid nonce from server"))
    (let* ((client-final-message-without-proof
	    (concat "c=" (base64-encode-string cbind-input) ","
		    "r=" nonce))
	   (password
	    ;; TODO: either apply saslprep or disallow non-ASCII characters
	    (sasl-read-passphrase
	     (format "%s passphrase for %s: "
		     (sasl-mechanism-name (sasl-client-mechanism client))
		     (sasl-client-name client))))
	   (salt (base64-decode-string salt-base64))
	   (salted-password
	    ;; Hi(str, salt, i):
	    (let ((digest (concat salt (string 0 0 0 1)))
		  (xored nil))
	      (dotimes (_i iteration-count xored)
		(setq digest (funcall hmac-fun digest password))
		(setq xored (if (null xored)
				digest
			      (sasl-cl-map 'string 'logxor xored digest))))))
	   (client-key
	    (funcall hmac-fun "Client Key" salted-password))
	   (stored-key (decode-hex-string (funcall hash-fun client-key)))
	   (auth-message
	    (concat
	     (sasl-scram--client-first-message-bare client) ","
	     step-data ","
	     client-final-message-without-proof))
	   (client-signature (funcall hmac-fun (encode-coding-string auth-message 'utf-8) stored-key))
	   (client-proof (sasl-cl-map 'string 'logxor client-key client-signature))
	   (client-final-message
	    (concat client-final-message-without-proof ","
		    "p=" (base64-encode-string client-proof))))
      (sasl-client-set-property client 'auth-message auth-message)
      (sasl-client-set-property client 'salted-password salted-password)
      client-final-message)))

(defun sasl-scram--authenticate-server (hash-fun block-length hash-length client step)
  (cond
   ((string-match "^e=\\([^,]+\\)" (sasl-step-data step))
    (sasl-error (format "Server error: %s" (match-string 1 (sasl-step-data step)))))
   ((string-match "^v=\\([^,]+\\)" (sasl-step-data step))
    (let* ((hmac-fun (lambda (text key)
		       (decode-hex-string
			(rfc2104-hash hash-fun block-length hash-length key text))))
	   (verifier (base64-decode-string (match-string 1 (sasl-step-data step))))
	   (auth-message (sasl-client-property client 'auth-message))
	   (salted-password (sasl-client-property client 'salted-password))
	   (server-key (funcall hmac-fun "Server Key" salted-password))
	   (expected-server-signature
	    (funcall hmac-fun (encode-coding-string auth-message 'utf-8) server-key)))
      (unless (string= expected-server-signature verifier)
	(sasl-error "Server not authenticated"))))
   (t
    (sasl-error "Invalid response from server"))))

;;; SCRAM-SHA-1

(defconst sasl-scram-sha-1-steps
  '(sasl-scram-client-first-message
    sasl-scram-sha-1-client-final-message
    sasl-scram-sha-1-authenticate-server))

(defun sasl-scram-sha-1-client-final-message (client step)
  (sasl-scram--client-final-message
   ;; HMAC-SHA1 uses block length 64 and hash length 20; see RFC 2104.
   'sha1 64 20 client step))

(defun sasl-scram-sha-1-authenticate-server (client step)
  (sasl-scram--authenticate-server
   'sha1 64 20 client step))

;; This needs to be at the end, because of how `sasl-make-mechanism'
;; handles step function names.
(put 'sasl-scram-sha-1 'sasl-mechanism
     (sasl-make-mechanism "SCRAM-SHA-1" sasl-scram-sha-1-steps))

(put 'sasl-scram-rfc 'sasl-mechanism (get 'sasl-scram-sha-1 'sasl-mechanism))

(provide 'sasl-scram-sha-1)

(provide 'sasl-scram-rfc)
;;; sasl-scram-rfc.el ends here
