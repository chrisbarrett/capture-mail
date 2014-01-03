;;; org-mail-capture.el --- Capture tasks and diary entries from mail

;; Copyright (C) 2014 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Defines utilities for parsing emails in your maildir and capturing them with
;; org-mode.

;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'cl-lib)
(autoload 'ido-read-directory-name "ido")

(defgroup org-mail-capture nil
  "Utilities for capturing emails with org-mode."
  :group 'org
  :prefix "omc--")

(defcustom omc-default-parser (list :type 'other
                                    :parser 'ignore
                                    :handler 'ignore)
  "The default parser to use if other parsers fail."
  :group 'org-mail-capture
  :type 'function)

(defcustom omc-archived-messages-dir (--first
                                      (not (s-starts-with? "." (f-filename it)))
                                      (f-directories (f-expand "~/Maildir/")))
  "The path to move messages to once they've been processed."
  :group 'org-mail-capture
  :type 'directory)

;; --------------------------- Internal -----------------------------------------

(defvar omc--parsers nil
  "Contains all the parsers and handlers.
Each element is a list of (TYPE PARSER HANDLER).")

;;; Message parsing

(defun omc--message-header-value (header msg)
  (cadr (s-match (eval `(rx bol ,header ":" (* space) (group (* nonl)))) msg)))

(defun omc--multipart-message? (msg)
  (-when-let (s (omc--message-header-value "content-type" msg))
    (s-matches? "multipart/alternative" s)))

(defun omc--split-message-head-and-body (msg)
  (let ((eoh (s-index-of "\n\n" msg)))
    (cons (substring-no-properties (substring msg 0 eoh))
          (substring-no-properties (substring msg eoh)))))

(defun omc--multipart-body-plaintext-section (msg)
  (cl-destructuring-bind (head . body) (omc--split-message-head-and-body msg)
    (->> body
      ;; Split the body by the boundary specified in the header and select
      ;; the section with plaintext MIME encoding.
      (s-split (cadr (s-match (rx "boundary=" (group (* nonl))) head)))
      (-first (~ s-contains? "text/plain"))
      ;; Tidy the section, dropping headers.
      s-trim
      (s-chop-suffix "--")
      (s-split "\n\n")
      cadr
      s-trim
      (s-chop-suffix "=")
      ;; Convert latin-1 line breaks.
      (s-replace "=\n" "")
      ;; (s-replace "=0A" "")
      )))

(defun omc--message-header->alist (hd)
  "Parse the given message header HD to an alist representation."
  (->> hd
    (s-match-strings-all (rx bol
                             (group (+? (not (any space)))) ":" (* space)
                             (group (+? anything)) (* (or ";" space)) eol))
    (--map
     (cl-destructuring-bind (_ key val) it
       (cons (intern (s-downcase key))
             (substring-no-properties val))))))

(defun omc--message->alist (message-str)
  "Convert the MESSAGE-STR as a string to an alist."
  (let* ((head-and-bod (omc--split-message-head-and-body message-str))
         (body (s-trim (if (omc--multipart-message? message-str)
                           (omc--multipart-body-plaintext-section message-str)
                         (cdr head-and-bod)))))
    (cons (cons 'body body)
          (omc--message-header->alist (car head-and-bod)))))

;;; Run parsers

(defun omc--validate-parser-spec (plist)
  "Assert that PLIST is a well-formed parser specification."
  (cl-assert (plist-get plist :type) t)
  (cl-assert (plist-get plist :parser) t)
  (cl-assert (functionp (plist-get plist :parser)) t)
  (cl-assert (plist-get plist :handler) t)
  (cl-assert (functionp (plist-get plist :handler)) t))

(defun omc--run-parsers (message parsers)
  "Run each parser over the given message until one succeeds.
Return a cons of the type and the parsed value.

MESSAGE is an alist of ([HEADERS...] BODY).

PARSERS is an alist of (TYPE PARSER HANDLER)."
  (-each parsers 'omc--validate-parser-spec)
  (cl-loop
   with alist = (omc--message->alist message)
   for p in parsers do
   (cl-destructuring-bind (&key type parser handler) p
     (-when-let (parsed-val (funcall parser alist))
       (cl-return (cons type (funcall handler parsed-val)))))))

(cl-defun omc--remove-message (filepath)
  "Mark the message at FILEPATH as read
In accordance with maildir conventions, this renames the message
at FILEPATH and moves it to the cur dir."
  (cl-assert (f-exists? omc-archived-messages-dir))
  (when (f-exists? filepath)
    (let* ((dest-file (format "%s:2,S" (car (s-split ":" (f-filename filepath)))))
           (dest-filepath (f-join omc-archived-messages-dir "cur" dest-file)))
      (ignore-errors
        (f-move filepath dest-filepath)))))

(defun omc--capture (files)
  "Parse and capture each of the given FILES."
  (let ((parsers (-concat omc--parsers (list omc-default-parser))))
    (--each files
      (if (omc--run-parsers (f-read-text it) parsers)
          (omc--remove-message it)
        (warn "Failed to parse: %s" it)))))

;; ------------------------- Public Interface ----------------------------------

(cl-defun omc-declare-message-parser (type &key parser handler)
  "Declare a maildir message parser.

TYPE is a symbol that identifies the type of the parsed message.

PARSER is a function that takes an alist of ([HEADERS...] BODY).
It should return the value needed by the HANDLER command if
parsing succeeds, or nil if parsing fails.

HANDLER is a command that takes a value returned by the parser
and performs an arbitrary action."
  (declare (indent 1))
  (cl-assert (symbolp type))
  (cl-assert (and parser (functionp parser)))
  (cl-assert (and handler (functionp handler)))
  (setq omc--parsers (--remove (equal type (car it)) omc--parsers))
  (add-to-list 'omc--parsers
               (list :type type
                     :parser parser
                     :handler handler)))

;;; Parser utilities

(defun omc-value (prop msg)
  "Get the value of property PROP from message MSG.
Return nil if prop is not found."
  (cdr (assoc prop msg)))

(defun omc-matches? (regexp prop msg)
  "Test whether the value of a property in a message matches a regexp."
  (s-matches? regexp (omc-value prop msg)))

(provide 'org-mail-capture)

;;; org-mail-capture.el ends here
