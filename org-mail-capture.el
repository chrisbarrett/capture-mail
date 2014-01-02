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
(require 'cl-lib)

(defgroup org-mail-capture nil
  "Utilities for capturing emails with org-mode."
  :group 'org
  :prefix "omc--")

;; --------------------------- Internal -----------------------------------------

(defvar omc--parsers nil
  "Contains all the parsers and handlers.
Each element is a list of (TYPE PARSER HANDLER).")

(defun omc--validate-parser-spec (plist)
  "Assert that PLIST is a well-formed parser specification."
  (cl-assert (plist-get plist :type))
  (cl-assert (plist-get plist :parser))
  (cl-assert (functionp (plist-get plist :parser)))
  (cl-assert (plist-get plist :handler))
  (cl-assert (functionp (plist-get plist :handler))))

(defun omc--run-parsers (message parsers)
  "Run each parser over the given message until one succeeds.
Return a cons of the type and the parsed value.

MESSAGE is a plist of ([HEADERS...] BODY), where body is a string.

PARSERS is a plist of (TYPE PARSER HANDLER)."
  (-each parsers 'omc--validate-parser-spec)
  (cl-loop for p in parsers do
           (cl-destructuring-bind (&key type parser handler) p
             (-when-let (parsed-val (funcall parser message))
               (cl-return (cons type (funcall handler parsed-val)))))))

;; ------------------------- Public Interface ----------------------------------

(cl-defun omc-declare-message-parser (type &key parser handler)
  "Declare a maildir message parser.

TYPE is a symbol that identifies the type of the parsed message.

PARSER is a function that takes a plist of ([HEADERS...] BODY).
It should return the value needed by the HANDLER command if
parsing succeeds, or nil if parsing fails.

HANDLER is a command that takes a value returned by the parser
and performs an arbitrary action."
  (declare (indent 1))
  (cl-assert (symbolp type))
  (cl-assert (and parser (functionp parser)))
  (cl-assert (and handler (functionp handler)))
  (setq omc--parsers (--remove (equal type (car it)) omc--parsers))
  (add-to-list 'omc--parsers (list :type type
                                   :parser parser
                                   :handler handler)))

(provide 'org-mail-capture)

;;; org-mail-capture.el ends here
