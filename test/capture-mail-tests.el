;;; capture-mail-tests.el --- Tests for capture-mail.el

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

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

;; Tests for capture-mail.el

;;; Code:

(require 'ert)

(defvar example-message "h1: 1\nh2: 2\n\nBody")

(ert-deftest successful-parse-has-type-in-car ()
  (cl-destructuring-bind (type . _)
      (cm--run-parsers example-message
                       '((:type success
                                :parser (lambda (msg) t)
                                :handler (lambda (it) t))))
    (should (equal 'success type))))

(ert-deftest successful-parse-has-result-in-cdr ()
  (cl-destructuring-bind (_ . parsed)
      (cm--run-parsers example-message
                       '((:type type
                                :parser (lambda (msg) (cdr (assoc 'body msg)))
                                :handler identity)))
    (should (equal "Body" parsed))))

(ert-deftest returns-first-successful-parse-result ()
  (cl-destructuring-bind (type . _)
      (cm--run-parsers example-message
                       '((:type a :parser (lambda (_) nil) :handler identity)
                         (:type b :parser identity :handler identity)
                         (:type c :parser identity :handler identity)))
    (should (equal 'b type))))

(ert-deftest parses-message-to-alist ()
  (should (equal '((body . "hello")
                   (from . "Jane")
                   (to . "Joe"))
                 (cm--message->alist
                  "From: Jane\nTo: Joe\n\nhello"))))

(provide 'capture-mail-tests)

;;; capture-mail-tests.el ends here
