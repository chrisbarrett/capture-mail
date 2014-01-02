;;; org-mail-capture-tests.el --- Tests for org-mail-capture.el

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

;; Tests for org-mail-capture.el

;;; Code:

(require 'ert)

(ert-deftest successful-parse-has-type-in-car ()
  (cl-destructuring-bind (type . _)
      (omc--run-parsers t
                        '((:type success
                                 :parser (lambda (msg) t)
                                 :handler (lambda (it) t))))
    (should (equal 'success type))))

(ert-deftest successful-parse-has-result-in-cdr ()
  (cl-destructuring-bind (_ . parsed)
      (omc--run-parsers '(:body "success")
                        '((:type type
                                 :parser (lambda (msg) (plist-get msg :body))
                                 :handler identity)))
    (should (equal "success" parsed))))

(ert-deftest returns-first-successful-parse-result ()
  (cl-destructuring-bind (type . _)
      (omc--run-parsers t
                        '((:type a :parser (lambda (_) nil) :handler identity)
                          (:type b :parser identity :handler identity)
                          (:type c :parser identity :handler identity)))
    (should (equal 'b type))))

(ert-deftest parses-message-to-alist ()
  (should (equal '((body . "hello")
                   (from . "Jane")
                   (to . "Joe"))
                 (omc--message->alist
                  "From: Jane\nTo: Joe\n\nhello"))))

(provide 'org-mail-capture-tests)

;;; org-mail-capture-tests.el ends here
