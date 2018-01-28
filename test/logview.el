;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018 Paul Pogonyshev

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses.


(require 'logview)
(require 'ert)
(require 'cus-edit)


(defvar logview--test-directory (file-name-directory (or load-file-name (buffer-file-name))))


(defun logview--test-display-warning-advice (&rest arguments)
  (error "Warning elevated to an error: %S" arguments))

(defmacro logview--test-with-file (filename &rest body)
  (declare (debug (form body))
           (indent 1))
  ;; We do the following to avoid customizations influence testing
  ;; results.  Of course, this has no visible effect when running
  ;; tests from command line, as customization never takes place in
  ;; this case to begin with.
  (let (erase-customizations)
    (dolist (customizable (custom-group-members 'logview nil))
      (when (eq (cadr customizable) 'custom-variable)
        (push (list (car customizable) (list 'quote (eval (car (get (car customizable) 'standard-value)) t))) erase-customizations)))
    `(let (,@erase-customizations
           (inhibit-message t))
       (advice-add 'display-warning :override #'logview--test-display-warning-advice)
       (unwind-protect
           (with-temp-buffer
             (insert-file (expand-file-name ,filename logview--test-directory))
             (logview-mode)
             ,@body)
         (advice-remove 'display-warning #'logview--test-display-warning-advice)))))


(ert-deftest logview-test-log4j-standard-1 ()
  (logview--test-with-file "log4j/en-1.log"
    (should (equal logview--submode-name "SLF4J"))))

(ert-deftest logview-test-log4j-national-timestamp-1 ()
  (logview--test-with-file "log4j/it-1.log"
    (should (equal logview--submode-name "SLF4J"))))

;; Issue #2.
(ert-deftest logview-test-log4j-parens-in-thread-name ()
  (logview--test-with-file "log4j/parens-in-thread-name.log"
    (should (equal logview--submode-name "SLF4J"))
    ;; Make sure that the second line is also recognized as an entry.
    ;; If it isn't, this will signal an error.
    (logview-next-entry)))
