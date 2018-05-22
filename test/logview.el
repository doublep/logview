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

(defmacro logview--test-with-file (filename extra-customizations &rest body)
  (declare (debug (form body))
           (indent 2))
  ;; We do the following to avoid customizations influence testing
  ;; results.  Of course, this has no visible effect when running
  ;; tests from command line, as customization never takes place in
  ;; this case to begin with.
  (let (erase-customizations)
    (dolist (customizable (custom-group-members 'logview nil))
      (when (eq (cadr customizable) 'custom-variable)
        (push (list (car customizable) (list 'quote (eval (car (get (car customizable) 'standard-value)) t))) erase-customizations)))
    `(let (,@erase-customizations
           ,@extra-customizations
           (inhibit-message t))
       ;; Not available on older 24.x versions.  Don't care enough to
       ;; rewrite differently.
       (when (fboundp 'advice-add)
         (advice-add 'display-warning :override #'logview--test-display-warning-advice))
       (unwind-protect
           (with-temp-buffer
             (insert-file (expand-file-name ,filename logview--test-directory))
             (logview-mode)
             ,@body)
         (when (fboundp 'advice-add)
           (advice-remove 'display-warning #'logview--test-display-warning-advice))))))


(ert-deftest logview-test-log4j-standard-1 ()
  (logview--test-with-file "log4j/en-1.log" ()
    (should (equal logview--submode-name "SLF4J"))
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-test-log4j-national-timestamp-1 ()
  (logview--test-with-file "log4j/it-1.log" ()
    (should (equal logview--submode-name "SLF4J"))))

;; Issue #2.
(ert-deftest logview-test-log4j-parens-in-thread-name ()
  (logview--test-with-file "log4j/parens-in-thread-name.log" ()
    (should (equal logview--submode-name "SLF4J"))
    ;; Make sure that the second line is also recognized as an entry.
    ;; If it isn't, this will signal an error.
    (logview-next-entry)))

(ert-deftest logview-test-go-to-message-beginning-1 ()
  (logview--test-with-file "log4j/navigation-1.log" ()
    (should (equal logview--submode-name "SLF4J"))
    (forward-line 2)
    (logview-go-to-message-beginning)
    (should (looking-at "message 3$"))))

(ert-deftest logview-test-go-to-message-beginning-2 ()
  (logview--test-with-file "log4j/navigation-1.log" ()
    (should (equal logview--submode-name "SLF4J"))
    (transient-mark-mode 1)
    (forward-line 2)
    (logview-go-to-message-beginning t)
    (should (looking-at "message 3$"))
    (should (string= (buffer-substring-no-properties (region-beginning) (region-end)) "message 3"))
    (should (use-region-p))))

(ert-deftest logview-test-unix-standard-1 ()
  (logview--test-with-file "unix/1.log" ()
    (should (equal logview--submode-name "UNIX"))
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-test-custom-submode-1 ()
  (logview--test-with-file "custom/1.log" ((logview-additional-submodes
                                            '(("custom" (format . "TIMESTAMP LEVEL [NAME] ") (levels . "SLF4J")))))
    (should (equal logview--submode-name "custom"))
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))
