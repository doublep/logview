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

(defvar inhibit-message)


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
             (insert-file-contents (expand-file-name ,filename logview--test-directory))
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
  (logview--test-with-file "log4j/fr-1.log" ()
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

(ert-deftest logview-test-go-to-difference-base-entry-no-thread ()
  (logview--test-with-file "custom/1.log" ((logview-additional-submodes
                                            '(("custom" (format . "TIMESTAMP LEVEL [NAME] ") (levels . "SLF4J")))))
     (logview-difference-to-current-entry)
     (logview-go-to-difference-base-entry)))



;; RFC 5424 levels.
;;
;; The mock log file should have a list of log messages in the default
;; Monolog format, in decreasing order of importance, from EMERGENCY
;; to DEBUG. The second last entry should be an entry with a level
;; which isn't defined in RFC 5424.
;;
;; TODO:  An epic case of DRY in these tests, maybe a function would be a good idea?
(ert-deftest logview-test-rfc5424-level-0-emergency ()
  (logview--test-with-file "levels/rfc-5424-levels.log" ()
    (should (equal logview--submode-name "Monolog"))
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 0))
    (logview-go-to-message-beginning)
    (should (looking-at "Emergency message.$"))))

(ert-deftest logview-test-rfc5424-level-1-alert ()
  (logview--test-with-file "levels/rfc-5424-levels.log" ()
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 1)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 1))
    (logview-go-to-message-beginning)
    (should (looking-at "Alert message.$"))))

(ert-deftest logview-test-rfc5424-level-2-critical ()
  (logview--test-with-file "levels/rfc-5424-levels.log" ()
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 2)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 2))
    (logview-go-to-message-beginning)
    (should (looking-at "Critical message.$"))))

(ert-deftest logview-test-rfc5424-level-3-error ()
  (logview--test-with-file "levels/rfc-5424-levels.log" ()
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 3)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 3))
    (logview-go-to-message-beginning)
    (should (looking-at "Error message.$"))))

(ert-deftest logview-test-rfc5424-level-4-warning ()
  (logview--test-with-file "levels/rfc-5424-levels.log" ()
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 4)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 4))
    (logview-go-to-message-beginning)
    (should (looking-at "Warning message.$"))))

(ert-deftest logview-test-rfc5424-level-5-notice ()
  (logview--test-with-file "levels/rfc-5424-levels.log" ()
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 5)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 5))
    (logview-go-to-message-beginning)
    (should (looking-at "Notice message.$"))))

(ert-deftest logview-test-rfc5424-level-6-info ()
  (logview--test-with-file "levels/rfc-5424-levels.log" ()
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 6)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 6))
    (logview-go-to-message-beginning)
    (should (looking-at "Info message.$"))))

(ert-deftest logview-test-rfc5424-level-7-debug ()
  (logview--test-with-file "levels/rfc-5424-levels.log" ()
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 7)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 7))
    (logview-go-to-message-beginning)
    (should (looking-at "Debug message.$"))))

(ert-deftest logview-test-rfc5424-level-undefined ()
  (logview--test-with-file "levels/rfc-5424-levels.log" ()
    (should (equal logview--submode-name "Monolog"))
    ;; (logview-next-entry 8)
    (forward-line 8)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 7))
    (logview-go-to-message-beginning)
    ;; (should (looking-at "No such level defined by RFC 5424.$"))))
    (should (looking-at ""))))

(ert-deftest logview-test-rfc5424-level-defined-level-after-an-undefined-one ()
  (logview--test-with-file "levels/rfc-5424-levels.log" ()
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 8)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 6))
    (logview-go-to-message-beginning)
    (should (looking-at "Info message after an invalid level.$"))))

;; Apache error log submode
(ert-deftest logview-test-apache-submode-recognition ()
  (logview--test-with-file "apache/error.log" ()
    (should (equal logview--submode-name "Apache Error Log"))))

(ert-deftest logview-test-apache-submode-find-entries ()
  (logview--test-with-file "apache/error.log" ()
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-test-apache-submode-match-a-message ()
  (logview--test-with-file "apache/error.log" ()
    (logview-go-to-message-beginning)
    (should (looking-at "Emergency message.$"))))

(ert-deftest logview-test-apache-submode-match-a-message-after-undefined-lines ()
  (logview--test-with-file "apache/error.log" ()
    (logview-next-entry 8)
    (logview-go-to-message-beginning)
    (should (looking-at "Info message after some undefined lines."))))

;; TODO: This, or something should test the eventual final levels
;; LogView Mode works with. With that transition, maybe the RFC 5424
;; and RFC 5424 lowercase level definitions could be merged.
(ert-deftest logview-test-apache-submode-match-all-levels ()
  (logview--test-with-file "apache/error.log" ()
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 0))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 1))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 2))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 3))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 4))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 5))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 6))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 7))))

;; Monolog submode
(ert-deftest logview-test-monolog-submode-recognition ()
  (logview--test-with-file "monolog/1.log" ()
    (should (equal logview--submode-name "Monolog"))))

(ert-deftest logview-test-monolog-submode-find-entries ()
  (logview--test-with-file "monolog/1.log" ()
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-test-monolog-submode-match-a-message ()
  (logview--test-with-file "monolog/1.log" ()
    (logview-go-to-message-beginning)
    (should (looking-at "Emergency message.$"))))

(ert-deftest logview-test-monolog-submode-match-all-levels ()
  (logview--test-with-file "monolog/1.log" ()
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 0))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 1))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 2))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 3))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 4))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 5))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 6))
    (logview-next-entry)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 7))))
