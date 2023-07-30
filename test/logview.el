;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Paul Pogonyshev

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
(require 'subr-x)


(defvar logview--test-directory (file-name-directory (or load-file-name (buffer-file-name))))

(defvar inhibit-message)


(defun logview--test-display-warning-advice (&rest arguments)
  (error "Warning elevated to an error: %S" arguments))

(defmacro logview--test-with-file (filename &rest body)
  (declare (debug (form body))
           (indent 1))
  ;; We do the following to avoid customizations influence testing
  ;; results.  Of course, this has no visible effect when running
  ;; tests from command line, as customization never takes place in
  ;; this case to begin with.
  (let (erase-customizations
        extra-customizations
        buffer-mode)
    (while (keywordp (car body))
      (pcase-exhaustive (pop body)
        (:extra-customizations (setf extra-customizations (eval (pop body) t)))
        (:buffer-mode          (setf buffer-mode          (pop body)))))
    (dolist (customizable (custom-group-members 'logview nil))
      (when (eq (cadr customizable) 'custom-variable)
        (push (list (car customizable) (list 'quote (eval (car (get (car customizable) 'standard-value)) t))) erase-customizations)))
    `(let (,@erase-customizations
           ,@extra-customizations
           (inhibit-message t))
       (advice-add 'display-warning :override #'logview--test-display-warning-advice)
       (unwind-protect
           (with-temp-buffer
             (insert-file-contents (expand-file-name ,filename logview--test-directory))
             (,(or buffer-mode 'logview-mode))
             ,@body)
         (advice-remove 'display-warning #'logview--test-display-warning-advice)))))


(defun logview--test-current-message ()
  (logview--std-temporarily-widening
    (logview--locate-current-entry entry start
      (string-trim (logview--entry-message entry start)))))

(defun logview--test-position-at-entry-with (message)
  (goto-char (point-min))
  (re-search-forward (rx-to-string `(seq ,message eol))))


(defmacro logview--subtest (save-excursion operation &rest etc)
  (declare (debug (form form body))
           (indent 2))
  `(,(if (eval save-excursion t) 'save-excursion 'progn)
    (ert-info ((format "operation: %S" ',operation))
      ,operation
      ,@etc)))


(ert-deftest logview-test-log4j-standard-1 ()
  (logview--test-with-file "log4j/en-1.log"
    (should (equal logview--submode-name "SLF4J"))
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-test-log4j-standard-2 ()
  ;; The start of the first entry in this file is not on the first line.
  (logview--test-with-file "log4j/part.log"
    (should (equal logview--submode-name "SLF4J"))
    (logview--locate-current-entry entry start
      ;; Adjust the number accordingly if you change that file for whatever reason.
      (should (and entry (equal start 174))))))

(ert-deftest logview-test-log4j-national-timestamp-1 ()
  (logview--test-with-file "log4j/fr-1.log"
    (should (equal logview--submode-name "SLF4J"))))

(ert-deftest logview-test-log4j-national-timestamp-2 ()
  ;; It's the same as above, but without comma in the timestamp.  See
  ;; `logview--all-timestamp-formats'.
  (logview--test-with-file "log4j/fr-2.log"
    (should (equal logview--submode-name "SLF4J"))))

;; Issue #2.
(ert-deftest logview-test-log4j-parens-in-thread-name ()
  (logview--test-with-file "log4j/parens-in-thread-name.log"
    (should (equal logview--submode-name "SLF4J"))
    ;; Make sure that the second line is also recognized as an entry.
    ;; If it isn't, this will signal an error.
    (logview-next-entry)))

(ert-deftest logview-test-go-to-message-beginning-1 ()
  (logview--test-with-file "log4j/navigation-1.log"
    (should (equal logview--submode-name "SLF4J"))
    (forward-line 2)
    (logview-go-to-message-beginning)
    (should (looking-at "message 3$"))))

(ert-deftest logview-test-go-to-message-beginning-2 ()
  (logview--test-with-file "log4j/navigation-1.log"
    (should (equal logview--submode-name "SLF4J"))
    (transient-mark-mode 1)
    (forward-line 2)
    (logview-go-to-message-beginning t)
    (should (looking-at "message 3$"))
    (should (string= (buffer-substring-no-properties (region-beginning) (region-end)) "message 3"))
    (should (use-region-p))))

(ert-deftest logview-test-unix-standard-1 ()
  (logview--test-with-file "unix/1.log"
    (should (equal logview--submode-name "UNIX"))
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-test-custom-submode-1 ()
  (logview--test-with-file "custom/1.log"
    :extra-customizations '((logview-additional-submodes
                             '(("custom" (format . "TIMESTAMP LEVEL [NAME] ") (levels . "SLF4J")))))
    (should (equal logview--submode-name "custom"))
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-test-go-to-difference-base-entry-no-thread ()
  (logview--test-with-file "custom/1.log"
    :extra-customizations '((logview-additional-submodes
                             '(("custom" (format . "TIMESTAMP LEVEL [NAME] ") (levels . "SLF4J")))))
    (logview-difference-to-current-entry)
    (logview-go-to-difference-base-entry)))

;; Bug: Logview would ignore entry lines if they didn't contain a space at the end.  This
;; would e.g. happen if you had code like 'log.info ("\n...");' in your program.
(ert-deftest logview-test-multiline-entries ()
  (logview--test-with-file "log4j/multiline-entries.log"
    (should (equal logview--submode-name "SLF4J"))
    ;; There are three entries in the file.
    (logview-next-entry)
    (logview-next-entry)
    (should-error (logview-next-entry) :type 'user-error)))


;; RFC 5424 levels.
;;
;; The mock log file should have a list of log messages in the default
;; Monolog format, in decreasing order of importance, from EMERGENCY
;; to DEBUG. The second last entry should be an entry with a level
;; which isn't defined in RFC 5424.
;;
;; TODO:  An epic case of DRY in these tests, maybe a function would be a good idea?
(ert-deftest logview-test-rfc5424-level-0-emergency ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 0))
    (logview-go-to-message-beginning)
    (should (looking-at "Emergency message.$"))))

(ert-deftest logview-test-rfc5424-level-1-alert ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 1)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 1))
    (logview-go-to-message-beginning)
    (should (looking-at "Alert message.$"))))

(ert-deftest logview-test-rfc5424-level-2-critical ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 2)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 2))
    (logview-go-to-message-beginning)
    (should (looking-at "Critical message.$"))))

(ert-deftest logview-test-rfc5424-level-3-error ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 3)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 3))
    (logview-go-to-message-beginning)
    (should (looking-at "Error message.$"))))

(ert-deftest logview-test-rfc5424-level-4-warning ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 4)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 4))
    (logview-go-to-message-beginning)
    (should (looking-at "Warning message.$"))))

(ert-deftest logview-test-rfc5424-level-5-notice ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 5)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 5))
    (logview-go-to-message-beginning)
    (should (looking-at "Notice message.$"))))

(ert-deftest logview-test-rfc5424-level-6-info ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 6)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 6))
    (logview-go-to-message-beginning)
    (should (looking-at "Info message.$"))))

(ert-deftest logview-test-rfc5424-level-7-debug ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 7)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 7))
    (logview-go-to-message-beginning)
    (should (looking-at "Debug message.$"))))

(ert-deftest logview-test-rfc5424-level-undefined ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    ;; (logview-next-entry 8)
    (forward-line 8)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 7))
    (logview-go-to-message-beginning)
    ;; (should (looking-at "No such level defined by RFC 5424.$"))))
    (should (looking-at ""))))

(ert-deftest logview-test-rfc5424-level-defined-level-after-an-undefined-one ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 8)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 6))
    (logview-go-to-message-beginning)
    (should (looking-at "Info message after an invalid level.$"))))

;; Apache error log submode
(ert-deftest logview-test-apache-submode-recognition ()
  (logview--test-with-file "apache/error.log"
    (should (equal logview--submode-name "Apache Error Log"))))

(ert-deftest logview-test-apache-submode-find-entries ()
  (logview--test-with-file "apache/error.log"
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-test-apache-submode-match-a-message ()
  (logview--test-with-file "apache/error.log"
    (logview-go-to-message-beginning)
    (should (looking-at "Emergency message.$"))))

(ert-deftest logview-test-apache-submode-match-a-message-after-undefined-lines ()
  (logview--test-with-file "apache/error.log"
    (logview-next-entry 8)
    (logview-go-to-message-beginning)
    (should (looking-at "Info message after some undefined lines."))))

;; TODO: This, or something should test the eventual final levels
;; LogView Mode works with. With that transition, maybe the RFC 5424
;; and RFC 5424 lowercase level definitions could be merged.
(ert-deftest logview-test-apache-submode-match-all-levels ()
  (logview--test-with-file "apache/error.log"
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
  (logview--test-with-file "monolog/1.log"
    (should (equal logview--submode-name "Monolog"))))

(ert-deftest logview-test-monolog-submode-find-entries ()
  (logview--test-with-file "monolog/1.log"
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-test-monolog-submode-match-a-message ()
  (logview--test-with-file "monolog/1.log"
    (logview-go-to-message-beginning)
    (should (looking-at "Emergency message.$"))))

(ert-deftest logview-test-monolog-submode-match-all-levels ()
  (logview--test-with-file "monolog/1.log"
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


;; This is a huge test, but I find it easier to test various combinations this way.
;;
;; Expected results of `logview-*-section-any-thread' may seem wrong, but remember that
;; sections temporary count as not-thread-bound for these functions, so certain lines
;; might count as belonging to unexpected sections.  All other behaviors I have considered
;; have their own downsides and surprising results as well, so I chose the one easiest to
;; implement.
(ert-deftest logview-test-sections-1 ()
  (logview--test-with-file "log4j/sections-1.log"
    :extra-customizations '((logview--views
                             '((:name "sections" :filters "lv INFO\na+ my\\.Server\nm+ serving request")))
                            (logview--views-initialized t)
                            (logview--views-need-saving nil))
    (logview-set-section-view "sections")
    (dolist (narrowing '((1 . 1) (2 . 1)))
      (save-restriction
        (when (> (car narrowing) 1)
          (goto-char (point-min))
          (logview-next-entry (1- (car narrowing)))
          (logview-narrow-from-this-entry))
        (when (> (cdr narrowing) 1)
          (goto-char (point-max))
          (logview-previous-entry (1- (cdr narrowing)))
          (logview-narrow-up-to-this-entry))
        (ert-info ((format "narrowing in effect: %s"
                           (if (equal narrowing '(1 . 1))
                               "none"
                             (format "%s-%s" (if (= (car narrowing) 1) "" (car narrowing)) (if (= (cdr narrowing) 1) "" (cdr narrowing))))))
          (let* ((have-first-line       (= (car narrowing) 1))
                 (first-visible-message (if have-first-line "starting up" "before any sections")))
            ;; "Section zero", i.e. before any section headers.
            (dolist (message '("starting up" "before any sections" "before any sections continued"))
              (when (or have-first-line (not (string= message "starting up")))
                (logview--test-position-at-entry-with message)
                (ert-info ((format "operating from line \"%s\"" message))
                  (logview--subtest t (logview-go-to-section-beginning)
                    (should (string= (logview--test-current-message) first-visible-message)))
                  (logview--subtest t (logview-go-to-section-end)
                    (should (string= (logview--test-current-message) "before any sections continued")))
                  (logview--subtest t (logview-next-section)
                    (should (string= (logview--test-current-message) "serving request 1")))
                  ;; There is no previous section, obviously.
                  (logview--subtest t (should-error (logview-previous-section) :type 'user-error)
                    (should (string= (logview--test-current-message) first-visible-message)))
                  (logview--subtest t (logview-next-section-any-thread)
                    (should (string= (logview--test-current-message) "serving request 1")))
                  (logview--subtest t (should-error (logview-previous-section-any-thread) :type 'user-error)
                    (should (string= (logview--test-current-message) first-visible-message)))
                  (logview--subtest t (logview-first-section)
                    (should (string= (logview--test-current-message) first-visible-message)))
                  (logview--subtest t (logview-first-section-any-thread)
                    (should (string= (logview--test-current-message) first-visible-message)))
                  (logview--subtest t (logview-last-section)
                    (should (string= (logview--test-current-message) "serving request 2")))
                  (logview--subtest t (logview-last-section-any-thread)
                    (should (string= (logview--test-current-message) "serving request 4 (in a different thread)"))))))
            ;; First real section.
            (dolist (message '("serving request 1" "inside section 1" "doing stuff (section 1)" "doing more stuff (section 1)"))
              (logview--test-position-at-entry-with message)
              (ert-info ((format "operating from line \"%s\"" message))
                (logview--subtest t (logview-go-to-section-beginning)
                  (should (string= (logview--test-current-message) "serving request 1")))
                (logview--subtest t (logview-go-to-section-end)
                  (should (string= (logview--test-current-message) "doing more stuff (section 1)")))
                (logview--subtest t (logview-next-section)
                  (should (string= (logview--test-current-message) "serving request 2")))
                (logview--subtest t (logview-previous-section)
                  (should (string= (logview--test-current-message) first-visible-message)))
                (logview--subtest t (logview-next-section-any-thread)
                  (should (string= (logview--test-current-message) "serving request 2")))
                (logview--subtest t (logview-previous-section-any-thread)
                  (should (string= (logview--test-current-message) first-visible-message)))
                (logview--subtest t (logview-first-section)
                  (should (string= (logview--test-current-message) first-visible-message)))
                (logview--subtest t (logview-first-section-any-thread)
                  (should (string= (logview--test-current-message) first-visible-message)))
                (logview--subtest t (logview-last-section)
                  (should (string= (logview--test-current-message) "serving request 2")))
                (logview--subtest t (logview-last-section-any-thread)
                  (should (string= (logview--test-current-message) "serving request 4 (in a different thread)")))))
            ;; Second real section, intervined with sections 3 and 4 in a different thread.
            (dolist (message '("serving request 2" "inside section 2" "doing stuff (section 2)" "doing more stuff (section 2)"))
              (logview--test-position-at-entry-with message)
              (ert-info ((format "operating from line \"%s\"" message))
                (logview--subtest t (logview-go-to-section-beginning)
                  (should (string= (logview--test-current-message) "serving request 2")))
                (logview--subtest t (logview-go-to-section-end)
                  (should (string= (logview--test-current-message) "doing more stuff (section 2)")))
                ;; There is no next section in this thread.
                (logview--subtest t (should-error (logview-next-section) :type 'user-error)
                  (should (string= (logview--test-current-message) "doing more stuff (section 2)")))
                (logview--subtest t (logview-previous-section)
                  (should (string= (logview--test-current-message) "serving request 1")))
                ;; Results of `logview-*-section-any-thread' depend on the starting line.
                (pcase message
                  ("serving request 2"
                   (logview--subtest t (logview-next-section-any-thread)
                     (should (string= (logview--test-current-message) "serving request 3 (in a different thread)"))))
                  ("doing more stuff (section 2)"
                   (logview--subtest t (should-error (logview-next-section-any-thread) :type 'user-error)
                     (should (string= (logview--test-current-message) "doing more stuff (section 4)"))))
                  (_
                   (logview--subtest t (logview-next-section-any-thread)
                     (should (string= (logview--test-current-message) "serving request 4 (in a different thread)")))))
                (pcase message
                  ("serving request 2"
                   (logview--subtest t (logview-previous-section-any-thread)
                     (should (string= (logview--test-current-message) "serving request 1"))))
                  ("doing more stuff (section 2)"
                   (logview--subtest t (logview-previous-section-any-thread)
                     (should (string= (logview--test-current-message) "serving request 3 (in a different thread)"))))
                  (_
                   (logview--subtest t (logview-previous-section-any-thread)
                     (should (string= (logview--test-current-message) "serving request 2")))))
                (logview--subtest t (logview-first-section)
                  (should (string= (logview--test-current-message) first-visible-message)))
                (logview--subtest t (logview-first-section-any-thread)
                  (should (string= (logview--test-current-message) first-visible-message)))
                (logview--subtest t (logview-last-section)
                  (should (string= (logview--test-current-message) "serving request 2")))
                (logview--subtest t (logview-last-section-any-thread)
                  (should (string= (logview--test-current-message) "serving request 4 (in a different thread)")))))
            ;; Third real section, intervined with section 2 in a different thread.
            (dolist (message '("serving request 3 (in a different thread)" "inside section 3" "doing stuff (section 3)" "doing more stuff (section 3)"))
              (logview--test-position-at-entry-with message)
              (ert-info ((format "operating from line \"%s\"" message))
                (logview--subtest t (logview-go-to-section-beginning)
                  (should (string= (logview--test-current-message) "serving request 3 (in a different thread)")))
                (logview--subtest t (logview-go-to-section-end)
                  (should (string= (logview--test-current-message) "doing more stuff (section 3)")))
                (logview--subtest t (logview-next-section)
                  (should (string= (logview--test-current-message) "serving request 4 (in a different thread)")))
                ;; No previous section in _this thread_.
                (logview--subtest t (should-error (logview-previous-section) :type 'user-error)
                  (should (string= (logview--test-current-message) "serving request 3 (in a different thread)")))
                (logview--subtest t (logview-next-section-any-thread)
                  (should (string= (logview--test-current-message) "serving request 4 (in a different thread)")))
                (logview--subtest t (logview-previous-section-any-thread)
                  (should (string= (logview--test-current-message) "serving request 2")))
                (logview--subtest t (logview-first-section)
                  (should (string= (logview--test-current-message) "serving request 3 (in a different thread)")))
                (logview--subtest t (logview-first-section-any-thread)
                  (should (string= (logview--test-current-message) first-visible-message)))
                (logview--subtest t (logview-last-section)
                  (should (string= (logview--test-current-message) "serving request 4 (in a different thread)")))
                (logview--subtest t (logview-last-section-any-thread)
                  (should (string= (logview--test-current-message) "serving request 4 (in a different thread)")))))
            ;; Fourth section, the very last in the file.
            (dolist (message '("serving request 4 (in a different thread)" "inside section 4" "doing stuff (section 4)" "doing more stuff (section 4)"))
              (logview--test-position-at-entry-with message)
              (ert-info ((format "operating from line \"%s\"" message))
                (logview--subtest t (logview-go-to-section-beginning)
                  (should (string= (logview--test-current-message) "serving request 4 (in a different thread)")))
                (logview--subtest t (logview-go-to-section-end)
                  (should (string= (logview--test-current-message) "doing more stuff (section 4)")))
                ;; There is no next section in this thread.
                (logview--subtest t (should-error (logview-next-section) :type 'user-error)
                  (should (string= (logview--test-current-message) "doing more stuff (section 4)")))
                (logview--subtest t (logview-previous-section)
                  (should (string= (logview--test-current-message) "serving request 3 (in a different thread)")))
                ;; No next section at all.
                (logview--subtest t (should-error (logview-next-section-any-thread) :type 'user-error)
                  (should (string= (logview--test-current-message) "doing more stuff (section 4)")))
                (logview--subtest t (logview-previous-section-any-thread)
                  (should (string= (logview--test-current-message) "serving request 3 (in a different thread)")))
                (logview--subtest t (logview-first-section)
                  (should (string= (logview--test-current-message) "serving request 3 (in a different thread)")))
                (logview--subtest t (logview-first-section-any-thread)
                  (should (string= (logview--test-current-message) first-visible-message)))
                (logview--subtest t (logview-last-section)
                  (should (string= (logview--test-current-message) "serving request 4 (in a different thread)")))
                (logview--subtest t (logview-last-section-any-thread)
                  (should (string= (logview--test-current-message) "serving request 4 (in a different thread)")))))))))))


(define-derived-mode logview--test-derived-mode logview-mode
  "Logview-derived"
  (font-lock-add-keywords nil `((,(rx bow "Class" eow) (0 'bold prepend)))  t))

(ert-deftest logview-test-logview-derived-mode-1 ()
  (logview--test-with-file "log4j/en-1.log"
    :buffer-mode logview--test-derived-mode
    (font-lock-ensure)
    (search-forward "Class")
    (backward-word)
    (let ((faces (get-text-property (point) 'face)))
      (should (memq 'logview-name              faces))
      (should (memq 'logview-information-entry faces))
      (should (memq 'bold                      faces)))))
