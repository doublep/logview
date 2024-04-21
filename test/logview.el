;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024 Paul Pogonyshev

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


(define-error 'logview-test-expected-error "Must be caught")

(defvar logview--test-directory (file-name-directory (or load-file-name (buffer-file-name))))

(defvar inhibit-message)


;; Copied from Eldev source code, see documentation there.
(defmacro logview--advised (spec &rest body)
  (declare (indent 1) (debug (sexp body)))
  (let ((symbol   (nth 0 spec))
        (where    (nth 1 spec))
        (function (nth 2 spec))
        (props    (nthcdr 3 spec))
        (fn       (make-symbol "$fn")))
    `(let ((,fn ,function))
       (when ,fn
         (if (advice-member-p ,fn ,symbol)
             (setf ,fn nil)
           (advice-add ,symbol ,where ,fn ,@props)))
       (unwind-protect
           ,(macroexp-progn body)
         (when ,fn
           (advice-remove ,symbol ,fn))))))


(defmacro logview--test-with-restriction (start end locking-label &rest body)
  (declare (indent 3))
  (if (fboundp 'with-restriction)
      `(with-restriction ,start ,end
         :label ,locking-label
         ,@body)
    `(progn (ignore ,locking-label)
            (narrow-to-region ,start ,end)
            ,@body)))


(ert-deftest logview--temporarily-widening ()
  (with-temp-buffer
    (insert "foo bar baz")
    ;; {LOCKED-NARROWING}
    ;; Emulate the retarded locked narrowing with "standard" tags.  If someone uses a
    ;; custom tag, Logview will have to fail, because it won't be able to work without
    ;; full buffer access, but oh well, this is Emacs for you.  They allowed peasants to
    ;; unlock at least something.
    ;;
    ;; Testing without emulation, with real Emacs-imposed locking seems unfeasible, since
    ;; relevant font-locking code is not activated in batch mode.
    (dolist (tag '(long-line-optimizations-in-fontification-functions long-line-optimizations-in-command-hooks))
      (logview--test-with-restriction 5 8 tag
        (should (string= (buffer-string) "bar"))
        (logview--temporarily-widening
          (should (string= (buffer-string) "foo bar baz")))))))


(ert-deftest logview-mode ()
  (with-temp-buffer
    (insert "2020-01-01 00:00:00 [thread 1] INFO hello - world\n")
    (logview-mode)
    (should (eq major-mode 'logview-mode))))


(ert-deftest logview-mode-unsuccessful-setup ()
  (condition-case nil
      (logview--advised (#'logview--set-up :override (lambda () (signal 'logview-test-expected-error nil)))
        (with-temp-buffer
          (logview-mode)
          ;; Errors during mode setup must not leave the mode half-initialized.
          (should (eq major-mode 'fundamental-mode))))
    (logview-test-expected-error)))


(defmacro logview--test-with-file (filename &rest body)
  "Activate Logview in a temporary buffer with contents of the file.
Instead of visiting the file, this macro creates a new buffer for
it.  This avoids annoying clashes if the file is already open
(when in interactive mode) and also allows to further modify
buffer if the test needs that."
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
      ;; Byte-compiled `let' with double-binding for the same variable behaves differently
      ;; than non-byte-compiled...  Avoid double-bindings to dodge this.
      (when (and (eq (cadr customizable) 'custom-variable) (not (assq (car customizable) extra-customizations)))
        (push (list (car customizable) (list 'quote (eval (car (get (car customizable) 'standard-value)) t))) erase-customizations)))
    `(let (,@erase-customizations
           ,@extra-customizations
           (inhibit-message t))
       (logview--advised ('display-warning :override (lambda (&rest arguments)
                                                       (error "Warning elevated to an error: %S" arguments)))
         (with-temp-buffer
           (insert-file-contents (expand-file-name ,filename logview--test-directory))
           (,(or buffer-mode 'logview-mode))
           ,@body)))))


(defun logview--test-user-visible-buffer-string ()
  (font-lock-ensure)
  (let ((from (point-min))
        chunks)
    (while from
      (let ((to (next-property-change from)))
        (unless (invisible-p from)
          (let ((display-as (get-text-property from 'display)))
            (unless (and display-as to (eq display-as (get-text-property to 'display)))
              (push (cond ((stringp display-as) display-as)
                          (display-as           (prin1-to-string display-as))
                          (t                    (buffer-substring-no-properties from (or to (point-max)))))
                    chunks))))
        (setf from to)))
    (mapconcat #'identity (nreverse chunks) nil)))


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


;; This can get called from `logview--test-with-file' at compilation time.
(eval-and-compile
  (defun logview--test-view-customizations (&rest views)
    `((logview--views             '(,@views))
      (logview--views-initialized t)
      (logview--views-need-saving nil))))


(ert-deftest logview-log4j-standard-1 ()
  (logview--test-with-file "log4j/en-1.log"
    (should (equal logview--submode-name "SLF4J"))
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-log4j-standard-2 ()
  ;; The start of the first entry in this file is not on the first line.
  (logview--test-with-file "log4j/part.log"
    (should (equal logview--submode-name "SLF4J"))
    (logview--locate-current-entry entry start
      ;; Adjust the number accordingly if you change that file for whatever reason.
      (should (and entry (equal start 174))))))

(ert-deftest logview-log4j-national-timestamp-1 ()
  (logview--test-with-file "log4j/fr-1.log"
    (should (equal logview--submode-name "SLF4J"))))

(ert-deftest logview-log4j-national-timestamp-2 ()
  ;; It's the same as above, but without comma in the timestamp.  See
  ;; `logview--all-timestamp-formats'.
  (logview--test-with-file "log4j/fr-2.log"
    (should (equal logview--submode-name "SLF4J"))))

;; Issue #2.
(ert-deftest logview-log4j-parens-in-thread-name ()
  (logview--test-with-file "log4j/parens-in-thread-name.log"
    (should (equal logview--submode-name "SLF4J"))
    ;; Make sure that the second line is also recognized as an entry.
    ;; If it isn't, this will signal an error.
    (logview-next-entry)))

(ert-deftest logview-go-to-message-beginning-1 ()
  (logview--test-with-file "log4j/navigation-1.log"
    (should (equal logview--submode-name "SLF4J"))
    (forward-line 2)
    (logview-go-to-message-beginning)
    (should (looking-at "message 3$"))))

(ert-deftest logview-go-to-message-beginning-2 ()
  (logview--test-with-file "log4j/navigation-1.log"
    (should (equal logview--submode-name "SLF4J"))
    (transient-mark-mode 1)
    (forward-line 2)
    (logview-go-to-message-beginning t)
    (should (looking-at "message 3$"))
    (should (string= (buffer-substring-no-properties (region-beginning) (region-end)) "message 3"))
    (should (use-region-p))))

(ert-deftest logview-unix-standard-1 ()
  (logview--test-with-file "unix/1.log"
    (should (equal logview--submode-name "UNIX"))
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-custom-submode-1 ()
  (logview--test-with-file "custom/1.log"
    :extra-customizations '((logview-additional-submodes
                             '(("custom" (format . "TIMESTAMP LEVEL [NAME] ") (levels . "SLF4J")))))
    (should (equal logview--submode-name "custom"))
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-go-to-difference-base-entry-no-thread ()
  (logview--test-with-file "custom/1.log"
    :extra-customizations '((logview-additional-submodes
                             '(("custom" (format . "TIMESTAMP LEVEL [NAME] ") (levels . "SLF4J")))))
    (logview-difference-to-current-entry)
    (logview-go-to-difference-base-entry)))

(ert-deftest logview-hidden-difference-base ()
  (logview--test-with-file "log4j/sections-1.log"
    ;; Testing only one line, but it should hopefully be the same for other lines.
    (should (string-match-p (rx bol "2010-03-10 20:03:44.100 [thread 1] DEBUG my.Class - before any sections" eol)
                            (logview--test-user-visible-buffer-string)))
    (logview-next-entry)
    (logview-difference-to-current-entry)
    (should (string-match-p (rx bol "                 -0.100 [thread 1] DEBUG my.Startup - starting up" eol)
                            (logview--test-user-visible-buffer-string)))
    (should (string-match-p (rx bol "2010-03-10 20:03:44.100 [thread 1] DEBUG my.Class - before any sections" eol)
                            (logview--test-user-visible-buffer-string)))
    (goto-char 1)
    (logview-go-to-difference-base-entry)
    (should (looking-at (rx "before any sections" eol)))
    (logview-show-errors-warnings-and-information)
    ;; Must be filtered out and invisible now.
    (should-not (string-match-p (rx bol "2010-03-10 20:03:44.100 [thread 1] DEBUG my.Class - before any sections" eol)
                                (logview--test-user-visible-buffer-string)))
    (should-error (logview-go-to-difference-base-entry) :type 'user-error)))

(ert-deftest logview-time-differences-after-full-buffer-reload ()
  ;; Using a test file with different timestamps for different entries.
  (logview--test-with-file "log4j/sections-1.log"
    ;; Testing only one line, but it should hopefully be the same for other lines.
    (should (string-match-p (rx bol "2010-03-10 20:03:44.100 [thread 1] DEBUG my.Class - before any sections" eol)
                            (logview--test-user-visible-buffer-string)))
    (logview-difference-to-current-entry)
    ;; This first entry should stay the same (not because it's the first, but because it's
    ;; the difference base).
    (should (string-match-p (rx bol "2010-03-10 20:03:44.000 [thread 1] DEBUG my.Startup - starting up" eol)
                            (logview--test-user-visible-buffer-string)))
    (should (string-match-p (rx bol "                 +0.100 [thread 1] DEBUG my.Class - before any sections" eol)
                            (logview--test-user-visible-buffer-string)))
    (goto-char (point-max))
    (logview-go-to-difference-base-entry)
    (should (looking-at (rx "starting up" eol)))
    ;; Emulate the log being fully changed, e.g. due to file rotation.
    (logview--std-altering
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (rx "20:03:") nil t)
          (replace-match "20:05:")
          (end-of-line)
          (insert " (changed)"))))
    (logview--invalidate-region-entries (point-min) (point-max))
    ;; Same buffer position, but the entry is now different, so must display time
    ;; difference.
    (should (string-match-p (rx bol "               +120.000 [thread 1] DEBUG my.Startup - starting up (changed)" eol)
                            (logview--test-user-visible-buffer-string)))
    (should (string-match-p (rx bol "               +120.100 [thread 1] DEBUG my.Class - before any sections (changed)" eol)
                            (logview--test-user-visible-buffer-string)))))


;; See https://github.com/doublep/logview/issues/48 for rationale to have this at all.
(ert-deftest logview-custom-submode-with-special-regexp ()
  (logview--test-with-file "custom/2.log"
    :extra-customizations '((logview-additional-submodes
                             '(("custom" (format . "TIMESTAMP IGNORED LEVEL T: <<RX:THREAD:.+?>> NAME - MESSAGE") (levels . "SLF4J")))))
    (should (equal logview--submode-name "custom"))
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1)))
      (should (equal (logview--entry-group entry start logview--name-group)   "WhateverName"))
      (should (equal (logview--entry-group entry start logview--thread-group) "Fake Thread")))
    (forward-line)
    (logview--locate-current-entry entry start
      (should entry)
      (should (equal (logview--entry-group entry start logview--name-group)   "LottieLockView"))
      (should (equal (logview--entry-group entry start logview--thread-group) "Subscription Manager Consumer Thread")))
    (forward-line)
    (logview--locate-current-entry entry start
      (should entry)
      (should (equal (logview--entry-group entry start logview--name-group)   "LottieLockView"))
      (should (equal (logview--entry-group entry start logview--thread-group) "pool-40-thread-1")))))

;; Bug: Logview would ignore entry lines if they didn't contain a space at the end.  This
;; would e.g. happen if you had code like 'log.info ("\n...");' in your program.
(ert-deftest logview-multiline-entries ()
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
(ert-deftest logview-rfc5424-level-0-emergency ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 0))
    (logview-go-to-message-beginning)
    (should (looking-at "Emergency message.$"))))

(ert-deftest logview-rfc5424-level-1-alert ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 1)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 1))
    (logview-go-to-message-beginning)
    (should (looking-at "Alert message.$"))))

(ert-deftest logview-rfc5424-level-2-critical ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 2)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 2))
    (logview-go-to-message-beginning)
    (should (looking-at "Critical message.$"))))

(ert-deftest logview-rfc5424-level-3-error ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 3)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 3))
    (logview-go-to-message-beginning)
    (should (looking-at "Error message.$"))))

(ert-deftest logview-rfc5424-level-4-warning ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 4)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 4))
    (logview-go-to-message-beginning)
    (should (looking-at "Warning message.$"))))

(ert-deftest logview-rfc5424-level-5-notice ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 5)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 5))
    (logview-go-to-message-beginning)
    (should (looking-at "Notice message.$"))))

(ert-deftest logview-rfc5424-level-6-info ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 6)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 6))
    (logview-go-to-message-beginning)
    (should (looking-at "Info message.$"))))

(ert-deftest logview-rfc5424-level-7-debug ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 7)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 7))
    (logview-go-to-message-beginning)
    (should (looking-at "Debug message.$"))))

(ert-deftest logview-rfc5424-level-undefined ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    ;; (logview-next-entry 8)
    (forward-line 8)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 7))
    (logview-go-to-message-beginning)
    ;; (should (looking-at "No such level defined by RFC 5424.$"))))
    (should (looking-at ""))))

(ert-deftest logview-rfc5424-level-defined-level-after-an-undefined-one ()
  (logview--test-with-file "levels/rfc-5424-levels.log"
    (should (equal logview--submode-name "Monolog"))
    (logview-next-entry 8)
    (should (equal (logview--locate-current-entry entry nil (logview--entry-level entry)) 6))
    (logview-go-to-message-beginning)
    (should (looking-at "Info message after an invalid level.$"))))

;; Apache error log submode
(ert-deftest logview-apache-submode-recognition ()
  (logview--test-with-file "apache/error.log"
    (should (equal logview--submode-name "Apache Error Log"))))

(ert-deftest logview-apache-submode-find-entries ()
  (logview--test-with-file "apache/error.log"
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-apache-submode-match-a-message ()
  (logview--test-with-file "apache/error.log"
    (logview-go-to-message-beginning)
    (should (looking-at "Emergency message.$"))))

(ert-deftest logview-apache-submode-match-a-message-after-undefined-lines ()
  (logview--test-with-file "apache/error.log"
    (logview-next-entry 8)
    (logview-go-to-message-beginning)
    (should (looking-at "Info message after some undefined lines."))))

;; TODO: This, or something should test the eventual final levels
;; LogView Mode works with. With that transition, maybe the RFC 5424
;; and RFC 5424 lowercase level definitions could be merged.
(ert-deftest logview-apache-submode-match-all-levels ()
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
(ert-deftest logview-monolog-submode-recognition ()
  (logview--test-with-file "monolog/1.log"
    (should (equal logview--submode-name "Monolog"))))

(ert-deftest logview-monolog-submode-find-entries ()
  (logview--test-with-file "monolog/1.log"
    (logview--locate-current-entry entry start
      (should (and entry (equal start 1))))))

(ert-deftest logview-monolog-submode-match-a-message ()
  (logview--test-with-file "monolog/1.log"
    (logview-go-to-message-beginning)
    (should (looking-at "Emergency message.$"))))

(ert-deftest logview-monolog-submode-match-all-levels ()
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
(ert-deftest logview-sections-1 ()
  (logview--test-with-file "log4j/sections-1.log"
    :extra-customizations (logview--test-view-customizations '(:name "sections" :filters "lv INFO\na+ my\\.Server\nm+ serving request"))
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


(ert-deftest logview-view-editing-1 ()
  (logview--test-with-file "log4j/en-1.log"
    :extra-customizations (logview--test-view-customizations)
    (logview--do-test-view-editing t)))

(ert-deftest logview-view-editing-2 ()
  (logview--test-with-file "log4j/en-1.log"
    :extra-customizations (logview--test-view-customizations)
    (logview--do-test-view-editing nil)))

(defun logview--do-test-view-editing (global)
  (if global (logview-edit-all-views) (logview-edit-submode-views))
  (insert (format "
view errors%s
LV ERROR
" (if global "" "\nsubmode SLF4J")))
  (logview-filter-edit-save)
  (should (equal logview--views `((:name "errors" :submode ,(unless global "SLF4J") :filters "LV ERROR"))))
  (should logview--views-need-saving)
  ;; Pretend they are saved.  Edit name of the created view.
  (setf logview--views-need-saving nil)
  (if global (logview-edit-all-views) (logview-edit-submode-views))
  (goto-char 1)
  (re-search-forward "errors")
  (replace-match "error view")
  ;; Apply changes intermediately.
  (logview-filter-edit-apply)
  (should (equal logview--views `((:name "error view" :submode ,(unless global "SLF4J") :filters "LV ERROR"))))
  (should logview--views-need-saving)
  (setf logview--views-need-saving nil)
  (goto-char 1)
  (re-search-forward "error view")
  (replace-match "Error view")
  (logview-filter-edit-save)
  (should (equal logview--views `((:name "Error view" :submode ,(unless global "SLF4J") :filters "LV ERROR"))))
  (should logview--views-need-saving))


(define-derived-mode logview--test-derived-mode logview-mode
  "Logview-derived"
  (font-lock-add-keywords nil `((,(rx bow "Class" eow) (0 'bold prepend)))  t))

(ert-deftest logview-derived-mode-1 ()
  (logview--test-with-file "log4j/en-1.log"
    :buffer-mode logview--test-derived-mode
    (font-lock-ensure)
    (search-forward "Class")
    (backward-word)
    (let ((faces (get-text-property (point) 'face)))
      (should (memq 'logview-name              faces))
      (should (memq 'logview-information-entry faces))
      (should (memq 'bold                      faces)))))
