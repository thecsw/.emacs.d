;;; emacsql-sqlite-builtin.el --- EmacSQL back-end for SQLite using builtin support  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Jonas Bernoulli <emacs.emacsql@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.emacsql@jonas.bernoulli.dev>

;; SPDX-License-Identifier: Unlicense

;;; Commentary:

;; This library provides an EmacSQL back-end for SQLite, which uses
;; the built-in SQLite support in Emacs 29 an later.

;;; Code:

(require 'emacsql-sqlite)

(declare-function sqlite-open "sqlite.c")
(declare-function sqlite-select "sqlite.c")
(declare-function sqlite-close "sqlite.c")

(emacsql-register-reserved emacsql-sqlite-reserved)

(defclass emacsql-sqlite-builtin-connection (emacsql--sqlite-base) ()
  "A connection to a SQLite database using builtin support.")

(cl-defmethod initialize-instance :after
  ((connection emacsql-sqlite-builtin-connection) &rest _)
  (oset connection handle
        (sqlite-open (oref connection file)))
  (emacsql-sqlite-set-busy-timeout connection)
  (emacsql connection [:pragma (= foreign-keys on)])
  (emacsql-register connection))

(cl-defun emacsql-sqlite-builtin (file &key debug)
  "Open a connected to database stored in FILE.
If FILE is nil use an in-memory database.

:debug LOG -- When non-nil, log all SQLite commands to a log
buffer.  This is for debugging purposes."
  (let ((connection (make-instance #'emacsql-sqlite-builtin-connection
                                   :file file)))
    (when debug
      (emacsql-enable-debugging connection))
    connection))

(cl-defmethod emacsql-live-p ((connection emacsql-sqlite-builtin-connection))
  (and (oref connection handle) t))

(cl-defmethod emacsql-close ((connection emacsql-sqlite-builtin-connection))
  (sqlite-close (oref connection handle))
  (oset connection handle nil))

(cl-defmethod emacsql-send-message
  ((connection emacsql-sqlite-builtin-connection) message)
  (condition-case err
      (let ((headerp emacsql-include-header))
        (mapcar (lambda (row)
                  (cond
                   (headerp (setq headerp nil) row)
                   ((mapcan (lambda (col)
                              (cond ((null col)     (list nil))
                                    ((equal col "") (list ""))
                                    ((numberp col)  (list col))
                                    ((emacsql-sqlite-read-column col))))
                            row))))
                (sqlite-select (oref connection handle) message nil
                               (and emacsql-include-header 'full))))
    ((sqlite-error sqlite-locked-error)
     (if (stringp (cdr err))
         (signal 'emacsql-error (list (cdr err)))
       (pcase-let* ((`(,_ ,errstr ,errmsg ,errcode ,ext-errcode) err)
                    (`(,_ ,_ ,signal ,_)
                     (assq errcode emacsql-sqlite-error-codes)))
         (signal (or signal 'emacsql-error)
                 (list errmsg errcode ext-errcode errstr)))))
    (error
     (signal 'emacsql-error (cdr err)))))

(cl-defmethod emacsql ((connection emacsql-sqlite-builtin-connection) sql &rest args)
  (emacsql-send-message connection (apply #'emacsql-compile connection sql args)))

(provide 'emacsql-sqlite-builtin)

;;; emacsql-sqlite-builtin.el ends here
