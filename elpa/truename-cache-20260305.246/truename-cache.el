;;; truename-cache.el --- Efficiently de-dup file-names  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author:   Martin Edström <meedstrom91@gmail.com>
;; URL:      https://github.com/meedstrom/truename-cache
;; Created:  2026-02-16
;; Keywords: lisp
;; Package-Version: 20260305.246
;; Package-Revision: 6540dd050c0d
;; Package-Requires: ((emacs "27.1") (compat "30.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


;; Minimum Emacs 27+ because of these NEWS.27 items:

;; ** File metadata primitives now signal an error if I/O, access, or
;; other serious errors prevent them from determining the result.
;; Formerly, these functions often (though not always) silently returned
;; nil. [...] The affected primitives are
;; 'directory-files-and-attributes', 'file-acl', 'file-attributes', [...]

;; ** 'file-name-absolute-p' no longer considers "~foo" to be an absolute
;; file name if there is no user named "foo".


;;; Commentary:

;; This library provides two things:

;; 1. A caching alternative to `file-truename'.

;;    See docstring of `truename-cache-get'.

;; 2. An alternative to `directory-files-recursively' that pre-populates said
;;    cache and returns truenames while minimizing calls to `file-truename'.

;;    See docstring of `truename-cache-collect-files-and-attributes'.


;;; Code:

(defconst truename-cache-internal-version 13 "A number sometimes incremented.")

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'compat) ;; Don't know if needed, but we have no test suite yet

;;;; Name-cache:

(defvar truename-cache--wild<>true      (make-hash-table :test 'equal))
(defvar truename-cache--true<>wilds     (make-hash-table :test 'equal))
(defvar truename-cache--true<>dir-abbr  (make-hash-table :test 'equal))
(defvar truename-cache--true<>full-abbr (make-hash-table :test 'equal))

(cl-defun truename-cache--populate
    (truename &optional wild-files (case-fold (file-name-case-insensitive-p truename)))
  "Taking TRUENAME as the truename of WILD-FILES, add them to the cache.
Return TRUENAME.
WILD-FILES may be a string or a list of strings.

Even if WILD-FILES is nil, this caches the abbreviations of TRUENAME as
well as TRUENAME itself, as wild file names it is possible to encounter.

CASE-FOLD is a boolean to which to bind `case-fold-search'.
Defaults to `file-name-case-insensitive-p' for TRUENAME.

For full theoretical correctness, you should make sure to have called
`truename-cache--init-abbreviator' prior to calling this function."
  (let* ((case-fold-search case-fold)
         (dir-abbr-true (directory-abbrev-apply truename))
         (full-abbr-true (truename-cache--fast-full-abbrev truename)))
    (puthash truename             truename truename-cache--wild<>true)
    (puthash dir-abbr-true        truename truename-cache--wild<>true)
    (puthash full-abbr-true       truename truename-cache--wild<>true)
    (puthash truename        dir-abbr-true truename-cache--true<>dir-abbr)
    (puthash truename       full-abbr-true truename-cache--true<>full-abbr)
    ;; Add to a mirror-table for efficient `truename-cache-invalidate'.
    (push truename       (gethash truename truename-cache--true<>wilds))
    (push dir-abbr-true  (gethash truename truename-cache--true<>wilds))
    (push full-abbr-true (gethash truename truename-cache--true<>wilds))
    ;; Add `wild-files' as additional wild names.
    (dolist (wild-file (ensure-list wild-files))
      (puthash wild-file          truename truename-cache--wild<>true)
      (push wild-file    (gethash truename truename-cache--true<>wilds))))
  truename)

(defun truename-cache-invalidate (bad-name)
  "Invalidate BAD-NAME in the name cache, allowing to cache fresh values.
Return the list of variant file names invalidated, including BAD-NAME.

BAD-NAME may be either a cached true name or a wild name that once led
to it, this will invalidate both."
  (let ((all-bad)
        (true (gethash bad-name truename-cache--wild<>true)))
    (when true
      (push true all-bad)
      (dolist (wild (gethash true truename-cache--true<>wilds))
        (unless (member wild all-bad)
          (push wild all-bad)))
      (cl-assert (member bad-name all-bad)))
    ;; Example situation: File WILD is a symlink that changed destination.
    ;; So cached TRUE led to a nonexistent file last time it was accessed,
    ;; whereupon hopefully, something arranged to have this function called
    ;; with TRUE as argument, bringing us here.
    ;; Now find and invalidate WILD so we can cache a correct TRUE next time.
    (dolist (wild (gethash bad-name truename-cache--true<>wilds))
      (unless (member wild all-bad)
        (push wild all-bad)))
    (dolist (bad-variant all-bad)
      (remhash bad-variant truename-cache--wild<>true)
      (remhash bad-variant truename-cache--true<>wilds)
      (remhash bad-variant truename-cache--true<>dir-abbr)
      (remhash bad-variant truename-cache--true<>full-abbr))
    all-bad))

(defun truename-cache-reset ()
  "Wipe the name cache."
  (clrhash truename-cache--wild<>true)
  (clrhash truename-cache--true<>wilds)
  (clrhash truename-cache--true<>dir-abbr)
  (clrhash truename-cache--true<>full-abbr))


;;;; Name-cache getters:

(defun truename-cache-get (wild)
  "Try to return the true name for file name WILD.
The first time this function sees WILD, it uses the expensive
`file-truename', but then it caches that result for future calls.

The cache can be cleaned by:
- `truename-cache-invalidate'
- `truename-cache-reset'

See alternatives:
- `truename-cache-get'
- `truename-cache-get-cached-p'
- `truename-cache-get-existed-p'
- `truename-cache-get-exists-p'
- `truename-cache-get-dir-abbrev'
- `truename-cache-get-dir-abbrev-cached-p'
- `truename-cache-get-dir-abbrev-existed-p'
- `truename-cache-get-dir-abbrev-exists-p'
- `truename-cache-get-full-abbrev'
- `truename-cache-get-full-abbrev-cached-p'
- `truename-cache-get-full-abbrev-existed-p'
- `truename-cache-get-full-abbrev-exists-p'"
  (unless (file-name-absolute-p wild)
    (setq wild (expand-file-name wild)))
  (or (gethash wild truename-cache--wild<>true)
      (progn (truename-cache--init-abbreviator)
             (truename-cache--populate (file-truename wild) wild))))

(defun truename-cache-get-cached-p (wild)
  "Try to return the true name for file name WILD, or nil.

Like `truename-cache-get', but on a cache miss, return nil.
In other words, never use `file-truename'."
  (unless (file-name-absolute-p wild)
    (setq wild (expand-file-name wild)))
  (gethash wild truename-cache--wild<>true))

(defun truename-cache-get-existed-p (wild)
  "Try to return the true name for file name WILD, or nil.

Like `truename-cache-get', but on a cache miss, pass WILD to
`file-truename' only if WILD exists now, then cache and return the
new truename only if it exists, else return nil.

Has alias `truename-cache-get-p'."
  (unless (file-name-absolute-p wild)
    (setq wild (expand-file-name wild)))
  (or (gethash wild truename-cache--wild<>true)
      (and (file-exists-p wild)
           (let ((true (file-truename wild)))
             (when (file-exists-p true)
               (truename-cache--init-abbreviator)
               (truename-cache--populate true wild))))))

(defun truename-cache-get-exists-p (wild)
  "Try to return the true name for file name WILD, or nil.

Like `truename-cache-get', but verify that the cached truename exists on
disk now, else invoke `truename-cache-invalidate' and then behave like
`truename-cache-get-existed-p'.

In theory, this can be slower than `truename-cache-get-existed-p' on a
poorly behaved or network filesystem, as each invocation always contacts
the filesystem, but it should still be much faster than `file-truename'
once most names have been cached.

Hint: you do not need to check file existence as much as you think.
It can give you a false sense of security.
Realize that even if a cached name indeed exists on disk, it can still
be stale because it may no longer be representing WILD but a
different file altogether!

Get a true sense of security by finding where in your program it will
make sense to invoke `truename-cache-invalidate', instead.  Or work only
with names output by `truename-cache-collect-files-and-attributes'.

As of 2026-02-18, the author regards `truename-cache-get-existed-p' as a
reasonable standard choice, which is why it has the short alias
`truename-cache-get-p'."
  (unless (file-name-absolute-p wild)
    (setq wild (expand-file-name wild)))
  (let ((true (gethash wild truename-cache--wild<>true)))
    (or (and true
             (if (file-exists-p true)
                 true
               (truename-cache-invalidate wild)
               nil))
        (when (file-exists-p wild)
          (setq true (file-truename wild))
          (when (file-exists-p true)
            (truename-cache--init-abbreviator)
            (truename-cache--populate true wild))))))


;;;; Name-cache getters with dir abbrev:

(defun truename-cache-get-dir-abbrev (wild)
  "Probable truename of WILD with `directory-abbrev-apply' applied.
Behave like `truename-cache-get'."
  (gethash (truename-cache-get wild) truename-cache--true<>dir-abbr))

(defun truename-cache-get-dir-abbrev-cached-p (wild)
  "Probable truename of WILD with `directory-abbrev-apply' applied, or nil.
Behave like `truename-cache-get-cached-p'."
  (gethash (truename-cache-get-cached-p wild) truename-cache--true<>dir-abbr))

(defun truename-cache-get-dir-abbrev-existed-p (wild)
  "Probable truename of WILD with `directory-abbrev-apply' applied, or nil.
Behave like `truename-cache-get-existed-p'.

Has alias `truename-cache-get-dir-abbrev-p'."
  (gethash (truename-cache-get-existed-p wild) truename-cache--true<>dir-abbr))

(defun truename-cache-get-dir-abbrev-exists-p (wild)
  "Probable truename of WILD with `directory-abbrev-apply' applied, or nil.
Behave like `truename-cache-get-exists-p'."
  (gethash (truename-cache-get-exists-p wild) truename-cache--true<>dir-abbr))


;;;; Name-cache getters with full abbrev:

(defun truename-cache-get-full-abbrev (wild)
  "Probable truename of WILD with `abbreviate-file-name' applied.
Behave like `truename-cache-get'."
  (gethash (truename-cache-get wild) truename-cache--true<>full-abbr))

(defun truename-cache-get-full-abbrev-cached-p (wild)
  "Probable truename of WILD with `abbreviate-file-name' applied, or nil.
Behave like `truename-cache-get-cached-p'."
  (gethash (truename-cache-get-cached-p wild) truename-cache--true<>full-abbr))

(defun truename-cache-get-full-abbrev-existed-p (wild)
  "Probable truename of WILD with `abbreviate-file-name' applied, or nil.
Behave like `truename-cache-get-existed-p'.

Has alias `truename-cache-get-full-abbrev-p'."
  (gethash (truename-cache-get-existed-p wild) truename-cache--true<>full-abbr))

(defun truename-cache-get-full-abbrev-exists-p (wild)
  "Probable truename of WILD with `abbreviate-file-name' applied, or nil.
Behave like `truename-cache-get-exists-p'."
  (gethash (truename-cache-get-exists-p wild) truename-cache--true<>full-abbr))


;;;; Short aliases for the variant you are most likely to use:

(defalias 'truename-cache-get-p             #'truename-cache-get-existed-p)
(defalias 'truename-cache-get-dir-abbrev-p  #'truename-cache-get-dir-abbrev-existed-p)
(defalias 'truename-cache-get-full-abbrev-p #'truename-cache-get-full-abbrev-existed-p)


;;;; Ersatz `abbreviate-file-name':

;; By using `truename-cache--init-abbreviator' to override
;; `truename-cache--home-re' to new regexp before use (even if it is
;; already set), this allows `truename-cache--fast-full-abbrev' to avoid
;; making `expand-file-name' calls every time as `abbreviate-file-name' does.
(defvar truename-cache--home-re nil)
(defun truename-cache--init-abbreviator ()
  "Set some variables needed by `truename-cache--fast-full-abbrev'."
  (let ((home (expand-file-name "~")))
    (when (and truename-cache--home-re
               (not (string-match-p truename-cache--home-re home)))
      (lwarn 'truename-cache :warning
             "Meaning of ~ has changed to \"%s\", does not match regexp \"%s\""
             home truename-cache--home-re))
    (setq truename-cache--home-re (directory-abbrev-make-regexp home))))

(truename-cache--init-abbreviator) ;; Set an initial value at load time

(defun truename-cache--fast-full-abbrev (file)
  "Abbreviate FILE, faster than `abbreviate-file-name'.
Can be even faster by let-binding `file-name-handler-alist' to nil.

Warning:
When not called by `truename-cache-collect-files-and-attributes':
1. You must let-bind `case-fold-search' around this function.
   See docstring `directory-abbrev-apply'.
2. You must call `truename-cache--init-abbreviator' first,
   ideally in the same buffer to avoid effect from buffer-local envvars."
  (if-let* ((handler (find-file-name-handler file 'abbreviate-file-name)))
      (funcall handler 'abbreviate-file-name file)
    (setq file (directory-abbrev-apply file))
    (if (and (string-match truename-cache--home-re file)
             (not (and (= (match-end 0) 1)
                       (= (aref file 0) ?/)))
             (not (and (memq system-type '(ms-dos windows-nt cygwin))
                       (string-match-p "\\`[a-zA-`]:/\\'" file))))
        (concat "~" (substring file (match-beginning 1)))
      file)))


;;;; The great machine:

(defun truename-cache--join-regexps (list-of-regexps)
  "Turn LIST-OF-REGEXPS into a single regexp."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             list-of-regexps
             "\\|"))

(defun truename-cache--mk-handler-alist (&rest subsets)
  "Return a value to which `file-name-handler-alist' can be bound.
Each element in SUBSETS is either t or a list of allowed possible cdrs
of `file-name-handler-alist'."
  (let ((subset (cl-loop for subset in subsets
                         if (eq t subset)
                         return t
                         else append subset)))
    (if (eq t subset)
        file-name-handler-alist
      (setq subset (delete-dups subset))
      (cl-loop for (regexp . handler) in file-name-handler-alist
               when (member handler subset)
               collect (cons regexp handler)))))

(defun truename-cache--mutate-args (args)
  "Mutate plist ARGS in place, setting defaults and adding some keys."
  (cl-assert args) ; So it's safe to use `plist-put' without `setq'
  (unless (memq :side-effect args)          (plist-put args :side-effect t))
  (unless (memq :keep-remotes args)         (plist-put args :keep-remotes t))
  (unless (memq :return-files args)         (plist-put args :return-files t))
  (unless (memq :assert-readable args)      (plist-put args :assert-readable t))
  (unless (memq :local-name-handlers args)  (plist-put args :local-name-handlers t))
  (unless (memq :remote-name-handlers args) (plist-put args :remote-name-handlers t))
  (map-let (:return-files :return-dirs :dirs-recursive :dirs-flat) args
    (unless (or return-files return-dirs)
      (error "Must have at least one of: RETURN-FILES, RETURN-DIRS"))
    (unless (or dirs-flat dirs-recursive (memq :infer-dirs-from args))
      (error "Must have at least one of: DIRS-FLAT, DIRS-RECURSIVE, INFER-DIRS-FROM")))
  (map-let (:full-dir-deny :relative-dir-deny :relative-file-deny) args
    (when relative-dir-deny
      (plist-put args :REL-DIR-DENY-RE
                 (truename-cache--join-regexps relative-dir-deny)))
    (when relative-file-deny
      (plist-put args :REL-FILE-DENY-RE
                 (truename-cache--join-regexps relative-file-deny)))
    (when full-dir-deny
      (plist-put args :FULL-DIR-DENY-RE
                 (truename-cache--join-regexps full-dir-deny))))
  (map-let (:local-name-handlers :remote-name-handlers) args
    (plist-put args :LOCAL-HANDLER-ALIST (truename-cache--mk-handler-alist
                                          local-name-handlers))
    (plist-put args :REMOTE-HANDLER-ALIST (truename-cache--mk-handler-alist
                                           remote-name-handlers))
    (plist-put args :MERGED-HANDLER-ALIST (truename-cache--mk-handler-alist
                                           local-name-handlers remote-name-handlers))))

(defvar truename-cache--dedupped-dirs (make-hash-table :test 'equal))
(defvar truename-cache--visited (make-hash-table :test 'equal))
(defvar truename-cache--results nil)

;; "If you have a procedure with ten parameters, you probably missed some."
;; --- https://www.cs.yale.edu/homes/perlis-alan/quotes.html
(cl-defun truename-cache-collect-files-and-attributes
    ( &rest args &key
      _side-effect
      dirs-flat
      dirs-recursive
      infer-dirs-from
      _relative-file-deny
      _relative-dir-deny
      _full-dir-deny
      _return-files
      _return-dirs
      _resolve-symlinks
      _dirs-recursive-follow-symlinks
      _keep-remotes
      _local-name-handlers
      _remote-name-handlers
      abbrev
      _assert-readable)
  "Return an unsorted alist ((FILE1 . ATTR1) (FILE2 . ATTR2) ...).


This is engineered to be fast at collecting a file list from a variety
of information sources (such from as both `org-id-files' and the
content of `org-directory'), de-duplicated and in truename form.

The file attribute lists ATTR1, ATTR2... (per `file-attributes') are
included in case they are needed, because that is faster than querying
the filesystem later on for the attributes of one file at a time.

When SIDE-EFFECT (default t), this function also pre-populates a cache
used by another tool `truename-cache-get', so that the tool need not be
expensive even the first time a given file name is passed to it.

This function does not itself look up cache.


ARGS are a plist of keyword arguments.
All arguments default to nil unless otherwise specified.

The list of candidates to return comes from scanning a set of
directories specified by any or all of the following arguments,
at least one of which must be non-nil:

- DIRS-FLAT :: List of directories to scan.
- DIRS-RECURSIVE :: List of directories to scan recursively.
- INFER-DIRS-FROM :: List or lists of file names which are then used to
    infer directories to scan, as if they had been passed to DIRS-FLAT.
    Non-absolute file names are quietly ignored.

    It is fine and normal to pass many duplicates.

    Unlike with DIRS-FLAT, any inferred directory that happens to be
    a descendant of DIRS-RECURSIVE is dropped, on the assumption that
    it will be found from DIRS-RECURSIVE anyway.

    If you want to include a descendant that otherwise fails a filter
    \(i.e. runs afoul of RELATIVE-DIR-DENY or RESOLVE-SYMLINKS while
    recursing\), pass it explicitly in DIRS-FLAT.

Most other arguments have the effect of narrowing down the candidates.

First, the following filters are applied to the part of a file
name that is relative to one of aforementioned set of directories.

\(To clarify: if the file was found from DIRS-RECURSIVE,
the relative name can contain multiple directory components,
otherwise it is simply the `file-name-nondirectory' of the file.\)

- RELATIVE-FILE-DENY :: List of regexps that reject a file or directory.

    When in doubt, prefer RELATIVE-DIR-DENY, see below.
    This mostly exists to support existing user options in various
    packages that are designed to operate on file names instead of
    directory names.
    If the purpose is only to filter names found inside a given set of
    directories, and not to prevent possible thousands or millions of
    recursions, you may as well just filter the final result.

- RELATIVE-DIR-DENY :: List of regexps that reject a directory.

    Only relevant with DIRS-RECURSIVE.
    Applied to directory names (with trailing slash) to decide whether
    or not to recurse into it.

    Important performance knob!  If this function is ever slow, the
    bottleneck is certainly from recursing too many times, if not
    accessing network file names on a slow connection.
    To check which directories were accessed, eval:
        \(hash-table-keys truename-cache--visited\)

Other filters:

- FULL-DIR-DENY :: Like RELATIVE-DIR-DENY but can be relevant even if
    DIRS-RECURSIVE is nil, and are applied to absolute names instead of
    relative names.

    In addition to the usual purpose of preventing recursion, they are
    applied to the names that were input to DIRS-FLAT, DIRS-RECURSIVE
    and the directories inferred via INFER-DIRS-FROM.

    At that time, they are applied to both the possibly non-true input
    name and the true name.  That is, if either the input name or the
    true name matches FULL-DIR-DENY, the directory is skipped.

The following arguments affect what kinds of files to include in the
result.  They also affect which symlink destinations to include.

- RETURN-FILES :: Include non-directory files. (default t)
- RETURN-DIRS :: Include directories.
- RESOLVE-SYMLINKS :: Include symlink destinations resolved into true
    names, if they exist and satisfy RETURN-FILES or RETURN-DIRS.
    If nil, symlinks are excluded entirely.

There is no way to include symlinks and not resolve them, and no way to
include inaccessible symlink destinations, because it cannot be verified
that they are true names.

DIRS-RECURSIVE-FOLLOW-SYMLINKS says to traverse symlinks found via
DIRS-RECURSIVE to other directories, recursively.  Infinite recursion
cannot happen.
Only relevant if RESOLVE-SYMLINKS is also t.

KEEP-REMOTES (default t) says to allow looking in directories that are
remote, i.e. that satisfy `file-remote-p'.

The following two performance knobs can be t, nil or a list of symbols
like the cdrs of `file-name-handler-alist'.  They specify which file
name handlers to allow, and t means allow all of them:

- LOCAL-NAME-HANDLERS (default t) :: Applied in directories that
    dissatisfy `file-remote-p'.  This can often be set to nil with no
    ill effect.
- REMOTE-NAME-HANDLERS (default t) :: Applied in directories that
    satisfy `file-remote-p'.
    Mostly used if KEEP-REMOTES is t, but may also affect the
    determination of whether a directory or a symlink destination is
    remote in the first place, so only set it to nil if you are
    confident there will be no remote names.

ABBREV transforms each file name returned.
It can take one of three values:

- nil :: Return true names.

- `dir' :: Return non-true names more likely to match instances of
    variable `buffer-file-name' and to work with `get-file-buffer'.
    Corresponds to `truename-cache-get-dir-abbrev'.

- `full' :: Return non-true names very likely to match instances of
    variable `buffer-file-truename' and to work with `get-truename-buffer'.
    Corresponds to `truename-cache-get-full-abbrev'.

    This match is reliable except when the value of LOCAL-NAME-HANDLERS
    or REMOTE-NAME-HANDLERS causes an abbreviation handler to not be
    used.  In most cases, you probably can assume it is reliable anyway.

ASSERT-READABLE (default t) says to signal an error when encountering
unreadable directories and invalid symlinks.
Otherwise, they are quietly skipped."
  (truename-cache--mutate-args args)
  (map-let ( :assert-readable :side-effect
             :MERGED-HANDLER-ALIST :FULL-DIR-DENY-RE) args
    (let* ((current-time-list nil)
           (file-name-handler-alist MERGED-HANDLER-ALIST)
           ;; NOTE: It's tempting to dedup with `string-prefix-p' so as to
           ;; only recurse from the ultimate roots, but user must be allowed to
           ;; pass specific subdirs that would otherwise be blocked by a
           ;; deny-regexp or by :resolve-symlinks nil.
           (filtered-true-recursive-roots
            (seq-filter
             (lambda (true-dir)
               (and (if assert-readable
                        (not (cl-assert (file-readable-p true-dir) nil
                                        "Directory not readable: %s" true-dir))
                      (file-readable-p true-dir))
                    (not (and FULL-DIR-DENY-RE
                              (string-match-p FULL-DIR-DENY-RE true-dir)))))
             (delete-dups
              (cl-loop
               for dir in (seq-uniq dirs-recursive)
               when (and (not (cl-assert (file-name-absolute-p dir) nil
                                         "Non-absolute name in DIRS-RECURSIVE: %s" dir))
                         (not (and FULL-DIR-DENY-RE
                                   (string-match-p FULL-DIR-DENY-RE dir))))
               collect (file-name-as-directory (file-truename dir))))))
           (test-roots (delete-dups (append filtered-true-recursive-roots
                                            dirs-recursive)))
           (inferred-dirs
            (progn
              ;; We expect a LOT of dups here, it's typical.
              ;; So use a dedicated table for dedupping.
              (clrhash truename-cache--dedupped-dirs)
              (let (file-name-handler-alist) ;; HACK: speed up `file-name-directory'
                ;; Take plural lists so caller needs not `append' large lists.
                (dolist (sublist (if (seq-find #'stringp infer-dirs-from)
                                     (list infer-dirs-from)
                                   infer-dirs-from))
                  (dolist (name sublist)
                    (when (file-name-absolute-p name)
                      (puthash (file-name-directory name)
                               t
                               truename-cache--dedupped-dirs)))))
              (cl-loop
               for dir being each hash-key of truename-cache--dedupped-dirs
               ;; Try to minimize redundant `file-truename' calls later,
               ;; which easily consume a majority of CPU time.
               unless (cl-some (lambda (root) (string-prefix-p root dir))
                               test-roots)
               collect dir)))

           (all-flat-dirs
            (progn
              (dolist (dir dirs-flat)
                (unless (file-name-absolute-p dir)
                  (error "Non-absolute name in DIRS-FLAT: %s" dir)))
              (seq-filter
               (lambda (true-dir)
                 (and (if assert-readable
                          (not (cl-assert (file-readable-p true-dir) nil
                                         "Directory not readable: %s" true-dir))
                        (file-readable-p true-dir))
                      (not (and FULL-DIR-DENY-RE
                                (string-match-p FULL-DIR-DENY-RE true-dir)))))
               (delete-dups
                (cl-loop
                 for dir in (delete-dups (append dirs-flat inferred-dirs))
                 when (not (and FULL-DIR-DENY-RE
                                (string-match-p FULL-DIR-DENY-RE dir)))
                 collect (file-name-as-directory (file-truename dir))))))))

      (clrhash truename-cache--visited)
      (setq truename-cache--results nil)
      (with-temp-buffer  ; No buffer-env
        (truename-cache--init-abbreviator)
        ;; NOTE: Do the recursive analyses first, lest a flat dir
        ;; added to `truename-cache--visited' block a later recursion.
        (dolist (dir filtered-true-recursive-roots)
          (truename-cache--analyze-recursively dir args))
        (dolist (dir all-flat-dirs)
          (truename-cache--analyze dir args)))
      (when abbrev
        (cl-assert (or (eq abbrev 'full) (eq abbrev 'dir)))
        (when (not side-effect)
          (error "Argument ABBREV non-nil depends on SIDE-EFFECT t"))
        (if (eq abbrev 'dir)
            (cl-loop
             for cell in truename-cache--results
             do (setcar cell (gethash (car cell)
                                      truename-cache--true<>dir-abbr)))
          (cl-loop
           for cell in truename-cache--results
           do (setcar cell (gethash (car cell)
                                    truename-cache--true<>full-abbr)))))
      truename-cache--results)))

(defun truename-cache--analyze-recursively (true-dir args)
  "Analyze TRUE-DIR recursively.
ARGS mostly as in `truename-cache-collect-files-and-attributes'."
  (truename-cache--analyze true-dir (append '(:RECURSE t) args)))

(defun truename-cache--analyze (true-dir args)
  "Analyze TRUE-DIR, maybe recursively.
ARGS mostly as in `truename-cache-collect-files-and-attributes'."
  (map-let (:keep-remotes :REMOTE-HANDLER-ALIST :LOCAL-HANDLER-ALIST) args
    (unless (gethash (directory-file-name true-dir) truename-cache--visited)
      (let ((default-directory (file-name-as-directory true-dir)))
        (if (file-remote-p true-dir)
            (when keep-remotes
              (let ((file-name-handler-alist REMOTE-HANDLER-ALIST))
                (truename-cache--analyze-1 "" args true-dir)))
          (let ((file-name-handler-alist LOCAL-HANDLER-ALIST))
            (truename-cache--analyze-1 "" args true-dir)))))))

(defun truename-cache--analyze-1 (rel-dir args true-dir)
  "Subroutine of `truename-cache--analyze'.
Analyze REL-DIR, a relative name to `default-directory'.

ARGS mostly as in `truename-cache-collect-files-and-attributes'.

TRUE-DIR is the true name of REL-DIR, such that when concatenated
\(using `file-name-concat'\) with the non-directory name of any
non-symlink file in REL-DIR, becomes the true name of that file."
  (map-let ( :side-effect :return-dirs :return-files :resolve-symlinks
             :dirs-recursive-follow-symlinks :keep-remotes :assert-readable
             :RECURSE :REL-FILE-DENY-RE :REL-DIR-DENY-RE :FULL-DIR-DENY-RE
             :MERGED-HANDLER-ALIST :LOCAL-HANDLER-ALIST :REMOTE-HANDLER-ALIST)
      args
    (cl-loop
     with case-fold-fs = (file-name-case-insensitive-p true-dir)
     for cell
     in (if assert-readable
            (directory-files-and-attributes rel-dir nil nil t 'integer)
          (ignore-error permission-denied
            (directory-files-and-attributes rel-dir nil nil t 'integer)))
     as (bare-name . attr) = cell
     as rel-name = (file-name-concat rel-dir bare-name)
     as true-name = (file-name-concat true-dir bare-name)
     when (and bare-name
               (not (member bare-name '("." "..")))
               (not (and REL-FILE-DENY-RE
                         (string-match-p REL-FILE-DENY-RE rel-name))))
     when (cond
           ((null (file-attribute-type attr))
            return-files)

           ((eq t (file-attribute-type attr))
            (when (and RECURSE
                       (not (gethash true-name truename-cache--visited))
                       (or (not REL-DIR-DENY-RE)
                           (not (string-match-p REL-DIR-DENY-RE
                                                (file-name-as-directory rel-name))))
                       (or (not FULL-DIR-DENY-RE)
                           (not (string-match-p FULL-DIR-DENY-RE
                                                (file-name-as-directory true-name)))))
              (puthash true-name t truename-cache--visited)
              (truename-cache--analyze-1 rel-name args true-name))
            return-dirs)

           ((stringp (file-attribute-type attr))
            (let ((file-name-handler-alist MERGED-HANDLER-ALIST)
                  resolved resolved-is-dir resolved-is-remote)
              (when (and resolve-symlinks
                         ;; Technically though unconventional, this could
                         ;; already be considered a true name; the true name
                         ;; of the symlink.  Now we need the true name of
                         ;; the symlink target, so get that.
                         (setq resolved (file-truename true-name))
                         (progn
                           (setq resolved-is-remote (file-remote-p resolved))
                           (or (not resolved-is-remote)
                               keep-remotes))
                         (prog1 t
                           (if resolved-is-remote
                               (setq file-name-handler-alist REMOTE-HANDLER-ALIST)
                             (setq file-name-handler-alist LOCAL-HANDLER-ALIST)))
                         (setq attr (if assert-readable
                                        (file-attributes resolved 'integer)
                                      (ignore-error permission-denied
                                        (file-attributes resolved 'integer)))))
                (when (or (and return-files
                               (null (file-attribute-type attr)))
                          (and (eq t (file-attribute-type attr))
                               (progn (setq resolved-is-dir t)
                                      return-dirs)))
                  (push (cons resolved attr) truename-cache--results)
                  (when side-effect
                    (truename-cache--populate resolved true-name)))
                (when (and resolved-is-dir
                           RECURSE
                           dirs-recursive-follow-symlinks
                           (not (gethash resolved truename-cache--visited))
                           (or (not REL-DIR-DENY-RE)
                               (not (string-match-p REL-DIR-DENY-RE
                                                    (file-name-as-directory rel-name))))
                           (or (not FULL-DIR-DENY-RE)
                               (not (string-match-p FULL-DIR-DENY-RE
                                                    (file-name-as-directory resolved)))))
                  (puthash resolved t truename-cache--visited)
                  (truename-cache--analyze-1 rel-name args resolved))))
            nil))

     do
     (setcar cell true-name)
     (push cell truename-cache--results)
     (when side-effect
       (truename-cache--populate true-name nil case-fold-fs)))))

(provide 'truename-cache)

;;; truename-cache.el ends here
