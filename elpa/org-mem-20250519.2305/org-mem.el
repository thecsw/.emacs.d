;;; org-mem.el --- Fast info from a large number of Org file contents -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author:   Martin Edström <meedstrom@runbox.eu>
;; URL:      https://github.com/meedstrom/org-mem
;; Created:  2025-03-15
;; Keywords: text
;; Package-Version: 20250519.2305
;; Package-Revision: 48e5d49fcd1e
;; Package-Requires: ((emacs "29.1") (el-job "2.4.2") (llama "0.5.0"))

;;; Commentary:

;; A cache of metadata about the structure of all your Org files - headings,
;; links and so on.

;; Builds quickly, so that there is no need to persist data
;; across sessions.

;; Provides two independent APIs:

;;  - Emacs Lisp: regularly named accessors such as `org-mem-entry-olpath',
;;                `org-mem-link-pos', `org-mem-all-id-nodes' etc

;;  - SQL: an in-memory SQLite database that can be queried like
;;         (emacsql (org-mem-roamy-db) [:select * :from links :limit 10])

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'llama)
(require 'el-job)
(require 'org-mem-parser)

(defvar org-id-locations)
(defvar org-id-track-globally)
(defvar org-id-locations-file)
(defvar org-id-extra-files)
(declare-function org-id-alist-to-hash "org-id")
(declare-function org-id-hash-to-alist "org-id")

(defgroup org-mem nil "Fast info from a large amount of Org file contents."
  :group 'text)

(defcustom org-mem-do-cache-text nil
  "Whether to also cache text contents of all entries.
Likely to slow down `org-mem-reset'.

This makes the raw text available via accessor `org-mem-entry-text',
and it can be fontified via function `org-mem-fontify-like-org'."
  :type 'boolean
  :package-version '(org-mem . "0.9.0"))

(defcustom org-mem-do-warn-title-collisions t
  "Whether to print a message when two ID-nodes have the same title.

To check manually, type \\[org-mem-list-title-collisions]."
  :type 'boolean
  :package-version '(org-mem . "0.2.0"))

(defcustom org-mem-do-sync-with-org-id nil
  "Whether to exchange data with `org-id-locations'.

Benefits:
- Org-mem gets to know about files outside `org-mem-watch-dirs', so long
  as they contain some ID and can thus be found in `org-id-locations'.
- Help ensure that ID-links to somewhere inside `org-mem-watch-dirs'
  always work, so they never trigger a fallback attempt to run
  `org-id-update-id-locations' when clicked, which can take a while.

No effect until after Org has loaded.
Only updates `org-id-locations', never runs `org-id-locations-save'."
  :type 'boolean
  :package-version '(org-mem . "0.6.0"))

(defcustom org-mem-watch-dirs nil
  "List of directories in which to look for Org files.
Each directory is checked recursively \(looking in subdirectories,
sub-subdirectories etc\).

Exceptions:

- Subdirectories starting with underscore or dot, such as \".emacs.d\".
  To check such a directory, add its full path explicitly.
- Subdirectories that are symlinks.
- Anything matching `org-mem-watch-dirs-exclude'.

Can be left at nil, if `org-mem-do-sync-with-org-id' is t.
Benefits of configuring it anyway:

- Awareness of files that contain no ID at all.
- React when new files appear in these directories.
  - Useful if this Emacs session is not the only program
    that may create, move or rename files.

Tip: If past misconfiguration has recorded duplicate IDs,
try command \\[org-mem-scrub-id-locations]."
  :type '(repeat directory)
  :package-version '(org-mem . "0.5.0"))

;; REVIEW: Use backslashes on Windows?
(defcustom org-mem-watch-dirs-exclude
  '("/logseq/bak/"
    "/logseq/version-files/"
    "/node_modules/"
    ".sync-conflict-"
    ".#"
    "/backup")
  "Literal substrings of file paths that should not be scanned.
Aside from this variable, some filters are hard-coded:

- We only scan files that end in precisely \".org\"
  - Thus files ending in ~, # or similar are excluded in any case
- We exclude subdirectories that start with a period or underscore
  - Thus directories like \".git\" \"_site\" are excluded in any case
- We exclude symlinks

Main reason to configure this is to prevent counting back-ups
and autosave files as duplicate ID locations.

You can also speed up `org-mem-reset' by excluding directories with a
humongous amount of files \(on the order of 100,000), such as the
infamous \"node_modules\", even if they contain no Org files."
  :type '(repeat string)
  :package-version '(org-mem . "0.2.0"))

(defcustom org-mem-seek-link-types
  '("http" "https" "id" "file")
  "Which types of plain links to look for."
  :type '(repeat string)
  :package-version '(org-mem . "0.2.0"))

(defvar org-mem-scratch nil
  "Work buffer held current while executing some hooks.
These are hooks called many times:
- `org-mem-record-file-functions'
- `org-mem-record-entry-functions'
- `org-mem-record-link-functions'
- `org-mem-forget-file-functions'
- `org-mem-forget-entry-functions'
- `org-mem-forget-link-functions'

This lets a function on these hooks sidestep the performance overhead of
`with-temp-buffer' or `with-work-buffer', in favor of using the
already current buffer:
    \(cl-assert (eq (current-buffer) org-mem-scratch))
    \(erase-buffer)

Buffer is in `fundamental-mode'.  For an Org buffer see function
`org-mem-org-mode-scratch'.")


;;; Basics

(defvar org-mem--title<>id (make-hash-table :test 'equal)
  "1:1 table mapping a heading, file-title or alias to an ID.")

(defvar org-mem--id<>entry (make-hash-table :test 'equal)
  "1:1 table mapping an ID to an `org-mem-entry' record.")

(defvar org-mem--file<>entries (make-hash-table :test 'equal)
  "1:N table mapping a file name to a sorted list of `org-mem-entry' records.
Sorted by field `org-mem-entry-pos'.")

(defvar org-mem--target<>links (make-hash-table :test 'equal)
  "1:N table mapping a destination to a list of `org-mem-link' records.

A destination is a citekey or the path component of a valid Org link.
In practice, it is often an org-id like
\"57707152-9c05-43f5-9f88-5d4f3a0d019a\", an URI path like
\"//www.gnu.org\", or a citekey like \"@ioannidis2005\".

NOTE! A future version may omit the sigil @ in citekeys.")

(defvar org-mem--internal-entry-id<>links (make-hash-table :test 'eq)
  "May become deprecated!
1:N table mapping internal entry ID to a list of `org-mem-link' records.
User should query via `org-mem-links-in-entry'.")

(defvar org-mem--key<>subtable (make-hash-table :test 'eq)
  "Big bag of memoized values, smelling faintly of cabbage.")

(define-inline org-mem--table (key subkey)
  "In a table identified by KEY, access value at SUBKEY.
Store all tables in `org-mem--key<>subtable'.
Note: All tables cleared often, meant for memoizations."
  (inline-quote
   (gethash ,subkey (or (gethash ,key org-mem--key<>subtable)
                        (puthash ,key (make-hash-table :test 'equal)
                                 org-mem--key<>subtable)))))

(defvar org-mem--file<>metadata (make-hash-table :test 'equal)
  "1:1 table mapping a file name to a list of assorted data.

Users have no reason to inspect this table, prefer stable API
in `org-mem-file-mtime' and friends.")

(defun org-mem--get-file-metadata (file/entry/link)
  "Return (FILE ATTRS LINES PTMAX) if FILE/ENTRY/LINK known, else error."
  (let ((wild-file (if (stringp file/entry/link)
                       file/entry/link
                     (if (org-mem-entry-p file/entry/link)
                         (org-mem-entry-file file/entry/link)
                       (org-mem-link-file file/entry/link)))))
    (or (gethash wild-file org-mem--file<>metadata)
        (gethash (org-mem--abbr-truename wild-file) org-mem--file<>metadata)
        (error "org-mem: File seems not yet scanned: %s" wild-file))))

(cl-defstruct (org-mem-link (:constructor nil) (:copier nil))
  (file              () :read-only t :type string)
  (pos               () :read-only t :type integer)
  (type              () :read-only t :type string)
  (target            () :read-only t :type string)
  (description       () :read-only t :type string)
  (citation-p        () :read-only t :type boolean)
  (nearby-id         () :read-only t :type string)
  (-internal-entry-id () :read-only t :type integer))

(cl-defstruct (org-mem-entry (:constructor nil) (:copier nil))
  (file           () :read-only t :type string)
  (lnum           () :read-only t :type integer)
  (pos            () :read-only t :type integer)
  (title-maybe    () :read-only t :type string)
  (level          () :read-only t :type integer)
  (id             () :read-only t :type string)
  (closed         () :read-only t :type string)
  (crumbs         () :read-only t :type list)
  (deadline       () :read-only t :type string)
  (priority       () :read-only t :type string)
  (properties     () :read-only t :type list)
  (scheduled      () :read-only t :type string)
  (tags-inherited () :read-only t :type list)
  (tags-local     () :read-only t :type list)
  (todo-state     () :read-only t :type string)
  (-internal-id   () :read-only t :type integer)
  text)


;;; To find objects to operate on

(defun org-mem-all-ids ()
  "All known org-ids."
  (with-memoization (org-mem--table 0 'org-mem-all-ids)
    (hash-table-keys org-mem--id<>entry)))

(defun org-mem-all-files ()
  "All Org files that have been found."
  (with-memoization (org-mem--table 0 'org-mem-all-files)
    (hash-table-keys org-mem--file<>metadata)))

(defun org-mem-all-entries ()
  "All entries with non-nil title."
  (with-memoization (org-mem--table 0 'org-mem-all-entries)
    (seq-filter #'org-mem-entry-title-maybe
                (apply #'append (hash-table-values org-mem--file<>entries)))))

(defun org-mem-all-id-nodes ()
  "All ID-nodes with non-nil title.
An ID-node is simply an entry that has an ID property."
  (with-memoization (org-mem--table 0 'org-mem-all-id-nodes)
    (seq-filter #'org-mem-entry-title-maybe
                (hash-table-values org-mem--id<>entry))))

(defun org-mem-all-links ()
  "All links and citations.
Citations are `org-mem-link' objects that satisfy
`org-mem-link-citation-p'."
  (with-memoization (org-mem--table 0 'org-mem-all-links)
    (apply #'append (hash-table-values org-mem--target<>links))))

(defun org-mem-all-id-links ()
  "All ID-links."
  (with-memoization (org-mem--table 0 'org-mem-all-id-links)
    (org-mem-links-of-type "id")))

(defun org-mem-entry-by-id (id)
  "The entry with unique :ID: property equal to ID."
  (and id (gethash id org-mem--id<>entry)))

(defun org-mem-entry-at-lnum-in-file (lnum file)
  "The entry that is current at line-number LNUM in FILE."
  (with-memoization (org-mem--table 11 (list lnum file))
    (let ((entries (org-mem-entries-in-file file)))
      (if (and (cadr entries) (= lnum 1 (org-mem-entry-lnum (cadr entries))))
          (cadr entries)
        (cl-loop
         for (prev next) on entries
         if (or (not next) (< lnum (org-mem-entry-lnum next))) return prev)))))

(defun org-mem-entry-at-pos-in-file (pos file)
  "The entry that is current at char-position POS in FILE."
  (with-memoization (org-mem--table 12 (list pos file))
    (let ((entries (org-mem-entries-in-file file)))
      (if (and (cadr entries) (= pos 1 (org-mem-entry-pos (cadr entries))))
          (cadr entries)
        (cl-loop
         for (prev next) on entries
         if (or (not next) (< pos (org-mem-entry-pos next))) return prev)))))

(define-inline org-mem-entry-at-file-pos (file pos)
  "Like `org-mem-entry-at-pos-in-file' with flipped argument order."
  (inline-quote (org-mem-entry-at-pos-in-file ,pos ,file)))

(define-inline org-mem-entry-at-file-lnum (file pos)
  "Like `org-mem-entry-at-lnum-in-file' with flipped argument order."
  (inline-quote (org-mem-entry-at-lnum-in-file ,pos ,file)))

(defun org-mem-next-entry (entry)
  "The next entry after ENTRY in the same file, if any."
  (with-memoization (org-mem--table 20 entry)
    (let ((entries (gethash (org-mem-entry-file entry) org-mem--file<>entries)))
      (while (and (car entries)
                  (not (= (org-mem-entry--internal-id (car entries))
                          (org-mem-entry--internal-id entry))))
        (pop entries))
      (pop entries)
      (car entries))))

(defun org-mem-previous-entry (entry)
  "The next entry after ENTRY in the same file, if any."
  (with-memoization (org-mem--table 21 entry)
    (let ((entries (gethash (org-mem-entry-file entry) org-mem--file<>entries)))
      (while (and (cadr entries)
                  (not (= (org-mem-entry--internal-id (cadr entries))
                          (org-mem-entry--internal-id entry))))
        (pop entries))
      (car entries))))

(defun org-mem-entries-in-file (file)
  "List of entries in same order as they appear in FILE, if FILE known.
The list always contains at least one entry, which
represents the content before the first heading.
2025-05-13: The last fact may change."
  (cl-assert (stringp file))
  (gethash file org-mem--file<>entries))

(defalias 'org-mem-file-entries #'org-mem-entries-in-file)

(defun org-mem-entries-in-files (files)
  "Combined list of entries from all of FILES."
  (with-memoization (org-mem--table 13 files)
    (seq-mapcat #'org-mem-entries-in-file files)))

(defun org-mem-file-by-id (id)
  "The file that contains an :ID: property matching ID."
  (let ((entry (and id (gethash id org-mem--id<>entry))))
    (and entry (org-mem-entry-file entry))))

(defun org-mem-entry-that-contains-link (link)
  "The entry where LINK was found."
  (org-mem-entry-at-pos-in-file (org-mem-link-file link)
                                (org-mem-link-pos link)))

(defun org-mem-id-nodes-in-files (files)
  "All ID-nodes in FILES."
  (with-memoization (org-mem--table 15 files)
    (setq files (ensure-list files))
    (seq-filter (##member (org-mem-entry-file %) files)
                (org-mem-all-id-nodes))))

(defun org-mem-links-with-type-and-path (type path)
  "Links with components TYPE and PATH, see `org-link-plain-re'."
  (cl-loop for link in (gethash path org-mem--target<>links)
           when (equal type (org-mem-link-type link))
           collect link))

(defun org-mem-links-to-entry (entry)
  "All links that point to ENTRY."
  (and entry (gethash (org-mem-entry--internal-id entry)
                      org-mem--internal-entry-id<>links)))

(defun org-mem-id-links-to-entry (entry)
  "All ID-links that point to ENTRY."
  (and entry (gethash (org-mem-entry-id entry) org-mem--target<>links)))

(defun org-mem-id-links-to-id (id)
  "All ID-links targeting ID."
  (org-mem-id-links-to-entry (org-mem-entry-by-id id)))

(defun org-mem-id-node-by-title (title)
  "The ID-node titled TITLE."
  (and title (gethash (org-mem-id-by-title title) org-mem--id<>entry)))

(defun org-mem-id-by-title (title)
  "The ID that currently corresponds to TITLE.
TITLE is either a heading, a file title, or an alias.

Assumes unique titles.  If two IDs exist with same title, it is
undefined which ID is returned.  User can prevent this from becoming a
problem with the help of option `org-mem-do-warn-title-collisions'."
  (and title (gethash title org-mem--title<>id)))

(defun org-mem-links-from-id (id)
  "Links from context where local or inherited ID property is ID."
  (with-memoization (org-mem--table 16 id)
    (seq-filter (##equal (org-mem-link-nearby-id %) id)
                (org-mem-all-links))))

;; TODO
(defun org-mem-links-to-file (_file)
  "(Unimplemented) All links leading into somewhere in FILE."
  ;; (let ((targets-for-file (org-mem-entries-in-file))))
  ;; (cl-loop for link in (org-mem-all-links)
  ;;          collect nil)
  (error "Unimplemented"))

(defun org-mem-id-links-from-id (id)
  "ID-links from context where local or inherited ID property is ID."
  (with-memoization (org-mem--table 17 id)
    (seq-filter (##equal (org-mem-link-nearby-id %) id)
                (org-mem-all-id-links))))

(defun org-mem-links-of-type (type)
  "All links of type TYPE."
  (with-memoization (org-mem--table 18 type)
    (seq-filter (##equal (org-mem-link-type %) type)
                (org-mem-all-links))))

(defun org-mem-links-in-file (file)
  "All links found inside FILE."
  (with-memoization (org-mem--table 19 file)
    (seq-mapcat #'org-mem-links-in-entry (org-mem-entries-in-file file))))

(defun org-mem-links-in-entry (entry)
  "All links found inside ENTRY, ignoring descendant entries."
  (and entry (gethash (org-mem-entry--internal-id entry)
                      org-mem--internal-entry-id<>links)))


;;; Entry info

(defun org-mem-entry-subtree-p (entry)
  "Non-nil if ENTRY is a subtree, nil if a \"file-level node\"."
  (not (= 0 (org-mem-entry-level entry))))

;; REVIEW: To make `org-mem-entry' objects take less visual space when
;;         printed, we could stop putting ancestor titles in CRUMBS, just look
;;         them up at this time via cross-ref with the char positions.
(defun org-mem-entry-olpath (entry)
  "Outline path to ENTRY."
  (with-memoization (org-mem--table 25 entry)
    (mapcar #'cl-fourth (cdr (reverse (cdr (org-mem-entry-crumbs entry)))))))

(defun org-mem-entry-olpath-with-self (entry)
  "Outline path, including ENTRY\\='s own heading."
  (with-memoization (org-mem--table 26 entry)
    (mapcar #'cl-fourth (cdr (reverse (org-mem-entry-crumbs entry))))))

(defun org-mem-entry-olpath-with-self-with-title
    (entry &optional filename-fallback)
  "Outline path, including file #+title, and ENTRY\\='s own heading.
With FILENAME-FALLBACK, use file basename if there is no #+title."
  (let ((olp (mapcar #'cl-fourth (reverse (org-mem-entry-crumbs entry)))))
    (when (null (car olp))
      (pop olp)
      (when filename-fallback
        (push (file-name-nondirectory (org-mem-entry-file entry)) olp)))
    olp))

(defalias 'org-mem-entry-olpath-with-title-with-self
  #'org-mem-entry-olpath-with-self-with-title)

(defun org-mem-entry-olpath-with-title (entry &optional filename-fallback)
  "Outline path to ENTRY, including file #+title.
With FILENAME-FALLBACK, use file basename if there is no #+title."
  (let ((olp (mapcar #'cl-fourth (reverse (cdr (org-mem-entry-crumbs entry))))))
    (when (null (car olp))
      (pop olp)
      (when filename-fallback
        (push (file-name-nondirectory (org-mem-entry-file entry)) olp)))
    olp))

(defun org-mem-entry-title (entry)
  "Like `org-mem-entry-title-maybe' but signal when ENTRY has no title."
  (or (org-mem-entry-title-maybe entry)
      (error "Entry has no title, try org-mem-entry-title-maybe: %S" entry)))

(defun org-mem-entry-property (prop entry)
  "Value of property PROP in ENTRY."
  (cdr (assoc (upcase prop) (org-mem-entry-properties entry))))

;; TODO: It would surely be useful to be able to get inherited tags even where
;;       it is not allowed.  Currently `org-mem-entry-tags-inherited' is just
;;       nil in that case, but maybe a separate field?
(defun org-mem-entry-tags (entry)
  "ENTRY tags, with inheritance if allowed at ENTRY."
  (delete-dups (append (org-mem-entry-tags-inherited entry)
                       (org-mem-entry-tags-local entry))))


;;; File data

(defun org-mem-file-attributes (file/entry/link)
  "The `file-attributes' list for file at FILE/ENTRY/LINK."
  (nth 1 (org-mem--get-file-metadata file/entry/link)))

;; REVIEW: Somehow uncomfortable with the name
(defun org-mem-file-line-count (file/entry/link)
  "Count of lines in whole file at FILE/ENTRY/LINK."
  (let ((x (nth 2 (org-mem--get-file-metadata file/entry/link))))
    (if (>= x 0) x
      (error "org-mem-file-line-count: Value not yet stored for file %s%s"
             file/entry/link
             "\nLikely due to scan errors, type M-x org-mem-list-problems"))))

(defun org-mem-file-ptmax (file/entry/link)
  "Count of characters in whole file at FILE/ENTRY/LINK.
Often close to but not exactly the size in bytes due to text encoding."
  (let ((x (nth 3 (org-mem--get-file-metadata file/entry/link))))
    (if (>= x 0) x
      (error "org-mem-file-ptmax: Value not yet stored for file %s%s"
             file/entry/link
             "\nLikely due to scan errors, type M-x org-mem-list-problems"))))

(defun org-mem-file-mtime (file/entry/link)
  "Modification time for file at FILE/ENTRY/LINK."
  (file-attribute-modification-time (org-mem-file-attributes file/entry/link)))

(defun org-mem-file-size (file/entry/link)
  "Modification time for file at FILE/ENTRY/LINK."
  (file-attribute-size (org-mem-file-attributes file/entry/link)))

(defun org-mem-file-mtime-int (file/entry/link)
  "Modification time for file at FILE/ENTRY/LINK, rounded-up integer."
  (ceiling (float-time (org-mem-file-mtime file/entry/link))))

;; Above getters accept a link as input, and the below could too
;; but extra LoC, so yolo.  Mainly wanted equivalents to
;; `org-roam-node-file-title' and `org-roam-node-file-mtime', and got 'em.

(defun org-mem-file-title-or-basename (file/entry)
  "Value of #+title in file at FILE/ENTRY; fall back on file basename.
Unlike `org-mem-entry-file-title' which may return nil,
this always returns a string."
  (or (org-mem-file-title-strict file/entry)
      (file-name-nondirectory
       (if (stringp file/entry) file/entry (org-mem-entry-file file/entry)))))

(defun org-mem-file-title-topmost (file/entry)
  "Topmost title in file at FILE/ENTRY, be that a heading or a #+title.
Can refer to a different entry than `org-mem-file-id-topmost', in the
case that there exists a file-level ID but no #+title:, or vice versa."
  ;; docstrings are hard
  ;; "Value of #+title in file at FILE/ENTRY; fall back on topmost heading."
  (let ((entries (org-mem-entries-in-file
                  (if (stringp file/entry) file/entry
                    (org-mem-entry-file file/entry)))))
    (or (org-mem-entry-title (car entries))
        (ignore-errors (org-mem-entry-title (cadr entries))))))

(defun org-mem-file-title-strict (file/entry)
  "Value of #+title setting in file at FILE/ENTRY, if any."
  (org-mem-entry-title-maybe
   (car (org-mem-entries-in-file
         (if (stringp file/entry) file/entry
           (org-mem-entry-file file/entry))))))

(defun org-mem-file-id-topmost (file/entry)
  "ID from file properties or topmost subtree in file at FILE/ENTRY."
  (let ((entries (org-mem-entries-in-file
                  (if (stringp file/entry) file/entry
                    (org-mem-entry-file file/entry)))))
    (or (org-mem-entry-id (car entries))
        (ignore-errors (org-mem-entry-id (cadr entries))))))

(defun org-mem-file-id-strict (file/entry)
  "File-level ID property in file at FILE/ENTRY, if any."
  (org-mem-entry-id (car (org-mem-entries-in-file
                          (if (stringp file/entry) file/entry
                            (org-mem-entry-file file/entry))))))


;;; Optional: Aliases and refs support

;; This used to come with `org-mem-roamy-db-mode', but bundling it here:
;; - allows a nicer namespace.
;; - frees Org-node users from needing to enable that mode at all.

;; Despite their names, properties ROAM_ALIASES and ROAM_REFS are not
;; only used by Org-roam, but can be seen as an emerging standard concept.
;; They need special handling because:

;; 1. These properties' values should be transformed from string to list via
;;    bespoke methods, not the generic `org-entry-get-multivalued-property'.
;; 2. The refs and aliases fished out of above lists should be cached, because
;;    they may be consulted a lot (just like vanilla IDs and titles), and it
;;    lets us check for collisions.

(defvar org-mem--id<>roam-refs (make-hash-table :test 'equal)
  "1:1 table mapping an ID to a list of ROAM_REFS substrings.")

(defvar org-mem--roam-ref<>id (make-hash-table :test 'equal)
  "1:1 table mapping a ROAM_REFS member to the nearby ID property.")

;; REVIEW: is it possible to get rid of this?
(defvar org-mem--roam-ref<>type (make-hash-table :test 'equal)
  "1:1 table mapping a ROAM_REFS member to its link type if any.")

(defun org-mem-entry-roam-aliases (entry)
  "Alternative titles for ENTRY, taken from property ROAM_ALIASES."
  (when-let* ((aliases (org-mem-entry-property "ROAM_ALIASES" entry)))
    (split-string-and-unquote aliases)))

(defun org-mem-entry-roam-refs (entry)
  "Valid substrings taken from property ROAM_REFS in ENTRY.
These substrings are determined by `org-mem--split-roam-refs-field'."
  (gethash (org-mem-entry-id entry) org-mem--id<>roam-refs))

(defun org-mem-roam-reflinks-to-entry (entry)
  "All links that point to a substring of ENTRY\\='s ROAM_REFS."
  (cl-loop for ref in (org-mem-entry-roam-refs entry)
           append (org-mem-links-to-roam-ref ref)))

(defun org-mem-entry-by-roam-ref (ref)
  "The entry that has ROAM_REFS property matching REF."
  (org-mem-entry-by-id (gethash ref org-mem--roam-ref<>id)))

(defun org-mem-links-to-roam-ref (ref)
  "All links to REF."
  (and ref (gethash ref org-mem--target<>links)))

(defun org-mem-all-roam-reflinks ()
  "All links targeting some existing ROAM_REFS."
  (cl-loop for ref being each hash-key of org-mem--roam-ref<>id
           append (gethash ref org-mem--target<>links)))

(defun org-mem--record-roam-aliases-and-refs (entry)
  "Add ENTRY\\='s ROAM_ALIASES and ROAM_REFS to tables."
  (when-let* ((id (org-mem-entry-id entry)))
    (dolist (alias (org-mem-entry-roam-aliases entry))
      ;; Include aliases in the collision-checks
      (when-let* ((other-id (gethash alias org-mem--title<>id)))
        (unless (string= id other-id)
          (push (list (format-time-string "%H:%M") alias id other-id)
                org-mem--title-collisions)))
      (puthash alias id org-mem--title<>id))
    (when-let* ((refs (org-mem--split-roam-refs-field
                       (org-mem-entry-property "ROAM_REFS" entry))))
      (puthash id refs org-mem--id<>roam-refs)
      (dolist (ref refs)
        (puthash ref id org-mem--roam-ref<>id)))))

(defun org-mem--forget-roam-aliases-and-refs (entry)
  "Remove ENTRY\\='s ROAM_ALIASES and ROAM_REFS from dedicated tables."
  (dolist (ref (org-mem-entry-roam-refs entry))
    (remhash (gethash ref org-mem--roam-ref<>id) org-mem--id<>roam-refs)
    (remhash ref org-mem--roam-ref<>id))
  (dolist (alias (org-mem-entry-roam-aliases entry))
    (remhash alias org-mem--title<>id)))

(defun org-mem--split-roam-refs-field (roam-refs)
  "Extract valid components of a ROAM-REFS field.
What is valid?  See \"org-mem-test.el\"."
  (when roam-refs
    (with-current-buffer (if (eq (current-buffer) org-mem-scratch)
                             org-mem-scratch
                           (setq org-mem-scratch
                                 (get-buffer-create " *org-mem-scratch*" t)))
      (erase-buffer)
      (insert roam-refs)
      (goto-char 1)
      (let (links beg end colon-pos)
        ;; Extract all [[bracketed links]]
        (while (search-forward "[[" nil t)
          (setq beg (match-beginning 0))
          (if (setq end (search-forward "]]" nil t))
              (progn
                (goto-char beg)
                (push (buffer-substring (+ 2 beg) (1- (search-forward "]")))
                      links)
                (delete-region beg end))
            (error "Missing close-bracket in ROAM_REFS property %s" roam-refs)))
        ;; Return merged list
        (cl-loop
         for link? in (append links (split-string-and-unquote (buffer-string)))
         ;; @citekey or &citekey
         if (string-match (rx (or bol (any ";:"))
                              (group (any "@&")
                                     (+ (not (any " ;]")))))
                          link?)
         ;; Replace & with @
         collect (let ((path (substring (match-string 1 link?) 1)))
                   (puthash path nil org-mem--roam-ref<>type)
                   (concat "@" path))
         ;; Some sort of uri://path
         else when (setq colon-pos (string-search ":" link?))
         collect (let ((path (string-replace
                              "%20" " "
                              (substring link? (1+ colon-pos)))))
                   ;; Remember the uri: prefix for pretty completions
                   (puthash path (substring link? 0 colon-pos)
                            org-mem--roam-ref<>type)
                   ;; .. but the actual ref is just the //path
                   path))))))

(add-hook 'org-mem-record-entry-functions
          #'org-mem--record-roam-aliases-and-refs -10)

(add-hook 'org-mem-forget-entry-functions
          #'org-mem--forget-roam-aliases-and-refs -10)


;;; Optional: Short names

;; These definitions are not used inside this file,
;; only convenience for end users (and for quick prototyping).
;; Up to them to write code readably.

(defalias 'org-mem-deadline                    #'org-mem-entry-deadline)
(defalias 'org-mem-heading-lvl                 #'org-mem-entry-level) ;; feels more legible
(defalias 'org-mem-level                       #'org-mem-entry-level)
(defalias 'org-mem-lnum                        #'org-mem-entry-lnum)
(defalias 'org-mem-olpath                      #'org-mem-entry-olpath)
(defalias 'org-mem-olpath-with-self            #'org-mem-entry-olpath-with-self)
(defalias 'org-mem-olpath-with-self-with-title #'org-mem-entry-olpath-with-self-with-title)
(defalias 'org-mem-olpath-with-title           #'org-mem-entry-olpath-with-title)
(defalias 'org-mem-olpath-with-title-with-self #'org-mem-entry-olpath-with-title-with-self)
(defalias 'org-mem-priority                    #'org-mem-entry-priority)
(defalias 'org-mem-properties                  #'org-mem-entry-properties)
(defalias 'org-mem-roam-aliases                #'org-mem-entry-roam-aliases)
(defalias 'org-mem-roam-refs                   #'org-mem-entry-roam-refs)
(defalias 'org-mem-scheduled                   #'org-mem-entry-scheduled)
(defalias 'org-mem-subtree-p                   #'org-mem-entry-subtree-p)
(defalias 'org-mem-tags                        #'org-mem-entry-tags)
(defalias 'org-mem-tags-inherited              #'org-mem-entry-tags-inherited)
(defalias 'org-mem-tags-local                  #'org-mem-entry-tags-local)
(defalias 'org-mem-text                        #'org-mem-entry-text)
(defalias 'org-mem-title                       #'org-mem-entry-title)
(defalias 'org-mem-todo-state                  #'org-mem-entry-todo-state)

(defalias 'org-mem-target         #'org-mem-link-target)
(defalias 'org-mem-nearby-id      #'org-mem-link-nearby-id)
(defalias 'org-mem-type           #'org-mem-link-type)
(defalias 'org-mem-citation-p     #'org-mem-link-citation-p)

;;; Short names, with polymorphism

(cl-defgeneric org-mem-pos (entry/link)
  "Char position of ENTRY/LINK."
  (:method ((xx org-mem-entry)) (org-mem-entry-pos xx))
  (:method ((xx org-mem-link)) (org-mem-link-pos xx)))

(cl-defgeneric org-mem-file (entry/link)
  "File name where ENTRY/LINK found."
  (:method ((xx org-mem-entry)) (org-mem-entry-file xx))
  (:method ((xx org-mem-link)) (org-mem-link-file xx)))

(cl-defgeneric org-mem-id (entry/file)
  "ID property of ENTRY/FILE - if file name, the file-level ID."
  (:method ((xx org-mem-entry)) (org-mem-entry-id xx))
  (:method ((xx string)) (org-mem-file-id-strict xx)))

(cl-defgeneric org-mem-title (entry/file)
  "Title of ENTRY/FILE - if file name, the value of #+title setting."
  (:method ((xx org-mem-entry)) (org-mem-entry-title xx))
  (:method ((xx string)) (org-mem-file-title-strict xx)))

(cl-defgeneric org-mem-roam-reflinks-to (entry/id/file)
  "All reflinks to or into ENTRY/ID/FILE."
  (:method ((xx org-mem-entry)) (org-mem-roam-reflinks-to-entry xx))
  (:method ((xx string))
           (if-let* ((entry (org-mem-entry-by-id xx)))
               (org-mem-roam-reflinks-to-entry entry)
             (seq-mapcat #'org-mem-roam-reflinks-to-entry
                         (org-mem-entries-in-file xx)))))

(cl-defgeneric org-mem-links-to (entry/id/file)
  "All links to or into ENTRY/ID/FILE."
  (:method ((xx org-mem-entry)) (org-mem-links-to-entry xx))
  (:method ((xx string))
           (if-let* ((entry (org-mem-entry-by-id xx)))
               (org-mem-links-to-entry entry)
             (org-mem-links-to-file xx))))

(defun org-mem-id-links-to (entry/id/file)
  "All ID-links to or into ENTRY/ID/FILE."
  (seq-filter (##equal (org-mem-link-type %) "id")
              (org-mem-links-to entry/id/file)))

(defun org-mem-entries-in (file/files)
  "All entries in FILE/FILES."
  (funcall (if (listp file/files) #'org-mem-entries-in-files
             #'org-mem-entries-in-file)
           file/files))


;;; Core logic

(defvar org-mem-pre-full-scan-functions nil
  "Hook passed the list of parse-results, before a full reset.")

(defvar org-mem-post-full-scan-functions nil
  "Hook passed the list of parse-results, after a full reset.")

(defvar org-mem-record-file-functions nil
  "Hook passed (FILE ATTRS LINES PTMAX) after adding that info to tables.")

(defvar org-mem-record-entry-functions nil
  "Hook passed one `org-mem-entry' object after adding it to tables.")

(defvar org-mem-record-link-functions nil
  "Hook passed one `org-mem-link' object after adding it to tables.")

(defvar org-mem-pre-targeted-scan-functions nil
  "Hook passed the list of parse-results, before a partial reset.
This occurs before scanning a targeted single file or set of files,
hence the name.  Contrast `org-mem-pre-full-scan-functions'.")

(defvar org-mem-post-targeted-scan-functions nil
  "Hook passed the list of parse-results, after a partial reset.
This occurs after scanning a targeted single file or set of files,
hence the name.  Contrast `org-mem-post-full-scan-functions'.")

(defvar org-mem-forget-file-functions nil
  "Hook passed (FILE ATTRS LINES PTMAX) after removing that info from tables.")

(defvar org-mem-forget-entry-functions nil
  "Hook passed one forgotten `org-mem-entry' object.")

(defvar org-mem-forget-link-functions nil
  "Hook passed one forgotten `org-mem-link' object.")

(defvar org-mem--problems nil)
(defvar org-mem--title-collisions nil)
(defvar org-mem--id-collisions nil)
(defvar org-mem--time-elapsed 1.0)
(defvar org-mem--next-message nil)
(defvar org-mem--time-at-begin-full-scan nil)

(defun org-mem-reset (&optional interactively)
  "Reset cache, and if called INTERACTIVELY, print statistics."
  (interactive "p")
  (when interactively
    (setq org-mem--next-message t))
  (org-mem--scan-full))

(defun org-mem--scan-full (&optional takeover)
  "Arrange a full scan, if one is not already ongoing.
With TAKEOVER t, stop any already ongoing scan to start a new one."
  (when (or takeover (not (el-job-is-busy 'org-mem)))
    (setq org-mem--time-at-begin-full-scan (current-time))
    (let ((result (el-job-launch
                   :id 'org-mem
                   :if-busy 'takeover
                   :inject-vars (org-mem--mk-work-vars)
                   :load-features '(org-mem-parser)
                   :inputs #'org-mem--list-files-from-fs
                   :funcall-per-input #'org-mem-parser--parse-file
                   :callback #'org-mem--finalize-full-scan)))
      (when (eq result 'inputs-were-empty)
        (if org-mem-do-sync-with-org-id
            (message "No org-ids found.  If you know they exist, try M-x %S."
                     (if (fboundp 'org-roam-update-org-id-locations)
                         'org-roam-update-org-id-locations
                       'org-id-update-id-locations))
          (message "No files found under `org-mem-watch-dirs'"))))))

(defun org-mem--debug-parse-file (file)
  "Debug wrapper for `org-mem-parser--parse-file'.
To use, first go to the source of that definition and type \\[edebug-defun].
Then eval this expression, substituting FILE for some file of yours:
\(org-mem--debug-parse-file \"~/org/some-file.org\")"
  (dolist (var (org-mem--mk-work-vars))
    (set (car var) (cdr var)))
  (org-mem-parser--parse-file file))

(defvar org-mem--caused-retry nil)
(defun org-mem--finalize-full-scan (parse-results _job)
  "Handle PARSE-RESULTS from `org-mem--scan-full'."
  (run-hook-with-args 'org-mem-pre-full-scan-functions parse-results)
  (mapc #'clrhash (hash-table-values org-mem--key<>subtable))
  (clrhash org-mem--title<>id)
  (clrhash org-mem--id<>entry)
  (clrhash org-mem--file<>metadata)
  (clrhash org-mem--file<>entries)
  (clrhash org-mem--internal-entry-id<>links)
  (clrhash org-mem--target<>links)
  (setq org-mem--title-collisions nil)
  (seq-let (bad-paths file-data entries links problems) parse-results
    (when bad-paths
      (org-mem--invalidate-file-names bad-paths))
    (with-current-buffer
        (setq org-mem-scratch (get-buffer-create " *org-mem-scratch*" t))
      (dolist (fdata file-data)
        (puthash (car fdata) fdata org-mem--file<>metadata)
        (run-hook-with-args 'org-mem-record-file-functions fdata))
      (dolist (entry entries)
        (org-mem--record-entry entry)
        (run-hook-with-args 'org-mem-record-entry-functions entry))
      (dolist (link links)
        (org-mem--record-link link)
        (run-hook-with-args 'org-mem-record-link-functions link)))
    (setq org-mem--time-elapsed
          (float-time (time-since org-mem--time-at-begin-full-scan)))
    (when org-mem--next-message
      (setq org-mem--next-message
            (format
             "Org-mem saw %d files, %d subtrees, %d links (%d IDs, %d ID-links) in %.2fs"
             (length (org-mem-all-files))
             (seq-count #'org-mem-entry-subtree-p (org-mem-all-entries))
             (length (org-mem-all-links))
             (length (org-mem-all-id-nodes))
             (length (org-mem-all-id-links))
             (float-time (time-since org-mem--time-at-begin-full-scan)))))
    (run-hook-with-args 'org-mem-post-full-scan-functions parse-results)
    (message "%s" org-mem--next-message)
    (setq org-mem--next-message nil)
    (when bad-paths
      ;; Scan again to catch relocated files, but guard against repeating.
      (unless (seq-intersection bad-paths org-mem--caused-retry)
        (setq org-mem--caused-retry (append bad-paths org-mem--caused-retry))
        (org-mem--scan-full)))
    (when (and org-mem--title-collisions org-mem-do-warn-title-collisions)
      (message "Some IDs share title, see M-x org-mem-list-title-collisions"))
    (when problems
      (when (and org-mem--problems
                 (string< (caar problems)
                          (caar org-mem--problems)))
        ;; Daily cleanup; last problem's HH:MM looks like the future.
        (setq org-mem--problems nil))
      (setq org-mem--problems (append problems org-mem--problems))
      (message "Scan had problems, see M-x org-mem-list-problems"))))

(defun org-mem--record-link (link)
  "Add info related to LINK to various tables."
  (push link (gethash (org-mem-link-target link) org-mem--target<>links))
  (push link (gethash (org-mem-link--internal-entry-id link)
                      org-mem--internal-entry-id<>links)))

(defun org-mem--record-entry (entry)
  "Add info related to ENTRY to various tables."
  (let ((id    (org-mem-entry-id entry))
        (file  (org-mem-entry-file entry))
        (title (org-mem-entry-title-maybe entry)))
    ;; NOTE: Puts entries in correct order because we're called by
    ;; `org-mem--finalize-full-scan' looping over entries in reverse order.
    (push entry (gethash file org-mem--file<>entries))
    (when id
      (org-mem--maybe-snitch-to-org-id entry)
      (when title
        (let ((other-id (gethash title org-mem--title<>id)))
          (when (and other-id (not (string= id other-id)))
            (push (list (format-time-string "%H:%M") title id other-id)
                  org-mem--title-collisions)))
        (puthash title id org-mem--title<>id))
      (puthash id entry org-mem--id<>entry))))

(defun org-mem--maybe-snitch-to-org-id (entry)
  "Add applicable ENTRY data to `org-id-locations'.
No-op if Org has not loaded."
  (when (and org-mem-do-sync-with-org-id
             (org-mem-entry-id entry)
             (featurep 'org-id)
             (org-mem--try-ensure-org-id-table-p))
    (puthash (org-mem-entry-id entry)
             (org-mem-entry-file entry)
             org-id-locations)))

(defun org-mem--try-ensure-org-id-table-p ()
  "Coerce `org-id-locations' into hash table form, return nil on fail."
  (require 'org-id)
  (and org-id-track-globally
       (or (hash-table-p org-id-locations)
           (ignore-errors
             (setq org-id-locations
                   (org-id-alist-to-hash org-id-locations)))
           ;; Not error because some things can still work.
           (progn
             (message "org-mem: Caught strange org-id bug, maybe restart Emacs")
             nil))))

(defun org-mem--mk-work-vars ()
  "Make alist of variables needed by `org-mem-parser--parse-file'."
  (let ((org-link-bracket-re
         ;; Mmm, copy-pasta.
         (rx "[["
	     (group (one-or-more
                     (or (not (any "[]\\"))
		         (and "\\" (zero-or-more "\\\\") (any "[]"))
		         (and (one-or-more "\\") (not (any "[]"))))))
	     "]"
	     (opt "[" (group (+? anything)) "]")
	     "]"))
        (custom-plain-re (org-mem--mk-plain-re org-mem-seek-link-types)))
    (list
     (cons '$bracket-re org-link-bracket-re)
     (cons '$plain-re custom-plain-re)
     (cons '$merged-re (concat org-link-bracket-re "\\|" custom-plain-re))
     (cons '$do-cache-text org-mem-do-cache-text)
     (cons '$inlinetask-min-level (bound-and-true-p org-inlinetask-min-level))
     (cons '$nonheritable-tags (bound-and-true-p org-tags-exclude-from-inheritance))
     (cons '$use-tag-inheritance
           (if (boundp 'org-use-tag-inheritance)
               (default-value 'org-use-tag-inheritance)
             t))
     (cons '$default-todo-re
           (let ((default (if (boundp 'org-todo-keywords)
                              (default-value 'org-todo-keywords)
                            '((sequence "TODO" "DONE")))))
             (org-mem-parser--make-todo-regexp
              (string-join (if (stringp (car default))
                               default
                             (apply #'append (mapcar #'cdr default)))
                           " "))))
     ;; These two are unused as yet.
     (cons '$structures-to-ignore (list "src" "comment" "example"))
     (cons '$drawers-to-ignore
           (delete-dups
            (list (or (and (boundp 'org-super-links-backlink-into-drawer)
                           (stringp org-super-links-backlink-into-drawer)
                           org-super-links-backlink-into-drawer)
                      "BACKLINKS")
                  "BACKLINKS"
                  "LOGBOOK"))))))

;; TODO: PR? It's useful.
;; Modified from part of `org-link-make-regexps'
(defun org-mem--mk-plain-re (link-types)
  "Build a moral equivalent to `org-link-plain-re', to match LINK-TYPES."
  (let ((non-space-bracket "[^][ \t\n()<>]"))
    (rx-let ((types-regexp (regexp (regexp-opt link-types t)))
             (parenthesis (seq (any "<([")
		               (0+ (or (regexp non-space-bracket)
			               (seq (any "<([")
			                    (0+ (regexp non-space-bracket))
			                    (any "])>"))))
		               (any "])>"))))
      (rx (seq word-start
               types-regexp
               ":"
               (group (1+ (or (regexp non-space-bracket)
                              parenthesis))
	              (or (regexp "[^[:punct:][:space:]\n]")
                          ?- ?/ parenthesis)))))))


;;; File-name subroutines

(defvar org-mem--wild-filename<>abbr-truename (make-hash-table :test 'equal)
  "1:1 table mapping a wild file name to its abbreviated truename.
See helper `org-mem--abbr-truename'.")

(defvar org-mem--first-run t
  "Hack preventing the use of `file-truename' at init.
Results are often correct anyway, and file-names found to be bad will be
fixed by an automatic re-scan.

As `file-truename' can be quite slow in some environments, it would be a
bad idea to execute it for every individual file on the first scan.
This hack effectively makes it so that it never needs to execute for the
majority of files.")

;; A design requirement is don't touch Tramp files -- neither analyze them,
;; nor scrub them from org-id-locations or the like.
;; Having this function return nil is one way to do that.
(defun org-mem--abbr-truename (wild-file)
  "For existing non-Tramp WILD-FILE, return its abbreviated truename.
Caches any non-nil result, so may return a name no longer correct."
  (and wild-file
       (or (gethash wild-file org-mem--wild-filename<>abbr-truename)
           (and (not (org-mem--tramp-file-p wild-file))
                (if-let* ((truename (and (file-exists-p wild-file)
                                         (if org-mem--first-run
                                             wild-file
                                           (file-truename wild-file)))))
                    (puthash wild-file
                             (org-mem--fast-abbrev truename)
                             org-mem--wild-filename<>abbr-truename)
                  (remhash wild-file org-mem--wild-filename<>abbr-truename))))))

(defun org-mem--fast-abbrev (absolute-file-name)
  "Abbreviate ABSOLUTE-FILE-NAME, faster than `abbreviate-file-name'."
  (let ((case-fold-search nil))
    (setq absolute-file-name (directory-abbrev-apply absolute-file-name))
    (if (string-match (with-memoization (org-mem--table 0 'org-mem--fast-abbrev)
                        (directory-abbrev-make-regexp (expand-file-name "~")))
                      absolute-file-name)
        (concat "~" (substring absolute-file-name (match-beginning 1)))
      absolute-file-name)))

(declare-function tramp-tramp-file-p "tramp")
(defun org-mem--tramp-file-p (file)
  "Pass FILE to `tramp-tramp-file-p' if Tramp loaded, else return nil.

The reasoning is that if the user has not done something in this session
to cause Tramp to load, the input FILE is unlikely to be a Tramp path.
If nevertheless it is, org-mem may have problems, but these problems
should go away after Tramp does load and `org-mem-reset' runs again."
  (and (featurep 'tramp)
       (tramp-tramp-file-p file)))

(defun org-mem--invalidate-file-names (bad)
  "Scrub bad file names BAD in the tables that can pollute a reset.
Notably, invalidate part of the cache used by `org-mem--abbr-truename'.
If `org-mem-do-sync-with-org-id' t, also scrub `org-id-locations'."
  (dolist (bad bad)
    (remhash bad org-mem--wild-filename<>abbr-truename))
  ;; Example situation: File WILD is a symlink that changed destination.
  ;; So cached TRUE led to a nonexistent file in the last scan.
  ;; Now invalidate it so we cache a correct TRUE next time.
  (maphash (lambda (wild true)
             (when (member true bad)
               (push wild bad)
               (remhash wild org-mem--wild-filename<>abbr-truename)))
           org-mem--wild-filename<>abbr-truename)
  (when (and org-mem-do-sync-with-org-id
             (org-mem--try-ensure-org-id-table-p))
    (setq org-id-locations
          (org-id-alist-to-hash
           (cl-loop for cell in (org-id-hash-to-alist org-id-locations)
                    unless (member (car cell) bad)
                    collect cell)))))


;;; File discovery

;; (benchmark-call #'org-mem--list-files-from-fs)  => 0.026 s
;; (benchmark-call #'org-roam-list-files)          => 4.141 s
(defvar org-mem--last-daa 0)
(defvar org-mem--last-trampp (featurep 'tramp))
(defvar org-mem--dedup-tbl (make-hash-table :test 'equal))
(declare-function org-id-locations-load "org-id")
(defun org-mem--list-files-from-fs ()
  "Look for Org files in `org-mem-watch-dirs'.

If user option `org-mem-do-sync-with-org-id' is t,
include files from `org-id-locations' in the result.

Return abbreviated truenames, to be directly comparable with
local variable `buffer-file-truename' and \(in most cases\)
the file names in `org-id-locations'.

Note that `org-id-locations' is not guaranteed to hold abbreviated
truenames, so this function transforms them to be sure.  That means it
is possible, though unlikely, that some resulting file names cannot be
cross-referenced with `org-id-locations' even though that is where this
function found out about the files.

If you have experienced issues programming against that reality, it may
help to set user option `find-file-visit-truename', quit Emacs, delete
`org-id-locations-file', and restart.  Or make frequent use of
`org-mem--abbr-truename'."
  (unless (or org-mem-watch-dirs org-mem-do-sync-with-org-id)
    (error "At least one setting must be non-nil: `org-mem-watch-dirs' or `org-mem-do-sync-with-org-id'"))
  ;; One of many complications we incur due to the original sin: upstream
  ;; org-id's choice to abbreviate file names.  It is after all possible for
  ;; `directory-abbrev-alist' to change during runtime.
  (when (or (not (eq org-mem--last-trampp (featurep 'tramp)))
            (not (eq org-mem--last-daa (sxhash directory-abbrev-alist))))
    (clrhash org-mem--wild-filename<>abbr-truename)
    (setq org-mem--last-trampp (featurep 'tramp))
    (setq org-mem--last-daa (sxhash directory-abbrev-alist)))

  (clrhash org-mem--dedup-tbl)
  (let ((file-name-handler-alist nil)) ;; PERF
    (dolist (dir (delete-dups (mapcar #'file-truename org-mem-watch-dirs)))
      (dolist (file (org-mem--dir-files-recursive
                     dir ".org" org-mem-watch-dirs-exclude))
        (puthash (org-mem--abbr-truename file) t org-mem--dedup-tbl)))
    ;; Maybe check org-id-locations.
    (when org-mem-do-sync-with-org-id
      (when (and (null org-mem-watch-dirs)
                 (not (featurep 'org))
                 (y-or-n-p "Option org-mem-watch-dirs unconfigured, load Org to find org-id-locations?"))
        (require 'org))
      ;; I wish for Christmas: a better org-id API...
      ;; Must be why org-roam decided to wrap around rather than fight it.
      (when (featurep 'org)
        (require 'org-id)
        (unless org-id-track-globally
          (error "If `org-mem-do-sync-with-org-id' is t, `org-id-track-globally' must also be t"))
        (when (and org-id-locations-file (null org-id-locations))
          (org-id-locations-load))
        (dolist (file (if (symbolp org-id-extra-files)
                          (symbol-value org-id-extra-files)
                        org-id-extra-files))
          (puthash (org-mem--abbr-truename file) t org-mem--dedup-tbl))
        (when (org-mem--try-ensure-org-id-table-p)
          (cl-loop
           for file being each hash-value of org-id-locations do
           (puthash (org-mem--abbr-truename file) t org-mem--dedup-tbl))))))
  (setq org-mem--first-run nil)
  (remhash nil org-mem--dedup-tbl)
  (hash-table-keys org-mem--dedup-tbl))

(defun org-mem--dir-files-recursive (dir suffix excludes)
  "Faster, purpose-made variant of `directory-files-recursively'.
Return a list of all files under directory DIR, its
sub-directories, sub-sub-directories and so on, with provisos:

- Don\\='t enter directories that are symlinks.
- Don\\='t enter directories whose name start with dot or underscore.
- Don\\='t enter directories where some substring of the full name
  matches one of strings EXCLUDES literally.
- Don\\='t collect any file where some substring of the basename
  matches one of strings EXCLUDES literally.
- Collect only files that end in SUFFIX literally.
- Don\\='t sort final results in any particular order.

Does not modify the match data."
  (let (result)
    (dolist (file (file-name-all-completions "" dir))
      (if (directory-name-p file)
          (unless (or (string-prefix-p "." file)
                      (string-prefix-p "_" file))
            (setq file (file-name-concat dir file))
            (unless (or (cl-loop for substr in excludes
                                 thereis (string-search substr file))
                        (file-symlink-p (directory-file-name file)))
              (setq result (nconc result (org-mem--dir-files-recursive
        		                  file suffix excludes)))))
        (when (string-suffix-p suffix file)
          (unless (cl-loop for substr in excludes
                           thereis (string-search substr file))
            (push (file-name-concat dir file) result)))))
    result))


;;; Assorted tools for downstream packages

(defun org-mem-block (who n-secs)
  "Wait for up to N-SECS for any current org-mem subprocesses to finish.
Symbol WHO is included in the echo area message during the wait, to help
trace who called this function.  If in doubt, pass your package name.

Return t on finish, or nil if N-SECS elapsed without finishing."
  (cl-assert (symbolp who))
  (el-job-await 'org-mem n-secs (format "%s waiting for org-mem..." who)))

;; Damn handy with llama.
(defun org-mem-delete (fn tbl)
  "Delete rows in hash table TBL that satisfy FN\(KEY VALUE)."
  (maphash (##if (funcall fn %1 %2) (remhash %1 tbl)) tbl) nil)

(defvar org-element-cache-persistent)
(defvar org-inhibit-startup)
(defun org-mem-org-mode-scratch (&optional bufname)
  "Get or create a hidden `org-mode' buffer.
Also enable `org-mode', but ignore `org-mode-hook' and startup options.

Like a temp buffer, but does not clean up.  You should probably use
`erase-buffer' in case it already contains text.  Then finish up with
`font-lock-ensure' if you need the contents fontified.

BUFNAME defaults to \" *org-mem-org-mode-scratch*\"."
  (require 'org)
  (let ((bufname (or bufname " *org-mem-org-mode-scratch*"))
        (org-inhibit-startup t)
        (org-element-cache-persistent nil))
    (or (get-buffer bufname)
        (with-current-buffer (get-buffer-create bufname t)
          (delay-mode-hooks (org-mode))
          (setq-local org-element-cache-persistent nil)
          (current-buffer)))))

(defun org-mem-fontify-like-org (string)
  "Return STRING with text properties from fontifying it in `org-mode'."
  (with-current-buffer (org-mem-org-mode-scratch)
    (erase-buffer)
    (insert string)
    (font-lock-ensure)
    (buffer-string)))


;;; End-user tool

;; Just one more thing an org-id refactor should think about.
(declare-function org-id-locations-save "org-id")
(defun org-mem-scrub-id-locations (dir)
  "Remove all references in `org-id-locations' to any files under DIR.

Note that if DIR descends from a member of `org-mem-watch-dirs',
this action may make no practical impact unless you also add DIR to
`org-mem-watch-dirs-exclude'.
This is because with `org-mem-do-sync-with-org-id' t, they simply get
added again on next scan.

Tip: In case of unsolvable problems, eval this to thoroughly wipe
org-id-locations:

\(progn
 (delete-file org-id-locations-file)
 (setq org-id-locations nil)
 (setq org-id--locations-checksum nil)
 (setq org-agenda-text-search-extra-files nil)
 (setq org-id-files nil)
 (setq org-id-extra-files nil))"
  (interactive "DForget all IDs recursively in directory: ")
  (require 'org-id)
  (let ((files (nconc (org-mem--dir-files-recursive dir ".org_exclude" nil)
                      (org-mem--dir-files-recursive dir ".org" nil))))
    (when files
      (setq files (nconc files (mapcar #'org-mem--abbr-truename files)))
      (message "Forgetting all IDs in directory %s..." dir)
      (redisplay t)
      (maphash (lambda (id file)
                 (when (member file files)
                   (remhash id org-mem--id<>entry)
                   (remhash id org-id-locations)))
               org-id-locations)
      (dolist (file files)
        (remhash file org-mem--file<>entries)
        (remhash file org-mem--file<>metadata))
      (org-id-locations-save)
      (message "Forgetting all IDs in directory %s...done" dir)
      (org-mem--scan-full))))


(define-obsolete-function-alias 'org-mem-link-dest           #'org-mem-link-target       "0.8.0 (2025-05-15)")
(define-obsolete-function-alias 'org-mem-dest                #'org-mem-target            "0.8.0 (2025-05-15)")
(define-obsolete-function-alias 'org-mem-x-fontify-like-org  #'org-mem-fontify-like-org  "0.10.0 (2025-05-18)")

(provide 'org-mem)

;;; org-mem.el ends here
