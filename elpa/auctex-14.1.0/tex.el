;;; tex.el --- Support for TeX documents.  -*- lexical-binding: t; -*-

;; Copyright (C) 1985-2025 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; AUCTeX is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides basic functions used by the AUCTeX modes.

;;; Code:

(when (< emacs-major-version 28)
  (error "AUCTeX requires Emacs 28.1 or later"))

(require 'custom)
(require 'auctex)
(require 'cl-lib)
(require 'subr-x)
(require 'texmathp)
;; seq.el is preloaded in Emacs 29, so the next form can be removed
;; once 29 is the minimum required Emacs version
(require 'seq)
;; Require dbus at compile time to get macro definition of
;; `dbus-ignore-errors'.
(eval-when-compile (require 'dbus))

;; Silence the compiler for functions:
(declare-function dbus-get-unique-name "ext:dbusbind.c"
                  (bus))
(declare-function dbus-ping "ext:dbus"
                  (bus service &optional timeout))
(declare-function dbus-introspect-get-method "ext:dbus"
                  (bus service path interface method))
(declare-function dbus-call-method "ext:dbus"
                  (bus service path interface method &rest args))
(declare-function dbus-register-signal "ext:dbus"
                  (bus service path interface signal handler &rest args))
(declare-function font-latex-setup "font-latex" nil)
(declare-function LaTeX-environment-list "latex" nil)
(declare-function LaTeX-bibliography-list "latex" nil)
(declare-function LaTeX-completion-label-annotation-function "latex" (label))
(declare-function LaTeX-completion-label-list "latex" nil)
(declare-function LaTeX-section-name "latex" (level))
(declare-function TeX-fold-mode "tex-fold" (&optional arg))
(declare-function comint-exec "ext:comint"
                  (buffer name command startfile switches))
(declare-function comint-mode "ext:comint" nil)
(declare-function tex--prettify-symbols-compose-p "ext:tex-mode"
                  (start end match))
(declare-function gnuserv-start "ext:gnuserv"
                  (&optional leave-dead) t)

;; Silence the compiler for variables:
;; tex.el: Variables defined somewhere in this file:
(defvar TeX-PDF-from-DVI)
(defvar TeX-PDF-mode)
(defvar TeX-PDF-mode-parsed)
(defvar TeX-all-extensions)
(defvar TeX-command-default)
(defvar TeX-default-extension)
(defvar TeX-esc)
(defvar TeX-interactive-mode)
(defvar TeX-macro-global)
(defvar TeX-mode-map)
(defvar TeX-mode-p)
(defvar TeX-output-extension)
(defvar TeX-source-correlate-mode)
(defvar TeX-source-specials-places)
(defvar TeX-source-specials-tex-flags)
(defvar TeX-synctex-tex-flags)
(defvar TeX-current-process-region-p)
(defvar TeX-region)
(defvar TeX-region-orig-buffer)
;; Variables defined in other AUCTeX libraries:
;; latex.el:
(defvar LaTeX-default-verb-delimiter)
(defvar LaTeX-optcl)
(defvar LaTeX-optop)
(defvar LaTeX-largest-level)
;; tex-ispell.el
(defvar TeX-ispell-verb-delimiters)
;; Others:
(defvar tex--prettify-symbols-alist)    ; tex-mode.el
(defvar Info-file-list-for-emacs)       ; info.el
(defvar ispell-parser)                  ; ispell.el
(defvar compilation-error-regexp-alist) ; compile.el
(defvar compilation-in-progress)        ; compile.el

(defconst TeX-mode-comparison-alist
  '((plain-tex-mode . plain-TeX-mode)
    (latex-mode . LaTeX-mode)
    (doctex-mode . docTeX-mode)
    (context-mode . ConTeXt-mode)
    (texinfo-mode . Texinfo-mode)
    (ams-tex-mode . AmSTeX-mode)
    (japanese-plain-tex-mode . japanese-plain-TeX-mode)
    (japanese-latex-mode . japanese-LaTeX-mode))
  "Comparison table of AUCTeX former and current mode names.
Each entry is of the form (FORMER . CURRENT) where FORMER and
CURRENT are each mode name symbols.")

(defgroup TeX-file nil
  "Files used by AUCTeX."
  :group 'AUCTeX)

(defgroup TeX-command nil
  "Calling external commands from AUCTeX."
  :group 'AUCTeX)

(defgroup LaTeX nil
  "LaTeX support in AUCTeX."
  :tag "LaTeX"
  :group 'AUCTeX
  :prefix "LaTeX-")

(defgroup TeX-misc nil
  "Various AUCTeX settings."
  :group 'AUCTeX)

;;; Site Customization
;;
;; The following variables are likely to need to be changed for your
;; site.  You should do this with customize.

(defcustom TeX-command "tex"
  "Command to run plain TeX."
  :group 'TeX-command
  :type 'string)

(defcustom TeX-Omega-command "omega"
  "Command to run plain TeX on Omega."
  :group 'TeX-command
  :type '(choice (const :tag "Aleph" "aleph")
                 (const :tag "Omega" "omega")
                 (string :tag "Other command")))

(defcustom LaTeX-command "latex"
  "Command to run LaTeX."
  :group 'TeX-command
  :type 'string)

(defcustom LaTeX-Omega-command "lambda"
  "Command to run LaTeX on Omega."
  :group 'TeX-command
  :type '(choice (const :tag "Lamed" "lamed")
                 (const :tag "Lambda" "lambda")
                 (string :tag "Other command")))

(defcustom TeX-file-line-error t
  "Whether to have TeX produce file:line:error style error messages."
  :group 'TeX-command
  :type 'boolean)

(defcustom ConTeXt-engine nil
  "Engine to use for --engine in the texexec command.
If nil, none is specified."
  :group 'TeX-command
  :type '(choice (const :tag "Unspecified" nil)
                 string))

(defcustom ConTeXt-Omega-engine TeX-Omega-command
  "Engine to use for --engine in the texexec command in Omega mode.
If nil, none is specified."
  :group 'TeX-command
  :type '(choice (const :tag "Unspecified" nil)
                 string))
;; At least in TeXLive 2009 ConTeXt does not support an omega option anymore.
(make-obsolete-variable 'ConTeXt-Omega-engine 'TeX-engine-alist "11.86")

(defcustom TeX-mode-hook nil
  "A hook run in TeX mode buffers."
  :type 'hook
  :group 'TeX-misc)

;; This is the major configuration variable.  Most sites will only need to
;; change the second string in each entry, which is the name of a command to
;; send to the shell.  If you use other formatters like AMSLaTeX or AMSTeX, you
;; can add those to the list.  See `TeX-expand-list' and
;; `TeX-expand-list-builtin' for a description of the % escapes

(defcustom TeX-command-list
  '(("TeX" "%(PDF)%(tex) %(file-line-error) %`%(extraopts) %S%(PDFout)%(mode)%' %(output-dir) %t"
     TeX-run-TeX nil
     (plain-TeX-mode AmSTeX-mode Texinfo-mode) :help "Run plain TeX")
    ("LaTeX" "%`%l%(mode)%' %T"
     TeX-run-TeX nil
     (LaTeX-mode docTeX-mode) :help "Run LaTeX")
    ;; Not part of standard TeX.
    ("Makeinfo" "makeinfo %(extraopts) %(o-dir) %t" TeX-run-compile nil
     (Texinfo-mode) :help "Run Makeinfo with Info output")
    ("Makeinfo HTML" "makeinfo %(extraopts) %(o-dir) --html %t" TeX-run-compile nil
     (Texinfo-mode) :help "Run Makeinfo with HTML output")
    ("AmSTeX" "amstex %(PDFout) %`%(extraopts) %S%(mode)%' %(output-dir) %t"
     TeX-run-TeX nil (AmSTeX-mode) :help "Run AMSTeX")
    ;; support for ConTeXt  --pg
    ;; first version of ConTeXt to support nonstopmode: 2003.2.10
    ("ConTeXt" "%(cntxcom) --once %(extraopts) %(execopts)%t"
     TeX-run-TeX nil (ConTeXt-mode) :help "Run ConTeXt once")
    ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t"
     TeX-run-TeX nil
     (ConTeXt-mode) :help "Run ConTeXt until completion")
    ("BibTeX" "bibtex %(O?aux)" TeX-run-BibTeX nil
     (plain-TeX-mode LaTeX-mode docTeX-mode AmSTeX-mode Texinfo-mode
                     ConTeXt-mode)
     :help "Run BibTeX")
    ("Biber" "biber %(output-dir) %s" TeX-run-Biber nil
     (plain-TeX-mode LaTeX-mode docTeX-mode AmSTeX-mode Texinfo-mode)
     :help "Run Biber")
    ;; Not part of standard TeX.
    ;; It seems that texindex doesn't support "--output-dir" option.
    ("Texindex" "texindex %s.??" TeX-run-command nil
     (Texinfo-mode) :help "Run Texindex")
    ;; TODO:
    ;; 1. Supply "--dvipdf" option if `TeX-PDF-mode' and
    ;;    `TeX-PDF-from-DVI' are non-nil.
    ;; 2. Supply "--build-dir=DIR" option when `TeX-output-dir' is
    ;;    non-nil.
    ("Texi2dvi" "%(PDF)texi2dvi %t" TeX-run-command nil
     (Texinfo-mode) :help "Run Texi2dvi or Texi2pdf")
    ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
    ("Print" "%p" TeX-run-command t t :help "Print the file")
    ("Queue" "%q" TeX-run-background nil t :help "View the printer queue"
     :visible TeX-queue-command)
    ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t
     (plain-TeX-mode LaTeX-mode docTeX-mode AmSTeX-mode Texinfo-mode)
     :help "Generate PostScript file")
    ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil
     (plain-TeX-mode LaTeX-mode docTeX-mode AmSTeX-mode Texinfo-mode)
     :help "Convert DVI file to PostScript")
    ("Dvipdfmx" "dvipdfmx -o %(O?pdf) %d" TeX-run-dvipdfmx nil
     (plain-TeX-mode LaTeX-mode docTeX-mode AmSTeX-mode Texinfo-mode)
     :help "Convert DVI file to PDF with dvipdfmx")
    ("Ps2pdf" "ps2pdf %f %(O?pdf)" TeX-run-ps2pdf nil
     (plain-TeX-mode LaTeX-mode docTeX-mode AmSTeX-mode Texinfo-mode)
     :help "Convert PostScript file to PDF")
    ("LaTeXMk" "latexmk %(latexmk-out) %(file-line-error) %(output-dir) \
%`%(extraopts) %S%(mode)%' %t"
     TeX-run-TeX nil (LaTeX-mode docTeX-mode) :help "Run LaTeXMk")
    ("Glossaries" "makeglossaries %(d-dir) %s" TeX-run-command nil
     (plain-TeX-mode LaTeX-mode docTeX-mode AmSTeX-mode Texinfo-mode)
     :help "Run makeglossaries to create glossary file")
    ("Index" "makeindex %(O?idx)" TeX-run-index nil
     (plain-TeX-mode LaTeX-mode docTeX-mode AmSTeX-mode Texinfo-mode)
     :help "Run makeindex to create index file")
    ("upMendex" "upmendex %(O?idx)" TeX-run-index t
     (plain-TeX-mode LaTeX-mode docTeX-mode AmSTeX-mode Texinfo-mode)
     :help "Run upmendex to create index file")
    ("Xindy" "texindy %s" TeX-run-command nil
     (plain-TeX-mode LaTeX-mode docTeX-mode AmSTeX-mode Texinfo-mode)
     :help "Run xindy to create index file")
    ("Check" "lacheck %s" TeX-run-compile nil (LaTeX-mode)
     :help "Check LaTeX file for correctness")
    ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil (LaTeX-mode)
     :help "Check LaTeX file for common mistakes")
    ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t
     :help "Spell-check the document")
    ("Clean" "TeX-clean" TeX-run-function nil t
     :help "Delete generated intermediate files")
    ("Clean All" "(TeX-clean t)" TeX-run-function nil t
     :help "Delete generated intermediate and output files")
    ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))
  "List of commands to execute on the current document.

Each element is a list, whose first element is the name of the command
as it will be presented to the user.

The second element is the string handed to the shell after being
expanded.  The expansion is done using the information found in
`TeX-expand-list'.

The third element is the function which actually start the process.
Several such hooks have been defined:

`TeX-run-command': Start up the process and show the output in a
separate buffer.  Check that there is not two commands running for the
same file.  Return the process object.

`TeX-run-format': As `TeX-run-command', but assume the output is created
by a TeX macro package.  Return the process object.

`TeX-run-TeX': For TeX output.

`TeX-run-interactive': Run TeX or LaTeX interactively.

`TeX-run-BibTeX': For BibTeX output.

`TeX-run-Biber': For Biber output.

`TeX-run-compile': Use `compile' to run the process.

`TeX-run-shell': Use `shell-command' to run the process.

`TeX-run-discard': Start the process in the background, discarding its
output.

`TeX-run-background': Start the process in the background, show output
in other window.

`TeX-run-silent': Start the process in the background.

`TeX-run-discard-foreground': Start the process in the foreground,
discarding its output.

`TeX-run-function': Execute the Lisp function or function call
specified by the string in the second element.  Consequently,
this hook does not start a process.

`TeX-run-discard-or-function': If the command is a Lisp function,
execute it as such, otherwise start the command as a process,
discarding its output.

To create your own hook, define a function taking three arguments: The
name of the command, the command string, and the name of the file to
process.  It might be useful to use `TeX-run-command' in order to
create an asynchronous process.

If the fourth element is non-nil, the user will get a chance to
modify the expanded string.

The fifth element indicates in which mode(s) the command should be
present in the Command menu.  Use t if it should be active in any
mode.  If it should only be present in some modes, specify a list with
the respective mode names.

Any additional elements get just transferred to the respective menu entries."
  :group 'TeX-command
  :type '(repeat (group :value ("" "" TeX-run-command nil t)
                        (string :tag "Name")
                        (string :tag "Command")
                        (choice :tag "How"
                                :value TeX-run-command
                                (function-item TeX-run-command)
                                (function-item TeX-run-format)
                                (function-item TeX-run-TeX)
                                (function-item TeX-run-interactive)
                                (function-item TeX-run-BibTeX)
                                (function-item TeX-run-Biber)
                                (function-item TeX-run-compile)
                                (function-item TeX-run-shell)
                                (function-item TeX-run-discard)
                                (function-item TeX-run-background)
                                (function-item TeX-run-silent)
                                (function-item TeX-run-discard-foreground)
                                (function-item TeX-run-function)
                                (function-item TeX-run-discard-or-function)
                                (function :tag "Other"))
                        (boolean :tag "Prompt")
                        (choice :tag "Modes"
                                (const :tag "All" t)
                                (set (const :tag "Plain TeX" plain-TeX-mode)
                                     (const :tag "LaTeX" LaTeX-mode)
                                     (const :tag "DocTeX" docTeX-mode)
                                     (const :tag "ConTeXt" ConTeXt-mode)
                                     (const :tag "Texinfo" Texinfo-mode)
                                     (const :tag "AmSTeX" AmSTeX-mode)))
                        (repeat :tag "Menu elements" :inline t sexp)))
  :package-version '(auctex . "14.0.9"))

(defcustom TeX-command-output-list
  '(
                                        ; Add the following line if you want to use htlatex (tex4ht)
                                        ;    ("\\`htlatex" ("html"))
    )
  "List of regexps and file extensions.

Each element is a list, whose first element is a regular expression to
match against the name of the command that will be used to process the TeX
file.

The second element is either a string or a list with a string as element.
If it is a string this is the default file extension that will be expected
for output files that are produced by commands that match the first
element.  The real file extension will be obtained from the logging output
if possible, defaulting to the given string.
If it is a list, the element of the list will be the fixed extension used
without looking at the logging output.

If this list does not yield an extension, the default is either \"dvi\"
or \"pdf\", depending on the setting of `TeX-PDF-mode'.
Extensions must be given without the \".\"."

  :group 'TeX-command
  :type '(repeat (group (regexp :tag "Command Regexp")
                        (choice (string :tag "Default Extension")
                                (group (string :tag "Fixed Extension"))))))

;; You may want to change the default LaTeX version for your site.
(defcustom LaTeX-version "2e"
  "Default LaTeX version.  Currently recognized is \"2\" and \"2e\"."
  :group 'LaTeX
  :type '(radio (const :format "%v\n%h"
                       :doc "\
The executable `latex' is LaTeX version 2."
                       "2")
                (const :format "%v\n%h"
                       :doc "\
The executable `latex' is LaTeX version 2e."
                       "2e")
                (string :tag "Other")))


;; Use different compilation commands depending on style.
;; Only works if parsing is enabled.

(defcustom LaTeX-command-style
  ;; They have all been combined in LaTeX 2e.
  '(("" "%(PDF)%(latex) %(file-line-error) %(extraopts) %(output-dir) %S%(PDFout)"))
"List of style options and LaTeX commands.

If the first element (a regular expression) matches the name of one of
the style files, any occurrence of the string `%l' in a command in
`TeX-command-list' will be replaced with the second element.  The first
match is used, if no match is found the `%l' is replaced with the empty
string."
  :group 'TeX-command
  :type '(repeat (group :value ("" "")
                        regexp (string :tag "Style"))))

;; Printing: If you want to print, TeX-print-command must be non-nil
;; (if it is nil, you'll get a complaint when using the print menu).
;; If you want to view the queue, TeX-queue-command needs to be
;; non-nil (if it is nil, it won't get mentioned in the menu).  If
;; TeX-printer-list is nil, nothing else gets asked: the menu entries
;; lead directly to the respective commands.  If those commands
;; contain %p, the value of TeX-printer-default gets inserted there,
;; no questions asked.  Now if TeX-printer-list is non-nil, you'll
;; always get asked which printer you want to use.  You can enter a
;; configured printer from TeX-printer-list, or an unknown one.  The
;; respective menus will show all configured printers.  Since you can
;; enter unknown printers, the printer name _must_ be set with %p in
;; TeX-print-command.

(defcustom TeX-print-command
  "{ test -e %d && %(o?)dvips -P%p %r %s; } || lpr -P%p %o"
  "Command used to print a file.

First `%p' is expanded to the printer name, then ordinary expansion is
performed as specified in `TeX-expand-list'.  If it is nil,
then customization is requested."
  :group 'TeX-command
  :type '(choice (string :tag "Print command")
                 (const :tag "No print command customized" nil)))

(defcustom TeX-queue-command "lpq -P%p"
  "Command used to show the status of a printer queue.

First `%p' is expanded to the printer name, then ordinary expansion is
performed as specified in `TeX-expand-list'.  If this is nil,
the printer has no corresponding command."
  :group 'TeX-command
  :type '(choice (string :tag "Queue check command")
                 (const :tag "No such command" nil)))

;; Enter the names of the printers available at your site, or nil if
;; you only have one printer.

(defcustom TeX-printer-list
  '(("Default"
     ;; Print to the (unnamed) default printer.  If there is a DVI
     ;; file print via Dvips.  If not, pass the output file (which
     ;; should then be a Postscript or PDF file) directly to lpr.
     "{ test -e %d && %(o?)dvips -f %r %s | lpr; } || lpr %o"
     ;; Show the queue for the (unnamed) default printer.
     "lpq"))
  "List of available printers.

The first element of each entry is the printer name.

The second element is the command used to print to this
printer.  It defaults to the value of `TeX-print-command' when nil.

The third element is the command used to examine the print queue for
this printer.  It defaults to the value of `TeX-queue-command' similarly.

Any occurrence of `%p' in the second or third element is expanded to
the printer name given in the first element, then ordinary expansion
is performed as specified in `TeX-expand-list'.

If this list is empty, only `TeX-print-command' and `TeX-queue-command'
get consulted."
  :group 'TeX-command
  :type '(repeat (group (string :tag "Name")
                        (option (group :inline t
                                       :extra-offset -4
                                       (choice :tag "Print"
                                               (const :tag "default")
                                               (string :format "%v"))
                                       (option (choice :tag "Queue"
                                                       (const :tag "default")
                                                       (string
                                                        :format "%v"))))))))

;; The name of the most used printer.

(defcustom TeX-printer-default (or (getenv "PRINTER")
                                   (and TeX-printer-list
                                        (car (car TeX-printer-list)))
                                   "lp")
  "Default printer to use with `TeX-command'."
  :group 'TeX-command
  :type 'string)

(defcustom TeX-print-style '(("^landscape$" "-t landscape"))
  "List of style options and print options.

If the first element (a regular expression) matches the name of one of
the style files, any occurrence of the string `%r' in a command in
`TeX-command-list' will be replaced with the second element.  The first
match is used, if no match is found the `%r' is replaced with the empty
string."
  :group 'TeX-command
  :type '(repeat (group regexp (string :tag "Command"))))

(defcustom TeX-command-extra-options ""
  "String with the extra options to be given to the TeX processor."
  :type 'string
  :local t)

(defvar TeX-command-text nil
  "Dynamically bound by `TeX-command-expand'.")
(defvar TeX-command-pos nil
  "Dynamically bound by `TeX-command-expand'.")
(defvar TeX-expand-pos nil
  "Dynamically bound by `TeX-command-expand'.")
(defvar TeX-expand-command nil
  "Dynamically bound by `TeX-command-expand'.")

;; This is the list of expansion for the commands in
;; TeX-command-list.  Not likely to be changed, but you may e.g. want
;; to handle .ps files.

(defvar TeX-expand-list-builtin
  '(("%q" (lambda ()
            (TeX-printer-query t)))
    ("%V" (lambda ()
            (TeX-source-correlate-start-server-maybe)
            (TeX-view-command-raw)))
    ("%r" (lambda ()
            (TeX-style-check TeX-print-style)))
    ("%l" (lambda ()
            (TeX-style-check LaTeX-command-style)))
    ("%(PDF)" (lambda ()
                (if (and (eq TeX-engine 'default)
                         (if TeX-PDF-mode
                             (not (TeX-PDF-from-DVI))
                           TeX-DVI-via-PDFTeX))
                    "pdf"
                  "")))
    ("%(PDFout)" (lambda ()
                   (cond ((eq major-mode 'AmSTeX-mode)
                          (if TeX-PDF-mode
                              " -output-format=pdf"
                            " -output-format=dvi"))
                         ((and (eq TeX-engine 'xetex)
                               (not TeX-PDF-mode))
                          " -no-pdf")
                         ((and (eq TeX-engine 'luatex)
                               (not TeX-PDF-mode))
                          " --output-format=dvi")
                         ((and (eq TeX-engine 'default)
                               (not TeX-PDF-mode)
                               TeX-DVI-via-PDFTeX)
                          " \"\\pdfoutput=0 \"")
                         (t ""))))
    ("%(mode)" (lambda ()
                 (if TeX-interactive-mode
                     ""
                   " -interaction=nonstopmode")))
    ("%(file-line-error)"
     (lambda () (if TeX-file-line-error " -file-line-error" "")))
    ("%(o?)" (lambda () (if (eq TeX-engine 'omega) "o" "")))
    ("%(tex)" (lambda () (eval (nth 2 (TeX-engine-in-engine-alist TeX-engine)))))
    ("%(latex)" (lambda () (eval (nth 3 (TeX-engine-in-engine-alist TeX-engine)))))
    ("%(cntxcom)" ConTeXt-expand-command)
    ("%(execopts)" ConTeXt-expand-options)
    ("%(extraopts)" (lambda () TeX-command-extra-options))
    ("%(output-dir)" TeX--output-dir-arg "--output-directory=")
    ("%(o-dir)" TeX--output-dir-arg "-o ")
    ("%(d-dir)" TeX--output-dir-arg "-d ")
    ("%S" TeX-source-correlate-expand-options)
    ("%dS" TeX-source-specials-view-expand-options)
    ("%cS" TeX-source-specials-view-expand-client)
    ("%(outpage)" (lambda ()
                    ;; When `TeX-source-correlate-output-page-function' is nil
                    ;; and we are using synctex, fallback on
                    ;; `TeX-synctex-output-page'.
                    (and TeX-source-correlate-mode
                         (null TeX-source-correlate-output-page-function)
                         (eq (TeX-source-correlate-method-active) 'synctex)
                         (setq TeX-source-correlate-output-page-function
                               #'TeX-synctex-output-page))
                    (or (if TeX-source-correlate-output-page-function
                            (funcall TeX-source-correlate-output-page-function))
                        "1")))
    ;; `TeX-active-master-with-quotes' calls either `TeX-master-file'
    ;; or `TeX-region-file' returning the master or region file, and
    ;; adds suitable quotes for use in shell command line.
    ("%s" TeX-active-master-with-quotes nil t)
    ("%t" TeX-active-master-with-quotes t t)
    ("%(s-filename-only)" TeX-active-master-with-quotes nil t nil nil file-name-nondirectory)
    ("%(t-filename-only)" TeX-active-master-with-quotes t t nil nil file-name-nondirectory)
    ;; If any TeX codes appear in the interval between %` and %', move
    ;; all of them after the interval and supplement " \input".  The
    ;; appearance is marked by leaving the bind to `TeX-command-text'
    ;; with the TeX codes.
    ;; Rule:
    ;; 1. %` and %' must appear in pair.
    ;; 2. %` and %' must not appear more than once in one command
    ;;    line string (including the results of %-expansion).
    ;; 3. Each TeX codes between %` and %' must be enclosed in
    ;;    double quotes and preceded by a space.
    ("%`" (lambda nil
            (setq TeX-command-pos t TeX-command-text nil)
            ""))
    (" \"\\" (lambda nil
               (if (eq TeX-command-pos t)
                   (setq TeX-command-pos TeX-expand-pos
                         TeX-expand-pos (+ 3 TeX-expand-pos))
                 (setq TeX-expand-pos (1+ TeX-expand-pos)))))
    ("\"" (lambda nil (if (numberp TeX-command-pos)
                          (setq TeX-command-text
                                (concat
                                 TeX-command-text
                                 (substring TeX-expand-command
                                            TeX-command-pos
                                            (1+ TeX-expand-pos)))
                                TeX-expand-command
                                (concat
                                 (substring TeX-expand-command
                                            0
                                            TeX-command-pos)
                                 (substring TeX-expand-command
                                            (1+ TeX-expand-pos)))
                                TeX-expand-pos TeX-command-pos
                                TeX-command-pos t)
                        (setq TeX-expand-pos (1+ TeX-expand-pos)))))
    ("%'" (lambda nil
            (setq TeX-command-pos nil)
            (if (stringp TeX-command-text)
                (progn
                  (setq TeX-expand-pos (+ TeX-expand-pos (length TeX-command-text) 9))
                  (concat TeX-command-text " \"\\input\""))
              "")))
    ;; The fourth argument of t directs to supply "\detokenize{}" when
    ;; necessary.  See doc string and comment of
    ;; `TeX-active-master-with-quotes'.
    ("%T" TeX-active-master-with-quotes t t nil t)
    ("%n" TeX-current-line)
    ("%d" TeX-active-master-with-quotes "dvi" t)
    ("%f" TeX-active-master-with-quotes "ps" t)
    ("%(O?aux)" TeX-active-master-with-quotes "aux" t)
    ("%(O?idx)" TeX-active-master-with-quotes "idx" t)
    ("%(O?pdf)" TeX-active-master-with-quotes "pdf" t)
    ("%o" (lambda nil (TeX-active-master-with-quotes (TeX-output-extension) t)))
    ;; for source specials the file name generated for the xdvi
    ;; command needs to be relative to the master file, just in
    ;; case the file is in a different subdirectory
    ("%b" TeX-current-file-name-master-relative)
    ;; Okular forward PDF search requires absolute path.
    ("%a" (lambda nil (prin1-to-string (expand-file-name (TeX-buffer-file-name)))))
    ;; the following is for preview-latex.
    ("%m" preview-create-subdirectory)
    ;; LaTeXMk support
    ("%(latexmk-out)"
     (lambda ()
       (cond ((eq TeX-engine 'xetex)
              " -pdf -pdflatex=xelatex")
             ((eq TeX-engine 'luatex)
              (cond ((and TeX-PDF-mode
                          (TeX-PDF-from-DVI))
                     " -dvilua -pdfdvi")
                    ((and (not TeX-PDF-mode)
                          TeX-DVI-via-PDFTeX)
                     " -dvilua -ps")
                    ;; This covers the case:
                    ;; (and TeX-PDF-mode (not (TeX-PDF-from-DVI)))
                    (t
                     " -pdflua")))
             ;; This covers everything else since we ignore 'omega:
             (t
              (cond ((and TeX-PDF-mode
                          (not (TeX-PDF-from-DVI)))
                     " -pdf")
                    ((and TeX-PDF-mode
                          (string= (TeX-PDF-from-DVI) "Dvips"))
                     " -pdfps")
                    ;; FIXME: This might be inaccurate:
                    ((and TeX-PDF-mode
                          (string= (TeX-PDF-from-DVI) "Dvipdfmx"))
                     " -pdfdvi")
                    ((and (not TeX-PDF-mode)
                          TeX-DVI-via-PDFTeX)
                     " -pdflatex -dvi -ps")
                    (t
                     " -dvi -ps")))))))
  "List of built-in expansion strings for TeX command names.

This should not be changed by the user who can use
`TeX-expand-list' variable.  The latter variable also contains a
description of the data format.

Programs should not use these variables directly but the function
`TeX-expand-list'.")

(defcustom TeX-expand-list nil
  "List of expansion strings for TeX command names defined by the user.

Each entry is a list with two or more elements.  The first
element is the string to be expanded.  The second element is the
name of a function returning the expanded string when called with
the remaining elements as arguments.
The second element can also be a variable name whose value is
such function.

Built-in expansions provided in `TeX-expand-list-builtin' can be
overwritten by defining expansions strings with the same
expander.  Only \"%p\" expander cannot be overwritten.

Programs should not use these variables directly but the function
`TeX-expand-list'."
  :group 'TeX-command
  :type '(repeat (group (string :tag "Key")
                        (sexp :tag "Expander")
                        (repeat :inline t
                                :tag "Arguments"
                                (sexp :format "%v")))))

(defun TeX-expand-list ()
  "Complete list of expansion strings for TeX command names.

Concatenate `TeX-expand-list' and `TeX-expand-list-builtin' making
sure \"%p\" is the first entry."
  (append
   ;; %p must be the first entry, see `TeX-print-command'.
   '(("%p" TeX-printer-query))
   TeX-expand-list
   TeX-expand-list-builtin))

(defcustom TeX-parse-all-errors t
  "Whether to automatically collect all warning and errors after running TeX.

If t, it makes it possible to use `TeX-previous-error' with TeX
commands."
  :group 'TeX-command
  :type 'boolean)

(defcustom TeX-LaTeX-sentinel-banner-regexp
  "^\\(\\*\\* \\)?J?I?p?\\(La\\|Sli\\)TeX\\(2e\\)? \
\\(Version\\|ver\\.\\|<[0-9/-]*\\(?:u[^>]*\\)?>\\)"
  "Regexp to identify the banner line in the LaTeX output."
  :group 'TeX-output
  :type 'regexp)

;;; Portability.

(defmacro TeX--if-macro-fboundp (name then &rest else)
  "Execute THEN if macro NAME is bound and ELSE otherwise.
Essentially,

  (TeX--if-macro-fboundp name then else...)

is equivalent to

  (if (fboundp \\='name) then else...)

but takes care of byte-compilation issues where the byte-code for
the latter could signal an error if it has been compiled with
emacs 24.1 and is then later run by emacs 24.5."
  (declare (indent 2) (debug (symbolp form &rest form)))
  (if (fboundp name)             ;If macro exists at compile-time, just use it.
      then
    `(if (fboundp ',name)               ;Else, check if it exists at run-time.
         (eval ',then)                  ;If it does, then run the then code.
       ,@else)))                ;Otherwise, run the else code.

(require 'easymenu)

;;; Documentation for Info-goto-emacs-command-node and similar

(eval-after-load 'info '(dolist (elt '("TeX" "LaTeX" "ConTeXt" "Texinfo"
                                       "docTeX"))
                          (add-to-list 'Info-file-list-for-emacs
                                       (cons elt "AUCTeX"))))

(advice-add 'hack-one-local-variable :after #'TeX--call-minor-mode)
(defun TeX--call-minor-mode (var val &rest _)
  "Call minor mode function if minor mode variable is found."
  ;; Instead of checking for each mode explicitly `minor-mode-list'
  ;; could be used.  But this may make the byte compiler pop up.
  (when (memq var '(TeX-PDF-mode
                    TeX-source-correlate-mode TeX-interactive-mode
                    TeX-fold-mode LaTeX-math-mode))
    (funcall var (if (symbol-value val) 1 0))))

(defvar TeX-overlay-priority-step 16
  "Numerical difference of priorities between nested overlays.
The step should be big enough to allow setting a priority for new
overlays between two existing ones.")

;; require crm here, because we often do
;;
;; (let ((crm-separator ","))
;;   (TeX-completing-read-multiple ...))
;;
;; which results in a void-variable error if crm hasn't been loaded before.
(require 'crm)

;; For GNU Emacs 24.4 or later, based on `completing-read-multiple' of
;; git commit b14abca9476cba2f500b5eda89441d593dd0f12b
;;   2013-01-10  * lisp/emacs-lisp/crm.el: Allow any regexp for separators.
(defun TeX-completing-read-multiple
    (prompt table &optional predicate require-match initial-input
            hist def inherit-input-method)
  "Like `completing-read-multiple' which see.
Retain zero-length substrings but ensure that empty input results
in nil across different emacs versions."
  (unwind-protect
      (progn
        (add-hook 'choose-completion-string-functions
                  #'crm--choose-completion-string)
        (let* ((minibuffer-completion-table #'crm--collection-fn)
               (minibuffer-completion-predicate predicate)
               ;; see completing_read in src/minibuf.c
               (minibuffer-completion-confirm
                (unless (eq require-match t) require-match))
               (crm-completion-table table)
               (map (if require-match
                        crm-local-must-match-map
                      crm-local-completion-map))
               ;; If the user enters empty input, `read-from-minibuffer'
               ;; returns the empty string, not DEF.
               (input (read-from-minibuffer
                       prompt initial-input map
                       nil hist def inherit-input-method))
               result)
          (and def (string-equal input "") (setq input def))
          (if (equal (setq result (split-string input crm-separator))
                     '(""))
              nil
            result)))
    (remove-hook 'choose-completion-string-functions
                 #'crm--choose-completion-string)))

(defun TeX-read-string (prompt &optional initial-input history default-value)
  (read-string prompt initial-input history default-value t))

(defun TeX-active-mark ()
  (and transient-mark-mode mark-active))

(defun TeX-activate-region ()
  (setq deactivate-mark nil)
  (activate-mark))

(defun TeX-overlay-prioritize (start end)
  "Calculate a priority for an overlay extending from START to END.
The calculated priority is lower than the minimum of priorities
of surrounding overlays and higher than the maximum of enclosed
overlays."
  (let (outer-priority inner-priority ov-priority)
    (dolist (ov (overlays-in start end))
      (when (or (eq (overlay-get ov 'category) 'TeX-fold)
                (overlay-get ov 'preview-state))
        (setq ov-priority (overlay-get ov 'priority))
        (if (>= (overlay-start ov) start)
            (setq inner-priority (max ov-priority (or inner-priority
                                                      ov-priority)))
          (setq outer-priority (min ov-priority (or outer-priority
                                                    ov-priority))))))
    (cond ((and inner-priority (not outer-priority))
           (+ inner-priority TeX-overlay-priority-step))
          ((and (not inner-priority) outer-priority)
           (/ outer-priority 2))
          ((and inner-priority outer-priority)
           (+ (/ (- outer-priority inner-priority) 2) inner-priority))
          (t TeX-overlay-priority-step))))

(defun TeX-delete-dups-by-car (alist &optional keep-list)
  "Return a list of all elements in ALIST, but each car only once.
Elements of KEEP-LIST are not removed even if duplicate."
  ;; Copy of `reftex-uniquify-by-car' (written by David Kastrup).
  (setq keep-list (TeX-sort-strings keep-list))
  (setq alist (sort (copy-sequence alist)
                    #'TeX-car-string-lessp))
  (let ((new alist) elt)
    (while (cdr new)
      (setq elt (caar new))
      (while (and keep-list (string< (car keep-list) elt))
        (setq keep-list (cdr keep-list)))
      (unless (and keep-list (string= elt (car keep-list)))
        (while (string= elt (car (cadr new)))
          (setcdr new (cddr new))))
      (setq new (cdr new))))
  alist)

(defun TeX-delete-duplicate-strings (list)
  "Return a list of all strings in LIST, but each only once."
  (setq list (TeX-sort-strings list))
  (let ((new list) elt)
    (while (cdr new)
      (setq elt (car new))
      (while (string= elt (cadr new))
        (setcdr new (cddr new)))
      (setq new (cdr new))))
  list)

(defun TeX-sort-strings (list)
  "Return sorted list of all strings in LIST."
  (sort (copy-sequence list) #'string<))

(defun TeX-car-string-lessp (s1 s2)
  "Compare the cars of S1 and S2 in lexicographic order.
Return t if first is less than second in lexicographic order."
  (string-lessp (car s1) (car s2)))

;;; Buffer

(defgroup TeX-output nil
  "Parsing TeX output."
  :prefix "TeX-"
  :group 'AUCTeX)

(defcustom TeX-display-help t
  "Control type of help display when stepping through errors with \\[TeX-next-error].
If t display help buffer.  If nil display message about error in
echo area.  If `expert' display output buffer with raw processor output."
  :group 'TeX-output
  :type '(choice (const :tag "Help buffer" t)
                 (const :tag "Echo area" nil)
                 (const :tag "Output buffer" expert)))

(defcustom TeX-debug-bad-boxes nil
  "Non-nil means also find overfull/underfull box warnings with \\[TeX-next-error]."
  :group 'TeX-output
  :type 'boolean)

(defcustom TeX-debug-warnings nil
  "Non-nil means also find LaTeX or package warnings with \\[TeX-next-error]."
  :group 'TeX-output
  :type 'boolean)

(defcustom TeX-ignore-warnings nil
  "Controls which warnings are to be ignored.

It can be either a regexp matching warnings to be ignored, or a
symbol with the name of a custom function taking as arguments all
the information of the warning listed in `TeX-error-list', except
the last one about whether to ignore the warning.

If you want to use the custom function, see how it is used in the
code of `TeX-warning'."
  :group 'TeX-command
  :type '(choice (const  :tag "Do not ignore anything" nil)
                 (string :tag "Regexp")
                 (symbol :tag "Function name")))

(defcustom TeX-suppress-ignored-warnings nil
  "Whether to actually show ignored warnings.

Note that `TeX-debug-warnings' always takes the precedence."
  :group 'TeX-command
  :type 'boolean)

(defun TeX-toggle-debug-bad-boxes ()
  "Toggle if the debugger should display \"bad boxes\" too."
  (interactive)
  (setq TeX-debug-bad-boxes (not TeX-debug-bad-boxes))
  (message (concat "TeX-debug-bad-boxes: "
                   (if TeX-debug-bad-boxes "on" "off"))))

(defun TeX-toggle-debug-warnings ()
  "Toggle if the debugger should display warnings too."
  (interactive)
  (setq TeX-debug-warnings (not TeX-debug-warnings))
  (message (concat "TeX-debug-warnings: "
                   (if TeX-debug-warnings "on" "off"))))

(defun TeX-toggle-suppress-ignored-warnings ()
  "Toggle if the debugger should display ignored warnings too.

See `TeX-suppress-ignored-warnings' and `TeX-ignore-warnings' for
more details."
  (interactive)
  (setq TeX-suppress-ignored-warnings (not TeX-suppress-ignored-warnings))
  (message (concat "TeX-suppress-ignored-warnings: "
                   (if TeX-suppress-ignored-warnings "on" "off"))))

;;; Mode names.

(defvar-local TeX-base-mode-name nil
  "Base name of mode.")

(defun TeX-set-mode-name (&optional changed local reset)
  "Build and set the mode name.
The base mode name will be concatenated with indicators for
helper modes where appropriate.

If CHANGED is non-nil, it indicates which global mode
may have changed so that all corresponding buffers
without a local value might get their name updated.
A value of t will thus update all buffer names.

If LOCAL is non-nil and CHANGED is buffer-local, only
a local change has been performed and only the local
name is to be updated.

If RESET is non-nil, `TeX-command-next' is reset to
`TeX-command-default' in updated buffers."
  (if (and changed
           (not (and local (local-variable-p changed (current-buffer)))))
      (dolist (buffer (buffer-list))
        (and (local-variable-p 'TeX-mode-p buffer)
             (not (local-variable-p changed buffer))
             (with-current-buffer buffer (TeX-set-mode-name nil nil reset))))
    (if TeX-mode-p
        (let ((trailing-flags
               (concat
                (and (boundp 'TeX-fold-mode) TeX-fold-mode "F")
                (and (boundp 'LaTeX-math-mode) LaTeX-math-mode "M")
                (and TeX-PDF-mode "P")
                (and TeX-interactive-mode "I")
                (and TeX-source-correlate-mode "S"))))
          (setq mode-name (concat TeX-base-mode-name
                                  (when (> (length trailing-flags) 0)
                                    (concat "/" trailing-flags))))
          (when reset
            (TeX-process-set-variable (TeX-master-file)
                                      'TeX-command-next TeX-command-default)
            (TeX-process-set-variable (TeX-region-file)
                                      'TeX-command-next TeX-command-default))
          (set-buffer-modified-p (buffer-modified-p))))))

(defun TeX-mode-prefix (&optional mode)
  "Return the prefix for the symbol MODE as string.
If no mode is given the current major mode is used."
  (cdr (assoc (or mode major-mode) '((plain-TeX-mode . "plain-TeX")
                                     (LaTeX-mode . "LaTeX")
                                     (AmSTeX-mode . "AmSTeX")
                                     (docTeX-mode . "docTeX")
                                     (Texinfo-mode . "Texinfo")
                                     (ConTeXt-mode . "ConTeXt")))))

;;; Viewing

(defgroup TeX-view nil
  "Calling viewers from AUCTeX."
  :group 'TeX-command)

(defvar TeX-view-predicate-list-builtin
  '((output-dvi
     (string-match "dvi" (TeX-output-extension)))
    (output-pdf
     (string-match "pdf" (TeX-output-extension)))
    (output-html
     (string-match "html" (TeX-output-extension)))
    (has-no-display-manager
     (not (display-graphic-p)))
    (style-pstricks
     (TeX-match-style "^pstricks$\\|^pst-\\|^psfrag$"))
    (engine-omega
     (eq TeX-engine 'omega))
    (engine-xetex
     (eq TeX-engine 'xetex))
    (mode-io-correlate
     TeX-source-correlate-mode)
    (paper-landscape
     (and (fboundp 'LaTeX-match-class-option)
          (LaTeX-match-class-option "\\`landscape\\'")))
    (paper-portrait
     (not (and (fboundp 'LaTeX-match-class-option)
               (LaTeX-match-class-option "\\`landscape\\'"))))
    (paper-a4
     (let ((regex "\\`\\(?:a4paper\\|a4dutch\\|a4wide\\|sem-a4\\)\\'"))
       (or (TeX-match-style regex)
           (and (fboundp 'LaTeX-match-class-option)
                (LaTeX-match-class-option regex)))))
    (paper-a5
     (let ((regex "\\`\\(?:a5paper\\|a5comb\\)\\'"))
       (or (TeX-match-style regex)
           (and (fboundp 'LaTeX-match-class-option)
                (LaTeX-match-class-option regex)))))
    (paper-b5
     (and (fboundp 'LaTeX-match-class-option)
          (LaTeX-match-class-option "\\`b5paper\\'")))
    (paper-letter
     (and (fboundp 'LaTeX-match-class-option)
          (LaTeX-match-class-option "\\`letterpaper\\'")))
    (paper-legal
     (and (fboundp 'LaTeX-match-class-option)
          (LaTeX-match-class-option "\\`legalpaper\\'")))
    (paper-executive
     (and (fboundp 'LaTeX-match-class-option)
          (LaTeX-match-class-option "\\`executivepaper\\'"))))
  "Alist of built-in predicates for viewer selection and invocation.
See the doc string of `TeX-view-predicate-list' for a short
description of each predicate.")

(defcustom TeX-view-predicate-list nil
  "Alist of predicates for viewer selection and invocation.
The key of each list item is a symbol and the value a Lisp form
to be evaluated.  The form should return nil if the predicate is
not fulfilled.

Built-in predicates provided in `TeX-view-predicate-list-builtin'
can be overwritten by defining predicates with the same symbol.

The following built-in predicates are available:
  `output-dvi': The output is a DVI file.
  `output-pdf': The output is a PDF file.
  `output-html': The output is an HTML file.
  `style-pstricks': The document loads a PSTricks package.
  `engine-omega': The Omega engine is used for typesetting.
  `engine-xetex': The XeTeX engine is used for typesetting.
  `mode-io-correlate': TeX Source Correlate mode is active.
  `paper-landscape': The document is typeset in landscape orientation.
  `paper-portrait': The document is not typeset in landscape orientation.
  `paper-a4': The paper format is A4.
  `paper-a5': The paper format is A5.
  `paper-b5': The paper format is B5.
  `paper-letter': The paper format is letter.
  `paper-legal': The paper format is legal.
  `paper-executive': The paper format is executive."
  :group 'TeX-view
  :type '(alist :key-type symbol :value-type (group sexp)))

;; XXX: Atril and xreader are forks of Evince and share an almost
;; identical interface with it.  Instead of having different functions
;; for each program, we keep the original *-evince-* functions and
;; make them accept arguments to specify the actual name of the
;; program and the desktop environment, that will be used to set up
;; DBUS communication.

(defun TeX-evince-dbus-p (de app &rest options)
  "Return non-nil, if an evince-compatible reader is accessible via DBUS.
Additional OPTIONS may be given to extend the check.  If none are
given, only the minimal requirements needed by backward search
are checked.  If OPTIONS include `:forward', which is currently
the only option, then additional requirements needed by forward
search are checked, too.

DE is the name of the desktop environment, APP is the name of viewer."
  (let ((dbus-debug nil))
    (and (featurep 'dbusbind)
         (require 'dbus nil :no-error)
         (dbus-ignore-errors (dbus-get-unique-name :session))
         ;; Apparently, `dbus-ping' can signal errors in certain
         ;; situations.  If so, fail gracefully (bug#59380).
         (ignore-errors
           (dbus-ping :session (format "org.%s.%s.Daemon" de app)
                      ;; Don't block for up to 25 secs if something
                      ;; is wonky.
                      2000))
         (or (not (memq :forward options))
             (let ((spec (dbus-introspect-get-method
                          :session (format "org.%s.%s.Daemon" de app)
                          (format "/org/%s/%s/Daemon" de app)
                          (format "org.%s.%s.Daemon" de app)
                          "FindDocument")))
               ;; FindDocument must exist, and its signature must be
               ;; (String, Boolean, String).  Evince versions between
               ;; 2.30 and 2.91.x didn't have the Boolean spawn
               ;; argument we need to start evince initially.
               (and
                spec
                (equal '("s" "b" "s")
                       (delq nil (mapcar
                                  (lambda (elem)
                                    (when (and (listp elem)
                                               (eq (car elem) 'arg))
                                      (cdr (caar (cdr elem)))))
                                  spec)))))))))

(defun TeX-pdf-tools-sync-view ()
  "Focus the focused page/paragraph in `pdf-view-mode'.
If `TeX-source-correlate-mode' is disabled, only find and pop to
the output PDF file.  Used by default for the PDF Tools viewer
entry in `TeX-view-program-list-builtin'."
  ;; Make sure `pdf-tools' is at least in the `load-path', but the user must
  ;; take care of properly loading and installing the package.  We used to test
  ;; "(featurep 'pdf-tools)", but that doesn't play well with deferred loading.
  (unless (fboundp 'pdf-tools-install)
    (error "PDF Tools are not available"))
  (unless TeX-PDF-mode
    (error "PDF Tools only work with PDF output"))
  (add-hook 'pdf-sync-backward-redirect-functions
            #'TeX-source-correlate-handle-TeX-region)
  (if (and TeX-source-correlate-mode
           (fboundp 'pdf-sync-forward-search))
      (with-current-buffer (or (when TeX-current-process-region-p
                                 (get-file-buffer (TeX-region-file t)))
                               (current-buffer))
        (save-restriction
          (widen)
          (pdf-sync-forward-search)))
    (let ((pdf (TeX-active-master (TeX-output-extension))))
      (pop-to-buffer (or (find-buffer-visiting pdf)
                         (find-file-noselect pdf))))))

(defcustom TeX-view-evince-keep-focus nil
  "Whether Emacs retains the focus when viewing PDF files with Evince.

When calling `TeX-evince-sync-view', Evince normally captures the
focus.  If this option is set to non-nil, Emacs will retain the
focus."
  :group 'TeX-view
  :type 'boolean)

(defun TeX-evince-sync-view-1 (de app)
  "Focus the focused page/paragraph in Evince with the position
of point in emacs by using Evince's DBUS API.  Used by default
for the Evince-compatible entries in
`TeX-view-program-list-builtin' if the requirements are met.

DE is the name of the desktop environment, APP is the name of
viewer."
  (require 'url-util)
  (let* ((uri (concat "file://"
                      ;; bug#45510: ? in filenames must be escaped as
                      ;; %3F to be a proper URI.
                      (replace-regexp-in-string
                       "[?]" "%3F"
                       (url-encode-url
                        (expand-file-name
                         (TeX-active-master (TeX-output-extension)))))))
         (owner (dbus-call-method
                 :session (format "org.%s.%s.Daemon" de app)
                 (format "/org/%s/%s/Daemon" de app)
                 (format "org.%s.%s.Daemon" de app)
                 "FindDocument"
                 uri
                 t)))
    (if owner
        (with-current-buffer (or (when TeX-current-process-region-p
                                   (get-file-buffer (TeX-region-file t)))
                                 (current-buffer))
          (dbus-call-method
           :session owner
           (format "/org/%s/%s/Window/0" de app)
           (format "org.%s.%s.Window" de app)
           "SyncView"
           (TeX-buffer-file-name)
           (list :struct :int32 (1+ (TeX-current-offset))
                 ;; FIXME: Using `current-column' here is dubious.
                 ;; Most of CJK letters count as occupying 2 columns,
                 ;; so the column number is not equal to the number of
                 ;; the characters counting from the beginning of a
                 ;; line.  What is the right number to specify here?
                 ;; number of letters? bytes in UTF8? or other?
                 :int32 (1+ (current-column)))
           :uint32 0)
          (when TeX-view-evince-keep-focus
            (select-frame-set-input-focus (selected-frame))))
      (error "Couldn't find the %s instance for %s" (capitalize app) uri))))

(defun TeX-atril-sync-view ()
  "Run `TeX-evince-sync-view-1', which see, set up for Atril."
  (TeX-evince-sync-view-1 "mate" "atril"))

(defun TeX-evince-sync-view ()
  "Run `TeX-evince-sync-view-1', which see, set up for Evince."
  (TeX-evince-sync-view-1 "gnome" "evince"))

(defun TeX-reader-sync-view ()
  "Run `TeX-evince-sync-view-1', which see, set up for Xreader."
  (TeX-evince-sync-view-1 "x" "reader"))

(defun TeX-view-program-select-evince (de app)
  "Select how to call the Evince-like viewer.

DE is the name of the desktop environment, APP is the name of
viewer."
  (if (TeX-evince-dbus-p de app :forward)
      (intern (format "TeX-%s-sync-view" app))
    `(,app (mode-io-correlate
            ;; When tex.el is loaded as response to opening a tex file
            ;; in a non-existent directory, we need to make sure
            ;; `default-directory' exists, otherwise the shell-command
            ;; below will error (bug#50225).
            ,(let ((default-directory (file-name-as-directory
                                       (expand-file-name "~"))))
               ;; With evince 3, -p N opens the page *labeled* N,
               ;; and -i,--page-index the physical page N.
               (if (string-match "--page-index"
                                 (shell-command-to-string (concat app " --help")))
                   " -i %(outpage)"
                 " -p %(outpage)"))) " %o")))

(defvar TeX-view-program-list-builtin
  (cond
   ((eq system-type 'windows-nt)
    '(("Yap" ("yap -1" (mode-io-correlate " -s %n%b") " %o") "yap")
      ("dviout" ("dviout -1 "
                 ((paper-a4 paper-portrait) "-y=A4 ")
                 ((paper-a4 paper-landscape) "-y=A4L ")
                 ((paper-a5 paper-portrait) "-y=A5 ")
                 ((paper-a5 paper-landscape) "-y=A5L ")
                 ((paper-b5 paper-portrait) "-y=E5 ")
                 ((paper-b5 paper-landscape) "-y=E5L ")
                 ((paper-b4jis paper-portrait) "-y=B4 ")
                 ((paper-b4jis paper-landscape) "-y=B4L ")
                 ((paper-b5jis paper-portrait) "-y=B5 ")
                 ((paper-b5jis paper-landscape) "-y=B5L ")
                 (paper-legal "-y=Legal ")
                 (paper-letter "-y=Letter ")
                 (paper-executive "-y=Executive ")
                 "%d" (mode-io-correlate " \"# %n '%b'\"")) "dviout")
      ("PDF Tools" TeX-pdf-tools-sync-view)
      ("SumatraPDF"
       ("SumatraPDF -reuse-instance"
        (mode-io-correlate " -forward-search \"%b\" %n") " %o")
       "SumatraPDF")
      ("dvips and start" "dvips %d -o && start \"\" %f" "dvips")
      ("start" "start \"\" %o")))
   ((eq system-type 'darwin)
    '(("Preview.app" "open -a Preview.app %o" "open")
      ("Skim" "open -a Skim.app %o" "open")
      ("PDF Tools" TeX-pdf-tools-sync-view)
      ("displayline" "displayline %n %o %b" "displayline")
      ("open" "open %o" "open")))
   (t
    `(("dvi2tty" ("dvi2tty -q -w 132 %o"))
      ("xdvi" ("%(o?)xdvi"
               (mode-io-correlate " -sourceposition \"%n %b\" -editor \"%cS\"")
               ((paper-a4 paper-portrait) " -paper a4")
               ((paper-a4 paper-landscape) " -paper a4r")
               ((paper-a5 paper-portrait) " -paper a5")
               ((paper-a5 paper-landscape) " -paper a5r")
               (paper-b5 " -paper b5")
               (paper-letter " -paper us")
               (paper-legal " -paper legal")
               (paper-executive " -paper 7.25x10.5in")
               " %d") "%(o?)xdvi")
      ("dvips and gv" "%(o?)dvips %d -o && gv %f" ,(list "%(o?)dvips" "gv"))
      ("gv" "gv %o" "gv")
      ("xpdf" ("xpdf -remote %s -raise %o" (mode-io-correlate " %(outpage)")) "xpdf")
      ("Evince" ,(TeX-view-program-select-evince "gnome" "evince") "evince")
      ("Atril" ,(TeX-view-program-select-evince "mate" "atril") "atril")
      ("Xreader" ,(TeX-view-program-select-evince "x" "reader") "xreader")
      ("Okular" ("okular --unique %o" (mode-io-correlate "#src:%n%a")) "okular")
      ("xdg-open" "xdg-open %o" "xdg-open")
      ("PDF Tools" TeX-pdf-tools-sync-view)
      ("Zathura"
       ("zathura %o"
        (mode-io-correlate
         " --synctex-forward %n:0:\"%b\" -x \"emacsclient +%{line} %{input}\""))
       "zathura")
      ("Sioyek"
       ("sioyek %o"
        (mode-io-correlate
         ,(concat
           " --forward-search-file \"%b\""
           " --forward-search-line %n"
           " --inverse-search \"emacsclient +%2 %1\"")))
       "sioyek"))))
  "Alist of built-in viewer specifications.
This variable should not be changed by the user who can use
`TeX-view-program-list' to add new viewers or overwrite the
definition of built-in ones.  The latter variable also contains a
description of the data format.")

(defcustom TeX-view-program-list nil
  "List of viewer specifications.
This variable can be used to specify how a viewer is to be invoked and
thereby add new viewers on top of the built-in list of viewers defined
in `TeX-view-program-list-builtin' or override entries in the latter
which also serves as an example for usage.

The car of each item is a string with a user-readable name.  The second
element can be a command line to be run as a process or a Lisp function
to be executed.  The command line can either be specified as a single
string or a list of strings and two-part lists.  The first element of
the two-part lists is a symbol or a list of symbols referring to one or
more of the predicates in `TeX-view-predicate-list' or
`TeX-view-predicate-list-builtin'.  The second part of the two-part
lists is a command line part.  The command line for the viewer is
constructed by concatenating the command line parts.  Parts with a
predicate are only considered if the predicate was evaluated with a
positive result.  Note that the command line can contain placeholders as
defined in the variable `TeX-expand-list' or in
`TeX-expand-list-builtin' which are expanded before the viewer is
called.  The third element of the item is optional and is a string, or a
list of strings, with the name of the executable, or executables, needed
to open the output file in the viewer.  Placeholders defined in the
variable `TeX-expand-list' or in `TeX-expand-list-builtin' can be used
here.  This element is used to check whether the viewer is actually
available on the system.

The use of a function as the second element only works if the View
command in `TeX-command-list' makes use of the hook
`TeX-run-discard-or-function'.

Note: Predicates defined in the current Emacs session will only show up
in the customization interface for this variable after restarting Emacs."
  :group 'TeX-view
  :type
  `(repeat
    (list
     (string :tag "Name")
     (choice
      (group :tag "Command" (string :tag "Command"))
      (group :inline t :tag "Command parts"
             (repeat
              :tag "Command parts"
              (choice
               (string :tag "Command part")
               (list :tag "Predicate and command part"
                     ,(let (list)
                        ;; Build the list of available predicates.
                        (mapc (lambda (spec)
                                (cl-pushnew `(const ,(car spec)) list :test #'equal))
                              (append TeX-view-predicate-list
                                      TeX-view-predicate-list-builtin))
                        ;; Sort the list alphabetically.
                        (setq list (sort list
                                         (lambda (a b)
                                           (string<
                                            (downcase (symbol-name (cadr a)))
                                            (downcase (symbol-name (cadr b)))))))
                        `(choice
                          (choice :tag "Predicate" ,@list)
                          (repeat :tag "List of predicates"
                                  (choice :tag "Predicate" ,@list))))
                     (string :tag "Command part")))))
      (group :tag "Function" function))
     (choice :tag "Viewer executable(s)"
             (string :tag "One executable")
             (repeat :tag "List of executables" (string :tag "Name"))
             (const :tag "No executable" nil)))))

(defcustom TeX-view-program-selection
  (cond
   ((eq system-type 'windows-nt)
    '(((output-dvi style-pstricks) "dvips and start")
      (output-dvi "Yap")
      (output-pdf "start")
      (output-html "start")))
   ((eq system-type 'darwin)
    '((output-dvi "open")
      (output-pdf "open")
      (output-html "open")))
   (t
    '(((output-dvi has-no-display-manager) "dvi2tty")
      ((output-dvi style-pstricks) "dvips and gv")
      (output-dvi "xdvi")
      (output-pdf "Evince")
      (output-html "xdg-open"))))
  "Alist of predicates and viewers.
Each entry consists of a list with two elements.  The first
element is a symbol or list of symbols referring to predicates as
defined in `TeX-view-predicate-list' or
`TeX-view-predicate-list-builtin'.  The second element is a
string referring to the name of a viewer as defined in
`TeX-view-program-list' or `TeX-view-program-list-builtin'.
\(Note: Viewers added to `TeX-view-program-list' in the current
Emacs session will not show up in the customization interface of
`TeX-view-program-selection' until you restart Emacs.)

When a viewer is called for, the entries are evaluated in turn
and the viewer related to the first entry all predicates of which
are evaluated positively is chosen."
  :group 'TeX-view
  :type `(alist :key-type
                ;; Offer list of defined predicates.
                ,(let (list)
                   (mapc (lambda (spec)
                           (cl-pushnew `(const ,(car spec)) list :test #'equal))
                         (append TeX-view-predicate-list
                                 TeX-view-predicate-list-builtin))
                   (setq list (sort list
                                    (lambda (a b)
                                      (string<
                                       (downcase (symbol-name (cadr a)))
                                       (downcase (symbol-name (cadr b)))))))
                   `(choice (choice :tag "Single predicate" ,@list)
                            (repeat :tag "Multiple predicates"
                                    (choice ,@list))))
                :value-type
                ;; Offer list of defined viewers.
                (group (choice :tag "Viewer"
                               ,@(let (list)
                                   (mapc (lambda (spec)
                                           (cl-pushnew `(const ,(car spec))
                                                       list :test #'equal))
                                         (append TeX-view-program-list
                                                 TeX-view-program-list-builtin))
                                   (sort list
                                         (lambda (a b)
                                           (string< (downcase (cadr a))
                                                    (downcase (cadr b))))))))))

(defun TeX-match-style (regexp)
  "Check if a style matching REGEXP is active."
  (TeX-member regexp (TeX-style-list) #'string-match))

(defun TeX-view-match-predicate (predicate)
  "Check if PREDICATE is true.
PREDICATE can be a symbol or a list of symbols defined in
`TeX-view-predicate-list-builtin' or `TeX-view-predicate-list'.
In case of a single symbol, return t if the predicate is true,
nil otherwise.  In case of a list of symbols, return t if all
predicates are true, nil otherwise."
  (let ((pred-symbols (if (listp predicate) predicate (list predicate)))
        (pred-defs (append TeX-view-predicate-list
                           TeX-view-predicate-list-builtin))
        (result t)
        elt)
    (while (and (setq elt (pop pred-symbols)) result)
      (unless (eval (cadr (assq elt pred-defs)) t)
        (setq result nil)))
    result))

(defun TeX-view-command-raw ()
  "Choose a viewer and return its unexpanded command string."
  (let ((selection TeX-view-program-selection)
        entry viewer item executable spec command)
    ;; Find the appropriate viewer.
    (while (and (setq entry (pop selection)) (not viewer))
      (when (TeX-view-match-predicate (car entry))
        (setq viewer (cadr entry))))
    (unless viewer
      (error "No matching viewer found"))
    (setq item (assoc viewer (append TeX-view-program-list
                                     TeX-view-program-list-builtin))
          ;; Get the command line or function spec.
          spec (cadr item)
          ;; Get the name of the executable(s) associated to the viewer.
          executable (nth 2 item))
    ;; Check the executable exists.
    (unless (or (null executable)
                (cond
                 ((stringp executable)
                  (executable-find (TeX-command-expand executable)))
                 ((listp executable)
                  (catch 'notfound
                    (dolist (exec executable t)
                      (unless (executable-find (TeX-command-expand exec))
                        (throw 'notfound nil)))))))
      (error (format "Cannot find %S viewer.  \
Select another one in `TeX-view-program-selection'" viewer)))
    (cond ((functionp spec)
           ;; Converting the function call to a string is ugly, but
           ;; the backend currently only supports strings.
           (prin1-to-string spec))
          ((stringp spec)
           spec)
          ((null spec)
           (error
            (format "Unknown %S viewer. \
Check the `TeX-view-program-selection' variable" viewer)))
          (t
           ;; Build the unexpanded command line.  Pieces with predicates are
           ;; only added if the predicate is evaluated positively.
           (dolist (elt spec)
             (cond ((stringp elt)
                    (setq command (concat command elt)))
                   ((listp elt)
                    (when (TeX-view-match-predicate (car elt))
                      (setq command (concat command (cadr elt)))))))
           (if (stringp command)
               command
             ;; Signal an error if `command' isn't a string.  This prevents an
             ;; infinite loop in `TeX-command-expand' if `command' is nil.
             (error "Wrong viewer specification in `TeX-view-program-list'"))))))

;;; Engine

(defvar TeX-engine-alist-builtin
  '((default "Default" TeX-command LaTeX-command ConTeXt-engine)
    (xetex "XeTeX" "xetex" "xelatex" "xetex")
    ;; Some lualatex versions before 0.71 would use "texput" as file
    ;; name if --jobname were not supplied
    (luatex "LuaTeX" "luatex" "lualatex --jobname=%(s-filename-only)" "luatex")
    (omega "Omega" TeX-Omega-command LaTeX-Omega-command ConTeXt-Omega-engine))
  "Alist of built-in TeX engines and associated commands.
For a description of the format see `TeX-engine-alist'.")

(defcustom TeX-engine-alist nil
  "Alist of TeX engines and associated commands.
Each entry is a list with a maximum of five elements.  The first
element is a symbol used to identify the engine.  The second is a
string describing the engine.  The third is the command to be
used for plain TeX.  The fourth is the command to be used for
LaTeX.  The fifth is the command to be used for the --engine
parameter of ConTeXt's texexec program.  Each command can either
be a variable or a string.  An empty string or nil means there is
no command available.

You can override a built-in engine defined in the variable
`TeX-engine-alist-builtin' by adding an entry beginning with the
same symbol as the built-in entry to `TeX-engine-alist'."
  :group 'TeX-command
  :type '(repeat (group symbol
                        (string :tag "Name")
                        (choice :tag "Plain TeX command" string variable)
                        (choice :tag "LaTeX command" string variable)
                        (choice :tag "ConTeXt command" string variable))))

(defun TeX-engine-alist ()
  "Return an alist of TeX engines.
The function appends the built-in engine specs from
`TeX-engine-alist-builtin' and the user-defined engines from
`TeX-engine-alist' and deletes any entries from the built-in part
where an entry with the same car exists in the user-defined part."
  (TeX-delete-dups-by-car (append TeX-engine-alist TeX-engine-alist-builtin)))

(defun TeX-engine-in-engine-alist (engine)
  "Return entry ENGINE in `TeX-engine-alist'.

Throw an error if `engine' is not present in the alist."
  (or
   (assq engine (TeX-engine-alist))
   (error "Unknown engine `%s'.  Valid values are: %s" engine
          (mapconcat
           (lambda (x) (prin1-to-string (car x)))
           (TeX-engine-alist) ", "))))

(defcustom TeX-engine 'default
  (concat "Type of TeX engine to use.
It should be one of the following symbols:\n\n"
          (mapconcat (lambda (x) (format "* `%s'" (car x)))
                     (TeX-engine-alist) "\n"))
  :group 'TeX-command
  :type `(choice ,@(mapcar (lambda (x)
                             `(const :tag ,(nth 1 x) ,(car x)))
                           (TeX-engine-alist)))
  :safe (lambda (arg) (memq arg (mapcar #'car TeX-engine-alist-builtin)))
  :local t)

(defun TeX-engine-set (type)
  "Set TeX engine to TYPE.
For available TYPEs, see variable `TeX-engine'."
  (interactive (list (completing-read "Engine: "
                                      (mapcar (lambda (x)
                                                (symbol-name (car x)))
                                              (TeX-engine-alist))
                                      nil t)))
  (when (stringp type)
    (setq type (intern type)))
  (setq TeX-engine type)
  ;; Automatically enable or disable TeX PDF mode as a convenience
  (cond ((eq type 'xetex)
         (TeX-PDF-mode 1)
         (setq TeX-PDF-from-DVI nil))
        ((eq type 'omega) (TeX-PDF-mode 0))))

;;; Forward and inverse search

(defcustom TeX-source-correlate-method
  '((dvi . source-specials) (pdf . synctex))
  "Method to use for enabling forward and inverse search.
This can be `source-specials' if source specials should be used,
`synctex' if SyncTeX should be used, or `auto' if AUCTeX should
decide.

The previous values determine the variable for both DVI and PDF
mode.  This variable can also be an alist of the kind

  ((dvi . <source-specials or synctex>)
   (pdf . <source-specials or synctex>))

in which the CDR of each entry is a symbol specifying the method
to be used in the corresponding mode.

Programs should not use this variable directly but the function
`TeX-source-correlate-method-active' which returns the method
actually used for forward and inverse search."
  :type '(choice (const auto)
                 (const synctex)
                 (const source-specials)
                 (list :tag "Different method for DVI and PDF"
                       (cons (const dvi)
                             (choice :tag "Method for DVI mode"
                                     (const synctex)
                                     (const source-specials)))
                       (cons (const pdf)
                             (choice :tag "Method for PDF mode"
                                     (const synctex)
                                     (const source-specials)))))
  :group 'TeX-view)

(defvar-local TeX-source-correlate-output-page-function nil
  "Symbol of function returning an output page relating to buffer position.
The function should take no arguments and return the page numer
as a string.")

(define-obsolete-variable-alias 'TeX-source-specials-view-start-server
  'TeX-source-correlate-start-server "11.86")

(defcustom TeX-source-correlate-start-server 'ask
  "Control if server should be started for inverse search."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "Ask" ask))
  :group 'TeX-view)

(defvar TeX-source-correlate-start-server-asked nil
  "Keep track if question about server start search was asked.")

(defvar TeX-source-correlate-start-server-flag nil
  "Non-nil means `TeX-source-correlate-start-server-maybe' will start a server.
Code related to features requiring a server, for example, for inverse
search, can set the variable.")

(defun TeX-source-correlate-gnuserv-p ()
  "Guess whether to use gnuserv when a server is requested."
  (cond ((and (boundp 'gnuserv-process)
              (processp gnuserv-process)))
        ((and (boundp 'server-process)
              (processp server-process))
         nil)))

(defun TeX-source-correlate-server-enabled-p ()
  "Return non-nil if Emacs server or gnuserv is enabled."
  (let* ((gnuserv-p (TeX-source-correlate-gnuserv-p))
         (process (if gnuserv-p 'gnuserv-process 'server-process)))
    (and (boundp process) (processp (symbol-value process)))))

(defun TeX-source-correlate-start-server-maybe ()
  "Start Emacs server or gnuserv if a feature using it is enabled.
This is the case if `TeX-source-correlate-start-server-flag' is non-nil."
  (when (and TeX-source-correlate-start-server-flag
             (not (TeX-source-correlate-server-enabled-p)))
    (let* ((gnuserv-p (TeX-source-correlate-gnuserv-p))
           (start (if gnuserv-p #'gnuserv-start #'server-start)))
      (cond
       ;; Server should be started unconditionally
       ((eq TeX-source-correlate-start-server t)
        (funcall start))
       ;; Ask user if server is to be started
       ((and (eq TeX-source-correlate-start-server 'ask)
             (not TeX-source-correlate-start-server-asked)
             (prog1
                 (y-or-n-p (format "Start %s for inverse search in viewer? "
                                   (if gnuserv-p
                                       "gnuserv"
                                     "Emacs server")))
               (setq TeX-source-correlate-start-server-asked t)))
        (funcall start))))))

(defun TeX-source-correlate-determine-method ()
  "Determine which method is available for forward and inverse search."
  (let ((help (condition-case nil
                  (with-output-to-string
                    (call-process LaTeX-command
                                  nil (list standard-output nil) nil "--help"))
                (error ""))))
    (if (string-match "^[ ]*-?-synctex" help)
        'synctex
      'source-specials)))

(defun TeX-source-correlate-method-active ()
  "Return the method actually used for forward and inverse search."
  (cond
   ((eq TeX-source-correlate-method 'auto)
    (TeX-source-correlate-determine-method))
   ((listp TeX-source-correlate-method)
    (if TeX-PDF-mode
        (cdr (assoc 'pdf TeX-source-correlate-method))
      (cdr (assoc 'dvi TeX-source-correlate-method))))
   (t
    TeX-source-correlate-method)))

(defun TeX-source-correlate-expand-options ()
  "Return TeX engine command line option for forward search facilities.
The return value depends on the value of `TeX-source-correlate-mode'.
If this is nil, an empty string will be returned."
  (if TeX-source-correlate-mode
      (if (eq (TeX-source-correlate-method-active) 'source-specials)
          (concat TeX-source-specials-tex-flags
                  (if TeX-source-specials-places
                      ;; -src-specials=WHERE: insert source specials
                      ;; in certain places of the DVI file.  WHERE is
                      ;; a comma-separated value list: cr display hbox
                      ;; math par parend vbox
                      (concat "=" (mapconcat #'identity
                                             TeX-source-specials-places ","))))
        TeX-synctex-tex-flags)
    ""))

(defvar TeX-source-correlate-map (make-sparse-keymap)
  "Keymap for `TeX-source-correlate-mode'.
You could use this for unusual mouse bindings.")
(set-keymap-parent TeX-source-correlate-map text-mode-map)

(defun TeX-source-correlate-handle-TeX-region (file line col)
  "Translate backward search info with respect to `TeX-region'.
That is, if FILE is `TeX-region', update FILE to the real tex
file and LINE to (+ LINE offset-of-region), but retain COL as is.
Else, return nil."
  (when (string-equal TeX-region (file-name-sans-extension
                                  (file-name-nondirectory file)))
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (goto-char 0)
      ;; Same regexp used in `preview-parse-messages'.  XXX: XEmacs doesn't
      ;; support regexp classes, so we can't use "[:digit:]" here.
      (when (re-search-forward "!offset(\\([---0-9]+\\))" nil t)
        (let ((offset (string-to-number (match-string-no-properties 1))))
          (when TeX-region-orig-buffer
            (list (expand-file-name (TeX-buffer-file-name TeX-region-orig-buffer))
                  (+ line offset) col)))))))

(defcustom TeX-raise-frame-function #'raise-frame
  "A function which will be called to raise the Emacs frame.
The function is called after `TeX-source-correlate-sync-source'
has processed an inverse search DBUS request from
Evince-compatible viewers in order to raise the Emacs frame.

The default value is `raise-frame', however, depending on window
manager and focus stealing policies, it might very well be that
Emacs doesn't pop into the foreground.  So you can do whatever it
takes here.

For some users, `x-focus-frame' does the trick.  For some
users (on GNOME 3.20),

  (lambda ()
    (run-at-time 0.5 nil #\\='x-focus-frame))

does the trick.  Some other users use the external wmctrl tool to
raise the Emacs frame like so:

  (lambda ()
    (call-process
     \"wmctrl\" nil nil nil \"-i\" \"-R\"
     (frame-parameter (selected-frame) \\='outer-window-id)))"
  :type 'function
  :group 'TeX-view)

(defun TeX-source-correlate-sync-source (file linecol &rest _ignored)
  "Show TeX FILE with point at LINECOL.
This function is called when emacs receives a SyncSource signal
emitted from the Evince document viewer.  IGNORED absorbs an
unused id field accompanying the DBUS signal sent by Evince-3.0.0
or newer.

If the Emacs frame isn't raised, customize
`TeX-raise-frame-function'."
  ;; FILE may be given as relative path to the TeX-master root document or as
  ;; absolute file:// URL.  In the former case, the tex file has to be already
  ;; opened.
  (let* ((file (progn
                 (require 'url-parse)
                 (require 'url-util)
                 (url-unhex-string (aref (url-generic-parse-url file) 6))))
         (flc (or (apply #'TeX-source-correlate-handle-TeX-region file linecol)
                  (apply #'list file linecol)))
         (file (car flc))
         (line (cadr flc))
         (col  (nth 2 flc)))
    (pop-to-buffer (or (find-buffer-visiting file)
                       (find-file-noselect file)))
    (push-mark nil 'nomsg)
    (let ((pos
           (when (> line 0)
             (save-excursion
               (save-restriction
                 (widen)
                 (goto-char 1)
                 (forward-line (1- line))
                 (when (> col 0)
                   (forward-char (1- col)))
                 (point))))))
      (when pos
        (when (or (< pos (point-min))
                  (> pos (point-max)))
          (widen))
        (goto-char pos))
      (when TeX-raise-frame-function
        (funcall TeX-raise-frame-function)))))

(define-minor-mode TeX-source-correlate-mode
  "Minor mode for forward and inverse search.

If enabled, the viewer can be advised to show the output page
corresponding to the point in the source and vice versa.

The method to be used can be controlled with the variable
`TeX-source-correlate-method'.  Currently source specials or
SyncTeX are recognized."
  :group 'TeX-view
  ;; Since this is a global minor mode and we don't want to require
  ;; tex.el when the mode variable is set, the mode function is called
  ;; explicitly (if necessary) in `TeX-mode'.  We do it there because
  ;; otherwise `kill-all-local-variables' would reset
  ;; `TeX-source-correlate-output-page-function' which is
  ;; buffer-local.
  :global t
  (set-keymap-parent TeX-mode-map (if TeX-source-correlate-mode
                                      TeX-source-correlate-map
                                    text-mode-map))
  (TeX-set-mode-name 'TeX-source-correlate-mode t t)
  (setq TeX-source-correlate-start-server-flag TeX-source-correlate-mode)
  ;; Register Emacs for the SyncSource DBUS signal emitted by
  ;; Evince-compatible viewers.
  (dolist (de-app '(("gnome" "evince") ("mate" "atril") ("x" "reader")))
    (when (TeX-evince-dbus-p (car de-app) (cadr de-app))
      (dbus-register-signal
       :session nil (format "/org/%s/%s/Window/0" (car de-app) (cadr de-app))
       (format "org.%s.%s.Window" (car de-app) (cadr de-app))
       "SyncSource"
       #'TeX-source-correlate-sync-source))))

(defalias 'TeX-source-specials-mode #'TeX-source-correlate-mode)
(make-obsolete 'TeX-source-specials-mode 'TeX-source-correlate-mode "11.86")
(defalias 'tex-source-correlate-mode #'TeX-source-correlate-mode)
(put 'TeX-source-correlate-mode 'safe-local-variable #'booleanp)
(setq minor-mode-map-alist
      (delq (assq 'TeX-source-correlate-mode minor-mode-map-alist)
            minor-mode-map-alist))


;;; Source Specials

(defcustom TeX-source-specials-tex-flags "-src-specials"
  "Extra flags to pass to TeX commands to generate source specials."
  :group 'TeX-view
  :type 'string)

(defcustom TeX-source-specials-places nil
  "List of places where to insert source specials into the DVI file.
If nil, use (La)TeX's defaults."
  :group 'TeX-view
  :type '(list (set :inline t
                    ;; :tag "Options known to work"
                    ;; cr display hbox math par parend vbox
                    (const "cr")
                    (const "display")
                    (const "hbox")
                    (const "math")
                    (const "par")
                    (const "parend")
                    (const "vbox"))
               (repeat :inline t
                       :tag "Other options"
                       (string))))

(defcustom TeX-source-specials-view-position-flags
  "-sourceposition \"%n %b\""
  "Flags to pass to the DVI viewer commands for the position in the source."
  :group 'TeX-view
  :type 'string)

(defcustom TeX-source-specials-view-editor-flags
  "-editor \"%cS\""
  "Flags to pass to DVI viewer commands for inverse search."
  :group 'TeX-view
  :type 'string)

(defcustom TeX-source-specials-view-gnuclient-flags
  "-q +%%l %%f"
  "Flags to pass to gnuclient for inverse search."
  :group 'TeX-view
  :type 'string)

(defcustom TeX-source-specials-view-emacsclient-flags
  "--no-wait +%%l %%f"
  "Flags to emacsclient for inverse search."
  :group 'TeX-view
  :type 'string)

;; FIXME: Make client binaries configurable.
(defun TeX-source-specials-view-expand-client ()
  "Return gnuclient or emacslient executable with options.
Return the full path to the executable if possible."
  (let* ((gnuserv-p (TeX-source-correlate-gnuserv-p))
         (client-base (if gnuserv-p
                          "gnuclient"
                        "emacsclient"))
         (client-full (and invocation-directory
                           (expand-file-name client-base
                                             invocation-directory)))
         (options (if gnuserv-p
                      TeX-source-specials-view-gnuclient-flags
                    TeX-source-specials-view-emacsclient-flags)))
    (if (and client-full (file-executable-p client-full))
        (concat client-full " " options)
      (concat client-base " " options))))

(defun TeX-source-specials-view-expand-options (&optional _viewer)
  "Return source specials command line option for viewer command.
The return value depends on the values of
`TeX-source-correlate-mode' and
`TeX-source-correlate-method-active'.  If those are nil or not
`source-specials' respectively, an empty string will be
returned."
  (if (and TeX-source-correlate-mode
           (eq (TeX-source-correlate-method-active) 'source-specials))
      (concat TeX-source-specials-view-position-flags
              (when (TeX-source-correlate-server-enabled-p)
                (concat " " TeX-source-specials-view-editor-flags)))
    ""))

;;; SyncTeX

(defvar TeX-synctex-tex-flags "--synctex=1"
  "Extra flags to pass to TeX commands to enable SyncTeX.")

(defun TeX-synctex-output-page-1 (file)
  "Return the page corresponding to the current position in FILE.
This method assumes that the document was compiled with SyncTeX
enabled and the `synctex' binary is available."
  (let ((synctex-output
         (with-output-to-string
           (call-process "synctex" nil (list standard-output nil) nil "view"
                         "-i" (format "%s:%s:%s" (1+ (TeX-current-offset))
                                      ;; FIXME: Using `current-column'
                                      ;; here is dubious.  See comment in
                                      ;; `TeX-evince-sync-view-1'.
                                      (1+ (current-column))
                                      file)
                         "-o" (TeX-active-master (TeX-output-extension))))))
    (when (string-match "^Page:\\([0-9]+\\)" synctex-output)
      (match-string 1 synctex-output))))

(defun TeX-synctex-output-page ()
  "Return the page corresponding to the position in the current buffer.
This method assumes that the document was compiled with SyncTeX
enabled and the `synctex' binary is available."
  (let* ((file (file-relative-name (TeX-buffer-file-name)
                                   (file-name-directory
                                    (TeX-active-master))))
         (abs-file (concat (expand-file-name (or (file-name-directory (TeX-active-master))
                                                 (file-name-directory (TeX-buffer-file-name))))
                           "./" file)))
    ;; It's known that depending on synctex version one of
    ;; /absolute/path/./foo/bar.tex, foo/bar.tex, or ./foo/bar.tex (relative to
    ;; TeX-master, and the "." in the absolute path is important) are needed.
    ;; So try all variants before falling back to page 1.
    (or (TeX-synctex-output-page-1 abs-file)
        (TeX-synctex-output-page-1 file)
        (TeX-synctex-output-page-1 (concat "./" file))
        "1")))

;;; Miscellaneous minor modes

(defvar-local TeX-mode-p nil
  "This indicates a TeX mode being active.")

(defun TeX-mode-set (var value)
  (set-default var value)
  (TeX-set-mode-name var nil t))

(defcustom TeX-PDF-mode t nil
  :group 'TeX-command
  :set #'TeX-mode-set
  :type 'boolean
  :safe #'booleanp)

(define-minor-mode TeX-PDF-mode
  "Minor mode for using PDFTeX.

If enabled, PDFTeX will be used as an executable by default.
You can customize an initial value, and you can use the
function `TeX-global-PDF-mode' for toggling this value."
  :group 'TeX-command
  (when (eq TeX-engine 'omega)
    (setq TeX-PDF-mode nil))
  (setq TeX-PDF-mode-parsed nil)
  (TeX-set-mode-name nil nil t)
  (setq TeX-output-extension
        (if TeX-PDF-mode "pdf" "dvi")))
(add-to-list 'minor-mode-alist '(TeX-PDF-mode ""))

(defun TeX-global-PDF-mode (&optional arg)
  "Toggle default for `TeX-PDF-mode'."
  (interactive "P")
  (prog1
      (setq-default TeX-PDF-mode
                    (if arg (> (prefix-numeric-value arg) 0)
                      (not (default-value 'TeX-PDF-mode))))
    (TeX-set-mode-name 'TeX-PDF-mode nil t)))

(defalias 'tex-pdf-mode #'TeX-PDF-mode)

(defvar-local TeX-PDF-mode-parsed nil
  "Set if `TeX-PDF-mode' has come about by parsing.")

(defun TeX-PDF-mode-parsed (arg)
  "Change `TeX-PDF-mode' to ARG based on parsing.
If this conflicts with previous parsed settings,
just use the default.  If an explicit setting is
already established, don't do anything."

  ;; Basically we have the following situations:
  ;; TeX-PDF-mode-parsed (local-variable-p 'TeX-PDF-mode):
  ;; nil nil : virgin state
  ;; nil t   : stably set state (possibly because of conflicting parse info)
  ;; t   t   : non-conflicting parsed info

  (if TeX-PDF-mode-parsed
      (unless (eq TeX-PDF-mode arg)
        (TeX-PDF-mode (if (default-value 'TeX-PDF-mode) 1 0)))
    (unless (local-variable-p 'TeX-PDF-mode (current-buffer))
      (TeX-PDF-mode (if arg 1 0))
      (setq TeX-PDF-mode-parsed t))))

(defun TeX-PDF-mode-on ()
  "Use only from parsing routines."
  (TeX-PDF-mode-parsed t))

(defun TeX-PDF-mode-off ()
  "Use only from parsing routines."
  (TeX-PDF-mode-parsed nil))

(defcustom TeX-DVI-via-PDFTeX nil
  "Whether to use PDFTeX also for producing DVI files."
  :group 'TeX-command
  :type 'boolean)

(defcustom TeX-PDF-from-DVI nil
  "Specify if and how to produce PDF output from a DVI file.

If non-nil, the default compiler produces DVI output.  The value
should be the name of the command used to convert the DVI file to
PDF or to an intermediate type.

Possible values are

* \"Dvips\": the DVI file is converted to PS with dvips.  After
  successfully running it, ps2pdf will be the default command to
  convert the PS file to PDF
* \"Dvipdfmx\": the PDF is produced with dvipdfmx

Programs should not use this variable directly but the function
`TeX-PDF-from-DVI' which handles now obsolete variable
`TeX-PDF-via-dvips-ps2pdf'."
  :group 'TeX-command
  :type '(choice
          (const :tag "No DVI to PDF conversion" nil)
          (const :tag "dvips - ps2pdf sequence" "Dvips")
          (const :tag "dvipdfmx" "Dvipdfmx"))
  :safe #'string-or-null-p
  :local t)
;; If you plan to support new values of `TeX-PDF-from-DVI' remember to update
;; `TeX-command-default' accordingly.

(defcustom TeX-PDF-via-dvips-ps2pdf nil
  "Whether to produce PDF output through the (La)TeX - dvips - ps2pdf sequence."
  :group 'TeX-command
  :type 'boolean
  :safe #'booleanp
  :local t)
(make-obsolete-variable 'TeX-PDF-via-dvips-ps2pdf 'TeX-PDF-from-DVI "11.90")

(defun TeX-PDF-from-DVI ()
  "Return the value of variable `TeX-PDF-from-DVI'.

If `TeX-PDF-from-DVI' is not set and obsolete option
`TeX-PDF-via-dvips-ps2pdf' is non-nil, return \"Dvips\"
for backward compatibility."
  (cond
   (TeX-PDF-from-DVI)
   (TeX-PDF-via-dvips-ps2pdf
    "Dvips")))

(define-minor-mode TeX-interactive-mode
  "Minor mode for interactive runs of TeX."
  :init-value nil :lighter nil :keymap nil
  :group 'TeX-command
  (TeX-set-mode-name 'TeX-interactive-mode t t))
(defalias 'tex-interactive-mode #'TeX-interactive-mode)
(add-to-list 'minor-mode-alist '(TeX-interactive-mode ""))

;;; Commands

(defgroup TeX-command-name nil
  "Names for external commands in AUCTeX."
  :group 'TeX-command)

(defcustom TeX-command-BibTeX "BibTeX"
  "The name of the BibTeX entry in `TeX-command-list'."
  :group 'TeX-command-name
  :type 'string
  :local t)

(defcustom TeX-command-Biber "Biber"
  "The name of the Biber entry in `TeX-command-list'."
  :group 'TeX-command-name
  :type 'string
  :local t)

(defcustom TeX-command-Show "View"
  "The default command to show (view or print) a TeX file.
Must be the car of an entry in `TeX-command-list'."
  :group 'TeX-command-name
  :type 'string
  :local t)

(defcustom TeX-command-Print "Print"
  "The name of the Print entry in `TeX-command-Print'."
  :group 'TeX-command-name
  :type 'string)

(defcustom TeX-command-Queue "Queue"
  "The name of the Queue entry in `TeX-command-Queue'."
  :group 'TeX-command-name
  :type 'string)

(defvar-local TeX-trailer-start nil
  "Regular expression delimiting start of trailer in a TeX file.")

(defvar-local TeX-header-end nil
  "Regular expression delimiting end of header in a TeX file.")

(defvar-local TeX-command-default nil
  "The default command for `TeX-command' in the current major mode.")

(put 'TeX-command-default 'safe-local-variable #'stringp)

(defvar TeX-clean-default-intermediate-suffixes
  '("\\.aux" "\\.bbl" "\\.blg" "\\.brf" "\\.fot"
    "\\.glo" "\\.gls" "\\.idx" "\\.ilg" "\\.ind"
    "\\.lof" "\\.log" "\\.lot" "\\.nav" "\\.out"
    "\\.snm" "\\.toc" "\\.url" "\\.synctex\\.gz"
    "\\.bcf" "\\.run\\.xml" "\\.fls" "-blx\\.bib"
    "\\.fdb_latexmk" "\\.atfi")
  "List of regexps matching suffixes of files to be cleaned.
Used as a default in TeX, LaTeX and docTeX mode.")

(defvar TeX-clean-default-output-suffixes
  '("\\.dvi" "\\.pdf" "\\.ps" "\\.xdv")
  "List of regexps matching suffixes of files to be cleaned.
Used as a default in TeX, LaTeX and docTeX mode.")

(defcustom TeX-clean-confirm t
  "If non-nil, ask before deleting files."
  :type 'boolean
  :group 'TeX-command)

(autoload 'dired-mark-pop-up "dired")

(defun TeX-clean (&optional arg)
  "Delete generated files associated with current master and region files.
If prefix ARG is non-nil, not only remove intermediate but also
output files."
  (interactive "P")
  (let* (;; Call with output extension then remove it, to make sure we
         ;; get the correct directory in cases TeX-output-dir is
         ;; non-nil
         (master (file-name-sans-extension (TeX-active-master (TeX-output-extension))))
         (master-dir (file-name-directory master))
         (regexp (concat "\\("
                         (regexp-quote (file-name-nondirectory master)) "\\|"
                         (regexp-quote (file-name-nondirectory (TeX-region-file nil t)))
                         "\\)"
                         "\\("
                         (TeX--clean-extensions-regexp arg)
                         "\\)\\'"
                         "\\|" (regexp-quote (file-name-nondirectory (TeX-region-file t t)))))
         (files (when (and regexp (or (not master-dir) (file-exists-p master-dir)))
                  (directory-files (or master-dir ".") nil regexp))))
    (if files
        (when (or (not TeX-clean-confirm)
                  (dired-mark-pop-up " *Deletions*" 'delete
                                     (if (> (length files) 1)
                                         files
                                       (cons t files))
                                     'y-or-n-p "Delete files? "))
          (dolist (file files)
            (delete-file (concat master-dir file))))
      (message "No files to be deleted"))))

(defun TeX--clean-extensions-regexp (&optional arg)
  "Return a regexp to match extensions that should be cleaned by `TeX-clean'.
If the optional argument ARG is non-nil then output files are
also included in the regexp."
  (let* ((mode-prefix (TeX-mode-prefix))
         (suffixes (and mode-prefix
                        (append (symbol-value
                                 (intern (concat mode-prefix
                                                 "-clean-intermediate-suffixes")))
                                (when arg
                                  (symbol-value
                                   (intern (concat mode-prefix
                                                   "-clean-output-suffixes"))))))))
    (when suffixes
      (mapconcat #'identity suffixes "\\|"))))

;;; Master File

(defcustom TeX-master t
  "The master file associated with the current buffer.
If the file being edited is actually included from another file, you
can tell AUCTeX the name of the master file by setting this variable.
If there are multiple levels of nesting, specify the top level file.

If this variable is nil, AUCTeX will query you for the name.

If the variable is t, AUCTeX will assume the file is a master file
itself.

If the variable is `shared', AUCTeX will query for the name, but not
change the file.

If the variable is `dwim', AUCTeX will try to avoid querying by
attempting to `do what I mean'; and then change the file.

It is suggested that you use the File Variables (see the info node
`File Variables') to set this variable permanently for each file."
  :group 'TeX-command
  :group 'TeX-parse
  :type '(choice (const :tag "Query" nil)
                 (const :tag "This file" t)
                 (const :tag "Shared" shared)
                 (const :tag "Dwim" dwim)
                 (string :format "%v"))
  :safe (lambda (x)
          (or (stringp x)
              (member x (quote (t nil shared dwim)))))
  :local t)

(defcustom TeX-one-master "\\.\\(texi?\\|[dl]tx\\)\\'"
  "Regular expression matching ordinary TeX files.

You should set this variable to match the name of all files, where
automatically adding a file variable with the name of the master file
is a good idea.  When AUCTeX adds the name of the master file as a
file variable, it does not need to ask next time you edit the file.

If you dislike AUCTeX automatically modifying your files, you can set
this variable to \"<none>\"."
  :group 'TeX-command
  :type 'regexp)

;; Can be let-bound temporarily in order to inhibit the master file question
;; by using its value instead in case `TeX-master' is nil or 'shared.
(defvar TeX-transient-master nil)

(defun TeX-dwim-master ()
  "Find a likely `TeX-master'."
  (let ((dir default-directory))
    (cl-loop for buf in (buffer-list)
             until
             (when (with-current-buffer buf
                     (and (equal dir default-directory)
                          (stringp TeX-master)))
               (cl-return (with-current-buffer buf TeX-master))))))

(defun TeX-master-file-ask ()
  "Ask for master file, set `TeX-master' and add local variables."
  (interactive)
  (if (TeX-local-master-p)
      (error "Master file already set")
    (let* ((default (TeX-dwim-master))
           (name (or (and (eq 'dwim TeX-master) default)
                     (condition-case nil
                         (read-file-name (format-prompt "Master file"
                                                        (or default "this file"))
                                         nil default)
                       (quit "<quit>")))))
      (cond ((string= name "<quit>")
             (setq TeX-master t))
            ((string= name default)
             (setq TeX-master default)
             (TeX-add-local-master))
            ((or
              ;; Default `read-file-name' proposes and buffer visits a file.
              (string= (expand-file-name name) (TeX-buffer-file-name))
              ;; Default of `read-file-name' and buffer does not visit a file.
              (string= name default-directory)
              ;; User typed <RET> in an empty minibuffer.
              (string= name ""))
             (setq TeX-master t)
             (TeX-add-local-master))
            (t
             (setq TeX-master (TeX-strip-extension (file-relative-name name)
                                                   (list TeX-default-extension)
                                                   'path))
             (TeX-add-local-master))))))

(defun TeX-master-file (&optional extension nondirectory ask)
  "Set and return the name of the master file for the current document.

If optional argument EXTENSION is non-nil, add that file extension to
the name.  Special value t means use `TeX-default-extension'.

If optional second argument NONDIRECTORY is non-nil, do not include
the directory.

If optional third argument ASK is non-nil, ask the user for the
name of master file if it cannot be determined otherwise."
  (interactive)
  (if (eq extension t)
      (setq extension TeX-default-extension))
  (with-current-buffer
      ;; In case this is an indirect buffer:
      (or (buffer-base-buffer) (current-buffer))
    (let ((my-name (if (TeX-buffer-file-name)
                       (TeX-strip-extension nil (list TeX-default-extension) t)
                     "<none>")))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (cond
           ((and TeX-transient-master
                 (or (not TeX-master) (eq TeX-master 'shared)))
            (setq TeX-master TeX-transient-master))

           ;; Special value 't means it is own master (a free file).
           ((equal TeX-master my-name)
            (setq TeX-master t))

           ;; For files shared between many documents.
           ((and (eq 'shared TeX-master) ask)
            (setq TeX-master
                  (let* ((default (TeX-dwim-master))
                         (name (read-file-name
                                (format-prompt "Master file"
                                               (or default "this file"))
                                nil default)))
                    (cond ((string= name default)
                           default)
                          ((or
                            ;; Default `read-file-name' proposes and
                            ;; buffer visits a file.
                            (string= (expand-file-name name)
                                     (TeX-buffer-file-name))
                            ;; Default of `read-file-name' and
                            ;; buffer does not visit a file.
                            (string= name default-directory)
                            ;; User typed <RET> in an empty minibuffer.
                            (string= name ""))
                           t)
                          (t
                           (TeX-strip-extension
                            name (list TeX-default-extension) 'path))))))

           ;; We might already know the name.
           ((or (eq TeX-master t) (stringp TeX-master)))

           ;; Ask the user (but add it as a local variable).
           (ask (TeX-master-file-ask)))))

      (let ((name (if (stringp TeX-master)
                      TeX-master
                    my-name)))

        (if (TeX-match-extension name)
            ;; If it already has an extension...
            (if (equal extension TeX-default-extension)
                ;; Use instead of the default extension
                (setq extension nil)
              ;; Otherwise drop it.
              (setq name (TeX-strip-extension name))))

        (let* ((reg (TeX--clean-extensions-regexp t))
               (is-output-ext (and reg
                                   (or (string-match-p reg (concat "." extension))
                                       (string= "prv" extension))))
               (output-dir (and is-output-ext
                                (TeX--master-output-dir
                                 (file-name-directory name)
                                 nondirectory))))
          (if output-dir
              (setq name (concat output-dir (file-name-nondirectory name)))
            ;; Remove directory if needed.
            (if nondirectory
                (setq name (file-name-nondirectory name)))))
        (if extension
            (concat name "." extension)
          name)))))

(defun TeX-master-directory ()
  "Directory of master file."
  (file-name-as-directory
   (abbreviate-file-name
    (substitute-in-file-name
     (expand-file-name
      (let ((dir (file-name-directory (TeX-master-file))))
        (if dir (directory-file-name dir) ".")))))))

(defun TeX-add-local-master ()
  "Add local variable for `TeX-master'.

Get `major-mode' from master file and enable it."
  (when (and (TeX-buffer-file-name)
             (string-match TeX-one-master
                           (file-name-nondirectory (TeX-buffer-file-name)))
             (not buffer-read-only))
    (goto-char (point-max))
    (if (re-search-backward "^\\([^\n]+\\)Local Variables:"
                            (- (point-max) 3000) t)
        (let ((prefix (TeX-match-buffer 1)))
          (re-search-forward (regexp-quote (concat prefix
                                                   "End:")))
          (beginning-of-line 1)
          (insert prefix "TeX-master: " (prin1-to-string TeX-master) "\n"))
      (let* ((mode (if (stringp TeX-master)
                       (with-current-buffer
                           (find-file-noselect
                            (TeX-master-file TeX-default-extension))
                         major-mode)
                     major-mode))
             (comment-prefix (cond ((eq mode 'Texinfo-mode) "@c ")
                                   ((eq mode 'docTeX-mode) "% ")
                                   (t "%%% ")))
             (mode-string (concat (and (boundp 'japanese-TeX-mode) japanese-TeX-mode
                                       "japanese-")
                                  (substring (symbol-name mode) 0 -5))))
        (newline)
        (when (eq major-mode 'docTeX-mode)
          (insert comment-prefix TeX-esc "endinput\n"))
        (insert
         comment-prefix "Local Variables:\n"
         comment-prefix "mode: " mode-string "\n"
         comment-prefix "TeX-master: " (prin1-to-string TeX-master) "\n"
         comment-prefix "End:\n")
        (unless (eq mode major-mode)
          (funcall mode)
          ;; On Emacs 26 and later, no need to reparse local variables
          ;; in order to retain `TeX-master' because major mode
          ;; function runs `hack-local-variables' through
          ;; `run-mode-hooks'.
          ;; (hack-local-variables)
          )))))

(defun TeX-local-master-p ()
  "Return non-nil if there is a `TeX-master' entry in local variables spec.
Return nil otherwise."
  (assq 'TeX-master file-local-variables-alist))

;;; Style Paths

(defcustom TeX-style-global (expand-file-name "style" TeX-data-directory)
  "Directory containing hand generated TeX information.

These correspond to TeX macros shared by all users of a site."
  :group 'TeX-file
  :type 'directory)

(defcustom TeX-auto-local "auto"
  "Directory containing automatically generated TeX information.

This correspond to TeX macros found in the current directory, and must
be relative to that."
  :group 'TeX-file
  :type 'string)

(defcustom TeX-output-dir nil
  "The path of the directory where output files should be placed.

A relative path is interpreted as being relative to the master
file in `TeX-master'.  The path cannot contain a directory that
starts with '.'.  If this variable is nil, the output directory
is assumed to be the same as the directory of `TeX-master'."
  :group 'TeX-file
  :safe #'string-or-null-p
  :type '(choice (const :tag "Directory of master file" nil)
                 (string :tag "Custom" "build"))
  :local t)

(defun TeX--master-output-dir (master-dir relative-to-master &optional ensure)
  "Return the directory path where output files should be placed.
If `TeX-output-dir' is nil, then return nil.

MASTER-DIR is the directory path where the master file is
located.  If RELATIVE-TO-MASTER is non-nil, make the returned
path relative to the directory in MASTER-DIR.  If ENSURE is
non-nil, the output directory is created if it does not exist."
  (when TeX-output-dir
    (let* ((master-dir (expand-file-name (or master-dir "")))
           (out-dir (file-name-as-directory
                     (abbreviate-file-name
                      (substitute-in-file-name
                       (expand-file-name
                        TeX-output-dir
                        master-dir))))))
      ;; Make sure the directory exists
      (unless (or (not ensure) (file-exists-p out-dir))
        (make-directory (file-name-as-directory out-dir) t))
      (if relative-to-master
          (file-relative-name out-dir master-dir)
        out-dir))))

(defun TeX--output-dir-arg (argname)
  "Format the output directory as a command argument.
ARGNAME is prepended to the quoted output directory.  If
`TeX-output-dir' is nil then return an empty string."
  (let ((out-dir (TeX--master-output-dir (TeX-master-directory) t t)))
    (if out-dir
        (concat argname "\"" out-dir "\"")
      "")))

(defun TeX-master-output-file (extension)
  "Return the output file with given EXTENSION.
If `TeX-output-dir' is nil, then defer to `TeX-master-file'.  Otherwise,
return the file of the same name, but in the build directory specified by
`TeX-output-dir'."
  (let ((master (TeX-master-file extension)))
    (if-let* ((output-dir (TeX--master-output-dir (TeX-master-directory) t)))
        (concat output-dir (file-name-nondirectory master))
      master)))

(defcustom TeX-style-local "style"
  "Directory containing hand generated TeX information.

These correspond to TeX macros found in the current directory, and must
be relative to that."
  :group 'TeX-file
  :type 'string)

;; Compatibility alias
(defun TeX-split-string (regexp string)
  (split-string string regexp))
(make-obsolete 'TeX-split-string
               "use (split-string STRING REGEXP) instead." "AUCTeX 13.0")

(defun TeX-parse-path (env)
  "Return a list if private TeX directories found in environment variable ENV."
  (let* ((value (getenv env))
         (entries (and value
                       (split-string
                        value
                        (if (string-match ";" value) ";" ":"))))
         (global (append '("/" "\\")
                         (mapcar #'file-name-as-directory
                                 TeX-macro-global)))
         entry
         answers)
    (while entries
      (setq entry (car entries))
      (setq entries (cdr entries))
      (setq entry (file-name-as-directory
                   (if (string-match "/?/?\\'" entry)
                       (substring entry 0 (match-beginning 0))
                     entry)))
      (or (not (file-name-absolute-p entry))
          (member entry global)
          (setq answers (cons entry answers))))
    answers))

(defun TeX-kpathsea-detect-path-delimiter ()
  "Auto detect the path delimiter for kpsewhich command.
Usually return \":\" or \";\".  If auto detect fails for some reason,
return nil."
  (let ((res (ignore-errors
               (with-output-to-string
                 (call-process "kpsewhich" nil
                               (list standard-output nil) nil
                               "--expand-path" "{.,..}")))))
    ;; kpsewhich expands "{.,..}" to ".:SOMEDIR" or ".;SOMEDIR"
    ;; according to its environment.
    ;; Don't use "{.,.}" instead because kpsewhich of MiKTeX 2.9
    ;; simplifies it to just a ".", not ".;.".
    (and (stringp res) (> (length res) 0)
         ;; Check whether ; is contained.  This should work even if
         ;; some implementation of kpsewhich considers it sane to
         ;; insert drive letters or directory separators or whatever
         ;; else to the current directory.
         (if (string-match ";" res) ";" ":"))))

(defcustom TeX-kpathsea-path-delimiter
  (TeX-kpathsea-detect-path-delimiter)
  "Path delimiter for kpathsea output.
t means autodetect, nil means kpathsea is disabled."
  :group 'TeX-file
  :type '(choice (const ":")
                 (const ";")
                 (const :tag "Autodetect" t)
                 (const :tag "Off" nil)))

(defun TeX-tree-expand (vars program &optional subdirs)
  "Return directories corresponding to the kpathsea variables VARS.
This is done calling `kpsewhich --expand-path' for the variables.
PROGRAM if non-nil is passed as the parameter for --progname.
Optional argument SUBDIRS are subdirectories which are appended
to the directories of the TeX trees.  Only existing directories
are returned."
  ;; FIXME: The GNU convention only uses "path" to mean "list of directories"
  ;; and uses "filename" for the name of a file even if it contains possibly
  ;; several elements separated by "/".
  (if (eq TeX-kpathsea-path-delimiter t)
      (setq TeX-kpathsea-path-delimiter
            (TeX-kpathsea-detect-path-delimiter)))
  (when TeX-kpathsea-path-delimiter
    (let* ((exit-status 1)
           (args `(,@(if program `("--progname" ,program))
                   "--expand-path"
                   ,(mapconcat #'identity vars
                               TeX-kpathsea-path-delimiter)))
           (path-list (ignore-errors
                        (with-output-to-string
                          (setq exit-status
                                (apply #'call-process
                                       "kpsewhich" nil
                                       (list standard-output nil) nil
                                       args))))))
      (if (not (zerop exit-status))
          ;; kpsewhich is not available.  Disable subsequent usage.
          (setq TeX-kpathsea-path-delimiter nil)
        (let ((separators (format "[\n\r%s]" TeX-kpathsea-path-delimiter))
              path input-dir-list)
          (dolist (item (split-string path-list separators t))
            (if subdirs
                (dolist (subdir subdirs)
                  (setq path (file-name-as-directory (concat item subdir)))
                  (when (file-exists-p path)
                    (cl-pushnew path input-dir-list :test #'equal)))
              (setq path (file-name-as-directory item))
              (when (file-exists-p path)
                (cl-pushnew path input-dir-list :test #'equal))))
          ;; No duplication in result is assured since `cl-pushnew' is
          ;; used above.  Should we introduce an option for speed just
          ;; to accumulate all the results without care for
          ;; duplicates?
          (nreverse input-dir-list))))))

(defun TeX-macro-global ()
  "Return directories containing the site's TeX macro and style files."
  (or (TeX-tree-expand '("$SYSTEXMF" "$TEXMFLOCAL" "$TEXMFMAIN" "$TEXMFDIST")
                       "latex" '("/tex/" "/bibtex/bst/"))
      '("/usr/share/texmf/tex/" "/usr/share/texmf/bibtex/bst/")))

(defun TeX-macro-private ()
  "Return directories containing the user's TeX macro and style files."
  (TeX-tree-expand '("$TEXMFHOME") "latex" '("/tex/" "/bibtex/bst/")))

(defcustom TeX-macro-global (TeX-macro-global)
  "Directories containing the site's TeX macro and style files."
  :group 'TeX-file
  :type '(repeat (directory :format "%v")))

(defcustom TeX-macro-private (or (append (TeX-parse-path "TEXINPUTS")
                                         (TeX-parse-path "BIBINPUTS"))
                                 (TeX-macro-private))
  "Directories where you store your personal TeX macros."
  :group 'TeX-file
  :type '(repeat (file :format "%v")))

(defcustom TeX-auto-private
  (list (expand-file-name TeX-auto-local
                          (or (concat user-emacs-directory "auctex/")
                              "~/.emacs.d/auctex/")))
  "List of directories containing automatically generated AUCTeX style files.

These correspond to the personal TeX macros."
  :group 'TeX-file
  :type '(repeat (file :format "%v")))

(if (stringp TeX-auto-private)          ;Backward compatibility
    (setq TeX-auto-private (list TeX-auto-private)))

(defcustom TeX-style-private
  (list (expand-file-name TeX-style-local
                          (or (concat user-emacs-directory "auctex/")
                              "~/.emacs.d/auctex/")))
  "List of directories containing hand-generated AUCTeX style files.

These correspond to the personal TeX macros."
  :group 'TeX-file
  :type '(repeat (file :format "%v")))

(if (stringp TeX-style-private)         ;Backward compatibility
    (setq TeX-style-private (list TeX-style-private)))

(defcustom TeX-style-path
  (let ((path))
    ;; Put directories in an order where the more local files can
    ;; override the more global ones.
    (mapc (lambda (file)
            (when (and file (not (member file path)))
              (setq path (cons file path))))
          (append (list TeX-auto-global TeX-style-global)
                  TeX-auto-private TeX-style-private
                  (list TeX-auto-local TeX-style-local)))
    (nreverse path))
  "List of directories to search for AUCTeX style files.
Per default the list is built from the values of the variables
`TeX-auto-global', `TeX-style-global', `TeX-auto-private',
`TeX-style-private', `TeX-auto-local', and `TeX-style-local'."
  :group 'TeX-file
  :type '(repeat (file :format "%v")))

(defcustom TeX-check-path
  (append (list ".") TeX-macro-private TeX-macro-global)
  "Directory path to search for dependencies.

If nil, just check the current file.
Used when checking if any files have changed."
  :group 'TeX-file
  :type '(repeat (file :format "%v")))

;;; Style Files

(define-obsolete-variable-alias 'LaTeX-dialect 'TeX-dialect "13.0")
(defconst TeX-dialect :latex
  "Default dialect for use with function `TeX-add-style-hook'.
This applies to the argument DIALECT-EXPR when the hook is to be
run only on LaTeX file, or any mode derived thereof.  See
variable `TeX-style-hook-dialect'." )

(defvar TeX-style-hook-list nil
  "List of TeX style hooks currently loaded.

Each entry is a list:

 (STYLE HOOK1 HOOK2 ...)

where the first element STYLE is the name of the style, and the
remaining elements HOOKN, if any, are hooks to be run when that
style is active.

A hook HOOKN may be a hook function HOOK-FUN to be run in
all TeX dialects (LaTeX, Texinfo, etc.), or a vector like:

     [TeX-style-hook HOOK-FUN DIALECT-SET]

where HOOK-FUN is the hook function to be run, and DIALECT-SET is
a non-empty set of dialects in which the hook function may be
run.

This set is instantiated by function `TeX-add-style-hook' through
functions manipulating style hook dialect expression named with a
`TeX-shdex-' prefix.

For supported dialects, see variables `TeX-style-hook-dialect'.")

(defvar TeX-style-hook-dialect :latex
  "Dialect for running hooks locally to the considered file.
Supported values are described below:

* `:bibtex'  for files in BibTeX mode.
* `:context' for files in ConTeXt mode.
* `:latex'   for files in LaTeX mode, or any mode derived
             thereof.
* `:plain-tex' for files in plain-TeX mode.
* `:texinfo' for Texinfo files.
* `:classopt' for class options of LaTeX document.  Just
              considered as a pseudo-dialect.

Purpose is notably to prevent non-Texinfo hooks to be run in
Texinfo files, due to ambiguous style name, as this may cause bad
side effect for example on variable `TeX-font-list'.")

(defcustom TeX-byte-compile nil
  "Not nil means try to byte compile auto files before loading."
  :group 'TeX-parse
  :type 'boolean)

(defun TeX-bibtex-set-BibTeX-dialect ()
  "Set `TeX-style-hook-dialect' to `:bibtex' locally to BibTeX buffers."
  (setq-local TeX-style-hook-dialect :bibtex))

(defun TeX-load-style (style)
  "Search for and load each definition for STYLE in `TeX-style-path'."
  (cond ((assoc style TeX-style-hook-list)) ; We already found it
        ((string-match "\\`\\(.+[/\\]\\)\\([^/\\]*\\)\\'" style) ;Complex path
         (let* ((dir (substring style (match-beginning 1) (match-end 1)))
                (style (substring style (match-beginning 2) (match-end 2)))
                (master-dir (if (stringp TeX-master)
                                (file-name-directory
                                 (file-relative-name TeX-master))
                              "./"))
                (TeX-style-path (append (list (expand-file-name
                                               TeX-auto-local dir)
                                              (expand-file-name
                                               TeX-auto-local master-dir)
                                              (expand-file-name
                                               TeX-style-local dir)
                                              (expand-file-name
                                               TeX-style-local master-dir))
                                        TeX-style-path)))
           (TeX-load-style style)))
        (t                              ;Relative path
         ;; Insert empty list to mark the fact that we have searched.
         (setq TeX-style-hook-list (cons (list style) TeX-style-hook-list))
         ;; Now check each element of the path
         (dolist (name TeX-style-path)
           (TeX-load-style-file (expand-file-name style name))))))

(defun TeX-load-style-file (file)
  "Load FILE checking for a Lisp extensions."
  (let ((el (concat file ".el"))
        (elc (concat file ".elc")))
    (cond ((file-newer-than-file-p el elc)
           (if (file-readable-p el)
               (if (and TeX-byte-compile
                        (file-writable-p elc)
                        (save-excursion
                          ;; `byte-compile-file' switches buffer in Emacs 20.3.
                          (byte-compile-file el))
                        (file-readable-p elc))
                   (load-file elc)
                 (load-file el))))
          ((file-readable-p elc)
           (load-file elc))
          ((file-readable-p el)
           (load-file el)))))

(defconst TeX-style-hook-dialect-weight-alist
  '((:latex . 1) (:texinfo . 2) (:bibtex . 4) (:plain-tex . 8) (:context . 16)
    (:classopt . 32))
  "Association list to map dialects to binary weight, in order to
implement dialect sets as bitmaps."  )

(defun TeX-shdex-eval (dialect-expr)
  "Evaluate a style hook dialect expression DIALECT-EXPR."
  (cond
   ((symbolp dialect-expr)
    (let ((cell (assq dialect-expr TeX-style-hook-dialect-weight-alist)))
      (if cell (cdr cell)
        (error "Invalid dialect expression : %S" dialect-expr))))
   ((and (consp dialect-expr)
         (memq (car dialect-expr) '(or not and nor)))
    (apply (intern
            (concat "TeX-shdex-" (symbol-name  (car dialect-expr))))
           (cdr dialect-expr)))
   (t
    (error "Invalid dialect expression : %S" dialect-expr))))

(defsubst TeX-shdex-or (&rest args)
  "OR operator for style hook dialect expressions."
  (apply #'logior (mapcar #'TeX-shdex-eval args)))

(defsubst TeX-shdex-and (&rest args)
  "AND operator for style hook dialect expressions."
  (apply #'logand (mapcar #'TeX-shdex-eval args)))

(defsubst TeX-shdex-nor (&rest args)
  "NOR operator for style hook dialect expressions."
  (lognot (apply #'TeX-shdex-or args)))

(defsubst TeX-shdex-not (arg)
  "NOT operator for style hook dialect expressions."
   (lognot (TeX-shdex-eval arg)))

(defsubst TeX-shdex-in-p (dialect dialect-set)
  "Test whether dialect DIALECT is in dialect set DIALECT-SET."
  (let ((cell (assq dialect TeX-style-hook-dialect-weight-alist)))
    (if cell
        (/= 0 (logand (cdr cell) dialect-set))
      (error "Invalid dialect %S" dialect))))

(defsubst TeX-shdex-listify (dialect-set)
  "Converts a dialect set DIALECT-SET to a list of all dialect
comprised in this set, where dialects are symbols"
  (let (ret)
    (dolist (c dialect-set)
      (when (/= 0 (logand (cdr c) dialect-set))
        (push (car c) ret)))
    ret))

(defun TeX-add-style-hook (style hook &optional dialect-expr)
  "Give STYLE yet another HOOK to run.

DIALECT-EXPR serves the purpose of marking the hook to be run only in
that dicontext.

DIALECT-EXPR may be a single symbol defining the dialect, see
variable `TeX-style-hook-dialect' for supported dialects.

DIALECT-EXPR can also be an expression like one of the following:

* (or  DIALECT1 DIALECT2 ...)
* (nor DIALECT1 DIALECT2 ...)
* (and DIALECT1 DIALECT2 ...)
* (not DIALECT )

When omitted DIALECT-EXPR is equivalent to `(nor )', ie all
dialected are allowed."
  (let ((entry (assoc-string style TeX-style-hook-list)))
    (and dialect-expr (setq hook (vector 'TeX-style-hook hook
                                         (TeX-shdex-eval dialect-expr))))
    (cond ((null entry)
           ;; New style, add entry.
           (setq TeX-style-hook-list (cons (list style hook)
                                           TeX-style-hook-list)))
          ((member hook entry)
           ;; Old style, hook already there, do nothing.
           nil)
          (t
           ;; Old style, new hook.
           (setcdr entry (cons hook (cdr entry)))))))

(defun TeX-keep-hooks-in-dialect (hooks dialect-list)
  "Scan HOOKS for all hooks the associated dialect of which is
found in DIALECT-LIST and return the list thereof."
  (let (ret dialect-list-1)
    (dolist (hook hooks)
      (setq dialect-list-1 (and (vectorp hook) (eq (aref hook 0) 'TeX-style-hook)
                                (TeX-shdex-listify (aref hook 2))))
      (while dialect-list-1
        (when (memq (pop dialect-list-1) dialect-list)
          (push hook ret)
          (setq dialect-list-1 nil)))
    ret)))

(defun TeX-unload-style (style &optional dialect-list)
  "Forget that we once loaded STYLE.
If DIALECT-LIST is provided, the STYLE is only removed for those
dialects in DIALECT-LIST.

See variable `TeX-style-hook-dialect' for supported dialects."
  (let ((style-data (assoc-string style TeX-style-hook-list)))
    (if style-data
        (let ((hooks (and dialect-list (TeX-keep-hooks-in-dialect (cdr style-data) dialect-list))))
          (if hooks
              (setcdr style-data hooks)
            (setq TeX-style-hook-list (delq style-data TeX-style-hook-list)))))))

(defcustom TeX-virgin-style (if (and TeX-auto-global
                                     (file-directory-p TeX-auto-global))
                                "virtex"
                              "NoVirtexSymbols")
  "Style all documents use."
  :group 'TeX-parse
  :type 'string)

(defvar-local TeX-active-styles nil
  "List of styles currently active in the document.")

(defun TeX-run-style-hooks (&rest styles)
  "Run the TeX style hooks STYLES."
  (mapcar (lambda (style)
            ;; Avoid recursion.
            (unless (TeX-member style TeX-active-styles #'string-equal)
              (setq TeX-active-styles
                    (cons style TeX-active-styles))
              (TeX-load-style style)
              (let ((default-directory default-directory))
                ;; Complex path.
                (when (string-match "\\`\\(.+[/\\]\\)\\([^/\\]*\\)\\'" style)
                  ;; Set `default-directory' to directory of master
                  ;; file since style files not stored in the fixed
                  ;; style directories are usually located there.
                  (setq default-directory (save-match-data
                                            (TeX-master-directory))
                        style (substring style
                                         (match-beginning 2) (match-end 2))))
                (condition-case nil
                    (mapcar (lambda (hook)
                              (cond
                               ((functionp hook)
                                (funcall hook))
                               ((and (vectorp hook)
                                     (eq (aref hook 0) 'TeX-style-hook))
                                (and (TeX-shdex-in-p TeX-style-hook-dialect (aref hook 2))
                                     (funcall (aref hook 1))))
                               (t (error "Invalid style hook %S" hook))))
                            ;; Reverse the list of style hooks in order to run
                            ;; styles in the order global, private, local
                            ;; (assuming TeX-style-path has that ordering,
                            ;; too).
                            (reverse (cdr-safe (assoc-string style TeX-style-hook-list))))
                  ;; This happens in case some style added a new parser, and
                  ;; now the style isn't used anymore (user deleted
                  ;; \usepackage{style}).  Then we're left over with, e.g.,
                  ;; (LaTeX-add-siunitx-units "\\parsec"), but the function is
                  ;; defined in a style siunitx.el that's not loaded anymore.
                  (void-function nil)))))
          styles))

(defcustom TeX-parse-self nil
  "Parse file after loading it if no style hook is found for it."
  :group 'TeX-parse
  :type 'boolean)

(defvar-local TeX-style-hook-applied-p nil
  "Non-nil means the style specific hooks have been applied.")

(defvar TeX-update-style-hook nil
  "Hook run as soon as style specific hooks were applied.")

(defun TeX-update-style (&optional force)
  "Run style specific hooks for the current document.

Only do this if it has not been done before, or if optional argument
FORCE is not nil."
  (unless (or (eq major-mode 'bibtex-mode) ; Not a real TeX buffer
              (and (not force)
                   TeX-style-hook-applied-p))
    (setq TeX-style-hook-applied-p t)
    (message "Applying style hooks...")
    (TeX-run-style-hooks (TeX-strip-extension nil nil t))
    ;; Run parent style hooks if it has a single parent that isn't itself.
    (if (or (not (memq TeX-master '(nil t)))
            (and (TeX-buffer-file-name)
                 (string-match TeX-one-master
                               (file-name-nondirectory (TeX-buffer-file-name)))))
        (TeX-run-style-hooks (TeX-master-file)))
    (if (and TeX-parse-self
             (null (cdr-safe (assoc (TeX-strip-extension nil nil t)
                                    TeX-style-hook-list))))
        (TeX-auto-apply))
    (run-hooks 'TeX-update-style-hook)
    (message "Applying style hooks...done")))

(defvar TeX-remove-style-hook nil
  "List of hooks to call when we remove the style specific information.")

(defun TeX-remove-style ()
  "Remove all style specific information."
  (setq TeX-style-hook-applied-p nil)
  (run-hooks 'TeX-remove-style-hook)
  (setq TeX-active-styles (list TeX-virgin-style)))

(defun TeX-style-list ()
  "Return a list of all styles (subfiles) used by the current document."
  (TeX-update-style)
  TeX-active-styles)

;;; Special Characters

(defvar-local TeX-esc "\\" "The TeX escape character.")

(defvar-local TeX-grop "{" "The TeX group opening character.")

(defvar-local TeX-grcl "}" "The TeX group closing character.")

;;; Symbols

;; Must be before keymaps.

(defgroup TeX-macro nil
  "Support for TeX macros in AUCTeX."
  :prefix "TeX-"
  :group 'AUCTeX)

(defcustom TeX-complete-word #'ispell-complete-word
  "Function to call for completing non-macros in `tex-mode'."
  :type 'function
  :group 'TeX-macro)

(defcustom TeX-complete-expert-commands nil
  "Complete macros and environments marked as expert commands.

Possible values are nil, t, or a list of style names.

  - nil           Don't complete expert commands (default).
  - t             Always complete expert commands.
  - (STYLES ...)  Only complete expert commands of STYLES."
  :group 'TeX-macro
  :type '(choice (const  :tag "Don't complete expert commands" nil)
                 (const  :tag "Always complete expert commands" t)
                 (repeat :tag "Complete expert commands of certain styles" string)))

(defmacro TeX-complete-make-expert-command-functions (thing list-var prefix)
  (let* ((plural (concat thing "s"))
         (upcase-plural (upcase plural))
         (table-var (intern (format "%s-expert-%s-table" prefix thing))))
    `(progn
       (defvar ,table-var
         (make-hash-table :test #'equal)
         ,(format "A hash-table mapping %s names to the style name providing it.

A %s occuring in this table is considered an expert %s and
treated specially in the completion." thing thing thing))

       (defun ,(intern (format "%s-declare-expert-%s" prefix plural)) (style &rest ,(intern plural))
         ,(format "Declare %s as expert %s of STYLE.

Expert %s are completed depending on `TeX-complete-expert-commands'."
                  upcase-plural plural plural)
         (dolist (x ,(intern plural))
           (if (null style)
               (remhash x ,table-var)
             (puthash x style ,table-var))))

       (defun ,(intern (format "%s-filtered" list-var)) ()
         ,(format "Filter (%s) depending on `TeX-complete-expert-commands'."
                  list-var)
         (delq nil
               (mapcar
                (lambda (entry)
                  (if (eq t TeX-complete-expert-commands)
                      entry
                    (let* ((cmd (car entry))
                           (style (gethash cmd ,table-var)))
                      (when (or (null style)
                                (member style TeX-complete-expert-commands))
                        entry))))
                (,list-var)))))))

(TeX-complete-make-expert-command-functions "macro" TeX-symbol-list "TeX")
(TeX-complete-make-expert-command-functions "environment" LaTeX-environment-list "LaTeX")

(defvar TeX-complete-list nil
  "List of ways to complete the preceding text.

Each entry is a list with the following elements:

0. Regexp matching the preceding text or a predicate of arity 0
which returns non-nil and sets `match-data' appropriately if it
is applicable.
1. A number indicating the subgroup in the regexp containing the
text.
2. A function returning an alist of possible completions.
3. Text to append after a succesful completion.

Or alternatively:

0. Regexp matching the preceding text.
1. Function to do the actual completion.")

(defun TeX--complete-find-entry ()
  "Return the first applicable entry of `TeX-complete-list'."
  (let ((list TeX-complete-list)
        entry)
    (while list
      (setq entry (car list)
            list (cdr list))
      (when (if (functionp (car entry))
                (funcall (car entry))
              (TeX-looking-at-backward (car entry) 250))
        (setq list nil)))
    entry))

(defun TeX-complete-symbol ()
  "Perform completion on TeX/LaTeX symbol preceding point."
  (interactive "*")
  (let ((entry (TeX--complete-find-entry)))
    (when entry
      (if (numberp (nth 1 entry))
          (let* ((sub (nth 1 entry))
                 (close (if (and (nth 3 entry)
                                 (listp (nth 3 entry))
                                 (symbolp (car (nth 3 entry))))
                            (eval (nth 3 entry) t)
                          (nth 3 entry)))
                 (begin (match-beginning sub))
                 (end (match-end sub))
                 (pattern (TeX-match-buffer 0))
                 (symbol (buffer-substring begin end))
                 (list (funcall (nth 2 entry)))
                 (completion (try-completion symbol list))
                 (buf-name "*Completions*"))
            (cond ((eq completion t)
                   (and close
                        (not (looking-at (regexp-quote close)))
                        (insert close))
                   (let ((window (get-buffer-window buf-name)))
                     (when window (delete-window window))))
                  ((null completion)
                   (error "Can't find completion for \"%s\"" pattern))
                  ((not (string-equal symbol completion))
                   (delete-region begin end)
                   (insert completion)
                   (and close
                        (eq (try-completion completion list) t)
                        (not (looking-at (regexp-quote close)))
                        (insert close))
                   (let ((window (get-buffer-window buf-name)))
                     (when window (delete-window window))))
                  (t
                   (completion-in-region begin end
                                         (all-completions symbol list nil)))))
        (funcall (nth 1 entry))))))

(defun TeX--completion-annotation-from-tex--prettify-symbols-alist (sym)
  (when (boundp 'tex--prettify-symbols-alist)
    (let ((ann (cdr (assoc (concat "\\" sym)
                           tex--prettify-symbols-alist))))
      (when ann
        (concat " " (char-to-string ann))))))

(declare-function LaTeX--completion-annotation-from-math-menu
                  "latex" (sym))

(defun TeX--completion-annotation-function (sym)
  "Annotation function for symbol/macro completion.
Used as `:annotation-function' in `completion-extra-properties'."
  (or (TeX--completion-annotation-from-tex--prettify-symbols-alist sym)
      (and (fboundp #'LaTeX--completion-annotation-from-math-menu)
           (LaTeX--completion-annotation-from-math-menu sym))))

(defun TeX--completion-at-point ()
  "(La)TeX completion at point function.
See `completion-at-point-functions'."
  (let ((entry (TeX--complete-find-entry)))
    (when entry
      (if (numberp (nth 1 entry))
          (let* ((sub (nth 1 entry))
                 (begin (match-beginning sub))
                 (end (match-end sub))
                 (symbol (buffer-substring-no-properties begin end))
                 (func (nth 2 entry))
                 (list (funcall func)))
            (list begin end (all-completions symbol list)
                  :annotation-function
                  (cond ((eq func #'LaTeX-completion-label-list)
                         #'LaTeX-completion-label-annotation-function)
                        (t
                         #'TeX--completion-annotation-function))))
        ;; We intentionally don't call the fallback completion functions because
        ;; they do completion on their own and don't work too well with things
        ;; like company-mode.  And the default function `ispell-complete-word'
        ;; isn't so useful anyway.
        nil))))

(defcustom TeX-default-macro "ref"
  "The default macro when creating new ones with `TeX-insert-macro'."
  :group 'TeX-macro
  :type 'string
  :local t)

(defcustom TeX-insert-braces t
  "If non-nil, append an empty pair of braces after inserting a macro.

See also `TeX-insert-braces-alist'."
  :group 'TeX-macro
  :type 'boolean)

(defcustom TeX-insert-braces-alist nil
  "Alist of macros to which braces should or should not be appended.

Each element is a cons cell, whose CAR is the macro name, and the
CDR is non-nil or nil, depending on whether a pair of braces
should be, respectively, appended or not to the macro.

If a macro has an element in this variable, `TeX-parse-macro'
will use its value to decide what to do, whatever the value of
the variable `TeX-insert-braces'."
  :group 'TeX-macro
  :type '(repeat (cons (string :tag "Macro name")
                       (boolean :tag "Append braces?")))
  :local t)

(defcustom TeX-insert-macro-default-style 'show-optional-args
  "Specifies whether `TeX-insert-macro' will ask for all optional arguments.

If set to the symbol `show-optional-args', `TeX-insert-macro'
asks for optional arguments of TeX marcos, unless the previous
optional argument has been rejected.  If set to
`show-all-optional-args', `TeX-insert-macro' asks for all
optional arguments.  If set to `mandatory-args-only',
`TeX-insert-macro' asks only for mandatory argument.

When `TeX-insert-macro' is called with \\[universal-argument], it's the other
way round.

Note that for some macros, there are special mechanisms, see for example
`LaTeX-includegraphics-options-alist' and `TeX-arg-cite-note-p'."
  :group 'TeX-macro
  :type '(choice (const mandatory-args-only)
                 (const show-optional-args)
                 (const show-all-optional-args)))

(defvar TeX-arg-opening-brace nil
  "String used as an opening brace for argument insertion.
The variable will be temporarily let-bound with the necessary value.")

(defvar TeX-arg-closing-brace nil
  "String used as a closing brace for argument insertion.
The variable will be temporarily let-bound with the necessary value.")

(defvar TeX-after-insert-macro-hook nil
  "A hook run after `TeX-insert-macro'.")

(defvar TeX-macro-history nil)

(defun TeX--symbol-completion-table ()
  (completion-table-dynamic
   (lambda (_str)
     (TeX-symbol-list-filtered))
   t))

(defun TeX-insert-macro (symbol)
  "Insert TeX macro SYMBOL with completion.

AUCTeX knows of some macros and may query for extra arguments, depending on
the value of `TeX-insert-macro-default-style' and whether `TeX-insert-macro'
is called with \\[universal-argument]."
  ;; When called with a prefix (C-u), only ask for mandatory arguments,
  ;; i.e. all optional arguments are skipped.  See `TeX-parse-arguments' for
  ;; details.  Note that this behavior may be changed in favor of a more
  ;; flexible solution in the future, therefore we don't document it at the
  ;; moment.
  (interactive (list
                (let ((completion-extra-properties
                       (list :annotation-function
                             #'TeX--completion-annotation-function)))
                  (completing-read (concat "Macro (default "
                                           TeX-default-macro
                                           "): "
                                           TeX-esc)
                                   (TeX--symbol-completion-table) nil nil nil
                                   'TeX-macro-history TeX-default-macro))))
  (when (called-interactively-p 'any)
    (setq TeX-default-macro symbol))
  (atomic-change-group
    (TeX-parse-macro symbol (cdr-safe (assoc symbol (TeX-symbol-list))))
    (run-hooks 'TeX-after-insert-macro-hook)))

(defvar TeX-electric-macro-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-completion-map)
    (define-key map " " #'minibuffer-complete-and-exit)
    map))

(defun TeX-electric-macro ()
  "Insert TeX macro with completion.

AUCTeX knows of some macros, and may query for extra arguments.
Space will complete and exit."
  (interactive)
  (cond ((eq (preceding-char) last-command-event)
         (call-interactively #'self-insert-command))
        ((eq (preceding-char) ?.)
         (let ((TeX-default-macro " ")
               (minibuffer-local-completion-map TeX-electric-macro-map))
           (call-interactively #'TeX-insert-macro)))
        (t
         (let ((minibuffer-local-completion-map TeX-electric-macro-map))
           (call-interactively #'TeX-insert-macro)))))

(defvar TeX-exit-mark nil
  "Dynamically bound by `TeX-parse-macro' and `LaTeX--env-parse-args'.")

(defun TeX-parse-macro (symbol args)
  "How to parse TeX macros which takes one or more arguments.

First argument SYMBOL is the name of the macro.

If ARGS is nil, insert macro with point inside braces.
Otherwise, each element in ARGS should match an argument to the
TeX macro.  What is done depend on the type of the element:

  string: Use the string as a prompt to prompt for the argument.

  number: Insert that many braces, leave point inside the first.

  nil: Insert empty braces.

  t: Insert empty braces, leave point between the braces.

  other symbols: Call the symbol as a function.  You can define
  your own hook, or use one of the predefined argument hooks.  If
  you add new hooks, you can assume that point is placed directly
  after the previous argument, or after the macro name if this is
  the first argument.  Please leave point located after the
  argument you are inserting.  If you want point to be located
  somewhere else after all hooks have been processed, set the
  value of `TeX-exit-mark'.  It will point nowhere, until the
  argument hook set it.  By convention, these hooks all start
  with `TeX-arg-'.

  list: If the car is a string, insert it as a prompt and the next
  element as initial input.  Otherwise, call the car of the list
  with the remaining elements as arguments.

  vector: Optional argument.  If it has more than one element,
  parse it as a list, otherwise parse the only element as above.
  Use square brackets instead of curly braces, and is not inserted
  on empty user input."
  (let ((TeX-grop (if (and (or (atom args) (= (length args) 1))
                           (fboundp 'LaTeX-verbatim-macros-with-delims)
                           (member symbol (LaTeX-verbatim-macros-with-delims)))
                      LaTeX-default-verb-delimiter
                    TeX-grop))
        (TeX-grcl (if (and (or (atom args) (= (length args) 1))
                           (fboundp 'LaTeX-verbatim-macros-with-delims)
                           (member symbol (LaTeX-verbatim-macros-with-delims)))
                      LaTeX-default-verb-delimiter
                    TeX-grcl)))
    (if (and (TeX-active-mark)
             (> (point) (mark)))
        (exchange-point-and-mark))
    (insert TeX-esc symbol)
    (let ((TeX-exit-mark (make-marker))
          (position (point)))
      (TeX-parse-arguments args)
      (cond ((marker-position TeX-exit-mark)
             (goto-char (marker-position TeX-exit-mark))
             (set-marker TeX-exit-mark nil))
            ((let ((element (assoc symbol TeX-insert-braces-alist)))
               ;; If in `TeX-insert-braces-alist' there is an element associated
               ;; to the current macro, use its value to decide whether inserting
               ;; a pair of braces, otherwise use the standard criterion.
               (if element
                   (cdr element)
                 (and TeX-insert-braces
                      ;; Do not add braces if the argument is 0 or -1.
                      (not (and (= (safe-length args) 1)
                                (numberp (car args))
                                (<= (car args) 0)))
                      (equal position (point))
                      (string-match "[a-zA-Z]+" symbol))))
             (if (texmathp)
                 (when (TeX-active-mark)
                   (insert TeX-grop)
                   (exchange-point-and-mark)
                   (insert TeX-grcl))
               (insert TeX-grop)
               (if (TeX-active-mark)
                   (progn
                     (exchange-point-and-mark)
                     (insert TeX-grcl))
                 (insert TeX-grcl)
                 (backward-char))))))))

(defun TeX-arg-string (optional &optional prompt initial-input
                                history default-value
                                leftbrace rightbrace)
  "Prompt for a string.

If OPTIONAL is not nil then the PROMPT will start with ``(Optional) ''.
INITIAL-INPUT is a string to insert before reading input.

HISTORY and DEFAULT-VALUE are ultimately passed to `read-string',
which see.

The brackets used are controlled by the string values of
LEFTBRACE and RIGHTBRACE."
  (let ((TeX-arg-opening-brace (or leftbrace TeX-arg-opening-brace))
        (TeX-arg-closing-brace (or rightbrace TeX-arg-closing-brace)))
    (TeX-argument-insert
     (if (and (not optional) (TeX-active-mark))
         (let ((TeX-argument (buffer-substring (point) (mark))))
           (delete-region (point) (mark))
           TeX-argument)
       (TeX-read-string (TeX-argument-prompt optional prompt "Text")
                        initial-input history default-value))
     optional)))

(defvar TeX-last-optional-rejected nil
  "Dynamically bound by `TeX-parse-arguments'.")

(defun TeX-parse-arguments (args)
  "Parse TeX macro arguments ARGS.

See `TeX-parse-macro' for details."
  (let ((TeX-last-optional-rejected nil))
    (while args
      (if (vectorp (car args))
          ;; Maybe get rid of all optional arguments.  See `TeX-insert-macro'
          ;; for more comments.  See `TeX-insert-macro-default-style'.
          ;; The macro `LaTeX-check-insert-macro-default-style' in
          ;; `latex.el' uses the code inside (unless ...)  This macro
          ;; should be adapted if the code here changs.
          (unless (if (eq TeX-insert-macro-default-style 'show-all-optional-args)
                      (equal current-prefix-arg '(4))
                    (or
                     (and (eq TeX-insert-macro-default-style 'show-optional-args)
                          (equal current-prefix-arg '(4)))
                     (and (eq TeX-insert-macro-default-style 'mandatory-args-only)
                          (null (equal current-prefix-arg '(4))))
                     TeX-last-optional-rejected))
            (let ((TeX-arg-opening-brace LaTeX-optop)
                  (TeX-arg-closing-brace LaTeX-optcl))
              (TeX-parse-argument t (if (equal (length (car args)) 1)
                                        (aref (car args) 0)
                                      (append (car args) nil)))))
        (let ((TeX-arg-opening-brace TeX-grop)
              (TeX-arg-closing-brace TeX-grcl))
          (setq TeX-last-optional-rejected nil)
          (TeX-parse-argument nil (car args))))
      (setq args (cdr args)))))

(defun TeX-parse-argument (optional arg)
  "Depending on OPTIONAL, insert TeX macro argument ARG.
If OPTIONAL is set, only insert if there is anything to insert, and
then use square brackets instead of curly braces.

See `TeX-parse-macro' for details."
  (let (insert-flag)
    (cond ((stringp arg)
           (TeX-arg-string optional arg)
           (setq insert-flag t))
          ((numberp arg)
           (cond ((< arg 0)
                  (when (TeX-active-mark)
                    ;; Put both the macro and the marked region in a TeX group.
                    (let ((beg (min (point) (mark)))
                          (end (set-marker (make-marker) (max (point) (mark)))))
                      (insert " ")
                      (goto-char beg)
                      (skip-chars-backward "^\\\\")
                      (backward-char)
                      (insert TeX-arg-opening-brace)
                      (goto-char (marker-position end))
                      (insert TeX-arg-closing-brace)
                      (setq insert-flag t)
                      (set-marker end nil))))
                 ((= arg 0)) ; nop for clarity
                 ((> arg 0)
                  (TeX-parse-argument optional t)
                  (while (> arg 1)
                    (TeX-parse-argument optional nil)
                    (setq arg (- arg 1))))))
          ((null arg)
           (insert TeX-arg-opening-brace)
           (when (and (not optional) (TeX-active-mark))
             (exchange-point-and-mark))
           (insert TeX-arg-closing-brace)
           (setq insert-flag t))
          ((eq arg t)
           (insert TeX-arg-opening-brace)
           (if (and (not optional) (TeX-active-mark))
               (progn
                 (exchange-point-and-mark))
             (set-marker TeX-exit-mark (point)))
           (insert TeX-arg-closing-brace)
           (setq insert-flag t))
          ((functionp arg)
           (funcall arg optional))
          ((listp arg)
           (let ((head (car arg))
                 (tail (cdr arg)))
             (cond ((stringp head)
                    (apply #'TeX-arg-string optional arg))
                   ((symbolp head)
                    (apply head optional tail))
                   (t (error "Unknown list argument type %s"
                             (prin1-to-string head))))))
          (t (error "Unknown argument type %s" (prin1-to-string arg))))
    (when (and insert-flag (not optional) (TeX-active-mark))
      (deactivate-mark))))

(defun TeX-argument-insert (name optional &optional prefix)
  "Insert NAME surrounded by curly braces.

If OPTIONAL, only insert it if not empty, and then use square brackets.
If PREFIX is given, insert it before NAME."
  (if (and optional (string-equal name ""))
      (setq TeX-last-optional-rejected t)
    (insert TeX-arg-opening-brace)
    (if prefix
        (insert prefix))
    (if (and (string-equal name "")
             (null (marker-position TeX-exit-mark)))
        (set-marker TeX-exit-mark (point))
      (insert name))
    (insert TeX-arg-closing-brace)))

(defun TeX-argument-prompt (optional prompt default &optional complete)
  "Return a argument prompt.

If OPTIONAL is not nil then the prompt will start with ``(Optional) ''.

PROMPT will be used if not nil, otherwise use DEFAULT.

Unless optional argument COMPLETE is non-nil, ``: '' will be appended."
  (concat (if optional "(Optional) " "")
          (if prompt prompt default)
          (if complete "" ": ")))

(defun TeX-string-divide-number-unit (string)
  "Divide number and unit in STRING and return a list (number unit)."
  (if (string-match "[0-9]*\\.?[0-9]+" string)
      (list (substring string 0 (string-match "[^.0-9]" string))
            (substring string (if (string-match "[^.0-9]" string)
                                  (string-match "[^.0-9]" string)
                                (length string))))
    (list "" string)))

(defcustom TeX-default-unit-for-image "cm"
  "Default unit when prompting for an image size."
  :group 'TeX-macro
  :type '(choice (const "cm")
                 (const "in")
                 (const "\\linewidth")
                 (string :tag "Other")))

(defun TeX-arg-maybe (symbol list form)
  "Evaluate FORM, if SYMBOL is an element of LIST."
  (when (memq symbol list)
    (eval form t)))

(defun TeX-arg-free (optional &rest args)
  "Parse its arguments but use no braces when they are inserted."
  (let ((TeX-arg-opening-brace "")
        (TeX-arg-closing-brace ""))
    (if (equal (length args) 1)
        (TeX-parse-argument optional (car args))
      (TeX-parse-argument optional args))))

(defun TeX-arg-literal (_optional &rest args)
  "Insert its arguments ARGS into the buffer.
Used for specifying extra syntax for a macro.  The compatibility
argument OPTIONAL is ignored."
  (apply #'insert args))

(defun TeX-arg-space (_optional &optional how-many)
  "Ignore OPTIONAL and insert 1 or HOW-MANY spaces.

This function is equivalent to
  (TeX-arg-literal \" \")
when HOW-MANY is omitted."
  (insert (make-string (or how-many 1) ?\s)))

(defun TeX-arg-set-exit-mark (_optional &optional pos)
  "Ignore OPTIONAL and set `TeX-exit-mark' to POS or current point."
  (set-marker TeX-exit-mark (or pos (point))))

;;; Font Locking

(defcustom TeX-install-font-lock #'font-latex-setup
  "Function to call to install font lock support.
Choose `ignore' if you don't want AUCTeX to install support for font locking."
  :group 'TeX-misc
  :type '(radio (function-item font-latex-setup)
                (function-item tex-font-setup)
                (function-item ignore)
                (function :tag "Other")))

;;; The Mode

(defvar TeX-format-list
  '(("JLATEX" japanese-LaTeX-mode
     "\\\\\\(documentstyle\\|documentclass\\)[^%\n]*{u?\\(j[s-]?\\|t\\)\
\\(article\\|report\\|book\\|slides\\)")
    ("JTEX" japanese-plain-TeX-mode
     "-- string likely in Japanese TeX --")
    ("AMSTEX" AmSTeX-mode
     "\\\\document\\b")
    ("CONTEXT" ConTeXt-mode
     "\\\\\\(start\\(text\\|tekst\\|proje[ck]t\\|proiect\\|\
produ[ck]t\\|produs\\|environment\\|omgeving\\|umgebung\\|prostredi\\|mediu\\|\
component\\|onderdeel\\|komponent[ea]\\|componenta\\)\
\\|inizia\\(testo\\|progetto\\|prodotto\\|ambiente\\|componente\\)\
\\)\\|%.*?interface=")
    ("LATEX" LaTeX-mode
     "\\\\\\(begin\\|\\(?:sub\\)\\{0,2\\}section\\|chapter\\|documentstyle\\|\
documentclass\\)\\b")
    ("TEX" plain-TeX-mode "."))
  "List of format packages to consider when choosing a TeX mode.

A list with an entry for each format package available at the site.

Each entry is a list with three elements.

1. The name of the format package (as string).
2. The name of the major mode (as symbol).
3. A regexp typically matched in the beginning of the file.

When entering `TeX-tex-mode', each regexp is tried in turn in
order to find the major mode to be used.")

(defcustom TeX-default-mode #'LaTeX-mode
  "Mode to enter for a new file when it can't be determined otherwise."
  :group 'TeX-misc
  :type '(radio (function-item LaTeX-mode)
                (function-item plain-TeX-mode)
                (function :tag "Other")))

(defcustom TeX-force-default-mode nil
  "If set to nil, try to infer the mode of the file from its content."
  :group 'TeX-misc
  :type 'boolean)

;;;###autoload
(defun TeX-tex-mode ()
  "Call suitable AUCTeX major mode for editing TeX or LaTeX files.
Tries to guess whether this file is for plain TeX or LaTeX.

The algorithm is as follows:

   1) If the file is empty or `TeX-force-default-mode' is not set to nil,
      `TeX-default-mode' is chosen.
   2) If non-commented out content matches with regular expression in
      `TeX-format-list', use the associated major mode.  For example,
      if \\documentclass or \\begin{, \\section{, \\part{ or \\chapter{ is
      found, `LaTeX-mode' is selected.
   3) Otherwise, use `TeX-default-mode'.

By default, `TeX-format-list' has a fallback entry for
`plain-TeX-mode', thus non-empty file which didn't match any
other entries will enter `plain-TeX-mode'."
  ;; This is a dispatch function meaningful only as target of
  ;; `auto-mode-alist' and `major-mode-remap-alist'.  Hence we don't
  ;; use `define-derived-mode'.  Note that it isn't a proper major
  ;; mode and it actually makes little sense to specify this for
  ;; "mode:" tag of file local variable.
  (interactive)

  (funcall (if (or (equal (buffer-size) 0)
                   TeX-force-default-mode)
               TeX-default-mode
             (save-excursion
               (goto-char (point-min))
               (let ((comment-start-skip ;Used by TeX-in-comment
                      (concat
                       "\\(\\(^\\|[^\\\n]\\)\\("
                       (regexp-quote TeX-esc)
                       (regexp-quote TeX-esc)
                       "\\)*\\)\\(%+ *\\)"))
                     (entry TeX-format-list)
                     answer case-fold-search)
                 (while (and entry (not answer))
                   (if (re-search-forward (nth 2 (car entry))
                                          10000 t)
                       (if (not (TeX-in-comment))
                           (setq answer (nth 1 (car entry))))
                     (setq entry (cdr entry))))
                 (if answer
                     answer
                   TeX-default-mode))))))

(defun TeX--prettify-symbols-compose-p (start end match)
  (and (tex--prettify-symbols-compose-p start end match)
       (not (let ((face (get-text-property end 'face)))
              (if (consp face)
                  (memq 'font-latex-verbatim-face face)
                (eq face 'font-latex-verbatim-face))))))

;; Delete alias predefined in tex-mode.el.
;;;###autoload (if (eq (symbol-function 'TeX-mode) 'tex-mode)
;;;###autoload     (defalias 'TeX-mode nil))
(define-derived-mode TeX-mode text-mode "TeX"
  "Base mode for AUCTeX major modes except Texinfo mode.

Not intended for direct use for user."
  :abbrev-table nil
  :after-hook (TeX-mode-cleanup)
  :interactive nil

  (setq TeX-mode-p t)
  (setq TeX-output-extension (if TeX-PDF-mode "pdf" "dvi"))
  (setq indent-tabs-mode nil)

  ;; Ispell support
  (setq-local ispell-parser 'tex)

  ;; Redefine some standard variables
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq-local comment-start "%")
  (setq-local comment-start-skip
              (concat "\\(\\(^\\|[^\\\n]\\)\\("
                      (regexp-quote TeX-esc)
                      (regexp-quote TeX-esc)
                      "\\)*\\)\\(%+[ \t]*\\)"))
  (setq-local comment-end-skip "[ \t]*\\(\\s>\\|\n\\)")
  (setq-local comment-use-syntax t)
  (setq-local comment-padding " ")
  ;; Removed as commenting in (La)TeX is done with one `%' not two
  ;; (make-local-variable 'comment-add)
  ;; (setq comment-add 1) ;default to `%%' in comment-region
  (setq-local comment-indent-function #'TeX-comment-indent)
  (setq-local comment-multi-line nil)
  (make-local-variable 'compile-command)
  (unless (boundp 'compile-command)
    (setq compile-command "make"))
  (setq-local words-include-escapes nil)

  ;; Make TAB stand out
  ;;  (make-local-variable 'buffer-display-table)
  ;;  (setq buffer-display-table (if standard-display-table
  ;;                             (copy-sequence standard-display-table)
  ;;                           (make-display-table)))
  ;;  (aset buffer-display-table ?\t (apply 'vector (append "<TAB>" nil)))

  (funcall TeX-install-font-lock)

  ;; We want this to be early in the list, so we do not add it before
  ;; we enter TeX mode the first time.
  (add-hook 'write-contents-functions #'TeX-safe-auto-write nil t)

  ;; Minor modes
  (when TeX-source-correlate-mode
    (TeX-source-correlate-mode 1))

  ;; Prettify Symbols mode
  (require 'tex-mode)
  (setq-local prettify-symbols-alist tex--prettify-symbols-alist)
  (add-function :override (local 'prettify-symbols-compose-predicate)
                #'TeX--prettify-symbols-compose-p)

  ;; Standard Emacs completion-at-point support
  (add-hook 'completion-at-point-functions
            #'TeX--completion-at-point nil t)

  ;; Let `TeX-master-file' be called after a new file was opened and
  ;; call `TeX-update-style' on any file opened.  (The addition to the
  ;; hook has to be made here because its local value will be deleted
  ;; by `kill-all-local-variables' if it is added e.g. in `tex-mode'.)
  ;;
  ;; `TeX-update-style' has to be called before
  ;; `global-font-lock-mode', which may also be specified in
  ;; `find-file-hook', gets called.  Otherwise style-based
  ;; fontification will break (in XEmacs).  That means, `add-hook'
  ;; cannot be called with a non-nil value of the APPEND argument.
  ;;
  ;; `(TeX-master-file nil nil t)' has to be called *before*
  ;; `TeX-update-style' as the latter will call `TeX-master-file'
  ;; without the `ask' bit set.
  (add-hook 'find-file-hook
            (lambda ()
              ;; Check if we are looking at a new or shared file.
              (when (or (not (file-exists-p (TeX-buffer-file-name)))
                        (eq TeX-master 'shared))
                (TeX-master-file nil nil t))
              (TeX-update-style t)) nil t))

(defun TeX-mode-cleanup ()
  "Cleanup function for `TeX-mode'.
Run after mode hooks and file local variables application."
  ;; Symbol & length completion.
  (or (local-variable-p 'TeX-complete-list)
      (setq-local TeX-complete-list
                  (list (list "\\\\\\([a-zA-Z@:_]*\\)"
                              1
                              (lambda ()
                                (append
                                 (TeX-symbol-list-filtered)
                                 ;; These LaTeX-*-list are called even
                                 ;; in non-LaTeX mode buffers, but
                                 ;; that is permissible because they
                                 ;; return empty list immediately.
                                 (when (fboundp 'LaTeX-length-list)
                                   (LaTeX-length-list))
                                 (when (fboundp 'LaTeX-counter-list)
                                   (mapcar (lambda (x)
                                             `(,(concat "the" (car x))))
                                           (LaTeX-counter-list)))))
                              (if TeX-insert-braces "{}"))
                        (list "" TeX-complete-word))))

  ;; Complete style initialization in buffers which don't visit files
  ;; and which are therefore missed by the setting of above
  ;; `find-file-hook'.  This is necessary for `xref-find-references',
  ;; for example. (bug#65912)
  (unless buffer-file-truename
    (TeX-update-style))

  (TeX-set-mode-name))

;; COMPATIBILITY for Emacs<29
;;;###autoload
(put 'TeX-mode 'auctex-function-definition (symbol-function 'TeX-mode))

;; COMPATIBILITY for Emacs<30
(unless (fboundp 'derived-mode-add-parents)
  (advice-add 'provided-mode-derived-p :after-until
              ;; Don't quote by #'-style to avoid compiler warning.
              'TeX--compat-provided-mode-derived-p)
  (defun TeX--compat-provided-mode-derived-p (mode &rest modes)
    "Add pseudo-parents facility to `provided-mode-derived-p' like Emacs 30.
Modes registered in `derived-mode-extra-parents' property of MODE
symbol are regarded as parent modes by `provided-mode-derived-p',
when MODE is one of the AUCTeX new mode names."
    (when (rassq mode TeX-mode-comparison-alist)
      (let ((extra-parents (get mode 'derived-mode-extra-parents)))
        (and extra-parents
             (cl-loop for parent in extra-parents
                      thereis (memq parent modes)))))))

;;; Hilighting

;; FIXME: It's likely that `hilit-patterns-alist' is much obsolete.
(if (boundp 'hilit-patterns-alist)
    (let ((latex-patterns (cdr-safe (assq 'latex-mode hilit-patterns-alist)))
          (plain-tex-patterns (cdr-safe (assq 'plain-tex-mode
                                              hilit-patterns-alist))))
      (if (and latex-patterns plain-tex-patterns)
          (setq hilit-patterns-alist
                (append (list (cons 'ams-tex-mode plain-tex-patterns))
                        hilit-patterns-alist)))))

;;; Parsing

(defgroup TeX-parse nil
  "Parsing TeX files from AUCTeX."
  :group 'AUCTeX)

(defvar TeX-auto-parser '((styles TeX-auto-file TeX-run-style-hooks)))
;; Alist of parsed information.
;; Each entry is a list with the following elements:
;;
;; 0. Name of information type.
;; 1. Name of temporary variable used when parsing.
;; 2. Name of function to add information to add to #3.
;; 3. Name of variable holding buffer local information.
;; 4. Name of variable indicating that #3 has changed.


(defconst TeX-auto-parser-temporary 1)
(defconst TeX-auto-parser-add 2)
(defconst TeX-auto-parser-local 3)
(defconst TeX-auto-parser-change 4)

(defvar TeX-auto-file nil)
;; Internal temporal variable.  Don't refer to it in your program
;; unless you know what you are doing.  Use (TeX-style-list) instead.

(defun TeX-auto-add-information (name entries)
  "For NAME in `TeX-auto-parser' add ENTRIES."
  (let* ((entry (assoc name TeX-auto-parser))
         (change (nth TeX-auto-parser-change entry))
         (change-value (symbol-value change))
         (local (nth TeX-auto-parser-local entry))
         (local-value (symbol-value local)))
    (if change-value
        (set local (cons entries local-value))
      (set change t)
      (set local (list entries local-value)))))

(defun TeX-auto-list-information (name)
  "Return information in `TeX-auto-parser' about NAME."
  (TeX-update-style)
  (let* ((entry (assoc name TeX-auto-parser))
         (change (nth TeX-auto-parser-change entry))
         (change-value (symbol-value change))
         (local (nth TeX-auto-parser-local entry)))
    (if (not change-value)
        ()
      (set change nil)
      ;; Sort it
      (message "Sorting %s..." name)
      (set local
           (sort (mapcar #'TeX-listify (apply #'append (symbol-value local)))
                 #'TeX-car-string-lessp))
      (message "Sorting %s...done" name)
      ;; Make it unique
      (message "Removing duplicates...")
      (let ((entry (symbol-value local)))
        (while (and entry (cdr entry))
          (let ((this (car entry))
                (next (car (cdr entry))))
            (if (not (string-equal (car this) (car next)))
                (setq entry (cdr entry))
              ;; We have two equal symbols.  Use the one with
              ;; most arguments.
              (if (> (length next) (length this))
                  (setcdr this (cdr next)))
              (setcdr entry (cdr (cdr entry)))))))
      (message "Removing duplicates...done"))
    (symbol-value local)))

(defmacro TeX-auto-add-type (name prefix &optional plural)
  "Add information about NAME to the parser using PREFIX.

Optional third argument PLURAL is the plural form of NAME.
By default just add an `s'.

This macro creates a set of variables and functions to maintain a
separate type of information in the parser."
  (let* ((names (or plural (concat name "s")))
         (tmp (intern (concat prefix "-auto-" name)))
         (add (intern (concat prefix "-add-" names)))
         (local (intern (concat prefix "-" name "-list")))
         (change (intern (concat prefix "-" name "-changed")))
         (vardoc (concat "Information about " names
                          " in the current buffer.
Generated by `TeX-auto-add-type'."))
         ;; Avoid clash between LaTeX environments and ConTeXt
         ;; environments in keys of `TeX-auto-parser'.
         (unique-key (concat prefix "-" name)))
    `(progn
       (defvar ,tmp nil ,vardoc)
       (defvar ,local nil ,vardoc)
       (make-variable-buffer-local ',local)
       (defvar ,change nil ,vardoc)
       (make-variable-buffer-local ',change)
       (defun ,add (&rest ,(intern names))
         ,(concat "Add information about " (upcase names)
                  ".
Information is added to the current buffer.
Generated by `TeX-auto-add-type'.")
         (TeX-auto-add-information ,unique-key ,(intern names)))
       (defun ,local ()
         ,(concat "List of " names
                  " active in the current buffer.
Generated by `TeX-auto-add-type'.")
         (TeX-auto-list-information ,unique-key))
       ;; Append new type to `TeX-auto-parser' in order to make `style' type
       ;; always the first.
       (add-to-list 'TeX-auto-parser
                    ',(list unique-key tmp add local change) t)
       (add-hook 'TeX-remove-style-hook
                 (lambda ()
                   (setq ,local nil))))))

(TeX-auto-add-type "symbol" "TeX")

(defvar TeX-auto-apply-hook nil
  "Hook run when a buffer is parsed and the information is applied.")

(defun TeX-auto-apply ()
  "Parse and apply TeX information in the current buffer."
  (TeX-auto-parse)
  (run-hooks 'TeX-auto-apply-hook)
  (mapcar #'TeX-auto-apply-entry TeX-auto-parser))

(defun TeX-auto-apply-entry (entry)
  "Apply the information in ENTRY in `TeX-auto-parser'."
  (let ((value (symbol-value (nth TeX-auto-parser-temporary entry)))
        (add (nth TeX-auto-parser-add entry)))
    (if value (apply add value))))

(defun TeX-safe-auto-write ()
  "Call `TeX-auto-write' safely."
  (condition-case _ignored
      (TeX-auto-write)
    (error nil))
  ;; Continue with the other write file hooks.
  nil)

(defcustom TeX-auto-save nil
  "Automatically save style information when saving the buffer."
  :group 'TeX-parse
  :type 'boolean)

(defcustom TeX-auto-untabify nil
  "Automatically untabify when saving the buffer."
  :group 'TeX-parse
  :type 'boolean)

(defcustom TeX-auto-save-aggregate t
  "When non-nil, save parsed information in one directory.
Each style file of automatically parsed information is saved in
\"auto\" subdirectory of master file.

When nil, saves in each \"auto\" subdirectory.

Subdirectory name is actually taken from `TeX-auto-local'."
  :group 'TeX-parse
  :type 'boolean)

(defun TeX-auto-write ()
  "Save all relevant TeX information from the current buffer."
  (if TeX-auto-untabify
      (untabify (point-min) (point-max)))
  (if (and TeX-auto-save TeX-auto-local)
      (let* ((file (expand-file-name
                    (concat
                     (file-name-as-directory TeX-auto-local)
                     (TeX-strip-extension nil TeX-all-extensions t)
                     ".el")
                    (if TeX-auto-save-aggregate
                        (TeX-master-directory)
                      default-directory)))
             (dir (file-name-directory file)))
        ;; Create auto directory if possible.
        (if (not (file-exists-p dir))
            (condition-case _ignored
                (make-directory dir)
              (error nil)))
        (if (file-writable-p file)
            (save-excursion
              (TeX-update-style)
              (TeX-auto-store file))
          (message "Can't write style information.")))))

(defcustom TeX-macro-default (car-safe TeX-macro-private)
  "Default directory to search for TeX macros."
  :group 'TeX-file
  :type 'directory)

(defcustom TeX-auto-default (car-safe TeX-auto-private)
  "Default directory to place automatically generated TeX information."
  :group 'TeX-file
  :type 'directory)

(defcustom TeX-ignore-file
  "\\(^\\|[/\\]\\)\\(\\.\\|\\.\\.\\|RCS\\|SCCS\\|CVS\\|babel\\..*\\)$"
  "Regular expression matching file names to ignore.

These files or directories will not be considered when searching for
TeX files in a directory."
  :group 'TeX-parse
  :type 'regexp)

(defcustom TeX-file-recurse t
  "Whether to search TeX directories recursively.
nil means do not recurse, a positive integer means go that far deep in the
directory hierarchy, t means recurse indefinitely."
  :group 'TeX-parse
  :type '(choice (const :tag "On" t)
                 (const :tag "Off" nil)
                 (integer :tag "Depth" :value 1)))

(defvar TeX-file-extensions)
(defvar BibTeX-file-extensions)
(defvar TeX-Biber-file-extensions)

;;;###autoload
(defun TeX-auto-generate (tex auto)
  "Generate style file for TEX and store it in AUTO.
If TEX is a directory, generate style files for all files in the directory."
  (interactive (list (setq TeX-macro-default
                           (expand-file-name (read-file-name
                                              "TeX file or directory: "
                                              TeX-macro-default
                                              TeX-macro-default 'confirm)))
                     (setq TeX-auto-default
                           (expand-file-name (read-file-name
                                              "AUTO lisp directory: "
                                              TeX-auto-default
                                              TeX-auto-default 'confirm)))))
  (cond ((not (file-readable-p tex)))
        ((string-match TeX-ignore-file tex))
        ((file-directory-p tex)
         (let ((files (directory-files (expand-file-name tex)))
               (default-directory (file-name-as-directory
                                   (expand-file-name tex)))
               (TeX-file-recurse (cond ((symbolp TeX-file-recurse)
                                        TeX-file-recurse)
                                       ((zerop TeX-file-recurse)
                                        nil)
                                       ((1- TeX-file-recurse)))))
           (mapcar (lambda (file)
                     (if (or TeX-file-recurse
                             (not (file-directory-p file)))
                         (TeX-auto-generate file auto)))
                   files)))
        ((not (file-newer-than-file-p
               tex
               (concat (file-name-as-directory auto)
                       (TeX-strip-extension tex TeX-all-extensions t)
                       ".el"))))
        ((TeX-match-extension tex (TeX-delete-duplicate-strings
                                   (append TeX-file-extensions
                                           BibTeX-file-extensions
                                           TeX-Biber-file-extensions)))
         (with-current-buffer (let (enable-local-eval)
                                (find-file-noselect tex))
           (message "Parsing %s..." tex)
           (TeX-auto-store (concat (file-name-as-directory auto)
                                   (TeX-strip-extension tex
                                                        TeX-all-extensions
                                                        t)
                                   ".el"))
           (kill-buffer (current-buffer))
           (message "Parsing %s...done" tex)))))

;;;###autoload
(defun TeX-auto-generate-global ()
  "Create global auto directory for global TeX macro definitions."
  (interactive)
  (unless (file-directory-p TeX-auto-global)
    (make-directory TeX-auto-global))
  (let ((TeX-file-extensions '("cls" "sty"))
        (BibTeX-file-extensions nil)
        (TeX-Biber-file-extensions nil))
    (mapc (lambda (macro) (TeX-auto-generate macro TeX-auto-global))
          TeX-macro-global))
  (byte-recompile-directory TeX-auto-global 0))

(defun TeX-auto-store (file)
  "Extract information for AUCTeX from current buffer and store it in FILE."
  (TeX-auto-parse)

  (if (member nil (mapcar #'TeX-auto-entry-clear-p TeX-auto-parser))
      (let ((style (TeX-strip-extension nil TeX-all-extensions t))
            (class-opts (if (boundp 'LaTeX-provided-class-options)
                            LaTeX-provided-class-options))
            (pkg-opts (if (boundp 'LaTeX-provided-package-options)
                          LaTeX-provided-package-options))
            (tex-cmd-opts TeX-command-extra-options)
            (verb-envs (when (boundp 'LaTeX-verbatim-environments-local)
                         LaTeX-verbatim-environments-local))
            (verb-macros-delims (when (boundp 'LaTeX-verbatim-macros-with-delims-local)
                                  LaTeX-verbatim-macros-with-delims-local))
            (verb-macros-braces (when (boundp 'LaTeX-verbatim-macros-with-braces-local)
                                  LaTeX-verbatim-macros-with-braces-local))
            (dialect TeX-style-hook-dialect)
            (bibtex-p (eq major-mode 'bibtex-mode)))
        (TeX-unload-style style)
        (with-current-buffer (generate-new-buffer file)
          (erase-buffer)
          (insert ";; -*- lexical-binding: t; -*-\n\n")
          (insert "(TeX-add-style-hook\n \""
                  style "\"\n (lambda ()")
          (unless (string= tex-cmd-opts "")
            (insert "\n   (setq TeX-command-extra-options\n"
                    "         " (prin1-to-string tex-cmd-opts) ")"))
          (when class-opts
            (insert "\n   (TeX-add-to-alist 'LaTeX-provided-class-options\n"
                    "                     '" (prin1-to-string class-opts) ")"))
          (when pkg-opts
            (insert "\n   (TeX-add-to-alist 'LaTeX-provided-package-options\n"
                    "                     '" (prin1-to-string pkg-opts) ")"))
          (dolist (env verb-envs)
            (insert
             (format "\n   (add-to-list 'LaTeX-verbatim-environments-local \"%s\")"
                     env)))
          (dolist (env verb-macros-braces)
            (insert
             (format "\n   (add-to-list 'LaTeX-verbatim-macros-with-braces-local \"%s\")"
                     env)))
          (dolist (env verb-macros-delims)
            (insert
             (format "\n   (add-to-list 'LaTeX-verbatim-macros-with-delims-local \"%s\")"
                     env)))
          (mapc (lambda (el) (TeX-auto-insert el style))
                TeX-auto-parser)
          (insert ")")
          (if dialect (insert (concat
                               "\n "
                               (prin1-to-string
                                (if bibtex-p
                                    ;; Add :latex since functions such
                                    ;; as `LaTeX-add-bibitems' are
                                    ;; only meaningful in LaTeX
                                    ;; document buffer.
                                    ;; FIXME: BibTeX is available to
                                    ;; plain TeX through eplain
                                    ;; (<URL:https://tug.org/eplain/doc/eplain.html#Citations>).
                                    ;; It would be nice if AUCTeX
                                    ;; supports such usage.
                                    `'(or ,dialect :latex)
                                  dialect)))))
          (insert ")\n\n")
          (write-region (point-min) (point-max) file nil 'silent)
          (kill-buffer (current-buffer))))
    (if (file-exists-p (concat file "c"))
        (delete-file (concat file "c")))
    (if (file-exists-p file)
        (delete-file file))))

(defun TeX-auto-entry-clear-p (entry)
  "Check if the temporary for `TeX-auto-parser' entry ENTRY is clear."
  ;; FIXME: This doc-string isn't clear to me.  -- rs
  (null (symbol-value (nth TeX-auto-parser-temporary entry))))

(defun TeX-auto-insert (entry &optional skip)
  "Insert code to initialize ENTRY from `TeX-auto-parser'.

If SKIP is not-nil, don't insert code for SKIP."
  (let ((name (symbol-name (nth TeX-auto-parser-add entry)))
        (list (symbol-value (nth TeX-auto-parser-temporary entry))))
    (unless (null list)
      (insert "\n   (" name)
      (dolist (el list)
        (cond ((and (stringp el) (not (string= el skip)))
               (insert "\n    ")
               (insert (prin1-to-string el)))
              ((not (stringp el))
               (insert "\n    ")
               (insert "'" (prin1-to-string el)))))
      (insert ")"))))

(defvar TeX-auto-ignore
  '("csname" "filedate" "fileversion" "docdate" "next" "labelitemi"
    "labelitemii" "labelitemiii" "labelitemiv" "labelitemv"
    "labelenumi" "labelenumii" "labelenumiii" "labelenumiv"
    "labelenumv" "theenumi" "theenumii" "theenumiii" "theenumiv"
    "theenumv" "document" "par" "do" "expandafter")
  "List of symbols to ignore when scanning a TeX style file.")

(defcustom TeX-auto-regexp-list 'TeX-auto-full-regexp-list
  "List of regular expressions used for parsing the current file.
It can also be a name of a variable having such value."
  :type '(radio (variable-item TeX-auto-empty-regexp-list)
                (variable-item TeX-auto-full-regexp-list)
                (variable-item plain-TeX-auto-regexp-list)
                (variable-item LaTeX-auto-minimal-regexp-list)
                (variable-item LaTeX-auto-label-regexp-list)
                (variable-item LaTeX-auto-regexp-list)
                (variable :tag "Other")
                (repeat :tag "Specify"
                        (group (regexp :tag "Match")
                               (sexp :tag "Groups")
                               symbol)))
  :group 'TeX-parse
  :local t)

(defun TeX-auto-add-regexp (regexp)
  "Add REGEXP to `TeX-auto-regexp-list' if not already a member."
  (if (symbolp TeX-auto-regexp-list)
      (setq TeX-auto-regexp-list (symbol-value TeX-auto-regexp-list)))
  (or (member regexp TeX-auto-regexp-list)
      (setq TeX-auto-regexp-list (cons regexp TeX-auto-regexp-list))))

(defvar TeX-auto-empty-regexp-list
  '(("<IMPOSSIBLE>\\(\\'\\`\\)" 1 ignore))
  "List of regular expressions guaranteed to match nothing.")

(defvar TeX-token-char
  "\\(?:[a-zA-Z]\\|\\cj\\)"
  "Regexp matching a character in a TeX macro.

Please use a shy group if you use a grouping construct, because
the functions/variables which use `TeX-token-char' expect not to
alter the numbering of any ordinary, non-shy groups.")

(defvar plain-TeX-auto-regexp-list
  (let ((token TeX-token-char))
    `((,(concat "\\\\\\(?:def\\|let\\)\\\\\\(" token "+\\)[^a-zA-Z@]")
       1 TeX-auto-symbol-check)
      (,(concat "\\\\"
                (regexp-opt '("font" "newfont" "chardef" "mathchardef"
                              "newcount" "newdimen" "newmuskip" "newskip"))
                "{?\\\\\\(" token "+\\)}?[^a-zA-Z@]")
       1 TeX-auto-symbol)
      (,(concat "\\\\typein\\[\\\\\\(" token "+\\)\\]") 1 TeX-auto-symbol)
      ("\\\\input +\\([^#}%\"\\\n\r]+?\\)\\(?:\\.[^#}%/\"\\.\n\r]+\\)?"
       1 TeX-auto-file)))
  "List of regular expression matching common plain TeX macro definitions.")

(defvar TeX-auto-full-regexp-list plain-TeX-auto-regexp-list
  "Full list of regular expression matching TeX macro definitions.")

(defvar TeX-auto-prepare-hook nil
  "List of hooks to be called before parsing a TeX file.")

(defvar TeX-auto-cleanup-hook nil
  "List of hooks to be called after parsing a TeX file.")

(defcustom TeX-auto-parse-length 999999
  "Maximal length of TeX file (in characters) that will be parsed."
  :group 'TeX-parse
  :type 'integer
  :local t)

(defcustom TeX-auto-x-parse-length 0
  "Maximum length of TeX file that will be parsed additionally.
Use `TeX-auto-x-regexp-list' for parsing the region between
`TeX-auto-parse-length' and this value."
  :group 'TeX-parse
  :type 'integer
  :local t)

(defcustom TeX-auto-x-regexp-list 'LaTeX-auto-label-regexp-list
  "List of regular expressions used for additional parsing.
It can also be a name of a variable having such value.
See `TeX-auto-x-parse-length'."
  :type '(radio (variable-item TeX-auto-empty-regexp-list)
                (variable-item TeX-auto-full-regexp-list)
                (variable-item plain-TeX-auto-regexp-list)
                (variable-item LaTeX-auto-minimal-regexp-list)
                (variable-item LaTeX-auto-label-regexp-list)
                (variable-item LaTeX-auto-regexp-list)
                (variable :tag "Other")
                (repeat :tag "Specify"
                        (group (regexp :tag "Match")
                               (sexp :tag "Groups")
                               symbol)))
  :group 'TeX-parse
  :local t)

(defun TeX-regexp-group-count (regexp)
  "Return number of groups in a REGEXP.  This is not foolproof:
you should not use something like `[\\(]' for a character range."
  (let (start (n 0))
    (while (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\\\([^?]"
                         regexp start)
      (setq start (- (match-end 0) 2)
            n (1+ n)))
    n))

(defun TeX-auto-parse-region (regexp-list beg end)
  "Parse TeX information according to REGEXP-LIST between BEG and END."
  (if (symbolp regexp-list)
      (setq regexp-list (and (boundp regexp-list) (symbol-value regexp-list))))
  (if regexp-list
      ;; Extract the information.
      (let* (groups
             (count 1)
             (regexp (concat "\\("
                             (mapconcat
                              (lambda(x)
                                (push (cons count x) groups)
                                (setq count
                                      (+ 1 count
                                         (TeX-regexp-group-count (car x))))
                                (car x))
                              regexp-list "\\)\\|\\(")
                             "\\)"))
             syms
             lst)
        ;; TODO: Emacs allows at most 255 groups in a regexp, see the
        ;; "#define MAX_REGNUM 255" in regex-emacs.c.  If our regex
        ;; has more groups, bad things may happen, e.g.,
        ;; (match-beginning 271) returns nil although the regexp that
        ;; matched contains group number 271.  Sadly, MAX_REGNUM is
        ;; not exposed to Lisp, so we need to hard-code it here (and
        ;; sometimes check if it increased in newer Emacs versions).
        (when (> count 255)
          (error "The TeX auto-parser's regexp has too many groups (%d)" count))
        (setq count 0)
        (goto-char (if end (min end (point-max)) (point-max)))
        (while (re-search-backward regexp beg t)
          (let* ((entry (cdr (TeX-member nil groups
                                         (lambda (_a b)
                                           (match-beginning (car b))))))
                 (symbol (nth 2 entry))
                 (match (nth 1 entry)))
            (unless (TeX-in-comment)
              (looking-at (nth 0 entry))
              (if (fboundp symbol)
                  (funcall symbol match)
                (puthash (if (listp match)
                             (mapcar #'TeX-match-buffer match)
                           (TeX-match-buffer match))
                         (setq count (1- count))
                         (cdr (or (assq symbol syms)
                                  (car (push
                                        (cons symbol
                                              (make-hash-table :test #'equal))
                                        syms)))))))))
        (setq count 0)
        (dolist (symbol syms)
          (setq lst (symbol-value (car symbol)))
          (while lst
            (puthash (pop lst)
                     (setq count (1+ count))
                     (cdr symbol)))
          (maphash (lambda (key value)
                     (push (cons value key) lst))
                   (cdr symbol))
          (clrhash (cdr symbol))
          (set (car symbol) (mapcar #'cdr (sort lst #'car-less-than-car)))))))

(defun TeX-auto-parse ()
  "Parse TeX information in current buffer.

Call the functions in `TeX-auto-prepare-hook' before parsing, and the
functions in `TeX-auto-cleanup-hook' after parsing."

  (let ((case-fold-search nil))

    (mapc #'TeX-auto-clear-entry TeX-auto-parser)
    (run-hooks 'TeX-auto-prepare-hook)

    (save-excursion
      (and (> TeX-auto-x-parse-length TeX-auto-parse-length)
           (> (point-max) TeX-auto-parse-length)
           (TeX-auto-parse-region TeX-auto-x-regexp-list
                                  TeX-auto-parse-length
                                  TeX-auto-x-parse-length))
      (TeX-auto-parse-region TeX-auto-regexp-list
                             nil TeX-auto-parse-length))

    ;; Cleanup ignored symbols.

    ;; NOTE: This is O(N M) where it could be O(N log N + M log M) if we
    ;; sorted the lists first.
    (while (member (car TeX-auto-symbol) TeX-auto-ignore)
      (setq TeX-auto-symbol (cdr TeX-auto-symbol)))
    (let ((list TeX-auto-symbol))
      (while (and list (cdr list))
        (if (member (car (cdr list)) TeX-auto-ignore)
            (setcdr list (cdr (cdr list)))
          (setq list (cdr list)))))

    (run-hooks 'TeX-auto-cleanup-hook)))

(defun TeX-auto-clear-entry (entry)
  "Set the temporary variable in ENTRY to nil."
  (set (nth TeX-auto-parser-temporary entry) nil))

(defvar LaTeX-auto-end-symbol nil)

(defun TeX-auto-symbol-check (match)
  "Add MATCH to TeX-auto-symbols.
Check for potential LaTeX environments."
  (let ((symbol (if (listp match)
                    (mapcar #'TeX-match-buffer match)
                  (TeX-match-buffer match))))
    (if (and (stringp symbol)
             (string-match "^end\\(.+\\)$" symbol))
        (add-to-list 'LaTeX-auto-end-symbol
                     (substring symbol (match-beginning 1) (match-end 1)))
      (if (listp symbol)
          (dolist (elt symbol)
            (add-to-list 'TeX-auto-symbol elt))
        (add-to-list 'TeX-auto-symbol symbol)))))


;;; File Extensions

(defgroup TeX-file-extension nil
  "File extensions recognized by AUCTeX."
  :group 'TeX-file)

(defcustom TeX-file-extensions '("tex" "sty" "cls" "ltx" "texi" "txi" "texinfo" "dtx")
  "File extensions used by manually generated TeX files."
  :group 'TeX-file-extension
  :type '(repeat (regexp :format "%v")))

(defcustom TeX-all-extensions '("[^.\n]+")
  "All possible file extensions."
  :group 'TeX-file-extension
  :type '(repeat (regexp :format "%v")))

(defcustom TeX-default-extension "tex"
  "Default extension for TeX files."
  :group 'TeX-file-extension
  :type 'string
  :local t)

(defvar TeX-doc-extensions
  '("dvi" "pdf" "ps" "txt" "html" "dvi\\.gz" "pdf\\.gz" "ps\\.gz" "txt\\.gz"
    "html\\.gz" "dvi\\.bz2" "pdf\\.bz2" "ps\\.bz2" "txt\\.bz2" "html\\.bz2")
  "File extensions of documentation files.")

(defcustom docTeX-default-extension "dtx"
  "Default extension for docTeX files."
  :group 'TeX-file-extension
  :type 'string)

(defvar-local TeX-output-extension nil
  "Extension of TeX output file.
This is either a string or a list with
a string as element.  Its value is obtained from `TeX-command-output-list'.
Access to the value should be through the function `TeX-output-extension'.")

(defcustom TeX-Biber-file-extensions '("bib" "ris" "xml")
  "Valid file extensions for Biber files."
  :group 'TeX-file-extension
  :type '(repeat (regexp :format "%v")))

(defcustom BibTeX-file-extensions '("bib")
  "Valid file extensions for BibTeX files."
  :group 'TeX-file-extension
  :type '(repeat (regexp :format "%v")))

(defcustom BibLaTeX-style-extensions '("bbx")
  "Valid file extensions for BibLaTeX styles."
  :group 'TeX-file-extension
  :type '(repeat (regexp :format "%v")))

(defcustom BibTeX-style-extensions '("bst")
  "Valid file extensions for BibTeX styles."
  :group 'TeX-file-extension
  :type '(repeat (regexp :format "%v")))

(defun TeX-match-extension (file &optional extensions)
  "Return non-nil if FILE has one of EXTENSIONS.

If EXTENSIONS is not specified or nil, the value of
`TeX-file-extensions' is used instead."

  (if (null extensions)
      (setq extensions TeX-file-extensions))

  (let ((regexp (concat "\\.\\("
                        (mapconcat #'identity extensions "\\|")
                        "\\)$"))
        (case-fold-search t))
    (string-match regexp file)))

(defun TeX-strip-extension (&optional string extensions nodir nostrip)
  "Return STRING without any trailing extension in EXTENSIONS.
If NODIR is t, also remove directory part of STRING.
If NODIR is `path', remove directory part of STRING if it is
equal to the current directory or is a member of
`TeX-macro-private' or `TeX-macro-global'.
If NOSTRIP is set, do not remove extension after all.
STRING defaults to the name of the current buffer.
EXTENSIONS defaults to `TeX-file-extensions'."

  (if (null string)
      (setq string (or (TeX-buffer-file-name) "<none>")))

  (if (null extensions)
      (setq extensions TeX-file-extensions))

  (let* ((strip (if (and (not nostrip)
                         (TeX-match-extension string extensions))
                    (substring string 0 (match-beginning 0))
                  string))
         (dir (expand-file-name (or (file-name-directory strip) "./"))))
    (if (or (eq nodir t)
            (string-equal dir (expand-file-name "./"))
            (member dir (mapcar #'file-name-as-directory TeX-macro-global))
            (member dir (mapcar #'file-name-as-directory TeX-macro-private)))
        (file-name-nondirectory strip)
      strip)))


;;; File Searching

(defun TeX-tree-roots ()
  "Return a list of available TeX tree roots."
  (let (list)
    (dolist (dir (TeX-tree-expand '("$TEXMFHOME" "$TEXMFMAIN" "$TEXMFLOCAL"
                                    "$TEXMFDIST")
                                  "latex"))
      (when (file-readable-p dir)
        (cl-pushnew dir list :test #'equal)))
    (nreverse list)))

(defcustom TeX-tree-roots (TeX-tree-roots)
  "List of all available TeX tree root directories."
  :group 'TeX-file
  :type '(repeat directory))

;; We keep this function in addition to `TeX-search-files' because it
;; is faster.  Since it does not look further into subdirectories,
;; this comes at the price of finding a smaller number of files.
(defun TeX-search-files-kpathsea (var extensions scope nodir strip &optional extra-dirs)
  "Return a list of files in directories determined by expanding VAR.
Only files which match EXTENSIONS are returned.  SCOPE defines
the scope for the search and can be `local' or `global' besides
nil.  If NODIR is non-nil, remove directory part.  If STRIP is
non-nil, remove file extension.
If SCOPE is `local' and the optional argument EXTRA-DIRS is passed, it
is appended to the list of local directories to search.  In `global'
scope, EXTRA-DIRS does nothing."
  (when TeX-kpathsea-path-delimiter
    (let ((dirs (if (eq scope 'local)
                    (cons "./" extra-dirs)
                  (TeX-tree-expand (list var) nil)))
          result)
      (if (eq scope 'global)
          (setq dirs (delete "./" dirs)))
      (setq extensions (concat "\\.\\(?:"
                               (mapconcat #'identity extensions "\\|")
                               "\\)\\'")
            result (apply #'append (mapcar (lambda (x)
                                             (when (file-readable-p x)
                                               (directory-files
                                                x (not nodir) extensions t)))
                                           dirs)))
      (if strip
          (mapcar (lambda (x)
                    (if (string-match extensions x)
                        (substring x 0 (match-beginning 0))
                      x))
                  result)
        result))))

(defun TeX-search-files (&optional directories extensions nodir strip)
  "Return a list of all reachable files in DIRECTORIES ending with EXTENSIONS.
If optional argument NODIR is set, remove directory part.
If optional argument STRIP is set, remove file extension.
If optional argument DIRECTORIES is set, search in those directories.
Otherwise, search in all TeX macro directories.
If optional argument EXTENSIONS is not set, use `TeX-file-extensions'"
  (when (null extensions)
    (setq extensions TeX-file-extensions))
  (when (null directories)
    (setq directories (cons "./" (append TeX-macro-private TeX-macro-global))))
  (let (match
        (TeX-file-recurse (cond ((symbolp TeX-file-recurse)
                                 TeX-file-recurse)
                                ((zerop TeX-file-recurse)
                                 nil)
                                ((1- TeX-file-recurse)))))
    (while directories
      (let* ((directory (car directories))
             (content (and directory
                           (file-readable-p directory)
                           (file-directory-p directory)
                           (directory-files directory))))
        (setq directories (cdr directories))
        (while content
          (let ((file (concat directory (car content))))
            (setq content (cdr content))
            (cond ((string-match TeX-ignore-file file))
                  ((not (file-readable-p file)))
                  ((file-directory-p file)
                   (if TeX-file-recurse
                       (setq match
                             (append match
                                     (TeX-search-files
                                      (list (file-name-as-directory file))
                                      extensions nodir strip)))))
                  ((TeX-match-extension file extensions)
                   (setq match (cons (TeX-strip-extension
                                      file extensions nodir (not strip))
                                     match))))))))
    match))

;; The variables `TeX-macro-private' and `TeX-macro-global' are not
;; used for specifying the directories because the number of
;; directories to be searched should be limited as much as possible
;; and the TeX-macro-* variables are just too broad for this.
(defvar TeX-search-files-type-alist
  '((texinputs "${TEXINPUTS}" ("tex/") TeX-file-extensions)
    (docs "${TEXDOCS}" ("doc/") TeX-doc-extensions)
    (bibinputs "${BIBINPUTS}" ("bibtex/bib/") BibTeX-file-extensions)
    (bstinputs "${BSTINPUTS}" ("bibtex/bst/") BibTeX-style-extensions))
  "Alist of filetypes with locations and file extensions.
Each element of the alist consists of a symbol expressing the
filetype, a variable which can be expanded on kpathsea-based
systems into the directories where files of the given type
reside, a list of absolute directories, relative directories
below the root of a TDS-compliant TeX tree or a list of variables
with either type of directories as an alternative for
non-kpathsea-based systems and a list of extensions to be matched
upon a file search.  Note that the directories have to end with a
directory separator.

Each AUCTeX mode should set the variable buffer-locally with a
more specific value.  See `LaTeX-search-files-type-alist' for an
example.")

(defun TeX-search-files-by-type (filetype &optional scope nodir strip extra-dirs)
  "Return a list of files in TeX's search path with type FILETYPE.
FILETYPE is a symbol used to choose the search paths and
extensions.  See `TeX-search-files-type-alist' for supported
symbols.

The optional argument SCOPE sets the scope for the search.
Besides nil the symbols `local' and `global' are accepted.
`local' means to search in the current directory only, `global'
in the global directories only and nil in both.

If optional argument NODIR is non-nil, remove directory part.

If optional argument STRIP is non-nil, remove file extension.

The optional argument EXTRA-DIRS is passed to `TeX-search-files-kpathsea'."
  (let* ((gc-cons-threshold 10000000)
         (spec (assq filetype TeX-search-files-type-alist))
         (kpse-var (nth 1 spec))
         (rawdirs (nth 2 spec))
         (exts (nth 3 spec))
         expdirs dirs local-files)
    (setq exts (if (symbolp exts) (eval exts t) exts))
    (or (TeX-search-files-kpathsea kpse-var exts scope nodir strip extra-dirs)
        (progn
          (unless (eq scope 'global)
            (setq local-files
                  (let ((TeX-file-recurse nil))
                    (TeX-search-files '("./") exts nodir strip))))
          (if (eq scope 'local)
              local-files
            (if (null TeX-tree-roots)
                (error "No TeX trees available; configure `TeX-tree-roots'")
              ;; Expand variables.
              (setq expdirs
                    ;; Don't use `delete-dups' instead of
                    ;; `TeX-delete-duplicate-strings' here.
                    ;; Otherwise, when the last element of `rawdirs'
                    ;; is a variable, its value might be truncated as
                    ;; side effect.
                    (TeX-delete-duplicate-strings
                     (apply #'append
                            (mapcar (lambda (rawdir)
                                      (if (symbolp rawdir)
                                          (symbol-value rawdir)
                                        (list rawdir)))
                                    rawdirs))))
              ;; Assumption: Either all paths are absolute or all are relative.
              (if (file-name-absolute-p (car expdirs))
                  (setq dirs expdirs)
                ;; Append relative TDS subdirs to all TeX tree roots.
                (dolist (root TeX-tree-roots)
                  (dolist (dir expdirs)
                    (let ((dir (expand-file-name dir root)))
                      (unless (member dir dirs)
                        (setq dirs (append dirs (list dir)))))))))
            (append local-files (TeX-search-files dirs exts nodir strip)))))))

;;; Narrowing

(defun TeX-narrow-to-group ()
  "Make text outside current group invisible."
  (interactive)
  (save-excursion
    (widen)
    (let ((opoint (point))
          beg end)
      (if (null (search-backward "{" nil t))
          (message "Nothing to be narrowed here.")
        (setq beg (point))
        (forward-sexp)
        (setq end (point))
        (if (< end opoint)
            (message "Nothing to be narrowed here.")
          (narrow-to-region beg end))))))
(put 'TeX-narrow-to-group 'disabled t)

;;; Utilities
;;
;; Some of these functions has little to do with TeX, but nonetheless we
;; should use the "TeX-" prefix to avoid name clashes.

(defun TeX-listify (elt)
  "Return a newly created list with element ELT.
If ELT already is a list, return ELT."
  (if (listp elt) elt (list elt)))

(defun TeX-member (elt list how)
  "Return the member ELT in LIST.  Comparison done with HOW.
Return nil if ELT is not a member of LIST."
  (while (and list (not (funcall how elt (car list))))
    (setq list (cdr list)))
  (car-safe list))

(defun TeX-elt-of-list-member (elts list)
  "Return non-nil if an element of ELTS is a member of LIST."
  (catch 'found
    (dolist (elt elts)
      (when (member elt list)
        (throw 'found t)))))

;; Compatibility alias
(defun TeX-assoc (key list)
  (assoc-string key list t))
(make-obsolete 'TeX-assoc
               "use (assoc-string KEY LIST t) instead." "AUCTeX 13.0")

(if (>= emacs-major-version 28)
    (defalias 'TeX-always #'always)
  (defun TeX-always (&rest _arguments)
    "Ignore ARGUMENTS, do nothing and return t.
This function accepts any number of arguments in ARGUMENTS.
Also see `ignore'.

This is a compatibility function for Emacs versions prior to v.28."
    t))

;; COMPATIBILITY for Emacs<30
(if (fboundp 'derived-mode-add-parents)
    (defalias 'TeX-derived-mode-add-parents #'derived-mode-add-parents)
  ;; Adapted copy of `derived-mode-add-parents'.
  (defun TeX-derived-mode-add-parents (mode extra-parents)
    "Add EXTRA-PARENTS to the parents of MODE.
Declares the parents of MODE to be its main parent (as defined
in `define-derived-mode') plus EXTRA-PARENTS."
    (put mode 'derived-mode-extra-parents extra-parents)))

(defun TeX-match-buffer (n)
  "Return the substring corresponding to the N'th match.
See `match-data' for details."
  (if (match-beginning n)
      (buffer-substring-no-properties (match-beginning n) (match-end n))
    ""))

(defun TeX-looking-at-backward (regexp &optional limit)
  "Return non-nil if the text before point matches REGEXP.
Optional second argument LIMIT gives a max number of characters
to look backward for."
  (let ((pos (point)))
    (save-excursion
      (and (re-search-backward regexp
                               (if limit (max (point-min) (- (point) limit)))
                               t)
           (eq (match-end 0) pos)))))

(defun TeX-current-line ()
  "The current line number."
  (format "%d" (1+ (TeX-current-offset))))

(defun TeX-current-file-name-master-relative ()
  "Return current filename, relative to master directory."
  (file-relative-name
   (TeX-buffer-file-name)
   (TeX-master-directory)))

(defun TeX-near-bobp ()
  "Return t if there's nothing but whitespace between (bob) and (point)."
  (save-excursion
    (skip-chars-backward " \t\n")
    (bobp)))

(defun TeX-add-to-alist (alist-var new-alist)
  "Add NEW-ALIST to the ALIST-VAR.
If an element with the same key as the key of an element of
NEW-ALIST is already present in ALIST-VAR, add the new values to
it; if a matching element is not already present, append the new
element to ALIST-VAR."
  ;; Loop over all elements of NEW-ALIST.
  (while new-alist
    (let* ((new-element (car new-alist))
           ;; Get the element of ALIST-VAR with the same key of the current
           ;; element of NEW-ALIST, if any.
           (old-element (assoc (car new-element) (symbol-value alist-var))))
      (if old-element
          (progn
            (set alist-var (delete old-element (symbol-value alist-var)))
            ;; Append to `old-element' the values of the current element of
            ;; NEW-ALIST.
            (mapc (lambda (elt)
                    (unless (member elt (cdr old-element))
                      (setq old-element (append old-element (list elt)))))
                  (cdr new-element))
            (add-to-list alist-var old-element t))
        (add-to-list alist-var new-element t)))
    ;; Next element of NEW-ALIST.
    (setq new-alist (cdr new-alist))))

;;; Syntax Table

(progn ; Define TeX-mode-syntax-table.
  (modify-syntax-entry (string-to-char TeX-esc)
                       "\\" TeX-mode-syntax-table)
  (modify-syntax-entry ?\f ">"  TeX-mode-syntax-table)
  (modify-syntax-entry ?\n ">"  TeX-mode-syntax-table)
  (modify-syntax-entry (string-to-char TeX-grop)
                       (concat "(" TeX-grcl)
                       TeX-mode-syntax-table)
  (modify-syntax-entry (string-to-char TeX-grcl)
                       (concat ")" TeX-grop)
                       TeX-mode-syntax-table)
  (modify-syntax-entry ?%  "<"  TeX-mode-syntax-table)
  (modify-syntax-entry ?\" "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?&  "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?_  "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?@  "_"  TeX-mode-syntax-table)
  (modify-syntax-entry ?~  "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?$  "$"  TeX-mode-syntax-table)
  (modify-syntax-entry ?'  "w"  TeX-mode-syntax-table)
  (modify-syntax-entry ?«  "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?»  "."  TeX-mode-syntax-table)
  (modify-syntax-entry ?|  "."  TeX-mode-syntax-table))

;;; Menu Support

(defvar TeX-command-current #'TeX-command-master
  "Specify whether to run command on master, buffer or region.")
;; Function used to run external command.

(defun TeX-command-select-master ()
  "Determine that the next command will be on the master file."
  (interactive)
  (message "Next command will be on the master file.")
  (setq TeX-command-current #'TeX-command-master))

(defun TeX-command-select-buffer ()
  "Determine that the next command will be on the buffer."
  (interactive)
  (message "Next command will be on the buffer")
  (setq TeX-command-current #'TeX-command-buffer))

(defun TeX-command-select-region ()
  "Determine that the next command will be on the region."
  (interactive)
  (message "Next command will be on the region")
  (setq TeX-command-current #'TeX-command-region))

(defvar TeX-command-force nil)
;; If non-nil, TeX-command-query will return the value of this
;; variable instead of quering the user.

(defun TeX-command-menu (name)
  "Execute `TeX-command-list' NAME from a menu."
  (let ((TeX-command-force name))
    (funcall TeX-command-current)))

(defun TeX-command-menu-print (printer command name)
  "Print on PRINTER using method COMMAND to run NAME."
  (let ((TeX-printer-default (unless (string= printer "Other") printer))
        (TeX-printer-list (and (string= printer "Other") TeX-printer-list))
        (TeX-print-command command)
        (TeX-queue-command command))
    (TeX-command-menu name)))

(defun TeX-command-menu-printer-entry (entry lookup command name)
  "Return `TeX-printer-list' ENTRY as a menu item."
  (vector (nth 0 entry)
          (list 'TeX-command-menu-print
                (nth 0 entry)
                (or (nth lookup entry) command)
                name)))

(defun TeX-command-menu-entry (entry)
  "Return `TeX-command-list' ENTRY as a menu item."
  (let ((name (car entry)))
    (cond ((and (string-equal name TeX-command-Print)
                TeX-printer-list)
           (cons TeX-command-Print
                 (mapcar (lambda (entry)
                           (TeX-command-menu-printer-entry
                            entry 1 TeX-print-command name))
                         (append TeX-printer-list '(("Other"))))))
          ((and (string-equal name TeX-command-Queue)
                TeX-printer-list)
           (cons TeX-command-Queue
                 (mapcar (lambda (entry)
                           (TeX-command-menu-printer-entry
                            entry 2 TeX-queue-command name))
                         (append TeX-printer-list '(("Other"))))))
          (t
           (vconcat `(,name (TeX-command-menu ,name))
                    (nthcdr 5 entry))))))

(defconst TeX-command-menu-name "Command"
  "Name to be displayed for the command menu in all modes defined by AUCTeX.")

;;; Keymap

(defcustom TeX-electric-escape nil
  "If non-nil, ``\\'' will offer on-the-fly completion.
In Texinfo-mode, ``@'' will do that job instead and ``\\'' is not
affected.  See `TeX-electric-macro' for detail."
  :group 'TeX-macro
  :type 'boolean)

(defcustom TeX-electric-sub-and-superscript nil
  "If non-nil, insert braces after typing `^' and `_' in math mode."
  :group 'TeX-macro
  :type 'boolean)

(defcustom TeX-newline-function #'newline
  "Function to be called upon pressing `RET'."
  :group 'TeX-indentation
  :type '(choice (const newline)
                 (const newline-and-indent)
                 (const reindent-then-newline-and-indent)
                 (sexp :tag "Other")))

(defun TeX--put-electric-delete-selection (symbol electricp)
  "Set appropriate `delete-selection' property for electric functions.

When the function bound to SYMBOL has «electric» behaviour, as
determined by predicate ELECTRICP, `delete-selection' is set to
nil.  In the other case, `delete-selection' is delegated to that
of the `self-insert-command'.

Note, that it is assumed that SYMBOL uses `self-insert-command'
to insert symbols on its non-electric path.

The backstory.

When a function bound to SYMBOL has optional «electric»
behaviour, it might interfere with other «electric» modes,
e.g. `electric-pair-mode', `smartparens-mode'; see bug#47936.

As a way to «override» those modes, we use raw `insert' instead
of `self-insert-command'.  That prevents those electric modes
from running their hooks tied to `self-insert-command'.

However, when /our/ electric behaviour is disabled (ELECTRICP
returns nil), we want other electric modes to operate freely.
That means, on the non-electric path, we should use
`self-insert-command' instead of `insert'.

Now, there arises an issue of `delete-selection'.  The electric
path usually doesn't want to delete selection, it wants to
operate some electricity on it; see bug#36385, bug#23177.  Now,
we could think that `delete-selection' for the non-electric path
should be t.  That would disable other electric modes from
working, as they also need to operate on selection.  The decision
is to inherit `delete-selection' from `self-insert-command',
which queries hooks from other electric modes to determine
whether deletion is necessary.

This function implements the idea from the last paragraph."
  (put symbol 'delete-selection
       (lambda ()
         (unless (funcall electricp)
           (get #'self-insert-command 'delete-selection)))))

(defun TeX-insert-backslash (arg)
  "Either insert typed key ARG times or call `TeX-electric-macro'.
`TeX-electric-macro' will be called if `TeX-electric-escape' is non-nil."
  (interactive "*p")
  (if TeX-electric-escape
      (TeX-electric-macro)
    (self-insert-command arg)))

(TeX--put-electric-delete-selection
 #'TeX-insert-backslash (lambda () TeX-electric-escape))

(defun TeX-insert-sub-or-superscript (arg)
  "Insert typed key ARG times and possibly a pair of braces.
Brace insertion is only done if point is in a math construct and
`TeX-electric-sub-and-superscript' has a non-nil value."
  (interactive "*p")
  (self-insert-command arg)
  (when (and TeX-electric-sub-and-superscript (texmathp))
    (insert (concat TeX-grop TeX-grcl))
    (backward-char)))

(defun TeX-newline ()
  "Call the function specified by the variable `TeX-newline-function'."
  (interactive) (call-interactively TeX-newline-function))

(put #'TeX-newline 'delete-selection t)

(progn
  (let ((map TeX-mode-map))
    ;; Standard
    ;; (define-key map "\177"     #'backward-delete-char-untabify)
    (define-key map "\C-c}"    #'up-list)
    (define-key map "\C-c#"    #'TeX-normal-mode)
    (define-key map "\C-c\C-n" #'TeX-normal-mode)
    (define-key map "\C-c?"    #'TeX-documentation-texdoc)
    (define-key map "\C-c\C-i" #'TeX-goto-info-page)
    (define-key map "\r"       #'TeX-newline)

    ;; From tex.el
    (define-key map "\""       #'TeX-insert-quote)
    (define-key map "$"        #'TeX-insert-dollar)
    ;; Removed because LaTeX 2e have a better solution to italic correction.
    ;; (define-key map "."        #'TeX-insert-punctuation)
    ;; (define-key map ","        #'TeX-insert-punctuation)
    (define-key map "\C-c{"    #'TeX-insert-braces)
    (define-key map "\C-c\C-f" #'TeX-font)
    (define-key map "\C-c\C-m" #'TeX-insert-macro)
    (define-key map "\\"       #'TeX-insert-backslash)
    (define-key map "^"        #'TeX-insert-sub-or-superscript)
    (define-key map "_"        #'TeX-insert-sub-or-superscript)
    (define-key map "\e\t"     #'TeX-complete-symbol) ;*** Emacs 19 way

    (define-key map "\C-c'"    #'TeX-comment-or-uncomment-paragraph) ;*** Old way
    (define-key map "\C-c:"    #'comment-or-uncomment-region) ;*** Old way
    (define-key map "\C-c\""   #'TeX-uncomment) ;*** Old way

    (define-key map "\C-c;"    #'comment-or-uncomment-region)
    (define-key map "\C-c%"    #'TeX-comment-or-uncomment-paragraph)

    (define-key map "\C-c\C-t\C-p"   #'TeX-PDF-mode)
    (define-key map "\C-c\C-t\C-i"   #'TeX-interactive-mode)
    (define-key map "\C-c\C-t\C-s"   #'TeX-source-correlate-mode)
    (define-key map "\C-c\C-t\C-r"   #'TeX-pin-region)
    (define-key map "\C-c\C-w"       #'TeX-toggle-debug-bad-boxes); to be removed
    (define-key map "\C-c\C-t\C-b"   #'TeX-toggle-debug-bad-boxes)
    (define-key map "\C-c\C-t\C-w"   #'TeX-toggle-debug-warnings)
    (define-key map "\C-c\C-t\C-x"   #'TeX-toggle-suppress-ignored-warnings)
    (define-key map "\C-c\C-v" #'TeX-view)
    (define-key map "\C-c\C-d" #'TeX-save-document)
    (define-key map "\C-c\C-r" #'TeX-command-region)
    (define-key map "\C-c\C-b" #'TeX-command-buffer)
    (define-key map "\C-c\C-c" #'TeX-command-master)
    (define-key map "\C-c\C-a" #'TeX-command-run-all)
    (define-key map "\C-c\C-k" #'TeX-kill-job)
    (define-key map "\C-c\C-l" #'TeX-recenter-output-buffer)
    (define-key map "\C-c^" #'TeX-home-buffer)
    (define-key map "\C-c`"    #'TeX-next-error)
    ;; Remap bindings of `next-error'
    (define-key map [remap next-error] #'TeX-next-error)
    ;; Remap bindings of `previous-error'
    (define-key map [remap previous-error] #'TeX-previous-error)
    ;; From tex-fold.el
    (define-key map "\C-c\C-o\C-f" #'TeX-fold-mode)

    ;; Multifile
    (define-key map "\C-c_" #'TeX-master-file-ask)  ;*** temporary

    (define-key map "\C-xng" #'TeX-narrow-to-group)

    ;; Hide "Text" menu entry inherited from text mode.
    (define-key map [menu-bar text] #'undefined)))

(defun TeX-mode-specific-command-menu (mode)
  "Return a Command menu specific to the major MODE."
  (list TeX-command-menu-name
        :filter (lambda (&rest _ignored)
                  (TeX-mode-specific-command-menu-entries mode))
        "Bug."))

(defun TeX-mode-specific-command-menu-entries (mode)
  "Return the entries for a Command menu specific to the major MODE."
  (append
   `("Command on"
     [ "Master File" TeX-command-select-master
       :keys "C-c C-c" :style radio
       :selected (eq TeX-command-current #'TeX-command-master)
       :help "Commands in this menu work on the Master File"]
     [ "Buffer" TeX-command-select-buffer
       :keys "C-c C-b" :style radio
       :selected (eq TeX-command-current #'TeX-command-buffer)
       :help "Commands in this menu work on the current buffer"]
     [ "Region" TeX-command-select-region
       :keys "C-c C-r" :style radio
       :selected (eq TeX-command-current #'TeX-command-region)
       :help "Commands in this menu work on the region"]
     [ "Fix the Region" TeX-pin-region
       :active (or (if prefix-arg
                       (<= (prefix-numeric-value prefix-arg) 0)
                     (and (boundp 'TeX-command-region-begin)
                          (markerp TeX-command-region-begin)))
                   (TeX-active-mark))
       ;;:visible (eq TeX-command-current 'TeX-command-region)
       :style toggle
       :selected (and (boundp 'TeX-command-region-begin)
                      (markerp TeX-command-region-begin))
       :help "Fix the region for \"Command on Region\""]
     "-"
     ["Recenter Output Buffer" TeX-recenter-output-buffer
      :help "Show the output of current TeX process"]
     ["Kill Job" TeX-kill-job
      :help "Kill the current TeX process"]
     ["Next Error" TeX-next-error
      :help "Jump to the next error of the last TeX run"]
     ["Previous Error" TeX-previous-error
      :help "Jump to the previous error of the last TeX run"
      :visible TeX-parse-all-errors]
     ["Error Overview" TeX-error-overview
      :help "Open an overview of errors occured in the last TeX run"
      :visible TeX-parse-all-errors]
     ["Quick View" TeX-view
      :help "Start a viewer without prompting"]
     "-"
     ("TeXing Options"
      ,@(mapcar (lambda (x)
                  (let ((symbol (car x)) (name (nth 1 x)))
                    `[ ,(format "Use %s engine" name) (TeX-engine-set ',symbol)
                       :style radio :selected (eq TeX-engine ',symbol)
                       :help ,(format "Use %s engine for compiling" name) ]))
                (TeX-engine-alist))
      "-"
      [ "Generate PDF" TeX-PDF-mode
        :style toggle :selected TeX-PDF-mode
        :active (not (eq TeX-engine 'omega))
        :help "Use PDFTeX to generate PDF instead of DVI"]
      ( "PDF from DVI"
        :visible TeX-PDF-mode
        :help "Compile to DVI with (La)TeX and convert to PDF"
        [ "Compile directly to PDF"
          (lambda () (interactive) (setq TeX-PDF-from-DVI nil))
          :style radio :selected (null (TeX-PDF-from-DVI))
          :help "Compile directly to PDF without intermediate conversions"]
        [ "dvips + ps2pdf"
          (lambda () (interactive) (setq TeX-PDF-from-DVI "Dvips"))
          :style radio :selected (equal (TeX-PDF-from-DVI) "Dvips")
          :help "Convert DVI to PDF with dvips + ps2pdf sequence"]
        [ "dvipdfmx"
          (lambda () (interactive) (setq TeX-PDF-from-DVI "Dvipdfmx"))
          :style radio :selected (equal (TeX-PDF-from-DVI) "Dvipdfmx")
          :help "Convert DVI to PDF with dvipdfmx"])
      [ "Run Interactively" TeX-interactive-mode
        :style toggle :selected TeX-interactive-mode :keys "C-c C-t C-i"
        :help "Stop on errors in a TeX run"]
      [ "Correlate I/O" TeX-source-correlate-mode
        :style toggle :selected TeX-source-correlate-mode
        :help "Enable forward and inverse search in the previewer"]
      ["Debug Bad Boxes" TeX-toggle-debug-bad-boxes
       :style toggle :selected TeX-debug-bad-boxes :keys "C-c C-t C-b"
       :help "Make \"Next Error\" show overfull and underfull boxes"]
      ["Debug Warnings" TeX-toggle-debug-warnings
       :style toggle :selected TeX-debug-warnings
       :help "Make \"Next Error\" show warnings"])
     ["Compile and view" TeX-command-run-all
      :help "Compile the document until it is ready and open the viewer"])
   (delq nil
         (mapcar #'TeX-command-menu-entry
                 (TeX-mode-specific-command-list mode)))))

(defun TeX-mode-specific-command-list (mode)
  "Return the list of commands available in the given MODE."
  (let ((full-list TeX-command-list)
        out-list
        entry fourth-element
        former-mode)
    (while (setq entry (pop full-list))
      (setq fourth-element (nth 4 entry))
      ;; `(nth 4 entry)' may be either an atom in case of which the
      ;; entry should be present in any mode or a list of major modes.
      (if (or (atom fourth-element)
              (memq mode fourth-element)
              ;; Compatibility for former mode names.  The user can
              ;; have customized `TeX-command-list' with former mode
              ;; names listed in `(nth 4 entry)'.
              (and (setq former-mode
                         (car (rassq mode TeX-mode-comparison-alist)))
                   (memq former-mode fourth-element)))
          (push entry out-list)))
    (nreverse out-list)))

(defvar TeX-fold-menu
  '("Show/Hide"
    ["Fold Mode" TeX-fold-mode
     :style toggle
     :selected (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Toggle folding mode"]
    "-"
    ["Hide All in Current Buffer" TeX-fold-buffer
     :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Hide all configured TeX constructs in the current buffer"]
    ["Hide All in Current Region" TeX-fold-region
     :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Hide all configured TeX constructs in the marked region"]
    ["Hide All in Current Paragraph" TeX-fold-paragraph
     :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Hide all configured TeX constructs in the paragraph containing point"]
    ["Hide All in Current Section" TeX-fold-section
     :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Hide all configured TeX constructs in the section containing point"]
    ["Hide Current Macro" TeX-fold-macro
     :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Hide the macro containing point"]
    ["Hide Current Environment" TeX-fold-env
     :visible (not (eq major-mode 'plain-TeX-mode))
     :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Hide the environment containing point"]
    ["Hide Current Comment" TeX-fold-comment
     :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Hide the comment containing point"]
    "-"
    ["Show All in Current Buffer" TeX-fold-clearout-buffer
     :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Permanently show all folded content again"]
    ["Show All in Current Region" TeX-fold-clearout-region
     :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Permanently show all folded content in marked region"]
    ["Show All in Current Paragraph" TeX-fold-clearout-paragraph
     :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Permanently show all folded content in paragraph containing point"]
    ["Show All in Current Section" TeX-fold-clearout-section
     :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Permanently show all folded content in section containing point"]
    ["Show Current Item" TeX-fold-clearout-item
     :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Permanently show the item containing point"]
    "-"
    ["Hide or Show Current Item" TeX-fold-dwim
     :active (and (boundp 'TeX-fold-mode) TeX-fold-mode)
     :help "Hide or show the item containing point"])
  "Menu definition for commands from tex-fold.el.")

(defvar TeX-customization-menu nil)

(defvar TeX-common-menu-entries
  `(("Multifile/Parsing"
     ["Switch to Master File" TeX-home-buffer
      :help "Switch to buffer of Master File, or buffer of last TeX command"]
     ["Save Document" TeX-save-document
      :help "Save all buffers associated with the current Master File"]
     ["Set Master File" TeX-master-file-ask
      :active (not (TeX-local-master-p))
      :help "Set the main file to run TeX commands on"]
     ["Reset Buffer" TeX-normal-mode
      :help "Save and reparse the current buffer for style information"]
     ["Reset AUCTeX" (TeX-normal-mode t) :keys "C-u C-c C-n"
      :help "Reset buffer and reload AUCTeX style files"])
    ["Find Documentation..." TeX-documentation-texdoc
     :help "Get help on commands, packages, or TeX-related topics in general"]
    ["Read the AUCTeX Manual" TeX-goto-info-page
     :help "Everything worth reading"]
    ("Customize AUCTeX"
     ["Browse Options"
      (customize-group 'AUCTeX)
      :help "Open the customization buffer for AUCTeX"]
     ["Extend this Menu"
      (progn
        (easy-menu-add-item
         nil
         ;; Ugly hack because docTeX mode uses the LaTeX menu and
         ;; ConTeXt mode uses "ConTeXt-en" or "ConTeXt-nl" for the
         ;; value of `TeX-base-mode-name'.
         ;; XXX: Perhaps we should have a new variable holding the
         ;; mode-specific menu title?
         (list
          (cond
           ((eq major-mode 'docTeX-mode) "LaTeX")
           ((eq major-mode 'ConTeXt-mode) "ConTeXt")
           (t TeX-base-mode-name)))
         (or TeX-customization-menu
             (setq TeX-customization-menu
                   (customize-menu-create 'AUCTeX "Customize AUCTeX")))))
      :help "Make this menu a full-blown customization menu"])
    ["Report AUCTeX Bug" TeX-submit-bug-report
     :help ,(format "Problems with AUCTeX %s? Mail us!"
                    AUCTeX-version)]))

;;; Verbatim constructs

(defvar-local TeX-verbatim-p-function nil
  "Mode-specific function to be called by `TeX-verbatim-p'.
It must accept optional argument POS for position.")

;; XXX: We only have an implementation for LaTeX mode at the moment (Oct 2009).
(defun TeX-verbatim-p (&optional pos)
  "Return non-nil if position POS is in a verbatim-like construct.
A mode-specific implementation is required.  If it is not
available, the function always returns nil."
  (when TeX-verbatim-p-function
    (funcall TeX-verbatim-p-function pos)))


;;; Comments

(defvar-local TeX-comment-start-regexp "%"
  "Regular expression matching a comment starter.
Unlike the variable `comment-start-skip' it should not match any
whitespace after the comment starter or any character before it.")

(defun TeX-uncomment ()
  "Delete comment characters from the beginning of each line in a comment."
  (interactive)
  (save-excursion
    ;; Find first comment line
    (beginning-of-line)
    (while (and (looking-at (concat "^[ \t]*" TeX-comment-start-regexp))
                (not (bobp)))
      (forward-line -1))
    (let ((beg (point)))
      (forward-line 1)
      ;; Find last comment line
      (while (and (looking-at (concat "^[ \t]*" TeX-comment-start-regexp))
                  (not (eobp)))
        (forward-line 1))
      ;; Uncomment region
      (uncomment-region beg (point)))))

(defun TeX-comment-or-uncomment-paragraph ()
  "Comment or uncomment current paragraph."
  (interactive)
  (if (TeX-in-commented-line)
      (TeX-uncomment)
    (save-excursion
      (beginning-of-line)
      ;; Don't do anything if we are in an empty line.  If this line
      ;; is followed by a lot of commented lines, this shall prevent
      ;; that mark-paragraph skips over these lines and marks a
      ;; paragraph outside the visible window which might get
      ;; commented without the user noticing.
      (unless (looking-at "^[ \t]*$")
        (mark-paragraph)
        (comment-region (point) (mark))))))

(defun TeX-in-comment ()
  "Return non-nil if point is in a comment."
  (if (or (bolp)
          (null comment-start-skip)
          (eq (preceding-char) ?\r))
      nil
    (save-excursion
      (save-match-data
        (let ((pos (point)))
          (beginning-of-line)
          (and (or (looking-at comment-start-skip)
                   (re-search-forward comment-start-skip pos t))
               (not (TeX-verbatim-p))))))))

(defun TeX-in-commented-line ()
  "Return non-nil if point is in a line consisting only of a comment.
The comment can be preceded by whitespace.  This means that
`TeX-in-commented-line' is more general than `TeX-in-line-comment'
which will not match commented lines with leading whitespace.  But
`TeX-in-commented-line' will match commented lines without leading
whitespace as well."
  (save-excursion
    (forward-line 0)
    (skip-chars-forward " \t")
    (string= (buffer-substring-no-properties
              (point) (min (point-max) (+ (point) (length comment-start))))
             comment-start)))

(defun TeX-in-line-comment ()
  "Return non-nil if point is in a line comment.
A line comment is a comment starting in column one, that is, there is
no whitespace before the comment sign."
  (save-excursion
    (forward-line 0)
    (string= (buffer-substring-no-properties
              (point) (min (point-max) (+ (point) (length comment-start))))
             comment-start)))

(defun TeX-comment-prefix ()
  "Return the comment prefix of the current line.
If there are no comment starters after potential whitespace at
the beginning of the line, return nil."
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (when (looking-at (concat "\\([ \t]*" TeX-comment-start-regexp "+\\)+"))
        (match-string 0)))))

(defun TeX-forward-comment-skip (&optional count limit)
  "Move forward to the next comment skip.
This may be a switch between commented and not commented adjacent
lines or between lines with different comment prefixes.  With
argument COUNT do it COUNT times.  If argument LIMIT is given, do
not move point further than this value."
  (unless count (setq count 1))
  ;; A value of 0 is nonsense.
  (when (= count 0) (setq count 1))
  (unless limit (setq limit (point-max)))
  (dotimes (_ (abs count))
    (if (< count 0)
        (forward-line -1)
      (beginning-of-line))
    (let ((prefix (when (looking-at (concat "\\([ \t]*"
                                            TeX-comment-start-regexp "+\\)+"))
                    (buffer-substring (+ (line-beginning-position)
                                         (current-indentation))
                                      (match-end 0)))))
      (while (save-excursion
               (and (if (> count 0)
                        (<= (point) limit)
                      (>= (point) limit))
                    (zerop (if (> count 0)
                               (forward-line 1)
                             (forward-line -1)))
                    (if prefix
                        (if (looking-at (concat "\\([ \t]*"
                                                TeX-comment-start-regexp
                                                "+\\)+"))
                            ;; If the preceding line is a commented line
                            ;; as well, check if the prefixes are
                            ;; identical.
                            (string= prefix
                                     (buffer-substring
                                      (+ (line-beginning-position)
                                         (current-indentation))
                                      (match-end 0)))
                          nil)
                      (not (looking-at (concat "[ \t]*"
                                               TeX-comment-start-regexp))))))
        (if (> count 0)
            (forward-line 1)
          (forward-line -1)))
      (if (> count 0)
          (forward-line 1)))))

(defun TeX-backward-comment-skip (&optional count limit)
  "Move backward to the next comment skip.
This may be a switch between commented and not commented adjacent
lines or between lines with different comment prefixes.  With
argument COUNT do it COUNT times.  If argument LIMIT is given, do
not move point to a position less than this value."
  (unless count (setq count 1))
  (when (= count 0) (setq count 1))
  (unless limit (setq limit (point-min)))
  (TeX-forward-comment-skip (- count) limit))

(defun TeX-comment-forward (&optional n)
  "Skip forward over N comments.
Just like `forward-comment' but only for positive N
and can use regexps instead of syntax."
  (comment-normalize-vars)
  (comment-forward n))

(defun TeX-comment-padding-string ()
  "Return comment padding as a string.
The variable `comment-padding' can hold an integer or a string.
This function will return the appropriate string representation
regardless of its data type."
  (if (integerp comment-padding)
      (make-string comment-padding ? )
    comment-padding))


;;; Indentation

(defgroup TeX-indentation nil
  "Indentation of TeX buffers in AUCTeX."
  :group 'AUCTeX)

(defcustom TeX-brace-indent-level 2
  "The level of indentation produced by an open brace."
  :group 'TeX-indentation
  :type 'integer)

(defcustom TeX-indent-open-delimiters ""
  "Additional open delimiters to increase indentation.
Include \"[\" to indent inside square brackets.
See `TeX-brace-count-line' and `TeX-indent-close-delimiters'."
  :group  'TeX-indentation
  :type '(string :tag "Open delimiters"))

(defcustom TeX-indent-close-delimiters ""
  "Additional close delimiters to increase indentation.
Include \"]\" to indent inside square brackets.
See `TeX-brace-count-line' and `TeX-indent-open-delimiters'."
  :group  'TeX-indentation
  :type '(string :tag "Close delimiters"))

(defun TeX-comment-indent ()
  "Determine the indentation of a comment."
  (if (looking-at "%%%")
      (current-column)
    (skip-chars-backward " \t")
    (max (if (bolp) 0 (1+ (current-column)))
         comment-column)))

(defun TeX-brace-count-line ()
  "Count indent caused by open/closed braces.
In addition to \"{\" and \"}\", characters in
`TeX-indent-open-delimiters' and `TeX-indent-close-delimiters'
are also taken into account.  Ignore them when they are escaped
by \"\\\".  In comments, ignore \"{\" and \"}\" but don't ignore
additional characters."
  (save-excursion
    (let ((count 0) (limit (line-end-position)) char)
      (while (progn
               (skip-chars-forward
                (concat "^{}\\\\"
                        TeX-indent-open-delimiters
                        TeX-indent-close-delimiters)
                limit)
               (when (and (< (point) limit)
                          (not (and (memq (setq char (char-after))
                                          '(?\{ ?\} ?\\))
                                    (TeX-in-comment))))
                 (forward-char)
                 ;; We have to cater for verb-macros with braces and
                 ;; what the function `TeX-verbatim-p' returns dep. on
                 ;; the position of point:
                 ;;       \Verb{\ or { or } are not special}.
                 ;;     nil  <-^^-> t                  t <-^^-> nil
                 (cond ((memq char (append
                                    TeX-indent-open-delimiters
                                    '(?\{)))
                        ;; Point is one char after `{', if char before
                        ;; isn't inside a verb macro, then the brace
                        ;; is a real delimiter and we increase
                        ;; `count', otherwise not:
                        (if (TeX-verbatim-p (1- (point)))
                            t
                          (setq count (+ count TeX-brace-indent-level))))
                       ((memq char (append
                                    TeX-indent-close-delimiters
                                    '(?\})))
                        ;; Point if one char after `}', if not inside
                        ;; a verb macro, this is a real delimiter and
                        ;; we decrease `count', otherwise not:
                        (if (TeX-verbatim-p)
                            t
                          (setq count (- count TeX-brace-indent-level))))
                       ((eq char ?\\)
                        ;; Point is one char-after after `\', so check
                        ;; if the char before point is inside a verb
                        ;; macro:
                        (when (< (point) limit)
                          (unless (TeX-verbatim-p (1- (point)))
                            (forward-char))
                          t))))))
      count)))

;;; Navigation

(defvar TeX-search-syntax-table
  (let ((table (make-syntax-table (make-char-table 'syntax-table))))
    ;; Preset mode-independent syntax entries.  (Mode-dependent
    ;; entries are set in the function `TeX-search-syntax-table'.)
    ;; ?\", ?\( and ?\) explicitly get whitespace syntax because
    ;; Emacs 21.3 and XEmacs don't generate a completely empty syntax
    ;; table.
    (dolist (elt '((?\f . ">") (?\n . ">") (?\" . " ") (?\( . " ") (?\) . " ")))
      (modify-syntax-entry (car elt) (cdr elt) table))
    table)
  "Syntax table used for searching purposes.
It should be accessed through the function `TeX-search-syntax-table'.")

(defun TeX-search-syntax-table (&rest args)
  "Return a syntax table for searching purposes.
ARGS may be a list of characters.  For each of them the
respective predefined syntax is set.  Currently the parenthetical
characters ?{, ?}, ?[, ?], ?(, ?), ?<, and ?> are supported.
The syntax of each of these characters not specified will be
reset to \" \"."
  (let ((char-syntax-alist '((?\{ . "(}") (?\} . "){")
                             (?\[ . "(]") (?\] . ")[")
                             (?\( . "()") (?\) . ")(")
                             (?\< . "(>") (?\> . ")<"))))
    ;; Clean entries possibly set before.
    (modify-syntax-entry ?\\ " " TeX-search-syntax-table)
    (modify-syntax-entry ?@ " " TeX-search-syntax-table)
    (modify-syntax-entry ?\% " " TeX-search-syntax-table)
    ;; Preset mode-dependent syntax entries.  (Mode-independent entries
    ;; are set when the variable `TeX-search-syntax-table' is created.)
    (modify-syntax-entry (string-to-char TeX-esc) "\\" TeX-search-syntax-table)
    (unless (eq major-mode 'Texinfo-mode)
      (modify-syntax-entry ?\% "<" TeX-search-syntax-table))
    ;; Clean up the entries which can be specified as arguments.
    (dolist (elt char-syntax-alist)
      (modify-syntax-entry (car elt) " " TeX-search-syntax-table))
    ;; Now set what we got.
    (dolist (elt args)
      (unless (assoc elt char-syntax-alist) (error "Char not supported"))
      (modify-syntax-entry elt (cdr (assoc elt char-syntax-alist))
                           TeX-search-syntax-table))
    ;; Return the syntax table.
    TeX-search-syntax-table))

(defun TeX-find-balanced-brace (&optional count depth limit)
  "Return the position of a balanced brace in a TeX group.
The function scans forward COUNT parenthetical groupings.
Default is 1.  If COUNT is negative, it searches backwards.  With
optional DEPTH>=1, find that outer level.  If LIMIT is non-nil,
do not search further than this position in the buffer."
  (let ((count (if count
                   (if (= count 0) (error "COUNT has to be <> 0") count)
                 1))
        (depth (if depth
                   (if (< depth 1) (error "DEPTH has to be > 0") depth)
                 1)))
    (save-restriction
      (when limit
        (if (> count 0)
            (narrow-to-region (point-min) limit)
          (narrow-to-region limit (point-max))))
      (with-syntax-table (TeX-search-syntax-table ?\{ ?\})
        (condition-case nil
            (scan-lists (point) count depth)
          (error nil))))))

(defun TeX-find-closing-brace (&optional depth limit)
  "Return the position of the closing brace in a TeX group.
The function assumes that point is inside the group, that is, after
an opening brace.  With optional DEPTH>=1, find that outer level.
If LIMIT is non-nil, do not search further down than this
position in the buffer."
  (TeX-find-balanced-brace 1 depth limit))

(defun TeX-find-opening-brace (&optional depth limit)
  "Return the position of the opening brace in a TeX group.
The function assumes that point is inside the group, that is, before
a closing brace.  With optional DEPTH>=1, find that outer level.
If LIMIT is non-nil, do not search further up than this position
in the buffer."
  (TeX-find-balanced-brace -1 depth limit))

(defun TeX-find-macro-boundaries (&optional lower-bound)
  "Return a cons containing the start and end of a macro.
If LOWER-BOUND is given, do not search backward further than this
point in buffer.  Arguments enclosed in brackets or braces are
considered part of the macro."
  ;; FIXME: Pay attention to `texmathp-allow-detached-args' and
  ;; `reftex-allow-detached-macro-args'.
  ;; Should we handle cases like \"{o} and \\[3mm] (that is, a macro
  ;; whose name is a symbol and takes some arguments) as well?  Note
  ;; that amsmath package arranges the macro \\ so that white spaces
  ;; between \\ and [something] prevents the latter to be interpreted
  ;; as an optional argument.  mathtools package arranges some
  ;; environments including gathered similarly.
  (save-restriction
    (when lower-bound
      (narrow-to-region lower-bound (point-max)))
    (let ((orig-point (point))
          start-point)
      ;; Point is located directly at the start of a macro. (-!-\foo{bar})
      (when (and (eq (char-after) (aref TeX-esc 0))
                 (not (TeX-escaped-p)))
        (setq start-point (point)))
      ;; Point is located on a macro. (\fo-!-o{bar})
      (unless start-point
        (save-excursion
          (skip-chars-backward "A-Za-z@*")
          (when (and (eq (char-before) (aref TeX-esc 0))
                     (not (TeX-escaped-p (1- (point)))))
            (setq start-point (1- (point))))))
      ;; Point is located in the argument of a macro. (\foo{ba-!-r})
      (unless start-point
        (save-excursion
          (catch 'abort
            (let ((parse-sexp-ignore-comments t))
              (when (condition-case nil (progn (up-list) t) (error nil))
                (while (progn
                         (condition-case nil (backward-sexp)
                           (error (throw 'abort nil)))
                         (forward-comment -1)
                         (skip-chars-backward " \t")
                         (and (memq (char-before) '(?\] ?\}))
                              (not (TeX-escaped-p (1- (point)))))))
                (skip-chars-backward "A-Za-z@*")
                (when (and (eq (char-before) (aref TeX-esc 0))
                           (not (TeX-escaped-p (1- (point)))))
                  (setq start-point (1- (point)))))))))
      ;; Search forward for the end of the macro.
      (when start-point
        (save-excursion
          (goto-char (TeX-find-macro-end-helper start-point))
          (if (< orig-point (point))
              (cons start-point (point))
            nil))))))

(defun TeX-find-macro-end-helper (start)
  "Find the end of a macro given its START.
START is the position just before the starting token of the macro.
If the macro is followed by square brackets or curly braces,
those will be considered part of it."
  (save-excursion
    (save-match-data
      (catch 'found
        (goto-char (1+ start))
        (if (zerop (skip-chars-forward "A-Za-z@"))
            (forward-char)
          (skip-chars-forward "*"))
        (while (not (eobp))
          (cond
           ;; Skip over pairs of square brackets
           ((or (looking-at "[ \t]*\n?[ \t]*\\(\\[\\)") ; Be conservative: Consider
                                        ; only consecutive lines.
                (and (looking-at (concat "[ \t]*" TeX-comment-start-regexp))
                     (save-excursion
                       (forward-line 1)
                       (looking-at "[ \t]*\\(\\[\\)"))))
            (goto-char (match-beginning 1))
            ;; Imitate `font-latex-find-matching-close', motivated by
            ;; examples like \begin{enumerate}[a{]}].
            (let ((syntax (TeX-search-syntax-table ?\[ ?\]))
                  (parse-sexp-ignore-comments
                   (not (derived-mode-p 'docTeX-mode))))
              (modify-syntax-entry ?\{ "|" syntax)
              (modify-syntax-entry ?\} "|" syntax)
              (modify-syntax-entry ?\\ "/" syntax)
              (condition-case nil
                  (with-syntax-table syntax
                    (forward-sexp))
                (scan-error (throw 'found (point))))))
           ;; Skip over pairs of curly braces
           ((or (looking-at "[ \t]*\n?[ \t]*{") ; Be conservative: Consider
                                        ; only consecutive lines.
                (and (looking-at (concat "[ \t]*" TeX-comment-start-regexp))
                     (save-excursion
                       (forward-line 1)
                       (looking-at "[ \t]*{"))))
            (goto-char (match-end 0))
            (goto-char (or (TeX-find-closing-brace)
                           ;; If we cannot find a regular end, use the
                           ;; next whitespace.
                           (save-excursion (skip-chars-forward "^ \t\n")
                                           (point)))))
           (t
            (throw 'found (point)))))
        ;; Make sure that this function does not return nil, even
        ;; when the above `while' loop is totally skipped. (bug#35638)
        (throw 'found (point))))))

(defun TeX-find-macro-start (&optional limit)
  "Return the start of a macro.
If LIMIT is given, do not search backward further than this point
in buffer.  Arguments enclosed in brackets or braces are
considered part of the macro."
  (car (TeX-find-macro-boundaries limit)))

(defun TeX-find-macro-end ()
  "Return the end of a macro.
Arguments enclosed in brackets or braces are considered part of
the macro."
  (cdr (TeX-find-macro-boundaries)))

(defun TeX-search-forward-unescaped (string &optional bound noerror)
  "Search forward from point for unescaped STRING.
The optional argument BOUND limits the search to the respective
buffer position.
If NOERROR is non-nil, return nil if the search failed instead of
throwing an error.
A pattern is escaped, if it is preceded by an odd number of escape
characters."
  (TeX-search-unescaped string 'forward nil bound noerror))

(defun TeX-search-backward-unescaped (string &optional bound noerror)
  "Search backward from point for unescaped STRING.
The optional argument BOUND limits the search to the respective
buffer position.
If NOERROR is non-nil, return nil if the search failed instead of
throwing an error.
A pattern is escaped, if it is preceded by an odd number of escape
characters."
  (TeX-search-unescaped string 'backward nil bound noerror))

(defun TeX-re-search-forward-unescaped (regexp &optional bound noerror)
  "Search forward from point for unescaped regular expression REGEXP.
The optional argument BOUND limits the search to the respective
buffer position.
If NOERROR is non-nil, return nil if the search failed instead of
throwing an error.
A pattern is escaped, if it is preceded by an odd number of escape
characters."
  (TeX-search-unescaped regexp 'forward t bound noerror))

(defun TeX-search-unescaped (pattern
                             &optional direction regexp-flag bound noerror)
  "Search for unescaped PATTERN in a certain DIRECTION.
DIRECTION can be indicated by the symbols `forward' and `backward'.
If DIRECTION is omitted, a forward search is carried out.
If REGEXP-FLAG is non-nil, PATTERN may be a regular expression,
otherwise a string.
The optional argument BOUND limits the search to the respective
buffer position.
If NOERROR is non-nil, return nil if the search failed instead of
throwing an error.
A pattern is escaped, if it is preceded by an odd number of escape
characters."
  (let ((search-fun (if (eq direction 'backward)
                        (if regexp-flag #'re-search-backward #'search-backward)
                      (if regexp-flag #'re-search-forward #'search-forward))))
    (catch 'found
      (while (funcall search-fun pattern bound noerror)
        (when (not (TeX-escaped-p (match-beginning 0)))
          (throw 'found (point)))))))

(defun TeX-escaped-p (&optional pos)
  "Return t if the character at position POS is escaped.
If POS is omitted, examine the character at point.
A character is escaped if it is preceded by an odd number of
escape characters, such as \"\\\" in LaTeX."
  (save-excursion
    (when pos (goto-char pos))
    (not (zerop (mod (skip-chars-backward (regexp-quote TeX-esc)) 2)))))

(defun TeX-current-macro ()
  "Return the name of the macro containing point, nil if there is none."
  (let ((macro-start (TeX-find-macro-start)))
    (when macro-start
      (save-excursion
        (goto-char macro-start)
        (forward-char (length TeX-esc))
        (buffer-substring-no-properties
         (point) (progn (skip-chars-forward "@A-Za-z*") (point)))))))

(defvar-local TeX-search-forward-comment-start-function nil
  "Function to find the start of a comment.
The function should accept an optional argument for specifying
the limit of the search.  It should return the position just
before the comment if one is found and nil otherwise.  Point
should not be moved.")

(defun TeX-search-forward-comment-start (&optional limit)
  "Search forward for a comment start from current position till LIMIT.
If LIMIT is omitted, search till the end of the buffer.

The search relies on `TeX-comment-start-regexp' being set
correctly for the current mode.

Set `TeX-search-forward-comment-start-function' in order to
override the default implementation."
  (if TeX-search-forward-comment-start-function
      (funcall TeX-search-forward-comment-start-function limit)
    (setq limit (or limit (point-max)))
    (when (TeX-re-search-forward-unescaped TeX-comment-start-regexp limit t)
      (match-beginning 0))))

;;; Fonts

(defcustom TeX-font-list '((?\C-b "{\\bf " "}")
                           (?\C-c "{\\sc " "}")
                           (?\C-e "{\\em " "\\/}")
                           (?\C-i "{\\it " "\\/}")
                           (?\C-r "{\\rm " "}")
                           (?\C-s "{\\sl " "\\/}")
                           (?\C-t "{\\tt " "}")
                           (?\C-d "" "" t))
  "List of fonts used by `TeX-font'.

Each entry is a list.
The first element is the key to activate the font.
The second element is the string to insert before point, and the third
element is the string to insert after point.
If the fourth and fifth element are strings, they specify the prefix and
suffix to be used in math mode.
An optional fourth (or sixth) element means always replace if t."
  :group 'TeX-macro
  :type '(repeat
           (group
            :value (?\C-a "" "")
            (character :tag "Key")
            (string :tag "Prefix")
            (string :tag "Suffix")
            (option (group
                     :inline t
                     (string :tag "Math Prefix")
                     (string :tag "Math Suffix")))
            (option (sexp :format "Replace\n" :value t)))))

(defvar TeX-font-replace-function #'TeX-font-replace
  "Determines the function which is called when a font should be replaced.")

(defun TeX-describe-font-entry (entry)
  "A textual description of an ENTRY in `TeX-font-list'."
  (concat (format "%16s  " (key-description (char-to-string (nth 0 entry))))
          (if (or (eq t (nth 3 entry)) (eq t (nth 5 entry)))
              "-- delete font"
            (format "%14s %-3s %14s %-3s"
                    (nth 1 entry) (nth 2 entry)
                    (if (stringp (nth 3 entry)) (nth 3 entry) "")
                    (if (stringp (nth 4 entry)) (nth 4 entry) "")))))

(defun TeX-font (replace what)
  "Insert template for font change command.
If REPLACE is not nil, replace current font.  WHAT determines the font
to use, as specified by `TeX-font-list'."
  (interactive "*P\nc")
  (TeX-update-style)
  (let* ((entry (assoc what TeX-font-list))
         (in-math (texmathp))
         (before (nth 1 entry))
         (after (nth 2 entry)))
    (setq replace (or replace (eq t (nth 3 entry)) (eq t (nth 5 entry))))
    (if (and in-math (stringp (nth 3 entry)))
        (setq before (nth 3 entry)
              after (nth 4 entry)))
    (cond ((null entry)
           (let ((help (concat
                        "Font list:   "
                        "KEY        TEXTFONT           MATHFONT\n\n"
                        (mapconcat #'TeX-describe-font-entry
                                   TeX-font-list "\n"))))
             (with-output-to-temp-buffer "*Help*"
               (set-buffer "*Help*")
               (insert help))))
          (replace
           (funcall TeX-font-replace-function before after))
          ((TeX-active-mark)
           (save-excursion
             (cond ((> (mark) (point))
                    (insert before)
                    (goto-char (mark))
                    (insert after))
                   (t
                    (insert after)
                    (goto-char (mark))
                    (insert before)))))
          (t
           (insert before)
           (save-excursion
             (insert after))))))

(defun TeX-font-replace (start end)
  "Replace font specification around point with START and END.
For modes with font specifications like `{\\font text}'.
See also `TeX-font-replace-macro' and `TeX-font-replace-function'."
  (save-excursion
    (while (not (looking-at "{\\\\[a-zA-Z]+ "))
      (up-list -1))
    (forward-sexp)
    (save-excursion
      (replace-match start t t))
    (if (save-excursion
          (backward-char 3)
          (if (looking-at (regexp-quote "\\/}"))
              (progn
                (delete-char 3)
                nil)
            t))
        (delete-char -1))
    (insert end)))

(defun TeX-font-replace-macro (start end)
  "Replace font specification around point with START and END.
For modes with font specifications like `\\font{text}'.
See also `TeX-font-replace' and `TeX-font-replace-function'."
  (let ((font-list TeX-font-list)
        cmds strings regexp)
    (while font-list
      (setq strings (cdr (car font-list))
            font-list (cdr font-list))
      (and (stringp (car strings)) (null (string= (car strings) ""))
           (setq cmds (cons (car strings) cmds)))
      (setq strings (cdr (cdr strings)))
      (and (stringp (car strings)) (null (string= (car strings) ""))
           (setq cmds (cons (car strings) cmds))))
    (setq regexp (mapconcat #'regexp-quote cmds "\\|"))
    (save-excursion
      (catch 'done
        (while t
          (if (/= ?\\ (following-char))
              (skip-chars-backward "a-zA-Z "))
          (skip-chars-backward (regexp-quote TeX-esc))
          (if (looking-at regexp)
              (throw 'done t)
            (up-list -1))))
      ;; Use stripped syntax table in order to get stuff like "\emph{(}" right.
      (with-syntax-table (TeX-search-syntax-table ?\{ ?\})
        (forward-sexp 2))
      (save-excursion
        (replace-match start t t))
      (delete-char -1)
      (insert end))))

;;; Dollars
;;
;; Rewritten from scratch with use of `texmathp' by
;; Carsten Dominik <dominik@strw.leidenuniv.nl>

(defcustom TeX-math-toggle-off-input-method t
  "If non-nil, auto turn off some input methods when entering math mode.
See `TeX-math-input-method-off-regexp'."
  :group 'TeX-macro
  :type 'boolean)

(defcustom TeX-electric-math nil
  "Math mode delimiters inserted by `TeX-insert-dollar'.
If non-nil, `TeX-insert-dollar' will insert symbols for opening and
closing inline equation and put the point between them.  If there is an
active region, `TeX-insert-dollar' will put around it symbols for
opening and closing inline equation and keep the region active, with
point after closing symbol.  If you press `$' again, you can toggle
between inline equation, display equation, and no equation.

If nil, `TeX-insert-dollar' will simply insert \"$\" at point,
this is the default.

If non-nil, this variable is a cons cell whose CAR is the string
to insert before point, the CDR is the string to insert after
point.  You can choose between \"$...$\" and \"\\(...\\)\"."
  :group 'TeX-macro
  :type '(choice (const :tag "No electricity" nil)
                 (const :tag "$...$" ("$" . "$"))
                 (const :tag "\\(...\\)" ("\\(" . "\\)"))
                 (cons :tag "Other"
                       (string :tag "Insert before point")
                       (string :tag "Insert after point"))))

(defcustom TeX-refuse-unmatched-dollar nil
  "When non-nil, don't insert unmatched dollar sign.
That is, `TeX-insert-dollar' refuses to insert \"$\" when `texmathp'
tells that the current position is in math mode which didn't start with
dollar(s).  Doesn't have an effect when `TeX-electric-math' is non-nil.

When nil, `TeX-insert-dollar' assumes the user knows that the current
position is not in math mode actually and behaves in the same way as
non-math mode."
  :group 'TeX-macro
  :type 'boolean)

(defun TeX-insert-dollar-electric-region ()
  "Perform electric math delimiter insertion on a region.
See `TeX-electric-math'."
  (when (> (point) (mark))
    (exchange-point-and-mark))
  (cond
   ;; Strip \[...\] or $$...$$
   ((and (eq last-command #'TeX-insert-dollar)
         (or (re-search-forward "\\=\\\\\\[\\([^z-a]*\\)\\\\\\]" (mark) t)
             (re-search-forward "\\=\\$\\$\\([^z-a]*\\)\\$\\$" (mark) t)))
    (replace-match "\\1" t)
    (set-mark (match-beginning 0)))
   ;; $...$ to $$...$$ or \[...\] dep. on mode:
   ((and (eq last-command #'TeX-insert-dollar)
         (re-search-forward "\\=\\$\\([^z-a]*\\)\\$" (mark) t))
    (replace-match (if (derived-mode-p 'LaTeX-mode)
                       "\\\\[\\1\\\\]"
                     "$$\\1$$")
                   t)
    (set-mark (match-beginning 0)))
   ;; \(...\) to \[...\]
   ((and (eq last-command #'TeX-insert-dollar)
         (re-search-forward "\\=\\\\(\\([^z-a]*\\)\\\\)" (mark) t))
    (replace-match "\\\\[\\1\\\\]" t)
    (set-mark (match-beginning 0)))
   (t
    ;; We use `save-excursion' because point must be situated
    ;; before opening symbol.
    (save-excursion (insert (car TeX-electric-math)))
    (exchange-point-and-mark)
    (insert (cdr TeX-electric-math))))
  (TeX-activate-region))

(defun TeX-insert-dollar-electric ()
  "Perform electric math symbol insertion.
See `TeX-electric-math'."
  (if (and (TeX-active-mark) (/= (point) (mark)))
      (TeX-insert-dollar-electric-region)
    (insert (car TeX-electric-math))
    (save-excursion (insert (cdr TeX-electric-math)))
    (TeX-math-input-method-off)))

(defun TeX--blink-matching-dollar ()
  "Blink the matching $, when appropriate.
Assume that `texmathp' has been called."
  (when (and blink-matching-paren
             (or (string= (car texmathp-why) "$")
                 (zerop (mod (save-excursion
                               (skip-chars-backward "$")) 2))))
    (save-excursion
      (goto-char (cdr texmathp-why))
      (if (pos-visible-in-window-p)
          (sit-for blink-matching-delay)
        (message "Matches %s"
                 (buffer-substring
                  (point) (line-end-position)))))))

(defun TeX-insert-dollar-action (arg)
  "Determine the action for `TeX-insert-dollar'.

Returns one of the following possible symbols that determine the
behavior of `TeX-insert-dollar':
 `just-insert'
     Just insert a literal $ ARG times.  For example, if you need
     exactly one $, you can use `C-1 $'.
 `electric'
     Behave according to `TeX-electric-math'.
 `begin-math'
     Insert a $ that starts the math mode.
 `end-math'
     Insert a $ that ends the math mode.
 `refuse'
     Refuse to insert a $ (or do anything else)."
  (cond
   ((or arg (TeX-escaped-p) (TeX-verbatim-p))
    'just-insert)
   (TeX-electric-math
    'electric)
   ((not (texmathp))
    'begin-math)
   ((member (car texmathp-why) '("$" "$$"))
    'end-math)
   ;; Math mode was not entered with dollar according to `texmathp'.
   (TeX-refuse-unmatched-dollar
    'refuse)
   ;; We assume that `texmathp' was wrong and behave as if not in
   ;; math mode. (bug#57626)
   ('begin-math)))

(defun TeX-insert-dollar (arg)
  "Insert dollar sign.

See `TeX-insert-dollar-action' for more information on the
behavior, as well as the role of ARG.

Show matching dollar sign if this dollar sign ends the TeX math
mode and `blink-matching-paren' is non-nil."
  (interactive "*P")
  (pcase (TeX-insert-dollar-action arg)
    ('just-insert
     (self-insert-command (prefix-numeric-value arg)))
    ('begin-math
     (self-insert-command (prefix-numeric-value arg))
     (TeX-math-input-method-off))
    ('end-math                        ; Assume texmathp's been called.
     (self-insert-command (prefix-numeric-value arg))
     (TeX--blink-matching-dollar))
    ('refuse
     (message "Math mode started with `%s' cannot be closed with dollar"
              (car texmathp-why)))
    ('electric
     (TeX-insert-dollar-electric))
    (action (error "Unknown `TeX-insert-dollar' action: `%s'" action))))

(put #'TeX-insert-dollar 'delete-selection
     (lambda ()
       (pcase (TeX-insert-dollar-action current-prefix-arg)
         ('refuse nil)
         ('electric nil)
         (_else (get #'self-insert-command 'delete-selection)))))

(defcustom TeX-math-input-method-off-regexp
  (concat "^" (regexp-opt '("chinese" "japanese" "korean" "bulgarian" "russian") t))
  "Regexp matching input methods to be deactivated when entering math mode."
  :group 'TeX-misc
  :type 'regexp)

(defun TeX-math-input-method-off ()
  "Toggle off input method when entering math mode."
  (and TeX-math-toggle-off-input-method
       (texmathp)
       current-input-method
       (string-match TeX-math-input-method-off-regexp current-input-method)
       (deactivate-input-method)))

;;; Simple Commands

(defvar TeX-normal-mode-reset-list '(TeX-style-hook-list)
  "List of variables to reset with `\\[universal-argument] \\[TeX-normal-mode]'.
AUCTeX libraries and styles should add variables for reset to
this list.")

(defun TeX-normal-mode (&optional arg)
  "Remove all information about this buffer, and apply the style hooks again.
Save buffer first including style information.
With optional argument ARG, also reload the style hooks."
  (interactive "*P")
  (with-current-buffer
  ;; In case this is an indirect buffer:
      (or (buffer-base-buffer) (current-buffer))
    (if arg
        (dolist (var TeX-normal-mode-reset-list)
          (set var nil)))
    (let ((gc-cons-percentage 0.5))
      (let ((TeX-auto-save t))
        (if (buffer-modified-p)
            (save-buffer)
          (TeX-auto-write)))
      (normal-mode)
      ;; See also addition to `find-file-hook' in `TeX-mode'.
      (when (eq TeX-master 'shared) (TeX-master-file nil nil t))
      (TeX-update-style t))))

(defgroup TeX-quote nil
  "Quoting in AUCTeX."
  :group 'AUCTeX)

(defcustom TeX-open-quote "``"
  "String inserted by typing \\[TeX-insert-quote] to open a quotation."
  :group 'TeX-quote
  :type 'string)

(defcustom TeX-close-quote "''"
  "String inserted by typing \\[TeX-insert-quote] to close a quotation."
  :group 'TeX-quote
  :type 'string)

(defcustom TeX-quote-after-quote nil
  "Behaviour of \\[TeX-insert-quote].
Nil means standard behaviour; when non-nil, opening and closing
quotes are inserted only after \"."
  :group 'TeX-quote
  :type 'boolean)

(defcustom TeX-quote-language-alist nil
  "Alist for overriding the default language-specific quote insertion.
First element in each item is the name of the language as set by
the language style file as a string.  Second element is the
opening quotation mark.  Third element is the closing quotation
mark.  Opening and closing quotation marks can be specified
directly as strings or as functions returning a string.  Fourth
element is a boolean specifying insertion behavior, overriding
`TeX-quote-after-quote'.  See Info node `(auctex)European' for
valid languages."
  :group 'TeX-quote
  :link '(custom-manual "(auctex)European")
  :type '(repeat (group (choice
                         (const "czech")
                         (const "danish")
                         (const "dutch")
                         (const "german")
                         (const "ngerman")
                         (const "french") ;; not frenchb or francais
                         (const "italian")
                         (const "polish")
                         (const "portuguese")
                         (const "slovak")
                         (const "swedish")
                         (string :tag "Other Language"))
                        (choice :tag "Opening quotation mark" string function)
                        (choice :tag "Closing quotation mark" string function)
                        (boolean :tag "Insert plain quote first" :value t))))

(defvar-local TeX-quote-language nil
  "If non-nil determines behavior of quote insertion.
It is usually set by language-related style files.  Its value has
the same structure as the elements of `TeX-quote-language-alist'.
The symbol `override' can be used as its car in order to override
the settings of style files.  Style files should therefore check
if this symbol is present and not alter `TeX-quote-language' if
it is.")

(defun TeX-get-quote-characters ()
  "Get appropriate open and close quote strings.
Return list consisting of three elements as in
`TeX-quote-language-alist'."
  (let* ((lang-override (if (eq (car TeX-quote-language) 'override)
                            TeX-quote-language
                          (assoc (car TeX-quote-language)
                                 TeX-quote-language-alist)))
         (lang (or lang-override TeX-quote-language))
         (open-quote (if lang (nth 1 lang) TeX-open-quote))
         (close-quote (if lang (nth 2 lang) TeX-close-quote))
         (q-after-q (if lang (nth 3 lang) TeX-quote-after-quote)))
    (when (functionp open-quote)
      (setq open-quote (funcall open-quote)))
    (when (functionp close-quote)
      (setq close-quote (funcall close-quote)))
    (list open-quote close-quote q-after-q)))

;; TODO: rework according to the slogan from
;; `TeX--put-electric-delete-selection'. That entails splitting off the
;; «electric» part that tries to do smart things and the plain part that
;; just inserts a quote.
(defun TeX-insert-quote (force)
  "Insert the appropriate quotation marks for TeX.
Inserts the value of `TeX-open-quote' (normally \\=`\\=`) or `TeX-close-quote'
\(normally \\='\\=') depending on the context.  If `TeX-quote-after-quote'
is non-nil, this insertion works only after \".
With prefix argument FORCE, always inserts \" characters."
  (interactive "*P")
  (if (or force
          ;; Do not insert TeX quotes in verbatim, math or comment constructs.
          (and (fboundp 'font-latex-faces-present-p)
               (font-latex-faces-present-p '(font-latex-verbatim-face
                                             font-latex-math-face
                                             font-lock-comment-face))
               (font-latex-faces-present-p '(font-latex-verbatim-face
                                             font-latex-math-face
                                             font-lock-comment-face)
                                           (1- (point))))
          (texmathp)
          (and (TeX-in-comment) (not (eq major-mode 'docTeX-mode))))
      (self-insert-command (prefix-numeric-value force))
    (expand-abbrev)
    (TeX-update-style)
    (pcase-let ((`(,open-quote ,close-quote ,q-after-q)
                 (TeX-get-quote-characters)))
      (if q-after-q
          (insert (cond ((bobp)
                         ?\")
                        ((save-excursion
                           (TeX-looking-at-backward
                            (concat (regexp-quote open-quote) "\\|"
                                    (regexp-quote close-quote))
                            (max (length open-quote) (length close-quote))))
                         (delete-char (- (length (match-string 0))))
                         "\"\"")
                        ((< (save-excursion (skip-chars-backward "\"")) -1)
                         ?\")
                        ((not (= (preceding-char) ?\"))
                         ?\")
                        ((save-excursion
                           (forward-char -1)
                           (bobp))
                         (delete-char -1)
                         open-quote)
                        ((save-excursion
                           (forward-char -2) ;;; at -1 there is double quote
                           (looking-at "[ \t\n]\\|\\s("))
                         (delete-char -1)
                         open-quote)
                        (t
                         (delete-char -1)
                         close-quote)))
        (insert (cond ((bobp)
                       open-quote)
                      ((= (preceding-char) (string-to-char TeX-esc))
                       ?\")
                      ((= (preceding-char) ?\")
                       ?\")
                      ((and (<= (length open-quote) (- (point) (point-min)))
                            (save-excursion
                              (forward-char (- (length open-quote)))
                              (looking-at (regexp-quote open-quote))))
                       (delete-char (- (length open-quote)))
                       ?\")
                      ((and (<= (length open-quote) (- (point) (point-min)))
                            (save-excursion
                              (forward-char (- (length close-quote)))
                              (looking-at (regexp-quote close-quote))))
                       (delete-char (- (length close-quote)))
                       ?\")
                      ((save-excursion
                         (forward-char -1)
                         (looking-at "[ \t\n]\\|\\s("))
                       open-quote)
                      (t
                       close-quote))))
      ;; Fold quotes if TeX-fold-quotes-on-insert is t
      (when (and (bound-and-true-p TeX-fold-mode)
                 (bound-and-true-p TeX-fold-quotes-on-insert)
                 (fboundp 'TeX-fold-quotes)
                 (not (eq (char-before) ?\")))  ; Don't fold single quotes
        (save-excursion
          (let* ((end (point))
                 (start (- end
                           (length
                            (if (string= (buffer-substring-no-properties
                                          (max (point-min)
                                               (- end (length open-quote)))
                                          end)
                                         open-quote)
                                open-quote
                              close-quote)))))
            (when start
              (TeX-fold-quotes start end))))))))


(put 'TeX-insert-quote 'delete-selection t)

(defun TeX-insert-punctuation ()
  "Insert point or comma, cleaning up preceding space."
  (interactive)
  (expand-abbrev)
  (if (TeX-looking-at-backward "\\\\/\\(}+\\)" 50)
      (replace-match "\\1" t))
  (call-interactively #'self-insert-command))

(defun TeX-insert-braces (arg)
  "Make a pair of braces around next ARG sexps and leave point inside.
No argument is equivalent to zero: just insert braces and leave point
between.

If there is an active region, ARG will be ignored, braces will be
inserted around the region, and point will be left after the
closing brace."
  (interactive "*P")
  (if (TeX-active-mark)
      (progn
        (if (< (point) (mark))
            (exchange-point-and-mark))
        (insert TeX-grcl)
        (save-excursion
          (goto-char (mark))
          (insert TeX-grop)))
    (insert TeX-grop)
    (save-excursion
      (if arg (forward-sexp (prefix-numeric-value arg)))
      (insert TeX-grcl))))

;;;###autoload
(defun TeX-submit-bug-report ()
  "Submit a bug report on AUCTeX via mail.

Don't hesitate to report any problems or inaccurate documentation.

If you don't have setup sending mail from Emacs, please copy the
output buffer into your mail program, as it gives us important
information about your AUCTeX version and AUCTeX configuration."
  (interactive)
  (require 'reporter)
  (defvar reporter-prompt-for-summary-p)
  (let ((reporter-prompt-for-summary-p "Bug report subject: "))
    (reporter-submit-bug-report
     "bug-auctex@gnu.org"
     AUCTeX-version
     (list 'window-system
           'LaTeX-version
           'TeX-style-path
           'TeX-auto-save
           'TeX-parse-self
           'TeX-master
           'TeX-command-list)
     nil
     ;; reporter adds too many new lines around salutation text, that we don't
     ;; want, since it's itself a new line.
     (lambda ()
       (save-excursion
         (goto-char (point-min))
         (re-search-forward mail-header-separator)
         (forward-char)
         (delete-char 1)
         (forward-char)
         (delete-char 2)))
     (propertize
      "\n" 'display
      (with-temp-buffer
        (insert
         "Remember to cover the basics, that is, what you expected to happen and
what in fact did happen.

Be sure to consult the FAQ section in the manual before submitting
a bug report.  In addition check if the bug is reproducable with an
up-to-date version of AUCTeX.  So please upgrade to the version
available from ")
        (insert-text-button
         "https://www.gnu.org/software/auctex/"
         'face 'link
         'help-echo (concat "mouse-2, RET: Follow this link")
         'action (lambda (_button)
                   (browse-url "https://www.gnu.org/software/auctex/"))
         'follow-link t)
        (insert " if your
installation is older than the one available from the web site.

If the bug is triggered by a specific (La)TeX file, you should try
to produce a minimal sample file showing the problem and include it
in your report.

Your report will be posted for the auctex package at the GNU bug
tracker.  Visit ")
        (insert-text-button
         "https://debbugs.gnu.org/cgi/pkgreport.cgi?pkg=auctex"
         'face 'link
         'help-echo (concat "mouse-2, RET: Follow this link")
         'action (lambda (_button)
                   (browse-url "https://debbugs.gnu.org/cgi/pkgreport.cgi?pkg=auctex"))
         'follow-link t)
        (insert "\nto browse existing AUCTeX bugs.
------------------------------------------------------------------------\n\n")
        (buffer-string))))))


;;; Documentation

(defun TeX-documentation-texdoc (&optional arg)
  "Run Texdoc to read documentation.

Prompt for selection of the package of which to show the documentation.

If called with a prefix argument ARG, after selecting the
package, prompt for selection of the manual of that package to
show."
  (interactive "P")
  (if (not (executable-find "texdoc"))
      (message "texdoc not found")
    (let ((pkg (thing-at-point 'symbol t))
          buffer list doc)
      (setq pkg (TeX-read-string "View documentation for: " pkg))
      (unless (zerop (length pkg))
        (if arg
            ;; Called with prefix argument:
            ;; run "texdoc --list --nointeract <pkg>"
            (progn
              ;; Create the buffer, insert the result of the command,
              ;; and accumulate the list of manuals.
              (with-current-buffer
                  (setq buffer (get-buffer-create (format "*texdoc: %s*" pkg)))
                (erase-buffer)
                (call-process "texdoc" nil t nil
                              "--list" "--nointeract" pkg)
                (goto-char 1)             ; No need to use `point-min' here.
                (while (re-search-forward
                        "^ *\\([0-9]+\\) +\\([-~/a-zA-Z0-9_.${}#%,:\\ ()]+\\)"
                        nil t)
                  (push (cons (match-string 1) (match-string 2)) list)))
              (unwind-protect
                  (cond
                   (list
                    ;; Go on if there are manuals listed: show the
                    ;; buffer, prompt for the number of the manual,
                    ;; then run
                    ;;     texdoc --just-view <doc>
                    (TeX-pop-to-buffer buffer)
                    (condition-case nil
                        (when (setq doc
                                    (cdr (assoc (TeX-read-string "Please \
enter the number of the file to view, anything else to skip: ") list)))
                          (call-process "texdoc" nil 0 nil "--just-view" doc))
                      ;; Exit gently if a `quit' signal is thrown.
                      (quit nil)))
                   (t (message "No documentation found for %s" pkg)))
                ;; In any case quit-and-kill the window.
                (when (get-buffer-window buffer)
                  (quit-window t (get-buffer-window buffer)))))
          ;; Called without prefix argument:
          ;; just run "texdoc -I --view <pkg>".
          (make-process
           :name "texdoc"
           :command (list "texdoc" "-I" "--view" pkg)
           :sentinel (lambda (proc _string)
                       ;; Recent Texdoc returns exit code 3 when it
                       ;; can't find the specified document:
                       ;; <URL:https://tug.org/texdoc/doc/texdoc.man1.pdf>
                       (when (= (process-exit-status proc) 3)
                         (message "No documentation found for %s" pkg)))
           ;; Use pipe rather than pty for particular situations.  See
           ;; <URL:https://lists.gnu.org/r/auctex/2024-09/msg00012.html>
           ;; for detail.
           :connection-type 'pipe))))))

(defun TeX-goto-info-page ()
  "Read documentation for AUCTeX in the info system."
  (interactive)
  (info "auctex"))

(autoload 'info-lookup->completions "info-look")

(defvar TeX-doc-backend-alist
  '((texdoc (plain-TeX-mode LaTeX-mode docTeX-mode AmSTeX-mode ConTeXt-mode)
            (lambda ()
              (when (executable-find "texdoc")
                (TeX-search-files-by-type 'docs 'global t t)))
            (lambda (doc)
              ;; texdoc in MiKTeX requires --view in order to start
              ;; the viewer instead of an intermediate web page.
              (call-process "texdoc" nil 0 nil "--view" doc)))
    (latex-info (LaTeX-mode)
                (lambda ()
                  (mapcar (lambda (x)
                            (let ((x (car x)))
                              (if (string-match "\\`\\\\" x)
                                  (substring x 1) x)))
                          (info-lookup->completions 'symbol 'LaTeX-mode)))
                (lambda (doc)
                  (info-lookup-symbol (concat "\\" doc) 'LaTeX-mode)))
    (texinfo-info (Texinfo-mode)
                  (lambda ()
                    (mapcar (lambda (x)
                              (let ((x (car x)))
                                (if (string-match "\\`@" x)
                                    (substring x 1) x)))
                            (info-lookup->completions 'symbol
                                                      'Texinfo-mode)))
                  (lambda (doc)
                    (info-lookup-symbol (concat "@" doc) 'Texinfo-mode))))
  "Alist of backends used for looking up documentation.
Each item consists of four elements.

The first is a symbol describing the backend's name.

The second is either a list of modes the backend should be activated in,
or the symbol t, which stands for all modes.

The third is a function returning a list of documents available
to the backend.  It should return nil if the backend is not
available, for example if a required executable is not present on the
system in question.

The fourth is a function for displaying the documentation.  The
function should accept a single argument, the chosen package,
command, or document name.")

(defun TeX-doc (&optional name)
  "Display documentation for string NAME.
NAME may be a package, a command, or a document."
  (interactive)
  (let (docs)
    ;; Build the lists of available documentation used for completion.
    (dolist (elt TeX-doc-backend-alist)
      (when (or (eq t (nth 1 elt))
                (memq major-mode (nth 1 elt)))
        (let ((completions (funcall (nth 2 elt))))
          (unless (null completions)
            (cl-pushnew (cons completions (nth 0 elt)) docs :test #'equal)))))
    (if (null docs)
        (progn
          (if (executable-find "texdoc")
              ;; Fallback if we did not find anything via the backend list.
              (let ((doc (read-from-minibuffer "Input for `texdoc': ")))
                (when doc (call-process "texdoc" nil 0 nil "--view" doc)))
            ;; Give up.
            (message "No documentation found")))
      ;; Ask the user about the package, command, or document.
      (when (and (called-interactively-p 'any)
                 (or (not name) (string= name "")))
        (let ((symbol (thing-at-point 'symbol))
              contained completions)
          ;; Is the symbol at point contained in the lists of available
          ;; documentation?
          (setq contained (catch 'found
                            (dolist (elt docs)
                              (when (member symbol (car elt))
                                (throw 'found t)))))
          ;; Setup completion list in a format suitable for `completing-read'.
          (dolist (elt docs)
            ;; FIXME: Probably not needed!
            (setq completions (nconc (mapcar #'list (car elt)) completions)))
          ;; Query user.
          (setq name (completing-read
                      (if contained
                          (format-prompt "Package, command, or document"
                                         symbol)
                        "Package, command, or document: ")
                      completions nil nil nil nil symbol))))
      (if (not name)
          (message "No documentation specified")
        ;; XXX: Provide way to choose in case a symbol can be found in
        ;; more than one backend.
        (let* ((backend (catch 'found
                          (dolist (elt docs)
                            (when (member name (car elt))
                              (throw 'found (cdr elt)))))))
          (if backend
              (funcall (nth 3 (assoc backend TeX-doc-backend-alist)) name)
            (message "Documentation not found")))))))


;;; Ispell Support

(defun TeX-run-ispell (_command _string file)
  "Run ispell on current TeX buffer."
  (cond ((string-equal file (TeX-region-file))
         (call-interactively #'ispell-region))
        (t
         (ispell-buffer))))

(defun TeX-ispell-document (name)
  "Run ispell on all open files belonging to the current document."
  (interactive (list (TeX-master-file)))
  (if (string-equal name "")
      (setq name (TeX-master-file)))

  (let ((regexp (concat "\\`\\("
                        (mapconcat (lambda (dir)
                                     (regexp-quote
                                      (expand-file-name
                                       (file-name-as-directory dir))))
                                   (append (when (file-name-directory name)
                                             (list (file-name-directory name)))
                                           TeX-check-path)
                                   "\\|")
                        "\\).*\\("
                        (mapconcat #'regexp-quote
                                   (cons (file-name-nondirectory name)
                                         (TeX-style-list))
                                   "\\|")
                        "\\)\\.\\("
                        (mapconcat #'identity TeX-file-extensions "\\|")
                        "\\)\\'"))
        (buffers (buffer-list)))
    (while buffers
      (let* ((buffer (car buffers))
             (name (TeX-buffer-file-name buffer)))
        (setq buffers (cdr buffers))
        (when (and name (string-match regexp name))
          (save-excursion (switch-to-buffer buffer) (ispell-buffer))
          t)))))

(defcustom TeX-ispell-extend-skip-list t
  "Whether to extend regions selected for skipping during spell checking."
  :group 'TeX-misc
  :type 'boolean)

;; These functions are used to add new items to
;; `ispell-tex-skip-alists' -- see tex-ispell.el:
(defun TeX-ispell-skip-setcar (skip)
  "Add SKIP to car of `ispell-tex-skip-alists'.
SKIP is an alist with the format described in
`ispell-tex-skip-alists'.  Each element in SKIP is added on top
of the car of `ispell-tex-skip-alists'.  This only happens if
`TeX-ispell-extend-skip-list' is non-nil."
  (when TeX-ispell-extend-skip-list
    (let ((raws (car ispell-tex-skip-alists))
          (envs (cadr ispell-tex-skip-alists)))
      (dolist (x skip)
        (cl-pushnew x raws :test #'equal))
      (setq ispell-tex-skip-alists (list raws envs)))))

(defun TeX-ispell-skip-setcdr (skip)
  "Add SKIP to cdr of `ispell-tex-skip-alists'.
SKIP is an alist with the format described in
`ispell-tex-skip-alists'.  Each element in SKIP is added on top
of the cdr of `ispell-tex-skip-alists'.  This only happens if
`TeX-ispell-extend-skip-list' is non-nil."
  (when TeX-ispell-extend-skip-list
    (let ((raws (car ispell-tex-skip-alists))
          (envs (cadr ispell-tex-skip-alists)))
      (dolist (x skip)
        (cl-pushnew x envs :test #'equal))
      (setq ispell-tex-skip-alists (list raws envs)))))

(defun TeX-ispell-tex-arg-end (&optional arg1 arg2 arg3)
  "Skip across ARG1, ARG2 and ARG3 number of braces and brackets.
This function is a variation of `ispell-tex-arg-end'.  It should
be used when adding skip regions to `ispell-tex-skip-alists' for
constructs like:

  \\begin{tabularx}{300pt}[t]{lrc} ...
    or
  \\fontspec{font name}[font features]

where optional and/or mandatory argument(s) follow(s) a mandatory
one.  ARG1 is the number of mandatory arguments before the
optional one, ARG2 the max. number of following optional
arguments, ARG3 is the max. number of mandatory arguments
following.  Omitting argument means 1.

Here some examples for additions to `ispell-tex-skip-alists':

  \\begin{tabularx}{300pt}[t]{lrc} ...
                ARG  1    2   3
  (\"tabularx\" TeX-ispell-tex-arg-end) or equivalent
  (\"tabularx\" TeX-ispell-tex-arg-end 1 1 1)

  \\fontspec{font name}[font features]
               ARG1         ARG2        ARG3=0
  (\"\\\\\\\\fontspec\" TeX-ispell-tex-arg-end 1 1 0)

  \\raisebox{lift}[height][depth]{contents}
            ARG1       ARG2       ARG3=0 (checked by Ispell)
  (\"\\\\\\\\raisebox\" TeX-ispell-tex-arg-end 1 2 0)

Optional arguments before the first mandatory one are all
skipped."
  (condition-case nil
      (progn
        (while (looking-at "[ \t\n]*\\[") (forward-sexp))
        (forward-sexp (or arg1 1))
        (let ((num 0))
          (while (and (looking-at "[ \t\n]*\\[")
                      (< num (or arg2 1)))
            (setq num (1+ num))
            (forward-sexp)))
        (forward-sexp (or arg3 1)))
    (error
     (message "Error skipping s-expressions at point %d" (point))
     (sit-for 2))))

(defun TeX-ispell-tex-arg-verb-end (&optional arg)
  "Skip an optional argument, ARG number of mandatory ones and verbatim content.
This function always checks if one optional argument in brackets
is given and skips over it.  If ARG is a number, it skips over
that many mandatory arguments in braces.  Then it checks for
verbatim content to skip which is enclosed by a character given
in `TeX-ispell-verb-delimiters' or in braces, otherwise raises an
error."
  (condition-case nil
      (progn
        (when (looking-at "[ \t\n]*\\[") (forward-sexp))
        (when (and arg (looking-at "{"))
          (forward-sexp arg))
        (cond ((looking-at (concat "[" TeX-ispell-verb-delimiters "]"))
               (forward-char)
               (skip-chars-forward (concat "^" (string (char-before))))
               (forward-char))
              ((looking-at "{")
               (forward-sexp))
              (t (error nil))))
    (error
     (message "Verbatim delimiter is not one of %s"
              (split-string TeX-ispell-verb-delimiters "" t))
     (sit-for 2))))

;;; Abbrev mode

(defmacro TeX-abbrev-mode-setup (mode usertable)
  "Set up the abbrev table and variable for MODE.
The table inherits from USERTABLE if it is a valid abbrev table."
  (let ((symbol (intern (concat (symbol-name mode) "-abbrev-table")))
        (name (TeX-mode-prefix mode)))
    `(progn
       (defvar ,symbol nil
         ,(format "Abbrev table for %s mode." name))
       (define-abbrev-table ',symbol nil)
       (let ((parents (list text-mode-abbrev-table)))
         ;; Users may already have user abbrevs in tables based on the
         ;; former mode names such as `latex-mode-abbrev-table',
         ;; stored in .emacs.d/abbrev_defs.  In that case, add them as
         ;; parent abbrev tables.
         (if (and (boundp ',usertable)
                  (abbrev-table-p ,usertable))
             (push ,usertable parents))
         (abbrev-table-put ,symbol :parents parents)))))


;;; Special provisions for other modes and libraries

;; desktop-locals-to-save is broken by design.  Don't have
;; buffer-local values of it.
(eval-after-load "desktop"
  '(progn
     (dolist (elt '(TeX-master))
       (unless (member elt (default-value 'desktop-locals-to-save))
         (setq-default desktop-locals-to-save
                       (cons elt (default-value 'desktop-locals-to-save)))))
     (add-hook 'desktop-after-read-hook (lambda ()
                                          (TeX-set-mode-name t)))))

(defun TeX--list-of-string-p (lst)
  "Return non-nil if LST is a list of strings.
Used as function for validating a variable's `safe-local-variable' property."
  (and (listp lst)
       (let ((all-strings t))
         (while (and all-strings lst)
           (setq all-strings (stringp (car lst)))
           (setq lst (cdr lst)))
         all-strings)))

;; add-log.el: This function is a variation of
;; `tex-current-defun-name' defined in `tex-mode.el'.  In `latex.el',
;; the variable `add-log-current-defun-function' is set to this
;; function.
(defun TeX-current-defun-name ()
  "Return the name of the TeX section/paragraph/chapter at point, or nil."
  (save-excursion
    (let (s1 e1 s2 e2)
      ;; If we are now precisely at the beginning of a sectioning
      ;; command, move forward and make sure `re-search-backward'
      ;;  finds this one rather than the previous one:
      (or (eobp) (progn
                   (when (looking-at-p "\\\\")
                     (forward-char))
                   (unless (eolp)
                     (forward-sexp))))
      ;; Search backward for sectioning command.  If
      ;; `LaTeX-section-label' is buffer-local, assume that a style
      ;; has changed the value and recalculate the string.  Otherwise
      ;; take the standard one:
      (when (re-search-backward
             (if (local-variable-p 'LaTeX-section-label)
                 (concat "\\\\"
                         (regexp-opt
                          (remove "part" (mapcar #'car LaTeX-section-label)))
                         "\\*?")
               "\\\\\\(sub\\)*\\(section\\|paragraph\\|chapter\\)\\*?")
             nil t)
        ;; Skip over the backslash:
        (setq s1 (1+ (point)))
        ;; Skip over the sectioning command, incl. the *:
        (setq e1 (goto-char (match-end 0)))
        ;; Skip over the optional argument, if any:
        (when (looking-at-p "[ \t]*\\[")
          (forward-sexp))
        ;; Skip over any chars until the mandatory argument:
        (skip-chars-forward "^{")
        ;; Remember the points for the mandatory argument:
        (setq s2 (point))
        (setq e2 (progn (forward-sexp)
                        (point)))
        ;; Now pick the content: For one-line title, return it
        ;; incl. the closing brace.  For multi-line, return the first
        ;; line of the mandatory argument incl. ellipsis and a brace;
        (concat
         (buffer-substring-no-properties s1 e1)
         (buffer-substring-no-properties
          (goto-char s2)
          (min (line-end-position) e2))
         (when (> e2 (line-end-position))
           (concat "..." TeX-grcl)))))))

;;; Customization:

(defcustom TeX-process-asynchronous (not (eq system-type 'ms-dos))
  "Use asynchronous processes."
  :group 'TeX-command
  :type 'boolean)

(defcustom TeX-shell
  (if (memq system-type '(ms-dos emx windows-nt))
      shell-file-name
    "/bin/sh")
  "Name of shell used to parse TeX commands."
  :group 'TeX-command
  :type 'file)

(defcustom TeX-shell-command-option
  (cond ((memq system-type '(ms-dos emx windows-nt))
         shell-command-switch)
        (t                              ;Unix & EMX (Emacs 19 port to OS/2)
         "-c"))
  "Shell argument indicating that next argument is the command."
  :group 'TeX-command
  :type 'string)

;;; Interactive Commands
;;
;; The general idea is, that there is one process and process buffer
;; associated with each master file, and one process and process
;; buffer for running TeX on a region.
;;
;; Some user commands operates on ``the'' process, which is the last
;; process still running or already finished.  Note that you cannot
;; run more than one process simultaneously, including preview by
;; preview-latex, because process filters and sentinels refer to
;; certain set of global variables which each invokation of the
;; process overwrites.  If you dare to do, the result is thus
;; unpredictable.

(defun TeX-save-document (name-or-file-fn)
  "Save all files belonging to the current document.
Return non-nil if document needs to be re-TeX'ed.
In Lisp program, NAME-OR-FILE-FN specifies the current document.
It is either the master name without extension or the function
`TeX-master-file'."
  (interactive (list #'TeX-master-file))
  (TeX-check-files (TeX--concat-ext name-or-file-fn (TeX-output-extension))
                   (cons (TeX--concat-ext name-or-file-fn) (TeX-style-list))
                   TeX-file-extensions))

(defun TeX--concat-ext (name-or-file-fn &optional extension)
  "Append EXTENSION to a filename specified by NAME-OR-FILE-FN.

If NAME-OR-FILE-FN is a string, interpret it as the filename.
Otherwise, assume it is a callable function and call it with
EXTENSION as an argument and return the result without
modification.  EXTENSION is a string which should not start with
'.'."
  (if (stringp name-or-file-fn)
      (if extension
          (concat name-or-file-fn "." extension)
        name-or-file-fn)
    (funcall name-or-file-fn extension)))

(defun TeX-command-master (&optional override-confirm)
  "Run command on the current document.

If a prefix argument OVERRIDE-CONFIRM is given, confirmation will
depend on it being positive instead of the entry in `TeX-command-list'."
  (interactive "P")
  (TeX-master-file nil nil t)  ;; call to ask if necessary
  (TeX-command (TeX-command-query #'TeX-master-file)
               #'TeX-master-file override-confirm))

(defcustom TeX-region-extra ""
  "String to insert in the region file between the header and the text."
  :group 'TeX-command
  :type 'string)

;; This was "{\\makeatletter\\gdef\\AucTeX@cite#1[#2]#3{[#3#1#2]}\
;;           \\gdef\\cite{\\@ifnextchar[{\\AucTeX@cite{, }}\
;;           {\\AucTeX@cite{}[]}}}\n"
;; However, that string is inappropriate for plain TeX and ConTeXt.
;; This needs reconsideration.

(defvar-local TeX-command-region-begin nil)
(defvar-local TeX-command-region-end nil)
;; Used for marking the last region.

(defun TeX-current-offset (&optional pos)
  "Calculate line offset of POS, or of point if POS is nil."
  (save-restriction
    (widen)
    (save-excursion
      (let ((inhibit-field-text-motion t))
        (if pos (goto-char pos))
        (+ (count-lines (point-min) (point))
           (if (bolp) 0 -1))))))

(defun TeX-pin-region (begin end)
  "Pin the TeX region specified by BEGIN and END.
If BEGIN is nil, the region is unpinned.

In interactive use, a positive prefix arg will pin the region,
a non-positive one will unpin it.  Without a prefix arg, if
a region is actively marked, it will get pinned.  If not, a
pinned region will get unpinned and vice versa."
  (interactive
   (if
       (if current-prefix-arg
           (> (prefix-numeric-value current-prefix-arg) 0)
         (or (TeX-active-mark)
             (null TeX-command-region-begin)))
       (list (region-beginning) (region-end))
     '(nil nil)))
  (if begin
      (progn
        (unless (markerp TeX-command-region-begin)
          (setq TeX-command-region-begin (make-marker))
          (setq TeX-command-region-end (make-marker)))
        (set-marker TeX-command-region-begin begin)
        (set-marker TeX-command-region-end end)
        (message "TeX region pinned."))
    (when (markerp TeX-command-region-begin)
      (set-marker TeX-command-region-begin nil)
      (set-marker TeX-command-region-end nil))
    (setq TeX-command-region-begin nil)
    (setq TeX-command-region-end nil)
    (message "TeX region unpinned.")))

(defun TeX-region-update ()
  "Update the TeX-region file."
  ;; Note that TeX-command-region-begin is not a marker when called
  ;; from TeX-command-buffer.
  (and (or (null TeX-command-region-begin)
           (markerp TeX-command-region-begin))
       (TeX-active-mark)
       (TeX-pin-region (region-beginning) (region-end)))
  (let* ((begin (or TeX-command-region-begin (region-beginning)))
         (end (or TeX-command-region-end (region-end)))
         (TeX-region-extra
          ;; Write out counter information to region.
          (concat (and (fboundp 'preview--counter-information)
                       (preview--counter-information begin))
                  TeX-region-extra)))
    (TeX-region-create (TeX-region-file TeX-default-extension)
                       (buffer-substring-no-properties begin end)
                       (file-name-nondirectory (TeX-buffer-file-name))
                       (TeX-current-offset begin))))

(defun TeX-command-region (&optional override-confirm)
  "Run TeX on the current region.

Query the user for a command to run on the temporary file specified by
the variable `TeX-region'.  If there is an explicitly active region,
it is stored for later commands.  If not, a previously stored region
\(can be also be set with `TeX-pin-region') overrides the current region,
if present.

If a prefix argument OVERRIDE-CONFIRM is given, prompting will
ignore the prompting flag from `TeX-command-list' and instead
will prompt only if the prefix is positive.

If the master file for the document has a header, it is written to the
temporary file before the region itself.  The document's header is all
text before `TeX-header-end'.

If the master file for the document has a trailer, it is written to
the temporary file after the region itself.  The document's trailer is
all text after `TeX-trailer-start'."
  (interactive "P")
  (TeX-region-update)
  ;; In the next line, `TeX-region-file' should be called with nil
  ;; `nondirectory' argument, otherwise `TeX-command-default' called
  ;; within `TeX-command-query' won't work in included files not
  ;; placed in `TeX-master-directory'.
  (TeX-command (TeX-command-query #'TeX-region-file) #'TeX-region-file
               override-confirm))

(defun TeX-command-buffer (&optional override-confirm)
  "Run TeX on the current buffer.

Query the user for a command to run on the temporary file specified by
the variable `TeX-region'.  The region file will be recreated from the
visible part of the buffer.

If a prefix argument OVERRIDE-CONFIRM is given, confirmation will
depend on it being positive instead of the entry in `TeX-command-list'."
  (interactive "P")
  (let ((TeX-command-region-begin (point-min))
        (TeX-command-region-end (point-max)))
    (TeX-command-region override-confirm)))

(defcustom TeX-record-buffer nil
  "Whether to record buffer names of generated TeX buffers.
When non-nil, these buffers are put at the front of the list of
recently selected ones."
  :group 'TeX-command
  :type 'boolean)

(defun TeX-pop-to-buffer (buffer &optional other-window norecord)
  "Compatibility wrapper for `pop-to-buffer'.

Select buffer BUFFER in some window, preferably a different one.
BUFFER may be a buffer, a string (a buffer name), or nil.
If BUFFER is a string which is not the name of an existing buffer,
then this function creates a buffer with that name.
If BUFFER is nil, then it chooses some other buffer.
If `pop-up-windows' is non-nil, windows can be split to do this.
If optional second arg OTHER-WINDOW is non-nil, insist on finding another
window even if BUFFER is already visible in the selected window,
and ignore `same-window-regexps' and `same-window-buffer-names'.
This function returns the buffer it switched to.
This uses the function `display-buffer' as a subroutine; see the documentation
of `display-buffer' for additional customization information.

Optional third arg NORECORD non-nil means do not put this buffer
at the front of the list of recently selected ones."
  (pop-to-buffer buffer other-window (and norecord (not TeX-record-buffer))))

(defun TeX-recenter-output-buffer (line)
  "Redisplay buffer of TeX job output so that most recent output can be seen.
The last line of the buffer is displayed on line LINE of the window, or
at bottom if LINE is nil."
  (interactive "P")
  (let ((buffer (TeX-active-buffer)))
    (if buffer
        (let ((old-buffer (current-buffer)))
          (TeX-pop-to-buffer buffer t t)
          (bury-buffer buffer)
          (goto-char (point-max))
          (recenter (if line
                        (prefix-numeric-value line)
                      (/ (window-height) 2)))
          (TeX-pop-to-buffer old-buffer nil t))
      (message "No process for this document."))))

(defun TeX-kill-job ()
  "Kill the currently running TeX job."
  (interactive)
  (let ((process (TeX-active-process)))
    (if process
        (kill-process process)
      ;; Should test for TeX background process here.
      (error "No TeX process to kill"))))

;; FIXME: The vars below are defined in this file, but they're defined too
;; far down (i.e. further down than their first use), so we have to pre-declare
;; them here to explain it to the compiler.
;; We should move those vars's definitions earlier instead!
(defvar TeX-current-process-region-p)
(defvar TeX-save-query)
(defvar TeX-parse-function)
(defvar TeX-sentinel-function)
(defvar TeX-sentinel-default-function)
(defvar TeX-current-page)
(defvar TeX-error-overview-open-after-TeX-run)
(defvar TeX-error-list)
(defvar TeX-command-buffer)
(defvar TeX-region)

(defun TeX-home-buffer ()
  "Go to the buffer where you last issued a TeX command.
If there is no such buffer, or you already are in that buffer, find
the master file."
  (interactive)
  (if (or (null TeX-command-buffer)
          (null (buffer-name TeX-command-buffer))
          (eq TeX-command-buffer (current-buffer)))
      (find-file (TeX-master-file TeX-default-extension))
    (switch-to-buffer TeX-command-buffer)))

(defvar-local TeX-error-last-visited -1
  "Index of the last visited error listed in `TeX-error-list'.

This variable is intended to be set only in output buffer so it
will be shared among all files of the same document.")

(defun TeX-get-parse-function ()
  "Get the parse function for the current buffer."
  (with-current-buffer TeX-command-buffer
    (TeX-process-get-variable (TeX-active-master) 'TeX-parse-function)))

(defun TeX-next-error (&optional arg reparse)
  "Find the next error in the TeX output buffer.

A prefix ARG specifies how many error messages to move;
negative means move back to previous error messages, if possible.

If REPARSE is non-nil, reparse the error message buffer.

\\[universal-argument] as a prefix means reparse the error
message buffer and start at the first error."
  (interactive "P")
  (if (or (null (TeX-active-buffer))
          (eq 'compilation-mode (with-current-buffer TeX-command-buffer
                                  major-mode)))
      (next-error arg reparse)

    ;; Force reparsing when the function is called with a universal-argument.
    (if (consp arg) (setq reparse t arg nil))

    (funcall (TeX-get-parse-function) arg reparse)))

(defun TeX-previous-error (arg)
  "Find the previous error in the TeX output buffer.

Prefix ARG says how many error messages to move backward (or
forward, if negative).

This works only with TeX commands and if the
`TeX-parse-all-errors' variable is non-nil."
  (interactive "p")
  (if (or (null (TeX-active-buffer))
          (eq 'compilation-mode (with-current-buffer TeX-command-buffer
                                  major-mode)))
      (previous-error arg)

    (let ((parse-function (TeX-get-parse-function)))
      (if (and TeX-parse-all-errors (equal parse-function #'TeX-parse-TeX))
          ;; When `TeX-parse-all-errors' is non-nil and the parsing function is
          ;; `TeX-parse-TeX' we can move backward in the errors.
          (TeX-parse-TeX (- arg) nil)
        ;; XXX: moving backward in the errors hasn't yet been implemented for
        ;; other parsing functions.
        (error "Jumping to previous error not supported")))))

;;; Command Query

(defvar TeX-error-overview-frame nil
  "The frame of the error overview.")

(defconst TeX-error-overview-buffer-name "*TeX errors*"
  "Name of the buffer in which to show error list.")

(defvar LaTeX-idx-md5-alist nil
  "Alist of MD5 hashes of idx file.

Car is the idx file, cdr is its md5 hash.")

(defvar LaTeX-idx-changed-alist nil
  "Whether the idx files changed.

Car is the idx file, cdr is whether idx changed after LaTeX
run.")

(defcustom TeX-check-engine t
  "Check the correct engine has been set before running TeX commands."
  :group 'TeX-command
  :type 'boolean)

(defvar-local TeX-check-engine-list '(default luatex omega xetex)
  "List of engines required by the loaded TeX packages.

Do not set this variable directly, use
`TeX-check-engine-add-engines' to specify required engines.")

(defun TeX-check-engine-add-engines (&rest engines)
  "Add ENGINES to list of required engines.

Set `TeX-check-engine-list' to the intersection between the list
itself and the list of provided engines.

See for example style/fontspec.el"
  (let ((list TeX-check-engine-list)
        (res nil))
    (setq TeX-check-engine-list
          ;; The following is based on the definition of `cl-intersection' of
          ;; GNU Emacs.
          (and list engines
               (if (equal list engines) list
                 (or (>= (length list) (length engines))
                     (setq list (prog1 engines (setq engines list))))
                 (while engines
                   (if (memq (car engines) list)
                       (push (car engines) res))
                   (pop engines))
                 res)))))

(defun TeX-check-engine (name)
  "Check the correct engine has been set.

Look into `TeX-check-engine-list' for the required engines.

NAME is the command to be run.  Actually do the check only if the
variable `TeX-check-engine' is non-nil and LaTeX is the command
to be run."
  (and
   (string= name "LaTeX")
   TeX-check-engine
   TeX-check-engine-list
   (null (memq TeX-engine TeX-check-engine-list))
   (memq TeX-engine '(default luatex omega xetex))
   ;; The set engine is not listed in `TeX-check-engine-list'.  We check only
   ;; builtin engines because we can't take care of custom ones.  Do nothing if
   ;; there is no allowed engine, we don't know what to do in that case.
   (let ((length (length TeX-check-engine-list))
         (name-alist '((default . "TeX")
                       (luatex  . "LuaTeX")
                       (omega   . "Omega")
                       (xetex   . "XeTeX")))
         (completion-ignore-case t)
         (engine nil))
     (when
         (cond
          ;; There is exactly one allowed engine.
          ((= length 1)
           (setq engine (car TeX-check-engine-list))
           (y-or-n-p (format "%s is required to build this document.
Do you want to use this engine? " (cdr (assoc engine name-alist)))))
          ;; More than one engine is allowed.
          ((> length 1)
           (if (y-or-n-p (format "It appears %s are required to build this document.
Do you want to select one of these engines? "
                                 (mapconcat
                                  (lambda (elt) (cdr (assoc elt name-alist)))
                                  TeX-check-engine-list ", ")))
               (setq engine
                     (car (rassoc
                           (completing-read
                            (format
                             "Choose between %s: "
                             (mapconcat
                              (lambda (elt) (cdr (assoc elt name-alist)))
                              TeX-check-engine-list ", "))
                            (mapcar
                             (lambda (elt) (cdr (assoc elt name-alist)))
                             TeX-check-engine-list))
                           name-alist)))
             ;; Don't keep asking.  If user doesn't want to change engine,
             ;; probably has a good reason.  In order to do so, without adding
             ;; yet another variable we just hack `TeX-check-engine-list' and
             ;; make it nil.
             (setq TeX-check-engine-list nil))))
       (TeX-engine-set engine)
       (when (y-or-n-p "Do you want to remember the choice? ")
         (add-file-local-variable 'TeX-engine engine)
         (save-buffer))))))

(defcustom TeX-check-TeX t
  "Whether AUCTeX should check if a working TeX distribution is present."
  :group 'TeX-command
  :type 'boolean)

(defcustom TeX-check-TeX-command-not-found 127
  "Numerical code returned by shell for a command not found error."
  :group 'TeX-command
  :type 'integer)

(defun TeX-command (name file-fn &optional override-confirm)
  "Run command NAME on the file returned by calling FILE-FN.

FILE-FN is the symbol of a function returning a file name.  The
function has one optional argument, the extension to use on the
file.  Valid choices are `TeX-master-file' and `TeX-region-file'.

Use the information in `TeX-command-list' to determine how to run
the command.

If OVERRIDE-CONFIRM is a prefix argument, confirmation will be
asked if it is positive, and suppressed if it is not.

Run function `TeX-check-engine' to check the correct engine has
been set."
  (TeX-check-engine name)

  ;; Make sure that `TeX-command-buffer' is set always.
  ;; It isn't safe to remove similar lines in `TeX-run-command' etc.
  ;; because preview-latex calls `TeX-run-command' directly.
  (setq-default TeX-command-buffer (current-buffer))

  (cond ((eq file-fn #'TeX-region-file)
         (setq TeX-current-process-region-p t))
        ((eq file-fn #'TeX-master-file)
         (setq TeX-current-process-region-p nil)))

  ;; When we're operating on a region, we need to update the position
  ;; of point in the region file so that forward search works.
  (if (string= name "View") (TeX-region-update-point))

  (let ((command (TeX-command-expand (nth 1 (assoc name TeX-command-list))))
        (hook (nth 2 (assoc name TeX-command-list)))
        (confirm (if override-confirm
                     (> (prefix-numeric-value override-confirm) 0)
                   (nth 3 (assoc name TeX-command-list)))))

    ;; Verify the expanded command
    (if confirm
        (setq command
              (read-from-minibuffer (concat name " command: ") command
                                    nil nil)))

    ;; Kill the frame and buffer associated to the error overview before running
    ;; the command, but keep them if the command to be run is View.
    (unless (string= name "View")
      (if (frame-live-p TeX-error-overview-frame)
          (delete-frame TeX-error-overview-frame))
      (if (get-buffer TeX-error-overview-buffer-name)
          (kill-buffer TeX-error-overview-buffer-name)))

    ;; Before running some commands, check that AUCTeX is able to find "tex"
    ;; program.
    (and TeX-check-TeX
         (member name '("TeX" "LaTeX" "AmSTeX" "ConTeXt" "ConTeXt Full"))
         (= TeX-check-TeX-command-not-found
            (call-process TeX-shell nil nil nil
                          TeX-shell-command-option TeX-command))
         (error (format "ERROR: AUCTeX cannot find a working TeX distribution.
Make sure you have one and that TeX binaries are in PATH environment variable%s"
                        (if (eq system-type 'darwin)
                            ".
If you are using macOS 10.14 Mojave or later
remember to add /Library/TeX/texbin/ to your PATH"
                          ""))))

    ;; Now start the process
    (let ((file (funcall file-fn)))
      (TeX-process-set-variable file 'TeX-command-next TeX-command-Show)
      (funcall hook name command file))))

(defun TeX-command-expand (command &optional list)
  "Expand COMMAND for `TeX-active-master' as described in LIST.
LIST default to `TeX-expand-list'.  As a special exception,
`%%' can be used to produce a single `%' sign in the output
without further expansion."
  (let ((TeX-expand-command command)
        TeX-expand-pos
        TeX-command-text
        TeX-command-pos
        pat entry case-fold-search string expansion arguments)
    (setq list (cons
                (list "%%" (lambda nil
                             (setq TeX-expand-pos (1+ TeX-expand-pos))
                             "%"))
                (or list (TeX-expand-list)))
          pat (regexp-opt (mapcar #'car list)))
    (while (setq TeX-expand-pos (string-match pat TeX-expand-command TeX-expand-pos))
      (setq string (match-string 0 TeX-expand-command)
            entry (assoc string list)
            expansion (car (cdr entry)) ;Second element
            arguments (cdr (cdr entry)) ;Remaining elements
            string (save-match-data
                     (cond
                      ((functionp expansion)
                       (apply expansion arguments))
                      ((boundp expansion)
                       (apply (symbol-value expansion) arguments))
                      (t
                       (error "Nonexpansion %s" expansion)))))
      (if (stringp string)
          (setq TeX-expand-command
                (replace-match string t t TeX-expand-command))))
    TeX-expand-command))

(defun TeX-active-master-with-quotes
    (&optional extension nondirectory ask extra preprocess-fn)
  "Return the current master or region file name with quote for shell.
Pass arguments EXTENSION NONDIRECTORY ASK to `TeX-active-master'.
If the returned file name contains space, enclose it within
quotes `\"' when \" \\input\" is supplemented (indicated by
dynamically bound variable `TeX-command-text' having string
value.)  Also enclose the file name within \\detokenize{} when
the following three conditions are met:
  1. compiling with standard (pdf)LaTeX or upLaTeX
  2. \" \\input\" is supplemented
  3. EXTRA is non-nil (default when expanding \"%T\")
Adjust dynamically bound variable `TeX-expand-pos' to avoid
possible infinite loop in `TeX-command-expand'.
If PREPROCESS-FN is non-nil then it is called with the filename
as an argument and the result is enclosed instead of the
filename.

Helper function of `TeX-command-expand'. Use only within entries
in `TeX-expand-list-builtin' and `TeX-expand-list'."
  (let* ((raw (TeX-active-master extension nondirectory ask))
         ;; String `TeX-command-text' means that the file name is
         ;; given through \input command.
         (quote-for-space (if (and (stringp TeX-command-text)
                                   (string-match " " raw))
                              "\"" ""))
         (res
          (shell-quote-argument
           (format
            (if (and extra
                     (stringp TeX-command-text)
                     (memq major-mode '(LaTeX-mode docTeX-mode))
                     (memq TeX-engine '(default uptex)))
                ;; Since TeXLive 2018, the default encoding for LaTeX
                ;; files has been changed to UTF-8 if used with
                ;; classic TeX or pdfTeX.  I.e.,
                ;; \usepackage[utf8]{inputenc} is enabled by default
                ;; in (pdf)latex.
                ;; c.f. LaTeX News issue 28
                ;; Due to this change, \detokenize is required to
                ;; recognize non-ascii characters in the file name
                ;; when \input precedes.
                "\\detokenize{ %s }" "%s")
            (concat quote-for-space
                    (if preprocess-fn
                        (funcall preprocess-fn raw)
                      raw)
                    quote-for-space)))))
    ;; Advance past the file name in order to
    ;; prevent expanding any substring of it.
    (setq TeX-expand-pos
          (+ TeX-expand-pos (length res)))
    res))

(defun TeX-check-files (derived originals extensions)
  "Check if DERIVED is newer than any of the ORIGINALS.
Try each original with each member of EXTENSIONS, in all directories
in `TeX-check-path'.  Returns true if any of the ORIGINALS with any of the
EXTENSIONS are newer than DERIVED.  Will prompt to save the buffer of any
ORIGINALS which are modified but not saved yet."
  (let (existingoriginals
        found
        (extensions (TeX-delete-duplicate-strings extensions))
        (buffers (buffer-list)))
    (dolist (path (TeX-delete-duplicate-strings
                   (mapcar (lambda (dir)
                             (expand-file-name (file-name-as-directory dir)))
                           (append
                            TeX-check-path
                            ;; In `TeX-command-default', this function is used to
                            ;; check whether bibliography databases are newer
                            ;; than generated *.bbl files, but bibliography
                            ;; database are relative to `TeX-master-directory'
                            ;; and the test can be run also from included files
                            ;; that are in directories different from
                            ;; `TeX-master-directory'.
                            (list (TeX-master-directory))))))
      (dolist (orig originals)
        (dolist (ext extensions)
          (let ((filepath (concat path orig "." ext)))
            (if (or (file-exists-p filepath)
                    (get-file-buffer filepath))
                (setq existingoriginals (cons filepath existingoriginals)))))))
    (while buffers
      (let* ((buffer (car buffers))
             (name (TeX-buffer-file-name buffer)))
        (setq buffers (cdr buffers))
        (if (and name (member name existingoriginals))
            (progn
              (and (buffer-modified-p buffer)
                   (or (not TeX-save-query)
                       (y-or-n-p (concat "Save file "
                                         (TeX-buffer-file-name  buffer)
                                         "? ")))
                   (with-current-buffer buffer (save-buffer)))))))
    (dolist (eo existingoriginals)
      (if (file-newer-than-file-p eo derived)
          (setq found t)))
    found))

(defcustom TeX-command-sequence-max-runs-same-command 4
  "Maximum number of runs of the same command."
  :type 'integer
  :group 'TeX-command)

(defcustom TeX-command-sequence-max-runs 12
  "Maximum number of total runs."
  :type 'integer
  :group 'TeX-command)

(defvar TeX-command-sequence-count-same-command 1
  "Counter for the runs of the same command in `TeX-command-sequence'.")

(defvar TeX-command-sequence-count 1
  "Counter for the total runs of `TeX-command-sequence'.")

(defvar TeX-command-sequence-last-command nil
  "Last command run in `TeX-command-sequence'.")

(defvar TeX-command-sequence-sentinel nil
  "Sentinel for `TeX-command-sequence'.")

(defvar TeX-command-sequence-file-function nil
  "File function for `TeX-command-sequence'.")

(defvar TeX-command-sequence-command nil
  "Command argument for `TeX-command-sequence'.

It is set in `TeX-command-sequence' and used in
`TeX-command-sequence-sentinel' to call again
`TeX-command-sequence' with the appropriate command argument.")

(defun TeX-command-sequence (command &optional reset file-fn)
  "Run a sequence of TeX commands defined by COMMAND.

The COMMAND argument may be

  * nil: no command will be run in this case

  * a string with a command from `TeX-command-list'

  * a non-nil list of strings, which are commands from
    `TeX-command-list'; the car of the list is used as command to
    be executed in the first run of `TeX-command-sequence', the
    cdr of the list will be passed to the function in the next
    run, etc.

  * a function name, returning a string which is command from
    `TeX-command-list'; it will be funcall'd (without arguments!)
    and used again in the next run of `TeX-command-sequence'.

  * with any other value the function `TeX-command-default' is
    used to determine the command to run, until a stopping
    condition is met.

This function runs at most
`TeX-command-sequence-max-runs-same-command' times the same
command in a row, and `TeX-command-sequence-max-runs' times in
total in any case.  It ends when `TeX-command-Show' is the
command to be run.

A non-nil value for the optional argument RESET means this is the
first run of the function and some variables need to be reset.

FILE-FN is a function of zero arguments returning the current
filename.  Valid choices are `TeX-master-file' (default if
omitted) and `TeX-region-file'."
  (setq TeX-command-sequence-file-function (or file-fn #'TeX-master-file))
  (if (null command)
      (message "No command to run.")
    (let (cmd process)
      (cond
       ((stringp command)
        (setq cmd command
              TeX-command-sequence-command nil))
       ((listp command)
        (setq cmd (pop command)
              TeX-command-sequence-command command))
       ((functionp command)
        (setq cmd (funcall command)
              TeX-command-sequence-command command))
       (t
        ;; We first call `TeX-master-file' with the third argument
        ;; (`ask') set to t, so that the master file is properly set.
        ;; This is also what `TeX-command-master' does.
        (funcall TeX-command-sequence-file-function nil nil t)
        (setq cmd (TeX-command-default TeX-command-sequence-file-function)
              TeX-command-sequence-command t)))
      (TeX-command cmd TeX-command-sequence-file-function 0)
      (when reset
        (setq TeX-command-sequence-count-same-command 1
              TeX-command-sequence-count 1
              TeX-command-sequence-last-command nil))
      (cond
       ;; Stop when the same command has been run
       ;; `TeX-command-sequence-max-runs-same-command' times in a row.
       ((>= TeX-command-sequence-count-same-command
            TeX-command-sequence-max-runs-same-command)
        (message "Stopping after running %S %d times in a row."
                 TeX-command-sequence-last-command
                 TeX-command-sequence-count-same-command))
       ;; Stop when there have been `TeX-command-sequence-max-runs' total
       ;; compilations.
       ((>= TeX-command-sequence-count TeX-command-sequence-max-runs)
        (message "Stopping after %d compilations." TeX-command-sequence-count))
       ;; The command just run is `TeX-command-Show'.
       ((equal command TeX-command-Show))
       ;; In any other case continue: increase counters (when needed), update
       ;; `TeX-command-sequence-last-command' and run the sentinel.
       (t
        (if (equal cmd TeX-command-sequence-last-command)
            (setq TeX-command-sequence-count-same-command
                  (1+ TeX-command-sequence-count-same-command))
          (setq TeX-command-sequence-count-same-command 1))
        (setq TeX-command-sequence-count (1+ TeX-command-sequence-count)
              TeX-command-sequence-last-command cmd)
        (and (setq process (get-buffer-process (current-buffer)))
             (setq TeX-command-sequence-sentinel (process-sentinel process))
             (set-process-sentinel process
                                   #'TeX-command-sequence-sentinel)))))))

(defcustom TeX-save-query t
  "If non-nil, ask user for permission to save files before starting TeX."
  :group 'TeX-command
  :type 'boolean)

(defvar TeX-command-history nil)

(defun TeX-command-default (name-or-file-fn)
  "Guess the next command to be run on NAME-OR-FILE-FN."
  (let ((command-next nil)
        (name (TeX--concat-ext name-or-file-fn)))
    (cond ((if (eq name-or-file-fn #'TeX-region-file)
               (TeX-check-files (TeX-region-file (TeX-output-extension))
                                ;; Each original will be checked for all dirs
                                ;; in `TeX-check-path' so this needs to be just
                                ;; a filename without directory.
                                (list (file-relative-name name))
                                TeX-file-extensions)
             (TeX-save-document name-or-file-fn))
           TeX-command-default)
          ((and (memq major-mode '(docTeX-mode LaTeX-mode))
                ;; Want to know if bib file is newer than .bbl
                ;; We don't care whether the bib files are open in emacs
                (TeX-check-files (TeX--concat-ext name-or-file-fn "bbl")
                                 (mapcar #'car
                                         (LaTeX-bibliography-list))
                                 (append BibTeX-file-extensions
                                         TeX-Biber-file-extensions)))
           ;; We should check for bst files here as well.
           (if (bound-and-true-p LaTeX-using-Biber)
               TeX-command-Biber TeX-command-BibTeX))
          ((and
            ;; Rationale: makeindex should be run when final document is almost
            ;; complete (see
            ;; https://tex-talk.net/2012/09/dont-forget-to-run-makeindex/),
            ;; otherwise, after following latex runs, index pages may change due
            ;; to changes in final document, resulting in extra makeindex and
            ;; latex runs.
            (member
             (setq command-next
                   (TeX-process-get-variable
                    name
                    'TeX-command-next
                    (or (and TeX-PDF-mode (TeX-PDF-from-DVI))
                        TeX-command-Show)))
             (list "Dvips" "Dvipdfmx" TeX-command-Show))
            (cdr (assoc (expand-file-name (TeX--concat-ext name-or-file-fn "idx"))
                        LaTeX-idx-changed-alist)))
           "Index")
          (command-next)
          (TeX-command-Show))))

(defun TeX-command-query (name-or-file-fn)
  "Query the user for what TeX command to use."
  (let* ((default (TeX-command-default name-or-file-fn))
         (completion-ignore-case t)
         (answer (or TeX-command-force
                     (completing-read
                      (format-prompt "Command" default)
                      (TeX-mode-specific-command-list major-mode) nil t
                      nil 'TeX-command-history default))))
    ;; If the answer is "latex" it will not be expanded to "LaTeX"
    (setq answer (car-safe (assoc-string answer TeX-command-list t)))
    (if (and answer
             (not (string-equal answer "")))
        answer
      default)))

(defvar-local TeX-command-next nil
  "The default command next time `TeX-command' is invoked.")

(defun TeX-printer-query (&optional queue)
  "Query the user for a printer name.
QUEUE is non-nil when we are checking for the printer queue."
  (let (command element printer)
    (if queue
        (unless (setq element 2 command TeX-queue-command)
          (error "Need to customize `TeX-queue-command'"))
      (unless (setq element 1 command TeX-print-command)
        (error "Need to customize `TeX-print-command'")))
    (while (progn
             (setq printer (if TeX-printer-list
                               (let ((completion-ignore-case t))
                                 (completing-read
                                  (format "Printer%s: "
                                          (if TeX-printer-default
                                              (format " (default %s)" TeX-printer-default) ""))
                                  TeX-printer-list))
                             ""))
             (setq printer (or (car-safe (assoc-string printer TeX-printer-list t))
                               printer))
             (not (if (or (null printer) (string-equal "" printer))
                      (setq printer TeX-printer-default)
                    (setq TeX-printer-default printer)))))

    (let ((expansion (let ((entry (assoc printer TeX-printer-list)))
                       (or (nth element entry)
                           command))))
      (if (string-match "%p" printer)
          (error "Don't use %s in printer names" "%p"))
      (while (string-match "%p" expansion)
        (setq expansion (replace-match printer t t expansion 0)))
      expansion)))

(defun TeX-style-check (styles)
  "Check STYLES compared to the current style options."
  (let ((files (TeX-style-list)))
    (while (and styles
                (not (TeX-member (car (car styles)) files #'string-match)))
      (setq styles (cdr styles))))
  (if styles
      (nth 1 (car styles))
    ""))

(defun TeX-output-extension ()
  "Get the extension of the current TeX output file."
  (if (listp TeX-output-extension)
      (car TeX-output-extension)
    (or (TeX-process-get-variable (TeX-active-master)
                                  'TeX-output-extension
                                  TeX-output-extension)
        TeX-output-extension)))

(defun TeX-view-mouse (event)
  "Start `TeX-view' at mouse position."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-start event)))
    (goto-char (posn-point (event-start event)))
    (TeX-view)))

(defun TeX-region-update-point ()
  "Syncs the location of point in the region file with the current file.

Thereafter, point in the region file is on the same text as in
the current buffer.

Do nothing in case the last command hasn't operated on the region
or `TeX-source-correlate-mode' is disabled."
  (when (and TeX-current-process-region-p TeX-source-correlate-mode)
    (let ((region-buf (get-file-buffer (TeX-region-file t)))
          (orig-line (TeX-current-offset))
          (pos-in-line (- (point) (max (line-beginning-position)
                                       (or TeX-command-region-begin
                                           (region-beginning))))))
      (when region-buf
        (with-current-buffer region-buf
          (goto-char (point-min))
          (when (re-search-forward "!offset(\\(-?[0-9]+\\)" nil t)
            (let ((offset (string-to-number (match-string 1))))
              (goto-char (point-min))
              (forward-line (- orig-line offset))
              (forward-char pos-in-line))))))))

(defun TeX-view ()
  "Start a viewer without confirmation.
The viewer is started either on region or master file,
depending on the last command issued."
  (interactive)
  (let ((output-file (TeX-active-master (TeX-output-extension))))
    (if (file-exists-p output-file)
        (TeX-command "View" #'TeX-active-master 0)
      (message "Output file %S does not exist." output-file))))

;;; Command Hooks

(defvar TeX-after-compilation-finished-functions nil
  "Hook being run after TeX/LaTeX/ConTeXt finished successfully.
The functions in this hook are run with the DVI/PDF output file
given as argument.  Using this hook can be useful for updating
the viewer automatically after re-compilation of the document.

If you use an emacs-internal viewer such as `doc-view-mode' or
`pdf-view-mode', add `TeX-revert-document-buffer' to this hook.")

(make-obsolete-variable 'TeX-after-TeX-LaTeX-command-finished-hook
                        'TeX-after-compilation-finished-functions
                        "11.89")

(defun TeX-revert-document-buffer (file)
  "Revert the buffer visiting FILE.
This function is intended to be used in
`TeX-after-compilation-finished-functions' for users that view
their compiled document with an emacs viewer such as
`doc-view-mode' or `pdf-view-mode'.  (Note that this function
just calls `revert-buffer' in the respective buffer and thus
requires that the corresponding mode defines a sensible
`revert-buffer-function'.)"
  (let ((buf (find-buffer-visiting file)))
    (when buf
      (with-current-buffer buf
        (revert-buffer nil t t)))))

(defvar TeX-after-start-process-function
  #'TeX-adjust-process-coding-system
  "Function to adjust coding system of an asynchronous process.
Called with one argument PROCESS.")

(defun TeX-adjust-process-coding-system (process)
  "Adjust coding system of PROCESS to suitable value.
Usually coding system is the same as the TeX file with eol format
adjusted to OS default value.  Take care of Japanese TeX, which
requires special treatment."
  (if (and (boundp 'japanese-TeX-mode)
           (fboundp 'japanese-TeX-set-process-coding-system)
           (with-current-buffer TeX-command-buffer
             japanese-TeX-mode))
      (japanese-TeX-set-process-coding-system process)
    (let ((cs (coding-system-base (with-current-buffer TeX-command-buffer
                                    buffer-file-coding-system))))
      ;; The value of `buffer-file-coding-system' is sometimes
      ;; undecided-{unix,dos,mac}.  That happens when the file
      ;; contains no multibyte chars and only end of line format is
      ;; determined.  Emacs lisp reference recommends not to use
      ;; undecided-* for process coding system, so it might seem
      ;; reasonable to change `undecided' into some fixed coding
      ;; system like this:
      ;; (if (eq 'undecided cs)
      ;;     (setq cs 'utf-8))
      ;; However, that can lose when the following conditions are met:
      ;; (1) The document is divided into multiple files.
      ;; (2) The command buffer contains no multibyte chars.
      ;; (3) The other files contain mutlibyte chars and saved in a
      ;;     coding system other than the one chosen above.
      ;; So we leave `undecided' unchanged here.  Although `undecided'
      ;; is not quite safe for the coding system for encoding, i.e.,
      ;; keyboard input to the TeX process, we expect that this does
      ;; not raise serious problems because it is pretty rare that TeX
      ;; process needs keyboard input of multibyte chars.

      ;; `utf-8-with-signature' (UTF-8 with BOM) doesn't suit at all
      ;; for the coding system for encoding because it always injects
      ;; 3-byte BOM in front of its return value (even when the string
      ;; to be sent has only ascii characters!)  Thus we change it
      ;; into `utf-8'.  On decoding, `utf-8' can decode UTF-8 with
      ;; BOM.  So it is safe for both decoding and encoding.
      (if (eq cs 'utf-8-with-signature)
          (setq cs 'utf-8))

      ;; Eol format of TeX files can differ from OS default. TeX
      ;; binaries accept all type of eol format in the given files
      ;; and output messages according to OS default.  So we set eol
      ;; format to OS default value.
      (setq cs (coding-system-change-eol-conversion
                cs
                ;; The eol of macosX is LF, not CR.  So we choose
                ;; other than `unix' only for w32 system.
                ;; FIXME: what should we do for cygwin?
                (if (eq system-type 'windows-nt) 'dos 'unix)))
      (set-process-coding-system process cs cs))))

(defcustom TeX-show-compilation nil
  "If non-nil, show output of TeX compilation in other window."
  :group 'TeX-command
  :type 'boolean)

(defvar TeX-suppress-compilation-message nil
  "If non-nil, suppress \"display results of compilation\" message.")

(defun TeX-run-command (name command file)
  "Create a process for NAME using COMMAND to process FILE.
Return the new process."
  (let ((default TeX-command-default)
        (buffer (TeX-process-buffer-name file))
        (dir (TeX-master-directory))
        (command-buff (current-buffer)))
    (TeX-process-check file)            ; Check that no process is running
    (setq-default TeX-command-buffer command-buff)
    (get-buffer-create buffer)
    (set-buffer buffer)
    (buffer-disable-undo)
    (erase-buffer)
    (setq-local line-number-display-limit 0)
    (setq TeX-output-extension nil)
    (setq-local TeX-command-buffer command-buff)
    (if dir (cd dir))
    (insert "Running `" name "' on `" file "' with ``" command "''\n")
    (TeX-output-mode)
    (if TeX-show-compilation
        (display-buffer buffer)
      (unless TeX-suppress-compilation-message
        (message "Type `%s' to display results of compilation."
                 (substitute-command-keys
                  "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]"))))
    (setq TeX-parse-function #'TeX-parse-command)
    (setq TeX-command-default default)
    (setq TeX-sentinel-function
          (lambda (_process name)
            (message (concat name ": done."))))
    (if TeX-process-asynchronous
        (let ((process (start-process name buffer TeX-shell
                                      TeX-shell-command-option command)))
          (if TeX-after-start-process-function
              (funcall TeX-after-start-process-function process))
          (TeX-command-mode-line process)
          (set-process-filter process #'TeX-command-filter)
          (set-process-sentinel process #'TeX-command-sentinel)
          (set-marker (process-mark process) (point-max))
          (require 'compile)
          (setq compilation-in-progress (cons process compilation-in-progress))
          process)
      (setq mode-line-process ": run")
      (force-mode-line-update)
      (call-process TeX-shell nil buffer nil
                    TeX-shell-command-option command))))

(defun TeX-run-set-command (name command)
  "Remember TeX command to use to NAME and set corresponding output extension."
  (setq TeX-command-default name
        TeX-output-extension
        (if (and (null (TeX-PDF-from-DVI)) TeX-PDF-mode) "pdf" "dvi"))
  (let ((case-fold-search t)
        (lst TeX-command-output-list))
    (while lst
      (if (string-match (car (car lst)) command)
          (setq TeX-output-extension (car (cdr (car lst)))
                lst nil)
        (setq lst (cdr lst))))))

(defvar TeX-error-report-switches nil
  "Reports presence of errors after `TeX-run-TeX'.
Actually, `TeX-run-format' sets it.
To test whether the current buffer has a compile error from last
run of `TeX-run-format', use
  (TeX-error-report-has-errors-p)")

(defun TeX-error-report-has-errors-p ()
  "Return non-nil if current buffer has compile errors from last TeX run."
  (plist-get TeX-error-report-switches (intern (TeX-master-file))))

(defun TeX-run-format (name command file)
  "Create a process for NAME using COMMAND to format FILE with TeX."
  (TeX-run-set-command name command)
  (let ((current-master (TeX-master-file))
        (buffer (TeX-process-buffer-name file))
        (process (TeX-run-command name command file)))

    ;; Save information in TeX-error-report-switches
    ;; Initialize error to nil (no error) for current master.
    ;; Presence of error is reported inside `TeX-TeX-sentinel-check'

    ;; the current master file is saved because error routines are
    ;; parsed in other buffers;
    (setq TeX-error-report-switches
          (plist-put TeX-error-report-switches
                     'TeX-current-master current-master))
    ;; reset error to nil (no error)
    (setq TeX-error-report-switches
          (plist-put TeX-error-report-switches
                     (intern current-master) nil))

    ;; Hook to TeX debugger.
    (with-current-buffer buffer
      (TeX-parse-reset)
      (setq TeX-parse-function #'TeX-parse-TeX)
      (setq TeX-sentinel-function #'TeX-TeX-sentinel)
      (if TeX-process-asynchronous
          (progn
            ;; Updating the mode line.
            (setq TeX-current-page "[0]")
            (TeX-format-mode-line process)
            (set-process-filter process #'TeX-format-filter)))
      process)))

(defun TeX-run-TeX (name command file)
  "Create a process for NAME using COMMAND to format FILE with TeX."

  (let ((idx-file nil) (element nil))
    ;; Store md5 hash of the index file before running LaTeX.
    (and (memq major-mode '(docTeX-mode LaTeX-mode))
         (prog1 (file-exists-p
                 (setq idx-file (expand-file-name (TeX-active-master "idx"))))
           ;; In order to avoid confusion and pollution of
           ;; `LaTeX-idx-md5-alist', remove from this alist all md5 hashes of
           ;; the current index file.  Note `assq-delete-all' doesn't work with
           ;; string keys and has problems with non-list elements in Emacs 21
           ;; (see file tex-site.el).
           (while (setq element (assoc idx-file LaTeX-idx-md5-alist))
             (setq LaTeX-idx-md5-alist (delq element LaTeX-idx-md5-alist))))
         (with-temp-buffer
           (insert-file-contents-literally idx-file)
           (push (cons idx-file (md5 (current-buffer))) LaTeX-idx-md5-alist))))

  ;; can we assume that TeX-sentinel-function will not be changed
  ;; during (TeX-run-format ..)? --pg
  ;; rather use let* ? --pg

  (if TeX-interactive-mode
      (TeX-run-interactive name command file)
    (let* ((sentinel-function TeX-sentinel-default-function)
           (process (TeX-run-format name command file)))
      (setq TeX-sentinel-function sentinel-function)
      (if TeX-process-asynchronous
          process
        (TeX-synchronous-sentinel name file process)))))

;; backward compatibilty

(defalias 'TeX-run-LaTeX #'TeX-run-TeX)


(defun TeX-run-BibTeX (name command file)
  "Create a process for NAME using COMMAND to format FILE with BibTeX."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'TeX-BibTeX-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-Biber (name command file)
  "Create a process for NAME using COMMAND to format FILE with Biber."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'TeX-Biber-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-dvips (name command file)
  "Create a process for NAME using COMMAND to convert FILE with dvips."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'TeX-dvips-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-dvipdfmx (name command file)
  "Create a process for NAME using COMMAND to convert FILE with dvipdfmx."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'TeX-dvipdfmx-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-ps2pdf (name command file)
  "Create a process for NAME using COMMAND to convert FILE with ps2pdf."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'TeX-ps2pdf-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-index (name command file)
  "Create a process for NAME using COMMAND to compile the index file."
  (let ((process (TeX-run-command name command file))
        (element nil))
    (setq TeX-sentinel-function #'TeX-index-sentinel)
    ;; Same cleaning as that for `LaTeX-idx-md5-alist' in `TeX-run-TeX'.
    (while (setq element
                 ;; `file' has been determined in `TeX-command-buffer', while
                 ;; this function has `TeX-master-directory' as
                 ;; `default-directory', then we have to expand `file' file-name
                 ;; in the same directory of `TeX-command-buffer'.
                 (assoc (with-current-buffer TeX-command-buffer
                            (expand-file-name (TeX-active-master "idx")))
                        LaTeX-idx-changed-alist))
      (setq LaTeX-idx-changed-alist (delq element LaTeX-idx-changed-alist)))
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-run-compile (_name command _file)
  "Ignore first and third argument, start compile with second argument."
  (let ((default-directory (TeX-master-directory)))
    (setq TeX-command-buffer (compile command)))
  ;; Make `compilation-mode' recognize file names with spaces.
  ;; (bug#36483)
  ;; FIXME: This is just an ad-hoc workaround and it's better to fix
  ;; the regular expression in compile.el properly, if possible.  But
  ;; there was no response to such request in emacs-devel@gnu.org.
  (require 'compile)
  (with-current-buffer TeX-command-buffer
    (make-local-variable 'compilation-error-regexp-alist)
    ;; Add slightly modified entry of the one associated with `comma'
    ;; in `compilation-error-regexp-alist-alist' to pick up file names
    ;; with spaces.
    (add-to-list 'compilation-error-regexp-alist
                 '("^\"\\([^,\"\n\t]+\\)\", line \\([0-9]+\\)\
\\(?:[(. pos]+\\([0-9]+\\))?\\)?[:.,; (-]\\( warning:\\|[-0-9 ]*(W)\\)?" 1 2 3 (4))
                 t)))

(defun TeX-run-shell (_name command _file)
  "Ignore first and third argument, start shell-command with second argument."
  (let ((default-directory (TeX-master-directory)))
    (shell-command command)
    (if (eq system-type 'ms-dos)
        (redraw-display))))

(defun TeX-run-discard (_name command _file)
  "Start COMMAND as process, discarding its output.
NAME and FILE are ignored."
  (let ((default-directory (TeX-master-directory)))
    (call-process TeX-shell
                  nil 0 nil
                  TeX-shell-command-option
                  command)))

(defun TeX-run-discard-foreground (_name command _file)
  "Call process with second argument in the foreground, discarding its output.
With support for MS-DOS, especially when dviout is used with PC-9801 series."
  (if (and (boundp 'dos-machine-type) (eq dos-machine-type 'pc98)) ;if PC-9801
      (send-string-to-terminal "\e[2J")) ; clear screen
  (call-process TeX-shell (if (eq system-type 'ms-dos) "con") nil nil
                TeX-shell-command-option command)
  (if (eq system-type 'ms-dos)
      (redraw-display)))
(defalias 'TeX-run-dviout #'TeX-run-discard-foreground)

(defun TeX-run-background (name command _file)
  "Start process with second argument, show output when and if it arrives."
  (let ((dir (TeX-master-directory)))
    (set-buffer (get-buffer-create "*TeX background*"))
    (if dir (cd dir))
    (erase-buffer)
    (let ((process (start-process (concat name " background")
                                  nil TeX-shell
                                  TeX-shell-command-option command)))
      (if TeX-after-start-process-function
          (funcall TeX-after-start-process-function process))
      (set-process-filter process #'TeX-background-filter)
      (set-process-query-on-exit-flag process nil))))

(defun TeX-run-silent (name command _file)
  "Start process with second argument."
  (let ((dir (TeX-master-directory)))
    (set-buffer (get-buffer-create "*TeX silent*"))
    (if dir (cd dir))
    (erase-buffer)
    (let ((process (start-process (concat name " silent")
                                  (current-buffer) TeX-shell
                                  TeX-shell-command-option command)))
      (if TeX-after-start-process-function
          (funcall TeX-after-start-process-function process))
      (set-process-query-on-exit-flag process nil))))

(defun TeX-run-interactive (name command file)
  "Run TeX interactively.
Run command in a buffer (in comint-shell-mode) so that it accepts user
interaction.  If you return to the file buffer after the TeX run,
Error parsing on \\[next-error] should work with a bit of luck."
  (TeX-run-set-command name command)
  (require 'comint)
  (let ((default TeX-command-default)
        (buffer (TeX-process-buffer-name file))
        (process nil)
        (dir (TeX-master-directory))
        (command-buff (current-buffer))
        (sentinel-function TeX-sentinel-default-function)) ; inherit from major mode
    (TeX-process-check file)            ; Check that no process is running
    (setq-default TeX-command-buffer command-buff)
    (with-output-to-temp-buffer buffer)
    (set-buffer buffer)
    (setq-local TeX-command-buffer command-buff)
    (setq buffer-read-only nil)
    (if dir (cd dir))
    (insert "Running `" name "' on `" file "' with ``" command "''\n")
    (comint-exec buffer name TeX-shell nil
                 (list TeX-shell-command-option command))
    (comint-mode)
    (add-hook 'comint-output-filter-functions #'TeX-interactive-goto-prompt)
    (setq mode-name name)
    (setq TeX-command-default default)
    (setq process (get-buffer-process buffer))
    (if TeX-after-start-process-function
        (funcall TeX-after-start-process-function process))
    (TeX-command-mode-line process)
    (set-process-sentinel process #'TeX-command-sentinel)
    (set-marker (process-mark process) (point-max))
    (require 'compile)
    (setq compilation-in-progress (cons process compilation-in-progress))
    (TeX-parse-reset)
    (setq TeX-parse-function #'TeX-parse-TeX)
    ;; use the sentinel-function that the major mode sets, not the LaTeX one
    (setq TeX-sentinel-function sentinel-function)))

(defun TeX-run-function (_name command _file)
  "Execute Lisp function or function call given as the string COMMAND.
Parameters NAME and FILE are ignored."
  (let ((fun (car (read-from-string command))))
    (if (functionp fun) (funcall fun) (eval fun t))))

(defun TeX-run-discard-or-function (name command file)
  "Start COMMAND as process or execute it as a Lisp function.
If run as a process, the output is discarded.  COMMAND is
expected to be a string.  NAME and FILE are ignored."
  (if (functionp (car (read-from-string command)))
      (TeX-run-function name command file)
    (TeX-run-discard name command file)))

(defun TeX-run-ispell-on-document (_command _ignored _name)
  "Run Ispell on all open files belonging to the current document.
This function is *obsolete* and only here for compatibility
reasons.  Use `TeX-run-function' instead."
  (interactive)
  (TeX-ispell-document ""))
(make-obsolete 'TeX-run-ispell-on-document 'TeX-run-function "2006-02-07")

;;; Command Sentinels

(defun TeX-synchronous-sentinel (name file result)
  "Process TeX command output buffer after the process dies."
  (let ((buffer (TeX-process-buffer (file-name-nondirectory file))))
    (with-current-buffer buffer

      ;; Append post-mortem information to the buffer
      (goto-char (point-max))
      (insert "\n" mode-name (if (and result (zerop result))
                                 " finished" " exited") " at "
              (substring (current-time-string) 0 -5))
      (setq mode-line-process ": exit")

      ;; Do command specific actions.
      (setq TeX-command-next TeX-command-Show)
      (goto-char (point-min))
      (apply TeX-sentinel-function nil name nil)

      ;; Force mode line redisplay soon
      (force-mode-line-update))))

(defun TeX-command-sentinel (process msg)
  "Process TeX command output buffer after the PROCESS dies.
Insert MSG with some additional information."
  ;; Set `TeX-transient-master' here because `preview-parse-messages'
  ;; may open files and thereby trigger master file questions which we
  ;; don't want and need because we already know the master.  Use
  ;; `TeX-master-file' instead of `TeX-active-master' to determine the
  ;; master because the region file should never be the master.
  (let* ((TeX-transient-master (TeX-master-file))
         (buffer (process-buffer process))
         (name (process-name process)))
    (cond ((null (buffer-name buffer))  ; buffer killed
           (set-process-buffer process nil)
           (set-process-sentinel process nil))
          ((memq (process-status process) '(signal exit))
           (with-current-buffer buffer

             ;; Append post-mortem information to the buffer
             (goto-char (point-max))
             (insert-before-markers "\n" mode-name " " msg)
             (forward-char -1)
             (insert " at "
                     (substring (current-time-string) 0 -5))
             (forward-char 1)

             ;; Do command specific actions.
             (TeX-command-mode-line process)
             (setq TeX-command-next TeX-command-Show)
             (goto-char (point-min))
             (apply TeX-sentinel-function process name nil)


             ;; If buffer and mode line will show that the process
             ;; is dead, we can delete it now.  Otherwise it
             ;; will stay around until M-x list-processes.
             (delete-process process))

           ;; Force mode line redisplay soon
           ;; Do this in all buffers (bug#38058 and bug#40965)
           (force-mode-line-update t))))

  (setq compilation-in-progress (delq process compilation-in-progress)))


(defvar-local TeX-sentinel-function (lambda (_process _name) nil)
  "Hook to cleanup TeX command buffer after termination of PROCESS.
NAME is the name of the process.")

(defvar-local TeX-sentinel-default-function (lambda (_process _name) nil)
  "Default for `TeX-sentinel-function'.  To be set in major mode.
Hook to cleanup TeX command buffer after termination of PROCESS.
NAME is the name of the process.")

(defun TeX-TeX-sentinel (process name)
  "Cleanup TeX output buffer after running TeX.

Parse the output buffer to collect errors and warnings if the
variable `TeX-parse-all-errors' is non-nil.

Open the error overview if
`TeX-error-overview-open-after-TeX-run' is non-nil and there are
errors or warnings to show."
  (if (TeX-TeX-sentinel-check process name)
      (progn
        (if TeX-parse-all-errors
            (TeX-parse-all-errors))
        (if (and (with-current-buffer TeX-command-buffer
                   TeX-error-overview-open-after-TeX-run)
                 (TeX-error-overview-make-entries
                  (TeX-master-directory) (TeX-active-buffer)))
            (TeX-error-overview)))
    (message (concat name ": formatted " (TeX-current-pages)))
    (let (dvi2pdf)
      (if (with-current-buffer TeX-command-buffer
            (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
          (setq TeX-command-next dvi2pdf)
        (setq TeX-command-next TeX-command-Show)))))

(defun TeX-current-pages ()
  "Return string indicating the number of pages formatted."
  (cond ((null TeX-current-page)
         "some pages")
        ((string-match "[^0-9]1[^0-9]" TeX-current-page)
         (concat TeX-current-page " page"))
        (t
         (concat TeX-current-page " pages"))))

(defun TeX-TeX-sentinel-check (process name)
  "Cleanup TeX output buffer after running TeX.
Return nil only if no errors were found."
  (save-excursion
    (goto-char (point-max))
    (cond
     ((and (string-match "ConTeXt" name) (boundp 'ConTeXt-Mark-version)
           (with-current-buffer TeX-command-buffer
             (string= ConTeXt-Mark-version "IV")))
      (when (re-search-backward " > result saved in file: \\(.*?\\), " nil t)
        (let ((output-file (TeX-match-buffer 1)))
          ;; Shave off quotation marks if present.
          (when (string-match "\\`\"\\(.*\\)\"\\'" output-file)
            (setq output-file (match-string 1 output-file)))
          (setq TeX-output-extension
                (if (string-match "\\.\\([^.]*\\)$" output-file)
                    (match-string 1 output-file)
                  "dvi")))
        (if (re-search-forward ", \\([0-9]+\\) shipped pages, " nil t)
            (setq TeX-current-page (concat "{" (TeX-match-buffer 1) "}")))))
     (t
      (if (re-search-backward "^Output written on \\(.*?\\) (\\([0-9]+\\) page"
                              nil t)
          (let ((output-file (TeX-match-buffer 1)))
            (setq TeX-current-page (concat "{" (TeX-match-buffer 2) "}"))
            ;; Shave off quotation marks if present.
            (when (string-match "\\`\"\\(.*\\)\"\\'" output-file)
              (setq output-file (match-string 1 output-file)))
            (setq TeX-output-extension
                  (if (string-match "\\.\\([^.]*\\)$" output-file)
                      (match-string 1 output-file)
                    "dvi")))))))
  (if process (TeX-format-mode-line process))
  (if (catch 'found
        (while (re-search-forward "^\\(?:!\\|\\(.+?\\):[0-9]+:\\) " nil t)
          (if (or (not (match-beginning 1))
                  ;; Ignore non-error warning. (bug#55065)
                  (file-exists-p (TeX-match-buffer 1)))
              (throw 'found t))))
      (progn
        (if TeX-error-overview-open-after-TeX-run
            ;; Don't leave inconsistent message.
            (message nil)
          (message "%s errors in `%s'. Use %s to display." name (buffer-name)
                   (substitute-command-keys
                    "\\<TeX-mode-map>\\[TeX-next-error]")))
        (setq TeX-command-next TeX-command-default)
        ;; error reported to TeX-error-report-switches
        (setq TeX-error-report-switches
              (plist-put TeX-error-report-switches
                         (intern (plist-get TeX-error-report-switches
                                            'TeX-current-master))
                         t))
        t)
    ;; In case that there were only non-error warnings of type
    ;; bug#55065, restore point to the initial position.
    (goto-char (point-min))
    (let (dvi2pdf)
        (if (with-current-buffer TeX-command-buffer
           (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
         (setq TeX-command-next dvi2pdf)
       (setq TeX-command-next TeX-command-Show)))
    nil))

;; This regexp should catch warnings of the type
;;   LaTeX Warning: ...
;;   LaTeX Font Warning: ...
;;   Package xyz123 Warning: ...
;;   Class xyz123 Warning: ...
(defvar LaTeX-warnings-regexp
  "\\(?:LaTeX\\|Class\\|Package\\|\\*\\) [-A-Za-z0-9]* ?[Ww]arning:"
  "Regexp matching LaTeX warnings.")

(defun TeX-LaTeX-sentinel-has-warnings ()
  "Return non-nil, if the output buffer contains warnings.
Warnings can be indicated by LaTeX or packages."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (concat "^" LaTeX-warnings-regexp) nil t)))

(defun TeX-LaTeX-sentinel-has-bad-boxes ()
  "Return non-nil, if LaTeX output indicates overfull or underfull boxes."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\(Ov\\|Und\\)erfull \\\\" nil t)))

;; should go into latex.el? --pg
(defun TeX-LaTeX-sentinel (process name)
  "Cleanup TeX output buffer after running LaTeX.

Parse the output buffer to collect errors and warnings if the
variable `TeX-parse-all-errors' is non-nil.

Open the error overview if
`TeX-error-overview-open-after-TeX-run' is non-nil and there are
errors or warnings to show."
  (if TeX-parse-all-errors
      (TeX-parse-all-errors))
  (if (and (with-current-buffer TeX-command-buffer
             TeX-error-overview-open-after-TeX-run)
           (TeX-error-overview-make-entries
            (TeX-master-directory) (TeX-active-buffer)))
      (TeX-error-overview))
  (cond ((TeX-TeX-sentinel-check process name))
        ((and (save-excursion
                (re-search-forward
                 "^Package biblatex Warning: Please (re)run Biber on the file"
                 nil t))
              (with-current-buffer TeX-command-buffer
                (and (LaTeX-bibliography-list)
                     (TeX-check-files (TeX-master-file "bbl")
                                      (TeX-style-list)
                                      (append TeX-file-extensions
                                              BibTeX-file-extensions
                                              TeX-Biber-file-extensions)))))
         (message "%s%s" "You should run Biber to get citations right, "
                  (TeX-current-pages))
         (setq TeX-command-next (with-current-buffer TeX-command-buffer
                                  TeX-command-Biber)))
        ((and (save-excursion
                (re-search-forward
                 "^\\(?:LaTeX\\|Package natbib\\) Warning: Citation" nil t))
              (with-current-buffer TeX-command-buffer
                (and (LaTeX-bibliography-list)
                     (TeX-check-files (TeX-master-file "bbl")
                                      (TeX-style-list)
                                      (append TeX-file-extensions
                                              BibTeX-file-extensions
                                              TeX-Biber-file-extensions)))))
         (message "%s%s" "You should run BibTeX to get citations right, "
                  (TeX-current-pages))
         (setq TeX-command-next (with-current-buffer TeX-command-buffer
                                  TeX-command-BibTeX)))
        ((re-search-forward "Package biblatex Warning: Please rerun LaTeX" nil t)
         (message "%s%s" "You should run LaTeX again, " (TeX-current-pages))
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "^(biblatex)\\W+Page breaks have changed" nil t)
         (message "%s%s" "You should run LaTeX again - page breaks have changed, "
                  (TeX-current-pages))
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "^\\(?:LaTeX Warning: Label(s)\\|\
Package natbib Warning: Citation(s)\\)" nil t)
         (message "%s%s" "You should run LaTeX again to get references right, "
                  (TeX-current-pages))
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward
          "^\\(?:(rerunfilecheck)\\|Package hyperref Warning:\\)\\W+\
Rerun to get outlines right" nil t)
         (message "%s%s" "You should run LaTeX again to get outlines right, "
                  (TeX-current-pages))
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "^LaTeX Warning: Reference" nil t)
         (message "%s%s%s" name ": there were unresolved references, "
                  (TeX-current-pages))
         (let (dvi2pdf)
           (if (with-current-buffer TeX-command-buffer
                 (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
               (setq TeX-command-next dvi2pdf)
             (setq TeX-command-next TeX-command-Show))))
        ((re-search-forward "^\\(?:LaTeX Warning: Citation\\|\
Package natbib Warning:.*undefined citations\\)" nil t)
         (message "%s%s%s" name ": there were unresolved citations, "
                  (TeX-current-pages))
         (let (dvi2pdf)
           (if (with-current-buffer TeX-command-buffer
                 (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
               (setq TeX-command-next dvi2pdf)
             (setq TeX-command-next TeX-command-Show))))
        ((re-search-forward "^No file .*\\.\\(toc\\|lof\\|lot\\)\\.$" nil t)
         (message "%s" (concat "You should run LaTeX again to get "
                               (upcase (match-string-no-properties 1))
                               " right"))
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "Package longtable Warning: Table widths have \
changed\\. Rerun LaTeX\\." nil t)
         (message
          "%s" "You should run LaTeX again to get table formatting right")
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "^hf-TikZ Warning: Mark '.*' changed\\. \
Rerun to get mark in right position\\." nil t)
         (message
          "%s" "You should run LaTeX again to get TikZ marks in right position")
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "^Package Changebar Warning: \
Changebar info has changed." nil t)
         (message
          "%s" "You should run LaTeX again to get the change bars right")
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "^LaTeX Warning: Endnotes may have changed. \
Rerun to get them right" nil t)
         (message
          "%s" "You should run LaTeX again to get the endnotes right")
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "^\\* xsim warning: \"rerun\"" nil t)
         (message
          "%s" "You should run LaTeX again to synchronize exercise properties")
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward
          TeX-LaTeX-sentinel-banner-regexp nil t)
         (let* ((warnings (and TeX-debug-warnings
                               (TeX-LaTeX-sentinel-has-warnings)))
                (bad-boxes (and TeX-debug-bad-boxes
                                (TeX-LaTeX-sentinel-has-bad-boxes)))
                (add-info (when (or warnings bad-boxes)
                            (concat " (with "
                                    (when warnings "warnings")
                                    (when (and warnings bad-boxes) " and ")
                                    (when bad-boxes "bad boxes")
                                    ")"))))
           (message "%s" (concat name ": successfully formatted "
                                 (TeX-current-pages) add-info)))
         (let (dvi2pdf)
           (if (with-current-buffer TeX-command-buffer
                 (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
               (setq TeX-command-next dvi2pdf)
             (setq TeX-command-next TeX-command-Show))))
        (t
         (message "%s%s%s" name ": problems after " (TeX-current-pages))
         (setq TeX-command-next TeX-command-default)))

  ;; Check whether the idx file changed.
  (let (idx-file)
    (and (file-exists-p
          (setq idx-file
                (with-current-buffer TeX-command-buffer
                  (expand-file-name (TeX-active-master "idx")))))
         ;; imakeidx package automatically runs makeindex, thus, we need to be
         ;; sure .ind file isn't newer than .idx.
         (TeX-check-files (with-current-buffer TeX-command-buffer
                            (expand-file-name (TeX-active-master "ind")))
                          (with-current-buffer TeX-command-buffer
                            (list (file-name-nondirectory (TeX-active-master))))
                          '("idx"))
         (with-temp-buffer
           (insert-file-contents-literally idx-file)
           (not (equal
                 ;; Compare old md5 hash of the idx file with the new one.
                 (cdr (assoc idx-file LaTeX-idx-md5-alist))
                 (md5 (current-buffer)))))
         (push (cons idx-file t) LaTeX-idx-changed-alist)))

  (unless (TeX-error-report-has-errors-p)
    (run-hook-with-args 'TeX-after-compilation-finished-functions
                        (with-current-buffer TeX-command-buffer
                          (expand-file-name
                           (TeX-active-master (TeX-output-extension)))))))

;; should go into latex.el? --pg
(defun TeX-BibTeX-sentinel (_process _name)
  "Cleanup TeX output buffer after running BibTeX."
  (goto-char (point-max))
  (cond
   ;; Check whether BibTeX reports any warnings or errors.
   ((re-search-backward (concat
                         "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
                         "\\(warnings?\\|error messages?\\))")
                        nil t)
    ;; Tell the user their number so that she sees whether the
    ;; situation is getting better or worse.
    (message (concat "BibTeX finished with %s %s. "
                     "Type `%s' to display output.")
             (match-string 1) (match-string 2)
             (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))
   (t
    (message (concat "BibTeX finished successfully. "
                     "Run LaTeX again to get citations right."))))
  ;; In any case, run the default next command.
  (setq TeX-command-next TeX-command-default))

(defun TeX-Biber-sentinel (_process _name)
  "Cleanup TeX output buffer after running Biber."
  (goto-char (point-max))
  (cond
   ((re-search-backward "^INFO - \\(WARNINGS\\|ERRORS\\): \\([0-9]+\\)" nil t)
    (message (concat "Biber finished with %s %s. "
                     "Type `%s' to display output.")
             (match-string 2) (downcase (match-string 1))
             (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]"))
    (setq TeX-command-next TeX-command-default))
   ((re-search-backward "^FATAL" nil t)
    (message (concat "Biber had a fatal error and did not finish! "
                     "Type `%s' to display output.")
             (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]"))
    (setq TeX-command-next TeX-command-Biber))
   (t
    (message (concat "Biber finished successfully. "
                     "Run LaTeX again to get citations right."))
    (setq TeX-command-next TeX-command-default))))

(defun TeX-dvips-sentinel (_process _name)
  "Cleanup TeX output buffer after running dvips."
  (goto-char (point-max))
  (cond
   ((search-backward "TeX Output exited abnormally" nil t)
    (message "Dvips failed.  Type `%s' to display output."
             (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))
   (t
    (if (with-current-buffer TeX-command-buffer
          (and (equal (TeX-PDF-from-DVI) "Dvips") TeX-PDF-mode))
        (setq TeX-output-extension "ps"
              TeX-command-next "Ps2pdf"))
    (message "Dvips finished successfully. "))))

(defun TeX-dvipdfmx-sentinel (_process _name)
  "Cleanup TeX output buffer after running dvipdfmx."
  (goto-char (point-max))
  (cond
   ((search-backward "TeX Output exited abnormally" nil t)
    (message "Dvipdfmx failed.  Type `%s' to display output."
             (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))
   (t
    (if (with-current-buffer TeX-command-buffer
          (and (equal (TeX-PDF-from-DVI) "Dvipdfmx") TeX-PDF-mode))
        (setq TeX-output-extension "pdf"
              TeX-command-next TeX-command-Show))
    (message "Dvipdfmx finished successfully. "))))

(defun TeX-ps2pdf-sentinel (_process _name)
  "Cleanup TeX output buffer after running ps2pdf."
  (goto-char (point-max))
  (cond
   ((search-backward "TeX Output exited abnormally" nil t)
    (message "ps2pdf failed.  Type `%s' to display output."
             (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))
   (t
    (if (with-current-buffer TeX-command-buffer
          (and (equal (TeX-PDF-from-DVI) "Dvips") TeX-PDF-mode))
        (setq TeX-command-next TeX-command-Show
              TeX-output-extension "pdf"))
    (message "ps2pdf finished successfully. "))))

(defun TeX-index-sentinel (_process _name)
  "Cleanup TeX output buffer after compiling index."
  (goto-char (point-max))
  (cond
   ((search-backward "TeX Output exited abnormally" nil t)
    (message "Index failed.  Type `%s' to display output."
             (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))
   (t
    (setq TeX-command-next TeX-command-default)
    (message (concat "Index finished successfully. "
                     "Run LaTeX again to get index right.")))))

(defun TeX-command-sequence-sentinel (process string)
  "Call the appropriate sentinel for the current PROCESS.
Pass two arguments PROCESS and STRING to the sentinel.

If there are no errors, call back `TeX-command-sequence' using
`TeX-command-sequence-command' as command argument, unless this
variable is nil."
  (with-current-buffer (process-buffer process)
    (funcall TeX-command-sequence-sentinel process string)
    (if (string-match "\\(finished\\|exited\\)" string)
        (with-current-buffer TeX-command-buffer
          (unless
              (or
               (TeX-error-report-has-errors-p)
               (null TeX-command-sequence-command))
            (TeX-command-sequence TeX-command-sequence-command nil
                                  TeX-command-sequence-file-function))))))

;;; Process Control

(defun TeX-process-get-variable (name symbol &optional default)
  "Return the value in the process buffer for NAME of SYMBOL.

Return DEFAULT if the process buffer does not exist or SYMBOL is not
defined."
  (let ((buffer (TeX-process-buffer name)))
    (if (and buffer
             (local-variable-p symbol buffer))
        (with-current-buffer buffer
          (symbol-value symbol))
      default)))

(defun TeX-process-set-variable (name symbol value)
  "Set the variable SYMBOL in the process buffer to VALUE.
Return nil only if no process buffer exists."
  (let ((buffer (TeX-process-buffer name)))
    (if buffer
        (with-current-buffer buffer
          (set symbol value)
          t)
      nil)))

(defcustom TeX-kill-process-without-query nil
  "If non-nil, abort a running document process without user query."
  :type 'boolean
  :safe #'booleanp
  :group 'TeX-command
  :local t)

(defun TeX-process-check (name)
  "Check if a process for the TeX document NAME already exists.
If so, give the user the choice of aborting the process or the current
command.  If the value of `TeX-kill-process-without-query' is non-nil,
user query is skipped and the process is aborted right away."
  (let (process)
    (while (and (setq process (TeX-process name))
                (eq (process-status process) 'run))
      (cond
       ((or TeX-kill-process-without-query
            (yes-or-no-p (concat "Process `"
                                 (process-name process)
                                 "' for document `"
                                 name
                                 "' running, kill it? ")))
        (delete-process process))
       ((eq (process-status process) 'run)
        (error "Cannot have two processes for the same document"))))))

(defun TeX-process-buffer-name (name)
  "Return name of AUCTeX buffer associated with the document NAME."
  (concat "*" (abbreviate-file-name (expand-file-name name)) " output*"))

(defun TeX-process-buffer (name)
  "Return the AUCTeX buffer associated with the document NAME."
  (get-buffer (TeX-process-buffer-name name)))

(defun TeX-process (name)
  "Return AUCTeX process associated with the document NAME."
  (and TeX-process-asynchronous
       (get-buffer-process (TeX-process-buffer name))))

;;; Process Filters

(defun TeX-command-mode-line (process)
  "Format the mode line for a buffer containing output from PROCESS."
    (setq mode-line-process (concat ": "
                                    (symbol-name (process-status process))))
    (force-mode-line-update))

(defun TeX-command-filter (process string)
  "Filter to process normal output."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (process-mark process))
      (insert-before-markers string)
      (set-marker (process-mark process) (point)))))

(defvar-local TeX-current-page nil
  "The page number currently being formatted, enclosed in brackets.")

(defun TeX-format-mode-line (process)
  "Format the mode line for a buffer containing TeX output from PROCESS."
  (setq mode-line-process (concat " " TeX-current-page ": "
                                  (symbol-name (process-status process))))
  (force-mode-line-update))

(defun TeX-format-filter (process string)
  "Filter to process TeX output."
  (with-current-buffer (process-buffer process)
    (let (str pos end (pt (marker-position (process-mark process))))
      (save-excursion
        (goto-char pt)
        (insert-before-markers string)
        (set-marker (process-mark process) (point))
        ;; Remove line breaks at columns 79 and 80
        (while (> (point) pt)
          (end-of-line 0)
          (when (and (memq (- (point) (line-beginning-position)) '(79 80))
                     ;; Heuristic: Don't delete the linebreak if the next line
                     ;; is empty or starts with an opening parenthesis, or if
                     ;; point is located after a period and in the next line no
                     ;; word char follows.
                     (not (memq (char-after (1+ (point))) '(?\n ?\()))
                     (not (and (eq (char-before) ?.)
                               (char-after (1+ (point)))
                               (not (eq ?w (char-syntax (char-after (1+ (point)))))))))
            (delete-char 1)))
        (goto-char (marker-position (process-mark process)))
        ;; Determine current page
        (while (and pt
                    (skip-chars-backward "^]" pt)
                    (> (point) pt))
          (setq end (point))
          (backward-char)
          (skip-chars-backward "-0-9\n." (max (point-min) (- pt 128)))
          (when (and (eq ?\[ (char-before))
                     (not (eq ?\] (char-after)))
                     (progn
                       (setq str (buffer-substring (1- (point)) end)
                             pos nil)
                       (while (setq pos (string-match "\n" str pos))
                         (setq str (replace-match "" t t str)))
                       (string-match
                        "\\`\\[-?[0-9]+\\(\\.-?[0-9]+\\)\\{0,9\\}\\]\\'"
                        str)))
            (setq TeX-current-page str
                  pt nil)
            (TeX-format-mode-line process)))))))

(defvar-local TeX-parse-function nil
  "Function to call to parse content of TeX output buffer.")

(defun TeX-background-filter (_process string)
  "Filter to process background output."
  (let ((old-window (selected-window))
        (pop-up-windows t))
    (TeX-pop-to-buffer "*TeX background*" nil t)
    (goto-char (point-max))
    (insert string)
    (select-window old-window)))

;; Copy and adaption of `comint-postoutput-scroll-to-bottom' from CVS
;; Emacs of 2005-04-24.
(defun TeX-interactive-goto-prompt (string)
  "Move point to prompt of an interactive TeX run."
  (let* ((selected (selected-window))
         (current (current-buffer))
         (process (get-buffer-process current)))
    (unwind-protect
        (when process
          (walk-windows
           (lambda (window)
             (when (eq (window-buffer window) current)
               (select-window window)
               (when (and (< (point) (process-mark process))
                          (string-match "^\\? $" string))
                 (goto-char (process-mark process)))
               (select-window selected)))
           nil t))
      (set-buffer current))))


;;; Active Process

(defvar TeX-current-process-region-p nil
  "Non-nil means that the last TeX command is on a region.")

(defun TeX-active-process ()
  "Return the active process for the current buffer."
  (TeX-process (TeX-active-master)))

(defun TeX-active-buffer ()
  "Return the buffer of the active process for this buffer."
  (and TeX-command-buffer
       (with-current-buffer TeX-command-buffer
         (TeX-process-buffer (TeX-active-master)))))

(defun TeX-active-master (&optional extension nondirectory _ignore)
  "The master file currently being compiled.

If optional argument EXTENSION is non-nil, add that file extension to
the name.  Special value t means use `TeX-default-extension'.

If optional second argument NONDIRECTORY is non-nil, do not include
the directory.

The compatibility argument IGNORE is ignored."
  ;; The third argument `_ignore' is kept for symmetry with
  ;; `TeX-master-file's third argument `ask'.  For example, it's used
  ;; in `TeX-active-master-with-quotes' for backward compatibility.
  ;; Keep this in mind should you want to use another argument here.
  ;; See also the similar comment in `TeX-region-file'.
  (if TeX-current-process-region-p
      (TeX-region-file extension nondirectory)
    (TeX-master-file extension nondirectory)))

(defvar TeX-command-buffer nil
  "The buffer from where the last TeX command was issued.")

;;; Region File


(defvar TeX-region-hook nil
  "List of hooks to run before the region file is saved.
The hooks are run in the region buffer, you may use the variable
`TeX-region-master-buffer' to access the buffer of the master
file and `TeX-region-orig-buffer' to access the buffer where
\\[TeX-command-region] or \\[TeX-command-buffer] is invoked
from.")

(defun TeX-quote-filename (file)
  "Convert file name FILE into a form acceptable to TeX."
  (let (pos)
    (while (setq pos (string-match "\\\\" file pos))
      (setq file (replace-match "/" t t file 0)
            pos (1+ pos)))
    (while (setq pos (string-match "[~#]" file pos))
      (setq file (replace-match "\\\\string\\&" t nil file 0)
            pos (+ pos 8))))
  ;; Use \unexpanded so that \message outputs the raw file name.
  ;; When \usepackage[utf8]{inputenc} is used in standard (pdf)latex,
  ;; \message does not output non-ascii file name in raw form without
  ;; \unexpanded, which makes AUCTeX to fail to recognize the file
  ;; names right when analysing the process output buffer.
  ;; Note that \usepackage[utf8]{inputenc} is enabled by default in
  ;; standard (pdf)latex since TeXLive 2018.
  (if (and (memq major-mode '(LaTeX-mode docTeX-mode))
           ;; Japanese upLaTeX requires the same treatment with
           ;; respect to non-ascii characters other than Japanese, in
           ;; file names within \message{}.
           ;; However, pLaTeX (non u- version) does not support
           ;; non-ascii file name encoded in UTF-8.  So considering
           ;; `ptex' doesn't make sense here.  We cater for only
           ;; `default' and `uptex' engines.
           (memq TeX-engine '(default uptex)))
      ;; It would fail to put entire `file' inside \unexpanded{} when
      ;; the above loop injects \string before "#" and "~".  So put
      ;; only multibyte characters inside \unexpanded{}.
      ;; It is safe in upLaTeX to use \unexpanded{} on Japanese
      ;; characters though they are handled by upLaTeX in a totally
      ;; different way from inputenc.
      ;; Thus put all multibyte characters, without considering
      ;; whether they are Japanese or not, inside \unexpanded{}.
      (replace-regexp-in-string "[[:multibyte:]]+"
                                "\\\\unexpanded{\\&}" file t)
    file))

(defvar-local TeX-region-orig-buffer nil
  "The original buffer in which the TeX-region was created.")

(defvar-local TeX-region-master-buffer nil
  "The TeX-master buffer of the document for which the TeX-region
was created.")

(defun TeX-region-create (file region original offset)
  "Create a new file named FILE with the string REGION.
The region is taken from ORIGINAL starting at line OFFSET.

The current buffer and master file is searched, in order to ensure
that the TeX header and trailer information is also included.

The OFFSET is used to provide the debugger with information about the
original file."
  (if (fboundp 'preview--skip-preamble-region)
      (let ((temp (preview--skip-preamble-region region offset)))
        (if temp
            ;; Skip preamble for the sake of predumped formats.
            (setq region (car temp)
                  offset (cdr temp)))))

  (let* (;; We shift buffer a lot, so we must keep track of the buffer
         ;; local variables.
         (header-end TeX-header-end)
         (trailer-start TeX-trailer-start)

         ;; We search for header and trailer in the master file.
         (orig-buffer (current-buffer))
         (master-name (TeX-master-file TeX-default-extension))
         (master-buffer (find-file-noselect master-name))

         ;; Attempt to disable font lock.
         (font-lock-mode-hook nil)
         ;; And insert them into the FILE buffer.
         (file-buffer (let (;; Don't query for master file
                            (TeX-transient-master t)
                            ;; Don't choose a special mode (and call its hooks)
                            (auto-mode-alist nil)
                            (magic-mode-alist nil)
                            (enable-local-variables nil)
                            ;; Don't run any f-f hooks
                            (find-file-hook nil))
                        (find-file-noselect file t)))
         ;; But remember original content.
         original-content

         ;; We search for the header from the master file, if it is
         ;; not present in the region.
         (header (if (string-match header-end region)
                     ""
                   (save-excursion
                     (save-restriction
                       (set-buffer master-buffer)
                       (save-excursion
                         (save-restriction
                           (widen)
                           (goto-char (point-min))
                           ;; NOTE: We use the local value of
                           ;; TeX-header-end from the master file.
                           (if (not (re-search-forward TeX-header-end nil t))
                               ""
                             (re-search-forward "[\r\n]" nil t)
                             (buffer-substring-no-properties
                              (point-min) (point)))))))))
         (header-offset 0)
         first-line
         ;; We search for the trailer from the master file, if it is
         ;; not present in the region.
         (trailer-offset 0)
         (trailer (if (string-match trailer-start region)
                      ""
                    (save-excursion
                      (save-restriction
                        (set-buffer master-buffer)
                        (save-excursion
                          (save-restriction
                            (widen)
                            (goto-char (point-max))
                            ;; NOTE: We use the local value of
                            ;; TeX-trailer-start from the master file.
                            (if (not (re-search-backward TeX-trailer-start nil t))
                                ""
                              ;;(beginning-of-line 1)
                              (re-search-backward "[\r\n]" nil t)
                              (setq trailer-offset (TeX-current-offset))
                              (buffer-substring-no-properties
                               (point) (point-max))))))))))
    ;; file name should be relative to master
    (unless (string= original "<none>") ; cf. `preview-region'
      (setq original (TeX-quote-filename (file-relative-name
                                          original (TeX-master-directory)))))
    (setq master-name (TeX-quote-filename master-name))

    ;; If the first line begins with "%&", put that line separately on
    ;; the very first line of the region file so that the first line
    ;; parsing will work.
    (setq first-line (if (and (> (length header) 1)
                              (string= (substring header 0 2) "%&"))
                         ;; This would work even if header has no newline.
                         (substring header 0 (string-match "\n" header))
                       ""))
    (unless (string= first-line "")
      ;; Remove first-line from header.
      (setq header (substring header (length first-line)))
      (setq first-line (concat first-line "\n")))

    (with-current-buffer file-buffer
      (setq buffer-read-only t
            buffer-undo-list t)
      (setq original-content (buffer-string))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq buffer-file-coding-system
              (with-current-buffer master-buffer buffer-file-coding-system))
        (insert first-line
                "\\message{ !name(" master-name ")}"
                header
                TeX-region-extra
                "\n\\message{ !name(" original ") !offset(")
        (setq header-offset (- offset
                               (1+ (TeX-current-offset))))
        (insert (int-to-string header-offset)
                ") }\n"
                region
                "\n\\message{ !name("  master-name ") !offset(")
        (insert (int-to-string (- trailer-offset
                                  (1+ (TeX-current-offset))))
                ") }\n"
                trailer)
        (setq TeX-region-orig-buffer orig-buffer)
        (setq TeX-region-master-buffer master-buffer)
        (run-hooks 'TeX-region-hook)
        (if (string-equal (buffer-string) original-content)
            (set-buffer-modified-p nil)
          (save-buffer 0))))))

(defun TeX-region-file (&optional extension nondirectory _ignore)
  "Return TeX-region file name with EXTENSION.
If optional second argument NONDIRECTORY is non-nil, do not include
the directory.

The compatibility argument IGNORE is ignored."
  ;; The third argument `_ignore' is kept for symmetry with `TeX-master-file's
  ;; third argument `ask'.  For example, it's used in `TeX-command-sequence',
  ;; where we don't know which function has to be called.  Keep this in mind
  ;; should you want to use another argument here.
  (let ((master-dir (TeX-master-directory)))
    (concat (or (TeX--master-output-dir master-dir nondirectory)
                (if nondirectory "" master-dir))
            (cond ((eq extension t)
                   (concat TeX-region "." TeX-default-extension))
                  (extension
                   (concat TeX-region "." extension))
                  (t
                   TeX-region)))))

(defcustom TeX-region "_region_"
  "Base name of temporary file for `TeX-command-region' and `TeX-command-buffer'."
  :group 'TeX-command
  :type 'string)

(defvar-local LaTeX-command-section-level nil
  "The section level used for `LaTeX-command-section'.
Will be initialized to `LaTeX-largest-level' buffer-locally.")

(defun LaTeX-command-section-level ()
  "Return the value of `LaTeX-command-section-level'.
Initialize it to `LaTeX-largest-level' if needed."
  (unless LaTeX-command-section-level
    (setq LaTeX-command-section-level LaTeX-largest-level))
  LaTeX-command-section-level)

(defun LaTeX-command-section-change-level (arg)
  "Change `LaTeX-command-section-level' by ARG.
`LaTeX-command-section-level' is the sectioning level used to
determine the current section by `LaTeX-command-section'.
The levels are defined by `LaTeX-section-list'."
  (interactive "p")
  (let ((old-level (LaTeX-section-name (LaTeX-command-section-level))))
    (setq LaTeX-command-section-level (+ LaTeX-command-section-level arg))
    (cond
     ((> LaTeX-command-section-level 6)
      (setq LaTeX-command-section-level 6)
      (message "Cannot shrink LaTeX-command-section-level below subparagraph."))
     ((< LaTeX-command-section-level 0)
      (setq LaTeX-command-section-level 0)
      (message "Cannot enlarge LaTeX-command-section-level above part."))
     (t (message "Changed level from %s to %s."
                 old-level (LaTeX-section-name
                            LaTeX-command-section-level))))))

(defun LaTeX-command-section-boundaries ()
  "Return the boundaries of the current section as (start . end).
The section is determined by `LaTeX-command-section-level'."
  (let* ((case-fold-search nil)
         (rx (concat "\\\\" (regexp-opt
                             (mapcar
                              #'LaTeX-section-name
                              (number-sequence
                               0 (LaTeX-command-section-level))))
                     "\\*?{")))
    (cons (save-excursion
            (re-search-backward rx nil t)
            (point))
          (save-excursion
            (re-search-forward (concat rx "\\|\\\\end{document}") nil t)
            (forward-line 0)
            (point)))))

(defun LaTeX-command-section (&optional override-confirm)
  "Run a command on the current section.

What makes the current section is defined by
`LaTeX-command-section-level' which can be enlarged or shrunken
with `LaTeX-command-section-change-level'.

Query the user for a command to run on the temporary file
specified by the variable `TeX-region'.  The region file will be
recreated from current section.

If a prefix argument OVERRIDE-CONFIRM is given, confirmation will
depend on it being positive instead of the entry in
`TeX-command-list'."
  (interactive "P")
  (if (eq major-mode 'LaTeX-mode)
      (let* ((bounds (LaTeX-command-section-boundaries))
             (TeX-command-region-begin (car bounds))
             (TeX-command-region-end (cdr bounds)))
        (TeX-command-region override-confirm))
    (error "LaTeX-command-section can only be run on LaTeX documents")))

(defun TeX-command-run-all-region ()
  "Compile the current region until an error occurs or it is finished."
  (interactive)
  (TeX-region-update)
  (TeX-command-sequence t t #'TeX-region-file))

(defun LaTeX-command-run-all-section ()
  "Compile the current section until an error occurs or it is finished."
  (interactive)
  (if (eq major-mode 'LaTeX-mode)
      (let* ((bounds (LaTeX-command-section-boundaries))
             (TeX-command-region-begin (car bounds))
             (TeX-command-region-end (cdr bounds)))
        (TeX-region-update)
        (TeX-command-sequence t t #'TeX-region-file))
    (error "LaTeX-command-run-all-section can only be run on LaTeX documents")))

(defun TeX-command-run-all (arg)
  "Compile the current document until an error occurs or it is finished.
With a prefix ARG (`\\[universal-argument] \\[TeX-command-run-all]'),
compile the current region instead, that is, call
`TeX-command-run-all-region'.  With multiple prefix
arguments (`\\[universal-argument] \\[universal-argument] \\[TeX-command-run-all]'),
compile the current section instead, that is, call
`LaTeX-command-run-all-section'."
  (interactive "P")
  (cond
   ((null arg)       (TeX-command-sequence t t))
   ((= 4 (car arg))  (TeX-command-run-all-region))
   (t                (LaTeX-command-run-all-section))))

;;; Parsing

;;; - Global Parser Variables

(defvar-local TeX-error-point nil
  "How far we have parsed until now.")

(defvar-local TeX-error-file nil
  "Stack of files in which errors have occurred.")

(defvar-local TeX-error-offset nil
  "Add this to any line numbers from TeX.  Stack like `TeX-error-file'.")

(defun TeX-parse-reset (&optional reparse)
  "Reset all variables used for parsing TeX output.
If optional argument REPARSE is non-nil, reparse the output log."
  (setq TeX-error-point (point-min)
        TeX-error-offset nil
        TeX-error-file nil
        TeX-error-list nil
        TeX-error-last-visited -1)
  (if reparse
      (TeX-parse-all-errors)))

;;; - Parsers Hooks

;; All this parsers hooks should have the same arguments even though they will
;; be ignored, because `TeX-next-error' can call any of these functions.
(defun TeX-parse-command (_arg _reparse)
  "We can't parse anything but TeX."
  (error "I cannot parse %s output, sorry"
         (if (TeX-active-process)
             (process-name (TeX-active-process))
           "this")))

(defun TeX-error-list-skip-warning-p (type ignore)
  "Decide if a warning of `TeX-error-list' should be skipped.

TYPE is one of the types listed in `TeX-error-list', IGNORE
is the flag to choose if the warning should be skipped."
  ;; The warning should be skipped if it...
  (or
   ;; ...is a warning and we want to ignore all warnings, or...
   (and (null TeX-debug-warnings)
        (equal type 'warning))
   ;; ...is a bad-box and we want to ignore all bad-boxes, or...
   (and (null TeX-debug-bad-boxes)
        (equal type 'bad-box))
   ;; ...is a warning to be ignored.
   (and TeX-suppress-ignored-warnings
        ignore)))

(defun TeX-parse-TeX (arg reparse)
  "Find the next error produced by running TeX.

ARG specifies how many error messages to move, when possible;
negative means move back to previous error messages.

If REPARSE is non-nil, reparse the output log.

If the file occurs in an included file, the file is loaded (if not
already in an Emacs buffer) and the cursor is placed at the error."
  (let ((old-buffer (current-buffer))
        max-index item)

    ;; Switch to the output buffer.
    (with-current-buffer (TeX-active-buffer)
      (if reparse
          (TeX-parse-reset reparse))
      (if TeX-parse-all-errors
          (progn
            (setq arg (or arg 1)
                  max-index (length TeX-error-list))
            ;; This loop is needed to skip ignored warnings, when
            ;; `TeX-suppress-ignored-warnings' is non-nil and there are ignore
            ;; warnings.
            (while (null (zerop arg))
              (setq TeX-error-last-visited
                    ;; Increase or decrese `TeX-error-last-visited' depending on
                    ;; the sign of `arg'.  Note: `signum' is a function from
                    ;; `cl' library, do not be tempted to use it.
                    (if (> arg 0)
                        (1+ TeX-error-last-visited)
                      (1- TeX-error-last-visited))
                    item (nth TeX-error-last-visited TeX-error-list))
              ;; Increase or decrease `arg' only if the warning isn't to be
              ;; skipped.
              (unless (TeX-error-list-skip-warning-p (nth 0 item) (nth 10 item))
                ;; Note: `signum' is a function from `cl' library, do not be
                ;; tempted to use it.
                (setq arg (if (> arg 0)
                              (1- arg)
                            (1+ arg)))))
            (if (< TeX-error-last-visited -1)
                (setq TeX-error-last-visited -1))
            (cond ((or (null item)
                       (< TeX-error-last-visited 0))
                   (if (> TeX-error-last-visited max-index)
                       (setq TeX-error-last-visited max-index))
                   (message "No more errors.")
                   (beep)
                   (TeX-pop-to-buffer old-buffer))
                  (t
                   (apply #'TeX-find-display-help item))))

        (goto-char TeX-error-point)
        (TeX-parse-error old-buffer)))))

;;; - Parsing (La)TeX

(defvar TeX-translate-location-file nil)
(defvar TeX-translate-location-offset nil)
(defvar TeX-translate-location-line nil)
(defvar TeX-translate-location-string nil)
(defvar TeX-translate-location-error nil)
(defvar TeX-translate-location-context nil)

(defvar TeX-translate-location-hook nil
  "List of functions to be called before showing an error or warning.

You might want to examine and modify the dynamically bound
variables
  `TeX-translate-location-file',
  `TeX-translate-location-offset',
  `TeX-translate-location-line',
  `TeX-translate-location-string',
  `TeX-translate-location-error', and
  `TeX-translate-location-context'
from this hook.")

;; `ignore' flag should be the always the last one in the list of information
;; for each error/warning, because it can be set within `TeX-warning' by a
;; custom function taking as argument all information present in
;; `TeX-error-list' but `ignore', see `TeX-ignore-warnings'.
(defvar-local TeX-error-list nil
  "List of warnings and errors.

Each element of the list is a list of information for a specific
error or warning.  This is the structure of each element:
 *  0: type (error, warning, bad-box)
 *  1: file
 *  2: line
 *  3: message of the error or warning
 *  4: offset
 *  5: context, to be displayed in the help window
 *  6: string to search in the buffer, in order to find location
       of the error or warning
 *  7: for warnings referring to multiple lines (for exapmle, bad boxes),
       the last line mentioned in the warning message
 *  8: t if it is a bad-box, nil otherwise
 *  9: value of `TeX-error-point'
 * 10: whether the warning should be ignored

This variable is intended to be set only in output buffer so it
will be shared among all files of the same document.")

(defun TeX-parse-all-errors ()
  "Parse TeX output buffer to collect all warnings and errors."
  ;; Reset error list.
  (setq TeX-error-list nil)
  (save-excursion
    (goto-char (point-min))
    (while (TeX-parse-error nil t)))
  ;; Reset last visited error.
  (setq TeX-error-last-visited -1))

(defun TeX-parse-error (old &optional store)
  "Goto next error.  Pop to OLD buffer if no more errors are found.

If the optional argument STORE is non-nil, the function will
store the found warning or error in `TeX-error-list' instead of
displaying the issue.

Return non-nil if an error or warning is found."
  (let ((regexp
         (concat
          ;; TeX error
          "^\\(!\\|\\(.+?\\):[0-9]+:\\) \\|"
          ;; New file
          "(\n?\\([^\n()]+\\)\\|"
          ;; End of file.
          "\\()\\)\\|"
          ;; Hook to change line numbers
          " !\\(?:offset(\\([---0-9]+\\))\\|"
          ;; Hook to change file name
          "name(\\([^)]+\\))\\)\\|"
          ;; Start of LaTeX bad box
          "^\\(\\(?:Overfull\\|Underfull\\|Tight\\|Loose\\) "
          ;;   Horizontal bad box
          "\\(?:\\\\hbox.* at lines? [0-9]+\\(?:--[0-9]+\\)?$\\|"
          ;;   Vertical bad box.  See also `TeX-warning'.
          "\\\\vbox ([ a-z0-9]+) has occurred while \\\\output is active \\[[^]]+\\]\\)\\)\\|"
          ;; LaTeX warning
          "^\\(" LaTeX-warnings-regexp ".*\\)"))
        (error-found nil))
    (while
        (cond
         ((null
           (re-search-forward regexp nil t))
          ;; No more errors.
          (unless store
            (message "No more errors.")
            (beep)
            (TeX-pop-to-buffer old))
          nil)
         ;; TeX error
         ((match-beginning 1)
          (if (or (not (match-beginning 2))
                  ;; Ignore non-error warning. (bug#55065)
                  (file-exists-p (TeX-match-buffer 2)))
              (progn
                (when (match-beginning 2)
                  (unless TeX-error-file
                    (push nil TeX-error-file)
                    (push nil TeX-error-offset))
                  (unless (car TeX-error-offset)
                    (rplaca TeX-error-file (TeX-match-buffer 2))))
                (setq error-found t)
                (if (looking-at "Preview ")
                    t
                  (TeX-error store)
                  nil))
            ;; This wasn't an actual TeX error.  Go to the least
            ;; possible point to search again.
            (goto-char (1+ (match-beginning 1)))
            t))
         ;; LaTeX bad box
         ((match-beginning 7)
          ;; In `TeX-error-list' we collect all warnings, also if they're going
          ;; to be actually skipped.
          (if (or store TeX-debug-bad-boxes)
              (progn
                (setq error-found t)
                (TeX-warning (TeX-match-buffer 7) (match-beginning 7) t store)
                nil)
            (re-search-forward "\r?\n\
\\(?:.\\{79\\}\r?\n\
\\)*.*\r?$")
            t))
         ;; LaTeX warning
         ((match-beginning 8)
          ;; In `TeX-error-list' we collect all warnings, also if they're going
          ;; to be actually skipped.
          (if (or store TeX-debug-warnings)
              (progn
                (setq error-found t)
                (TeX-warning (TeX-match-buffer 8) (match-beginning 8) nil store)
                nil)
            t))

         ;; New file -- Push on stack
         ((match-beginning 3)
          (let ((file (TeX-match-buffer 3))
                (end (match-end 3)))
            ;; Strip quotation marks and remove newlines if necessary
            (when (or (eq (string-to-char file) ?\")
                      (string-match "[ \t\n]" file))
              (setq file (mapconcat #'identity (split-string file "[\"\n]+") "")))
            ;; Polish `file' string
            (setq file
                  (let ((string file))
                    (setq string
                          (if (string-match "\\`[ \t\n\r]+" string)
                              (replace-match "" t t string)
                            string))
                    ;; Sometimes `file' is something like
                    ;;     "./path/to/file.tex [9] [10 <./path/to/file>] "
                    ;; where "[9]" and "[10 <./path/to/file>]" are pages of the
                    ;; output file, with path to an included file.  Remove these
                    ;; numbers together with whitespaces at the end of the
                    ;; string.
                    (if (string-match "\\( *\\(\\[[^]]+\\]\\)? *\\)*\\'" string)
                        (replace-match "" t t string)
                      string)))
            (push file TeX-error-file)
            (push nil TeX-error-offset)
            (goto-char end))
          t)

         ;; End of file -- Pop from stack
         ((match-beginning 4)
          (when (> (length TeX-error-file) 0)
            (pop TeX-error-file)
            (pop TeX-error-offset))
          (goto-char (match-end 4))
          t)

         ;; Hook to change line numbers
         ((match-beginning 5)
          (setq TeX-error-offset
                (list (string-to-number (TeX-match-buffer 5))))
          t)

         ;; Hook to change file name
         ((match-beginning 6)
          (setq TeX-error-file
                (list (TeX-match-buffer 6)))
          t)))
    error-found))

(defun TeX-find-display-help (type file line error offset context string
                                   line-end _bad-box error-point _ignore)
  "Find the error and display the help.

For a description of arguments, see `TeX-error-list'.  IGNORE
value is not used here."
  ;; Go back to TeX-buffer
  (let ((runbuf (TeX-active-buffer))
        (master (with-current-buffer TeX-command-buffer
                  (expand-file-name (TeX-master-file))))
        (command-buffer TeX-command-buffer)
        (TeX-translate-location-file file)
        (TeX-translate-location-line line)
        (TeX-translate-location-error error)
        (TeX-translate-location-offset offset)
        (TeX-translate-location-context context)
        (TeX-translate-location-string string)
        error-file-buffer start)

    (run-hooks 'TeX-translate-location-hook)

    (if TeX-translate-location-file
        (progn
          (setq error-file-buffer
                (find-file
                 (expand-file-name TeX-translate-location-file
                                   (file-name-directory master))))
          ;; Use the major mode of `TeX-command-buffer' when visiting
          ;; the error point.
          (if (eq major-mode (default-value 'major-mode))
              (funcall (buffer-local-value 'major-mode command-buffer)))
          ;; Set the value of `TeX-command-buffer' in the next file
          ;; with an error to be displayed to the value it has in the
          ;; current buffer.
          (setq-local TeX-command-buffer command-buffer)

          ;; Find the location of the error or warning.
          (when TeX-translate-location-line
            (goto-char (point-min))
            (forward-line (+ TeX-translate-location-offset
                             TeX-translate-location-line -1))
            (cond
             ;; Error.
             ((equal type 'error)
              (if (not (string= TeX-translate-location-string " "))
                  (search-forward TeX-translate-location-string nil t)))
             ;; Warning or bad box.
             (t
              (beginning-of-line 0)
              (setq start (point))
              (goto-char (point-min))
              (forward-line (+ TeX-translate-location-offset
                               line-end -1))
              (end-of-line)
              (when TeX-translate-location-string
                (search-backward TeX-translate-location-string start t)
                (search-forward TeX-translate-location-string nil t))))))
      ;; When the file cannot be determined stay here but issue a
      ;; warning.
      (message "Could not determine file for %s"
               (if (eq type 'error) "error" "warning"))
      (beep))

    ;; Display the help.
    (cond ((eq TeX-display-help 'expert)
           (TeX-pop-to-buffer runbuf nil t)
           (goto-char error-point)
           (if error-file-buffer
               (TeX-pop-to-buffer error-file-buffer nil t)))
          (TeX-display-help
           (TeX-help-error
            TeX-translate-location-error
            (if (equal type 'warning)
                (concat "\n" TeX-translate-location-context)
              TeX-translate-location-context)
            runbuf type))
          (t
           (message "! %s" TeX-translate-location-error)))))

(defun TeX-error (&optional store)
  "Display an error.

If optional argument STORE is non-nil, store the error
information in `TeX-error-list' instead of displaying the error."

  (let* ( ;; We need the error message to show the user.
         (error (progn
                  (re-search-forward "\\(.*\\)")
                  (TeX-match-buffer 1)))

         ;; And the context for the help window.
         (context-start (point))
         context-available

         ;; And the line number to position the cursor.
         (line (cond
                ;; regular style
                ((re-search-forward "l\\.\\([0-9]+\\)" nil t)
                 (setq context-available t)
                 (string-to-number (TeX-match-buffer 1)))
                ;; file:line:error style
                ((save-excursion
                   (re-search-backward ":\\([0-9]+\\): "
                                       (line-beginning-position) t))
                 (string-to-number (TeX-match-buffer 1)))
                ;; nothing found
                (t 1)))

         ;; And a string of the context to search for.
         (string (progn
                   (beginning-of-line)
                   (re-search-forward " \\(\\([^ \t]*$\\)\\|\\($\\)\\)")
                   (TeX-match-buffer 1)))

         ;; And we have now found to the end of the context.
         (context (if context-available
                      (buffer-substring context-start (progn (forward-line 1)
                                                             (end-of-line)
                                                             (point)))
                    ;; There is no real context available, so we
                    ;; simply show the line with the error message.
                    (buffer-substring (1- (line-beginning-position))
                                      context-start)))
         ;; We may use these in another buffer.
         (offset (or (car TeX-error-offset) 0))
         (file (car TeX-error-file))
         info-list)

    ;; Remember where we was.
    (setq TeX-error-point (point)
          info-list (list 'error file line error offset context string nil nil
                          TeX-error-point nil))
    (if store
        ;; Store the error information.
        (add-to-list 'TeX-error-list info-list t)
      ;; Find the error point and display the help.
      (apply #'TeX-find-display-help info-list))))

(defun TeX-warning (warning warning-start bad-box &optional store)
  "Display a warning for WARNING.

WARNING-START is the position where WARNING starts.  If BAD-BOX
is non-nil, the warning refers to a bad-box, otherwise it is a
generic warning.

If optional argument STORE is non-nil, store the warning
information in `TeX-error-list' instead of displaying the
warning."

  (let* ( ;; line-string: match 1 is beginning line, match 2 is end line
         (line-string (if bad-box
                          "at lines? \\([0-9]*\\)\\(?:--\\([0-9]*\\)\\)?"
                        ;; Traditional messages say "on input line X",
                        ;; the LaTeX3 \msg_line_context:. just reads
                        ;; "on line X".
                        "on \\(?:input \\)?line \\([0-9]*\\)\\."))
         ;; word-string: match 1 is the word
         (word-string (if bad-box "[][\\W() ---]\\(\\w+\\)[][\\W() ---]*$"
                        ;; Match "ref" in both "Reference `ref' on page NN
                        ;; undefined" and "Citation 'ref' on page NN undefined".
                        "\\(?:`\\|'\\)\\([-a-zA-Z0-9:]+\\)'"))

         ;; Get error-line (warning).  Don't search before `warning-start' to
         ;; avoid catching completely unrelated line numbers.
         (line (when (save-excursion (re-search-backward line-string
                                                         warning-start t))
                 (string-to-number (TeX-match-buffer 1))))
         ;; If this is a bad box and the warning ends with "...at lines MM--NN"
         ;; we can use "NN" as `line-end', in any other case (including bad
         ;; boxes ending with "...at line NN") just use `line'.
         (line-end (if (and bad-box (match-beginning 2))
                       (string-to-number (TeX-match-buffer 2))
                     line))

         ;; Find the context
         (context-start (progn (cond
                                ((and bad-box (string-match "\\\\hbox" warning))
                                 ;; Horizontal bad box
                                 (end-of-line))
                                (bad-box
                                 ;; Vertical bad box (by exclusion), don't move
                                 ;; point.  In the output buffer, unlike in the
                                 ;; actual *.log file, these warnings do not end
                                 ;; with "...is active []", but in the same line
                                 ;; there may be something else, including a new
                                 ;; file opened.  Thus, point shouldn't move
                                 ;; from the end of the actual bad box warning.
                                 ;; This is why the corresponding regexp in
                                 ;; `TeX-parse-error' doesn't match everything
                                 ;; until the end of the line.
                                 nil)
                                (t
                                 ;; Generic warning.
                                 (beginning-of-line)))
                               (point)))

         (context (cond ((string-match LaTeX-warnings-regexp warning)
                         ;; The warnings matching `LaTeX-warnings-regexp' are
                         ;; emitted by \GenericWarning macro, or macros based on
                         ;; it (\ClassWarning, \PackageWarning, etc).  After
                         ;; such warnings there is an empty line, just look for
                         ;; it to find the end.
                         (beginning-of-line)
                         (while (null (eolp))
                           (forward-line 1))
                         (buffer-substring context-start (progn (end-of-line)
                                                                (point))))

                        ((and bad-box (string-match "\\\\vbox" warning))
                         ;; Vertical bad boxes don't provide any additional
                         ;; information.  In this case, reuse the `warning' as
                         ;; `context' and don't move point, so that we avoid
                         ;; eating the next line that may contain another
                         ;; warning.  See also comment for `context-start'.
                         (concat "\n" warning))

                        (t
                         ;; Horizontal bad boxes.
                         (forward-line 1)
                         (end-of-line)
                         (while (equal (current-column) 79)
                           (forward-line 1)
                           (end-of-line))
                         (buffer-substring context-start (point)))))

         ;; This is where we want to be.
         (error-point (point))

         ;; Now find the error word.
         (string (when (save-excursion
                         (re-search-backward word-string context-start t))
                   (TeX-match-buffer 1)))

         ;; We might use these in another file.
         (offset (or (car TeX-error-offset) 0))
         (file (car TeX-error-file))
         info-list ignore)

    ;; Second chance to get line number right.  If `line' is nil, check whether
    ;; the reference to the line number is in `context'.  For example, this is
    ;; the case for warnings emitted with \ClassWarning and \PackageWarning.
    ;; XXX: maybe it suffices to evaluate `line' after `context' above, but I
    ;; don't know if there are cases in which it's important to get `line'
    ;; before `context'.
    (and (null line)
         (string-match line-string context)
         (setq line-end
               (setq line (and (match-beginning 1)
                               (string-to-number (match-string 1 context))))))

    ;; This is where we start next time.
    (goto-char error-point)
    (setq TeX-error-point (point))

    ;; Explanation of what follows: we add the warning to `TeX-error-list' even
    ;; if it has to be ignored, with a flag specifying whether it is ignored.
    ;; We do so in order to be able to change between "ignore" and "dont-ignore"
    ;; behavior by just looking to the flag, without the need to reparse the
    ;; output log.

    ;; Store the list of information about the warning.
    (setq info-list (list (if bad-box 'bad-box 'warning) file line warning
                          offset context string line-end bad-box
                          TeX-error-point)
          ;; Decide whether it should be ignored.
          ignore (and TeX-ignore-warnings
                      (cond
                       ((stringp TeX-ignore-warnings)
                        (string-match TeX-ignore-warnings warning))
                       ((fboundp TeX-ignore-warnings)
                        (apply TeX-ignore-warnings info-list))))
          ;; Update `info-list'.
          info-list (append info-list (list ignore)))

    (if store
        ;; Store the warning information.
        (add-to-list 'TeX-error-list info-list t)
      ;; Find the warning point and display the help.
      (apply #'TeX-find-display-help info-list))))

;;; Error Messages

(defcustom TeX-error-description-list nil
  "User defined help messages for errors in TeX run.
See `TeX-error-description-list-local' for its format.  All
entries have higher priority than those in
`TeX-error-description-list-local'.
It must not have a fallback entry that matches any error."
  :group 'TeX-output
  :type '(repeat (cons :tag "Entry"
                       (regexp :tag "Match")
                       (string :format "Description:\n%v"))))

(defvar TeX-error-description-list-local
  '((".*" . "No help available"))
  "Buffer local help messages for errors in TeX run.
A list of the form (ERR-REGEXP . CONTEXT) used by function
`TeX-help-error' to display help-text on an error message or warning.
ERR-REGEXP should be a regular expression matching the error message
given from TeX/LaTeX, and CONTEXT should be some lines describing that
error.
Major modes of AUCTeX can set its own catalogue as buffer local
value of this variable, as LaTeX mode does.
Style files of AUCTeX can also add their own entries to buffer local
value of this variable to provide their own help messages.
It must end with a fallback entry that matches any error, for example
\(\".*\" . \"No help available\")")

;;; - Help

(defgroup TeX-error-description-faces nil
  "Faces used in error descriptions."
  :prefix "TeX-error-description-"
  :group 'TeX-output)

(defface TeX-error-description-error
  ;; This is the same as `error' face in latest GNU Emacs versions.
  '((((class color) (min-colors 88) (background light))
     :foreground "Red1" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :foreground "Pink" :weight bold)
    (((class color) (min-colors 16) (background light))
     :foreground "Red1" :weight bold)
    (((class color) (min-colors 16) (background dark))
     :foreground "Pink" :weight bold)
    (((class color) (min-colors 8))
     :foreground "red" :weight bold)
    (t (:inverse-video t :weight bold)))
  "Face for \"Error\" string in error descriptions.")

(defface TeX-error-description-warning
  ;; This is the same as `warning' face in latest GNU Emacs versions.
  '((((class color) (min-colors 16)) :foreground "DarkOrange" :weight bold)
    (((class color)) :foreground "yellow" :weight bold))
  "Face for \"Warning\" string in error descriptions.")

(defface TeX-error-description-tex-said
  ;; This is the same as `font-lock-function-name-face' face in latest GNU
  ;; Emacs versions.
  '((((class color) (min-colors 88) (background light))
     :foreground "Blue1")
    (((class color) (min-colors 88) (background dark))
     :foreground "LightSkyBlue")
    (((class color) (min-colors 16) (background light))
     :foreground "Blue")
    (((class color) (min-colors 16) (background dark))
     :foreground "LightSkyBlue")
    (((class color) (min-colors 8))
     :foreground "blue" :weight bold)
    (t (:inverse-video t :weight bold)))
  "Face for \"TeX said\" string in error descriptions.")

(defface TeX-error-description-help
  '((t (:inherit TeX-error-description-tex-said)))
  "Face for \"Help\" string in error descriptions.")

(defvar-local TeX--log-file-readin-modtime nil
  "Recorded modification time of the TeX log file.
It is updated each time the file is read into the buffer, and is to
avoid unnecessary reads of the log file.")

(defun TeX-help-error (error output runbuffer type)
  "Print ERROR in context OUTPUT from RUNBUFFER in another window.
TYPE is a symbol specifing if ERROR is a real error, a warning or
a bad box."

  (let ((old-buffer (current-buffer))
        (log-file (with-current-buffer runbuffer
                    (with-current-buffer TeX-command-buffer
                      (expand-file-name (TeX-active-master "log")))))
        (error-description-list
         (append TeX-error-description-list
                 (buffer-local-value 'TeX-error-description-list-local
                                     (buffer-local-value
                                      'TeX-command-buffer
                                      runbuffer))))
        (TeX-error-pointer 0))

    ;; Find help text entry.
    (while (not (string-match (car (nth TeX-error-pointer
                                        error-description-list))
                              error))
      (setq TeX-error-pointer (+ TeX-error-pointer 1)))

    (TeX-pop-to-buffer (get-buffer-create "*TeX Help*") nil t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       (cond
        ((equal type 'error)
         (propertize "ERROR" 'font-lock-face 'TeX-error-description-error))
        ((equal type 'warning)
         (propertize "WARNING" 'font-lock-face 'TeX-error-description-warning))
        ((equal type 'bad-box)
         (propertize "BAD BOX" 'font-lock-face 'TeX-error-description-warning)))
       ": " error
       (propertize "\n\n--- TeX said ---" 'font-lock-face
                   'TeX-error-description-tex-said)
       output
       (propertize "\n--- HELP ---\n" 'font-lock-face
                   'TeX-error-description-help)
       (let ((help (cdr (nth TeX-error-pointer
                             error-description-list))))
         (or (and (= (1+ TeX-error-pointer)
                     (length error-description-list))
                  (with-current-buffer
                      (get-buffer-create (format " *%s*" log-file))
                    (let ((modtime (file-attribute-modification-time
                                    (file-attributes log-file))))
                      (unless (and TeX--log-file-readin-modtime
                                   (time-equal-p TeX--log-file-readin-modtime
                                                 modtime))
                        (insert-file-contents log-file nil nil nil 'replace)
                        (setq TeX--log-file-readin-modtime modtime)))
                    (goto-char (point-min))
                    (when (and (search-forward error nil t 1)
                               (re-search-forward "^l\\." nil t)
                               (re-search-forward "^ [^\n]+$" nil t))
                      (let ((start (1+ (point))))
                        (forward-char 1)
                        (re-search-forward "^$")
                        (concat "From the .log file...\n\n"
                                (buffer-substring start (point)))))))
             help))))
    (goto-char (point-min))
    (TeX-special-mode)
    (TeX-pop-to-buffer old-buffer nil t)))

;;; Error Overview

(defvar TeX-error-overview-active-buffer nil
  "The active buffer for the current error overview.")

(defvar TeX-error-overview-orig-frame nil
  "Frame from which the error overview has been launched.")

(defvar TeX-error-overview-orig-window nil
  "Window from which the error overview has been launched.")

(defcustom TeX-error-overview-setup nil
  "The frame setup of the error overview.

The possible value is: `separate-frame' (error oveview in a
separate frame); with a nil value the current frame is used.

If the display does not support multi frame, the current frame
will be used regardless of the value of this variable."
  :group 'TeX-output
  :type '(choice
          (const :tag "Error overview in separate frame" separate-frame)
          (const :tag "Use current frame" nil)))

(defun TeX-error-overview-setup ()
  "Return the frame setup of the error overview for the current display."
  (and (display-multi-frame-p) TeX-error-overview-setup))

(defun TeX-error-overview-goto-source (&optional button)
  "Go to the error point in the source.
If optional argument BUTTON is non-nil, go to source associated
to the selected error."
  (interactive)
  (let ((index (if button (button-get button 'id) (tabulated-list-get-id)))
        item window)
    (if index
        (progn
          ;; Select the source frame/window, if still live.
          (if (TeX-error-overview-setup)
              (if (frame-live-p TeX-error-overview-orig-frame)
                  (select-frame TeX-error-overview-orig-frame)
                (error "You have deleted a vital frame---\
please restart TeX error overview"))
            (if (window-live-p TeX-error-overview-orig-window)
                (select-window TeX-error-overview-orig-window)
              (error "You have deleted a vital window---\
please restart TeX error overview")))
          ;; Get the error details.
          (with-current-buffer TeX-error-overview-active-buffer
            (setq item (nth index TeX-error-list)
                  TeX-error-last-visited index))
          ;; Find the error and display the help.
          (with-current-buffer TeX-command-buffer
            ;; Find the error and display the help.
            (apply #'TeX-find-display-help item))
          ;; Return to the error overview.
          (if (TeX-error-overview-setup)
              (select-frame TeX-error-overview-frame)
            (if (setq window
                      (get-buffer-window TeX-error-overview-buffer-name))
                ;; If error overview window is visible just select it.
                (select-window window)
              ;; Otherwise, split the help window and display the error overview
              ;; near to it.  This should be the only reason for the error
              ;; overview window not being still visible after the beginning of
              ;; the function.
              (select-window
               (get-buffer-window (cond
                                   ((eq TeX-display-help 'expert)
                                    TeX-error-overview-active-buffer)
                                   (TeX-display-help  "*TeX Help*"))))
              (if (window-splittable-p (selected-window) t)
                  (split-window-horizontally)
                (split-window-vertically))
              (switch-to-buffer TeX-error-overview-buffer-name))))
      (message "No more errors.")
      (beep))))

(defun TeX-error-overview-make-entries (&optional master-dir active-buffer)
  "Generate the list of errors to be printed using `tabulated-list-entries'.
Write file names relative to MASTER-DIR when they are not absolute.

ACTIVE-BUFFER is used as buffer from which to extract the list of
errors.  If nil, defaults to `TeX-error-overview-active-buffer'."
  (with-current-buffer (or active-buffer TeX-error-overview-active-buffer)
    (let ((id 0)
          type file line msg entries)
      (mapc
       (lambda (entry)
         (setq type (nth 0 entry)
               file (nth 1 entry)
               line (nth 2 entry)
               msg  (nth 3 entry))
         ;; Add the entry only if it isn't to be skipped.
         (unless (TeX-error-list-skip-warning-p type (nth 10 entry))
           (push
            (list
             ;; ID.
             id
             (vector
              ;; File.
              (if (stringp file)
                  (if (file-name-absolute-p file)
                      file
                    (file-relative-name file master-dir))
                "")
              ;; Line.
              (if (numberp line)
                  (number-to-string line)
                "")
              ;; Type.
              (cond
               ((equal type 'error)
                (propertize "Error" 'font-lock-face 'TeX-error-description-error))
               ((equal type 'warning)
                (propertize "Warning" 'font-lock-face
                            'TeX-error-description-warning))
               ((equal type 'bad-box)
                (propertize "Bad box" 'font-lock-face
                            'TeX-error-description-warning))
               (t
                ""))
              ;; Message.
              (list (if (stringp msg)
                        ;; Sometimes, the message can be longer than one line,
                        ;; but print here only the first one.
                        (progn
                          (string-match "^.*" msg)
                          (match-string 0 msg))
                      "")
                    'face 'link
                    'follow-link t
                    'id id
                    'action #'TeX-error-overview-goto-source)))
            entries))
         ;; Increase the `id' counter in any case.
         (setq id (1+ id)))
       TeX-error-list)
      (reverse entries))))

(defun TeX-error-overview-next-error (&optional arg)
  "Move to the next line and find the associated error.

A prefix ARG specifies how many error messages to move; negative
means move back to previous error messages."
  (interactive "p")
  (if (= (forward-line arg) 0)
      (TeX-error-overview-goto-source)
    ;; If there are lines left to move we are at the beginning or at the end of
    ;; the buffer and there are no more errors.
    (message "No more errors.")
    (beep)))

(defun TeX-error-overview-previous-error (&optional arg)
  "Move to the previous line and find the associated error.

Prefix ARG says how many error messages to move backward (or
forward, if negative)."
  (interactive "p")
  (TeX-error-overview-next-error (- arg)))

(defun TeX-error-overview-jump-to-source ()
  "Display the help and move point to the error source."
  (interactive)
  (TeX-error-overview-goto-source)
  (pop-to-buffer
   (save-window-excursion
     (select-window TeX-error-overview-orig-window)
     (current-buffer))))

(defun TeX-error-overview-goto-log ()
  "Display the current error in log buffer."
  (interactive)
  (let ((TeX-display-help 'expert))
    (TeX-error-overview-goto-source)))

(defun TeX-error-overview-toggle-debug-bad-boxes ()
  "Run `TeX-toggle-debug-bad-boxes' and update entries list."
  (interactive)
  (TeX-toggle-debug-bad-boxes)
  (setq tabulated-list-entries
        (TeX-error-overview-make-entries
         (with-current-buffer TeX-command-buffer (TeX-master-directory))))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun TeX-error-overview-toggle-debug-warnings ()
  "Run `TeX-toggle-debug-warnings' and update entries list."
  (interactive)
  (TeX-toggle-debug-warnings)
  (setq tabulated-list-entries
        (TeX-error-overview-make-entries
         (with-current-buffer TeX-command-buffer (TeX-master-directory))))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun TeX-error-overview-toggle-suppress-ignored-warnings ()
  "Toggle visibility of ignored warnings and update entries list."
  (interactive)
  (TeX-toggle-suppress-ignored-warnings)
  (setq tabulated-list-entries
        (TeX-error-overview-make-entries
         (with-current-buffer TeX-command-buffer (TeX-master-directory))))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun TeX-error-overview-quit ()
  "Delete the window or the frame of the error overview."
  (interactive)
  (if (TeX-error-overview-setup)
      (delete-frame TeX-error-overview-frame)
    (delete-window))
  (setq TeX-error-overview-orig-frame nil))

(defvar TeX-error-overview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "b"    #'TeX-error-overview-toggle-debug-bad-boxes)
    (define-key map "j"    #'TeX-error-overview-jump-to-source)
    (define-key map "l"    #'TeX-error-overview-goto-log)
    (define-key map "n"    #'TeX-error-overview-next-error)
    (define-key map "p"    #'TeX-error-overview-previous-error)
    (define-key map "q"    #'TeX-error-overview-quit)
    (define-key map "w"    #'TeX-error-overview-toggle-debug-warnings)
    (define-key map "x"    #'TeX-error-overview-toggle-suppress-ignored-warnings)
    (define-key map "\C-m" #'TeX-error-overview-goto-source)
    map)
  "Local keymap for `TeX-error-overview-mode' buffers.")

(easy-menu-define TeX-error-overview-menu
  TeX-error-overview-mode-map
  "Menu used in TeX error overview mode."
  '("TeX errors"
    ["Next error" TeX-error-overview-next-error
     :help "Jump to the next error"]
    ["Previous error" TeX-error-overview-previous-error
     :help "Jump to the previous error"]
    ["Go to source" TeX-error-overview-goto-source
     :help "Show the error in the source"]
    ["Jump to source" TeX-error-overview-jump-to-source
     :help "Move point to the error in the source"]
    ["Go to log" TeX-error-overview-goto-log
     :help "Show the error in the log buffer"]
    "-"
    ["Debug Bad Boxes" TeX-error-overview-toggle-debug-bad-boxes
     :style toggle :selected TeX-debug-bad-boxes
     :help "Show overfull and underfull boxes"]
    ["Debug Warnings" TeX-error-overview-toggle-debug-warnings
     :style toggle :selected TeX-debug-warnings
     :help "Show warnings"]
    ["Ignore Unimportant Warnings"
     TeX-error-overview-toggle-suppress-ignored-warnings
     :style toggle :selected TeX-suppress-ignored-warnings
     :help "Hide specified warnings"]
    "-"
    ["Quit" TeX-error-overview-quit
     :help "Quit"]))

(defvar TeX-error-overview-list-entries nil
  "List of errors to be used in the error overview.")

(define-derived-mode TeX-error-overview-mode tabulated-list-mode
                     "TeX errors"
  "Major mode for listing TeX errors."
  :syntax-table nil :abbrev-table nil :interactive nil
  (setq tabulated-list-format [("File" 25 nil)
                               ("Line" 4 nil :right-align t)
                               ("Type" 7 nil)
                               ("Message" 0 nil)]
        tabulated-list-padding 1
        tabulated-list-entries TeX-error-overview-list-entries)
  (tabulated-list-init-header)
  (tabulated-list-print))

(defcustom TeX-error-overview-frame-parameters
  '((name . "TeX errors")
    (title . "TeX errors")
    (height . 10)
    (width . 80)
    (top . (- 0))
    (left . (- 0))
    (unsplittable . t)
    (minibuffer . nil)
    (vertical-scroll-bars . t)
    (tool-bar-lines . 0))
  "Parameters of the error overview frame."
  :group 'TeX-output
  :type 'alist
  :options '((name string) (title string) (height integer) (width integer)
             (top integer) (left integer) (unsplittable boolean)
             (minibuffer boolean) (vertical-scroll-bars boolean)
             (tool-bar-lines integer)))

(defcustom TeX-error-overview-open-after-TeX-run nil
  "Whether to open automatically the error overview after running TeX."
  :group 'TeX-output
  :type 'boolean)

(defun TeX-error-overview ()
  "Show an overview of the errors occurred in the last TeX run."
  (interactive)
  ;; Check requirements before start.
  (if (setq TeX-error-overview-active-buffer (TeX-active-buffer))
      ;; `TeX-error-overview-list-entries' is going to be used only as value
      ;; of `tabulated-list-entries' in `TeX-error-overview-mode'.  In
      ;; principle, we don't need `TeX-error-overview-list-entries', but
      ;; `tabulated-list-entries' is buffer-local and we need the list of
      ;; entries before creating the error overview buffer in order to
      ;; decide whether we need to show anything.
      (if (setq TeX-error-overview-list-entries
                (TeX-error-overview-make-entries
                 (TeX-master-directory)))
          (progn
            (setq TeX-error-overview-orig-window (selected-window)
                  TeX-error-overview-orig-frame
                  (window-frame TeX-error-overview-orig-window))
            ;; Create the error overview buffer.  This is
            ;; automatically killed before running TeX commands, so if
            ;; exists it is up-to-date and doesn't need to be
            ;; re-created.
            (unless (get-buffer TeX-error-overview-buffer-name)
              (with-current-buffer
                  (get-buffer-create TeX-error-overview-buffer-name)
                (TeX-error-overview-mode)))
            ;; Move point to the line associated to the last visited
            ;; error.
            (with-current-buffer TeX-error-overview-buffer-name
              (goto-char (point-min))
              (forward-line (with-current-buffer
                                TeX-error-overview-active-buffer
                              TeX-error-last-visited))
              ;; Create a new frame for the error overview or display the
              ;; buffer in the same frame, depending on the setup.
              (if (TeX-error-overview-setup)
                  (if (frame-live-p TeX-error-overview-frame)
                      ;; Do not create a duplicate frame if there is
                      ;; already one, just select it.
                      (select-frame-set-input-focus
                       TeX-error-overview-frame)
                    ;; Create a new frame and store its name.
                    (select-frame
                     (setq TeX-error-overview-frame
                           (make-frame
                            TeX-error-overview-frame-parameters)))
                    (set-window-buffer (selected-window)
                                       TeX-error-overview-buffer-name)
                    (set-window-dedicated-p (selected-window) t))
                (TeX-pop-to-buffer TeX-error-overview-buffer-name))))
        (error (concat "No error or warning to show"
                       ;; Suggest to display warnings and bad boxes with the
                       ;; appropriate key-bindings if there are such
                       ;; messages in the output buffer.  Rationale of the
                       ;; test: `TeX-error-overview-list-entries' is nil,
                       ;; but if `TeX-error-list' is not nil it means that
                       ;; there are hidden warnings/bad boxes.
                       (when (TeX-process-get-variable (TeX-active-master)
                                                       'TeX-error-list)
                         (format ".  Type `%s' and `%s' to display \
warnings and bad boxes"
                                 (substitute-command-keys
                                  "\\<TeX-mode-map>\\[TeX-toggle-debug-warnings]")
                                 (substitute-command-keys
                                  "\\<TeX-mode-map>\\[TeX-toggle-debug-bad-boxes]"))))))
    (error "No process for this document")))

;;; Output mode

(define-derived-mode TeX-special-mode special-mode "TeX"
  :syntax-table nil :abbrev-table nil :interactive nil)

(defvar TeX-output-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map TeX-special-mode-map)
    (define-key map "n" #'TeX-next-error)
    (define-key map "p" #'TeX-previous-error)
    (define-key map "b" #'TeX-toggle-debug-bad-boxes)
    (define-key map "w" #'TeX-toggle-debug-warnings)
    (define-key map "i" (lambda ()
                          (interactive)
                          (with-current-buffer TeX-command-buffer
                            (TeX-interactive-mode (if TeX-interactive-mode -1 1)))))
    (define-key map "s" (lambda ()
                          (interactive)
                          (with-current-buffer TeX-command-buffer
                            (TeX-source-correlate-mode (if TeX-source-correlate-mode -1 1)))))
    map)
  "Keymap for `TeX-output-mode'.")

(define-derived-mode TeX-output-mode TeX-special-mode "TeX Output"
  "Major mode for viewing TeX output.
\\{TeX-output-mode-map} "
  :syntax-table nil :abbrev-table nil :interactive nil
  (setq-local revert-buffer-function #'TeX-output-revert-buffer)
  ;; special-mode makes it read-only which prevents input from TeX.
  (setq buffer-read-only nil))

(defun TeX-output-revert-buffer (_ignore-auto _noconfirm)
  "Rerun the TeX command which of which this buffer was the output."
  (goto-char (point-min))
  (if (looking-at "Running `\\(.*\\)' on `\\(.*\\)' with ``\\(.*\\)''$")
      (let ((name (match-string 1))
            (file (match-string 2)))
        (with-current-buffer TeX-command-buffer
          (TeX-command name (if (string-match TeX-region file)
                                #'TeX-region-file
                              #'TeX-master-file))))
    (error "Unable to find what command to run")))

(defun TeX-buffer-file-name (&optional BUFFER)
  "Return name of file BUFFER is visiting, or nil if none.
No argument or nil as argument means use the current buffer.
If BUFFER is indirect, return the file that the base buffer is visiting."
  (buffer-file-name (or (buffer-base-buffer BUFFER) BUFFER)))

(provide 'tex)

;; Local Variables:
;; coding: utf-8
;; End:

;;; tex.el ends here
