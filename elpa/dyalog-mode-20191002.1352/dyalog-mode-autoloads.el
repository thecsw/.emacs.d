;;; dyalog-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dyalog-mode" "dyalog-mode.el" (0 0 0 0))
;;; Generated autoloads from dyalog-mode.el

(autoload 'dyalog-fix-altgr-chars "dyalog-mode" "\
Fix a key map so AltGr+char isn't confused with C-M-char.

KEYMAP is an Emacs keymap.

APLCHARS is a string of APL-characters produced by pressing AltGr together
with some character.

REGULARCHARS is a string of the characters that when pressed
together with AltGr produce the corresponding apl character in APLCHARS.

\(fn KEYMAP APLCHARS REGULARCHARS)" nil nil)

(autoload 'dyalog-ediff-forward-word "dyalog-mode" "\
Move point forward one word.

\(fn)" t nil)

(autoload 'dyalog-session-connect "dyalog-mode" "\
Connect to a Dyalog session.
HOST (defaults to localhost) and PORT (defaults to 7979) give
adress to connect to.

\(fn &optional HOST PORT)" t nil)

(autoload 'dyalog-editor-connect "dyalog-mode" "\
Connect to a Dyalog process as an editor.
HOST (defaults to localhost) and PORT (defaults to 8080) give
adress to connect to.

\(fn &optional HOST PORT)" t nil)

(autoload 'dyalog-mode "dyalog-mode" "\
Major mode for editing Dyalog APL code.

\\{dyalog-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.dyalog$" . dyalog-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dyalog-mode" '("dyalog-")))

;;;***

;;;### (autoloads nil nil ("dyalog-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dyalog-mode-autoloads.el ends here
