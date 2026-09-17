;;; .emacs --- startup entry point -*- lexical-binding: t; -*-

;; Load during init so packages can register their startup hooks in time.
;; The Emacs 31 font workaround lives with the Centaur Tabs configuration.
(org-babel-load-file (expand-file-name "configuration.org" user-emacs-directory))
;;(setq debug-on-error t) ; <-- uncomment if emacs crashes
(provide '.emacs)
