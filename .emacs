;;(package-initialize)
;; Emacs 31 on macOS can crash in face_for_font/char-displayable-p while
;; loading packages before the initial graphical frame is fully initialized.
;; Load the literate configuration after startup so the frame/font system is
;; ready first.
(add-hook 'window-setup-hook
          (lambda ()
            ;; Wait until the initial NS frame exists and its font cache is
            ;; initialized; `after-init-hook` is too early on Emacs 31.
            (run-at-time 2 nil
                         (lambda ()
                           (org-babel-load-file
                            "~/.emacs.d/configuration.org")))))
;;(setq debug-on-error t) ; <-- uncomment if emacs crashes
(provide '.emacs)
