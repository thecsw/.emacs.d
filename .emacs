(package-initialize)

(org-babel-load-file "~/.emacs.d/configuration.org")

(provide '.emacs)
(setq olivetti-body-width 80)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yaml-mode tramp smex powerline origami org-superstar org-special-block-extras org-ref org-chef olivetti matlab-mode markdown-mode+ magit lush-theme go-rename go-gopath go-autocomplete format-all dot-mode dockerfile-mode dashboard cquery company clang-format caddyfile-mode autopair)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(diff-function ((t (:inherit nil))))
 '(line-number ((t (:inherit nil))))
 '(linum ((t (:background "unspecified-bg" :foreground "#565761" :slant italic))))
 '(magit-diff-added ((t (:foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:background "color-233" :foreground "#22aa22"))))
 '(magit-diff-context-highlight ((t (:background "color-233" :foreground "brightyellow"))))
 '(magit-diff-file-heading-highlight ((t (:foreground "magenta"))))
 '(magit-diff-removed ((t (:foreground "#aa2222"))))
 '(magit-diff-removed-highlight ((t (:background "color-233" :foreground "#aa2222"))))
 '(magit-section-highlight ((t (:background "color-232" :foreground "brightmagenta" :weight bold)))))
