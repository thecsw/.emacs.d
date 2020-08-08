;;(package-initialize)
(org-babel-load-file "~/.emacs.d/configuration.org")
(provide '.emacs)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-messenger:use-magit-popup t)
 '(package-selected-packages
   '(yaml-mode tramp smex powerline origami org-superstar org-special-block-extras org-ref org-chef matlab-mode markdown-mode+ magit lush-theme go-rename go-gopath go-autocomplete format-all dot-mode dockerfile-mode dashboard cquery company clang-format caddyfile-mode autopair)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
