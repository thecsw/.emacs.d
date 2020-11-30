;;(package-initialize)
(org-babel-load-file "~/.emacs.d/configuration.org")
;;(setq debug-on-error t)
(provide '.emacs)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d9aa334b2011d57c8ce279e076d6884c951e82ebc347adbe8b7ac03c4b2f3d72" default))
 '(git-messenger:use-magit-popup t)
 '(package-selected-packages
   '(exec-path-from-shell adoc-mode color-identifiers-mode yasnippet-snippets yasnippet-classic-snippets yaml-mode wolfram-mode wolfram which-key use-package undo-propose tramp smex sed-mode rust-mode rg projectile project prettier powerline posframe origami org-superstar org-special-block-extras org-ref org-chef olivetti matlab-mode markdown-mode+ magit lush-theme lua-mode lsp-ui lsp-haskell goto-line-preview gopher go-tag go-snippets git-messenger gemini-mode format-all fish-mode figlet erlang emojify-logos emms-state elpher eldoc dotenv-mode dot-mode dockerfile-mode docker-api docker deadgrep dashboard csv-mode cquery company clang-format caddyfile-mode autopair ascii-table ascii-art-to-unicode artbollocks-mode arduino-mode anzu ac-html abyss-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
