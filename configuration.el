(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(add-hook 'after-init-hook 'global-company-mode)
(ac-config-default)
(global-auto-complete-mode t)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(setq ac-auto-start 1)
(setq ac-auto-show-menu 0.8)

;; (add-hook 'markdown-mode-hook 'ac-emoji-setup)
;; (add-hook 'git-commit-mode-hook 'ac-emoji-setup)

;; (set-fontset-font
;;  t 'symbol
;;  (font-spec :family "Symbola") nil 'prepend)

(setq-default c-basic-offset 8
	      c-default-style "k&r"
	      tab-width 8
	      indent-tabs-mode t)

(setq-default python-basic-offset 8
	      tab-width 8
	      indent-tabs-mode t)

(setq backup-directory-alist `(("." . ".saves")))

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(menu-bar-mode -1)

(setq linum-format "%4d  ")
(global-linum-mode 1)

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq ring-bell-function 'ignore)

(add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(require 'autopair)
(autopair-global-mode)

(global-set-key (kbd "<f1>") 'shell)

(show-paren-mode 1)
(setq show-paren-delay 0)

(setq org-html-validation-link nil)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(add-to-list 'exec-path "/home/thecsw/go/bin")

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(auto-complete-mode 1)
(add-hook 'go-mode-hook 'auto-complete-for-go)
(require 'go-autocomplete)
(global-set-key (kbd"C-c C-c") 'godef-jump)

;;(require 'company)
;;(require 'smex)
;;(add-hook 'after-init-hook 'global-company-mode)
;;(global-set-key (kbd "M-n") 'company-complete)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))


;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(global-set-key (kbd "C-x g") 'magit-status)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dockerfile-mode yaml-mode w3m tramp solidity-mode smex powerline org-ref olivetti matlab-mode markdown-mode+ magit irony-eldoc goto-chg go-scratch go-gopath go-gen-test go-complete go-autocomplete git-gutter-fringe format-all elisp-benchmarks dot-mode dashboard cquery clang-format caddyfile-mode autopair ac-emoji abyss-theme)))
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

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)

(require 'powerline)
(powerline-center-theme)
;;(powerline-default-theme)
;;(powerline-center-evil-theme)
;;(powerline-vim-theme)
;;(powerline-nano-theme)

(require 'org-ref)
(setq org-latex-pdf-process
'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b %f"
"bibtex %b"
"makeindex %b"
"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b %f"
"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b %f"))

;; (setq org-latex-listings 'minted
;;       org-latex-packages-alist '(("" "minted")))

(setq tramp-default-method "ssh")

(global-set-key (kbd "M-x") 'smex)

(load-theme 'lush t)

(setq inferior-lisp-program "sbcl")
