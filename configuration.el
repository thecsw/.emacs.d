(require 'package)
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

;; Enable default auto-complete
;;    (ac-config-default)
;;    (global-auto-complete-mode t)
;;    (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;; ; Quick auto-complete
;;    (setq ac-auto-start 1)
;;    (setq ac-auto-show-menu 0.8)

(setq-default c-basic-offset 8
	      c-default-style "k&r"
	      tab-width 8
	      indent-tabs-mode t)

(setq-default python-basic-offset 8
	      tab-width 8
	      indent-tabs-mode t)

(add-hook 'html-mode-hook
	  (lambda ()
	    ;; Default indentation is usually 2 spaces, changing to 4.
	    (set (make-local-variable 'sgml-basic-offset) 4)))

(defun html-show-toc ()
  "Shows a TOC based on headings tags <H[1-6]>"
  (interactive)
  (if sgml-tags-invisible
      (error "SGML tags are invisible")
    (occur "<h[1-6]>")
    (pop-to-buffer "*Occur*")
    (vc-toggle-read-only)
    (goto-char (point-min))
    (replace-string "<h1>" "")
    (goto-char (point-min))
    (replace-string "<h2>" "  ")
    (goto-char (point-min))
    (replace-string "<h3>" "    ")
    (goto-char (point-min))
    (replace-string "<h4>" "      ")
    (goto-char (point-min))
    (replace-string "<h5>" "        ")
    (goto-char (point-min))
    (replace-string "<h6>" "          ")
    (goto-char (point-min))
    (replace-regexp "</h[1-6]>" "")
    (goto-char (point-min))
    (toggle-read-only 1)))

(defun html-end-of-line ()
  "If there is an HTML tag at the end of the line, then go to start of tag.
 Otherwise go to the real end of the line."
  (interactive)
  (if (or (looking-at ".*>$") ; if we're on a line that ends with a tag
	  (and (= (char-before) 62)
	       (= (point) (save-excursion
			    (end-of-line)
			    (point))))) ; or we're at the end of a line
					; with a tag
      (let ((where-now (point)))
	(narrow-to-region
	 (save-excursion
	   (beginning-of-line)
	   (point))
	 (save-excursion
	   (end-of-line)
	   (point)))
	(end-of-line)
	(re-search-backward "<" nil t)
	(if (= (point) where-now)
	    (end-of-line))
	(widen))
    (end-of-line)))

(add-hook 'html-helper-mode-hook
	  (lambda ()
	    (define-key html-helper-mode-map "\C-e" 'html-end-of-line)))

;; Instea of putting *~ backups in current directory,
;; put them in local .saves
(setq backup-directory-alist `(("." . ".saves")))

;; Just to stop spamming backup files
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

(global-set-key [remap goto-line] 'goto-line-preview)

;; Give emacs some RAW power, yes
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package lsp-mode
  :hook ((go-mode . lsp)
	 (rust-mode . lsp)
	 (c++-mode . lsp)
	 (c-mode . lsp)
	 (js-mode . lsp)
	 (html-mode . lsp)
	 (python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(global-set-key (kbd"C-c C-c") 'lsp-find-definition)
(global-set-key (kbd"C-c f") 'lsp-find-definition)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Automatically format code on save
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
;; (use-package company
;;   :ensure t
;;   :config
;;   ;; Optionally enable completion-as-you-type behavior.
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 1))

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

;; Enable dashboard
(require 'dashboard)
;; Add the hook
(dashboard-setup-startup-hook)
;; Set the dashboard as the default buffer
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Press C-x g to open magit
(global-set-key (kbd "C-x g") 'magit-status)

(require 'powerline)
;; Use the vim powerline theme
(powerline-default-theme)

;; Use org-ref
(require 'org-ref)

;; Build nonstopmode with pdflatex
(setq org-latex-pdf-process
'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b %f"
"bibtex %b"
"makeindex %b"
"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b %f"
"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %b %f"))

;; Default to ssh when using tramp
(setq tramp-default-method "ssh")

;; Fuzzy command complete on M-x
(global-set-key (kbd "M-x") 'smex)

;; I like lush and use it by default
;(load-theme 'lush t)
(load-theme 'abyss t)

(setq org-capture-templates
      '(("c" "Cookbook" entry (file "~/org/cookbook.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
        ("m" "Manual Cookbook" entry (file "~/org/cookbook.org")
         "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredients\n   %?\n** Directions\n\n")))

;; Add the Unicode bullets package
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
;; This is usually the default, but keep in mind it must be nil
(setq org-hide-leading-stars nil)
;; This line is necessary.
(setq org-superstar-leading-bullet ?\s)
;; Add the new fancy extra org mode blocks
(add-hook 'org-mode-hook #'org-special-block-extras-mode)
;; Add timestamp when marked DONE
(setq org-log-done 'time)

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
 )

;; Press C-c d to open docker interface
;;(global-set-key (kbd "C-c d") 'docker)

;; Press C-c c to open git-messenge
(global-set-key (kbd "C-c c") 'git-messenger:popup-message)
(custom-set-variables
 '(git-messenger:use-magit-popup t))

(add-hook 'after-init-hook #'global-emojify-mode)

(global-set-key (kbd "<f5>") #'deadgrep)

(global-anzu-mode +1)

(autoload 'artbollocks-mode "artbollocks-mode")
(add-hook 'text-mode-hook 'artbollocks-mode)

(emms-all)
(emms-default-players)

(put 'narrow-to-region 'disabled nil)

(which-key-mode)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(add-hook 'gemini-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'gemini-mode-hook 'turn-on-auto-fill)
