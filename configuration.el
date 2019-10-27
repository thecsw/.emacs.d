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

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

(add-to-list 'exec-path "/home/thecsw/go/bin")

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(defun auto-complete-for-go ()
(auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)
(with-eval-after-load 'go-mode
(require 'go-autocomplete))
(global-set-key (kbd"C-c C-c") 'godef-jump)

;;(require 'company)
;;(require 'smex)
;;(add-hook 'after-init-hook 'global-company-mode)
;;(global-set-key (kbd "M-n") 'company-complete)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))


(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;;(add-to-list 'load-path "~/.emacs.d/evil")
;;(require 'evil)
;;(evil-mode 1)
;;(setq evil-default-state 'emacs) ;; changes default state to emacs

(global-set-key (kbd "C-x g") 'magit-status)

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
'("xelatex -shell-escape -interaction nonstopmode -output-directory %o %b %f"
"bibtex %b"
"makeindex %b"
"xelatex -shell-escape -interaction nonstopmode -output-directory %o %b %f"
"xelatex -shell-escape -interaction nonstopmode -output-directory %o %b %f"))

;; (setq org-latex-listings 'minted
;;       org-latex-packages-alist '(("" "minted")))

(setq tramp-default-method "ssh")

(global-set-key (kbd "M-x") 'smex)
