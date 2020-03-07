Emacs has very good support for multiple fonts in a single
file.  Poet uses this support to make it much more convenient to
write prose within Emacs, with particular attention paid to
org-mode and markdown-mode.  Code blocks, tables, etc are
formatted in monospace text with the appropriate backgrounds.

Theme Customizations
- `poet-variable-headers`
   Enable / disable different text heights for different faces.

Recommended customizations for using this theme

- Set up the base fonts you'd like to use in Emacs before loading Poet
    (set-face-attribute 'default nil :family "Iosevka" :height 130)
    (set-face-attribute 'fixed-pitch nil :family "Iosevka")
    (set-face-attribute 'variable-pitch nil :family "Baskerville")
  On loading this theme captures the default and treats that for fixed-pitch
  rendering.

- Enable variable pitch mode for editing text
    (add-hook 'text-mode-hook
               (lambda ()
                (variable-pitch-mode 1))

- Some other modes I like to enable/disable
    (olivetti-mode 1)        ;; Centers text in the buffer
    (flyspell-mode 1)        ;; Catch Spelling mistakes
    (typo-mode 1)            ;; Good for symbols like em-dash
    (blink-cursor-mode 0)    ;; Reduce visual noise
    (linum-mode 0)           ;; No line numbers for prose

- And prettier org mode bullets:
    (setq org-bullets-bullet-list
        '("◉" "○"))
    (org-bullets 1)
