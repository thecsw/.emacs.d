;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "copilot" "20260331.713"
  "An Emacs plugin for GitHub Copilot."
  '((emacs         "27.2")
    (editorconfig  "0.8.2")
    (jsonrpc       "1.0.14")
    (compat        "30")
    (track-changes "1.4"))
  :url "https://github.com/copilot-emacs/copilot.el"
  :commit "ab5c58bc969f52f6d75e972658f2c3381c70b4fa"
  :revdesc "ab5c58bc969f"
  :keywords '("convenience" "copilot")
  :authors '(("zerol" . "z@zerol.me"))
  :maintainers '(("Bozhidar Batsov" . "bozhidar@batsov.dev")))
