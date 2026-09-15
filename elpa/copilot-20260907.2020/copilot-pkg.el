;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "copilot" "20260907.2020"
  "An Emacs plugin for GitHub Copilot."
  '((emacs         "27.2")
    (editorconfig  "0.8.2")
    (jsonrpc       "1.0.14")
    (compat        "30")
    (track-changes "1.4"))
  :url "https://github.com/copilot-emacs/copilot.el"
  :commit "90f429b100418d897b673d6c36c2817364b28e3e"
  :revdesc "90f429b10041"
  :keywords '("convenience" "copilot")
  :authors '(("zerol" . "z@zerol.me"))
  :maintainers '(("Bozhidar Batsov" . "bozhidar@batsov.dev")))
