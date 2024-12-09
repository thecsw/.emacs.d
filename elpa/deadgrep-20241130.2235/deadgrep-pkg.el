;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "deadgrep" "20241130.2235"
  "Fast, friendly searching with ripgrep."
  '((emacs   "25.1")
    (dash    "2.12.0")
    (s       "1.11.0")
    (spinner "1.7.3"))
  :url "https://github.com/Wilfred/deadgrep"
  :commit "d89468d82abb778ef0938c5753be4498d5802a10"
  :revdesc "d89468d82abb"
  :keywords '("tools")
  :authors '(("Wilfred Hughes" . "me@wilfred.me.uk"))
  :maintainers '(("Wilfred Hughes" . "me@wilfred.me.uk")))
