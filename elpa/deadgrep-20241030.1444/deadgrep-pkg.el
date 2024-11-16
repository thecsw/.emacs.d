;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "deadgrep" "20241030.1444"
  "Fast, friendly searching with ripgrep."
  '((emacs   "25.1")
    (dash    "2.12.0")
    (s       "1.11.0")
    (spinner "1.7.3"))
  :url "https://github.com/Wilfred/deadgrep"
  :commit "2f4ac50e297ce6fe219606f4709ad6cab4436914"
  :revdesc "2f4ac50e297c"
  :keywords '("tools")
  :authors '(("Wilfred Hughes" . "me@wilfred.me.uk"))
  :maintainers '(("Wilfred Hughes" . "me@wilfred.me.uk")))
