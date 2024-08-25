(define-package "magit" "20240822.1737" "A Git porcelain inside Emacs."
  '((emacs "26.1")
    (compat "30.0.0.0")
    (dash "20210826")
    (git-commit "20240808")
    (magit-section "20240808")
    (seq "2.24")
    (transient "20240805")
    (with-editor "20240806"))
  :commit "fe427e64382db783d33cf1cfd46fe851dc3c8113" :authors
  '(("Marius Vollmer" . "marius.vollmer@gmail.com")
    ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainer
  '("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
  :keywords
  '("git" "tools" "vc")
  :url "https://github.com/magit/magit")
;; Local Variables:
;; no-byte-compile: t
;; End:
