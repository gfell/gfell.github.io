(TeX-add-style-hook "examsolution"
 (lambda ()
    (LaTeX-add-labels
     "eq:1"
     "eq:4"
     "eq:16"
     "eq:18"
     "eq:19"
     "eq:23"
     "eq:25"
     "eq:2"
     "eq:3")
    (TeX-run-style-hooks
     "setspace"
     "amssymb"
     "amsmath"
     "fullpage"
     "latex2e"
     "art12"
     "article"
     "a4"
     "12pt")))

