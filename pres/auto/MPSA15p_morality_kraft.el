(TeX-add-style-hook
 "MPSA15p_morality_kraft"
 (lambda ()
   (TeX-run-style-hooks
    "latex2e"
    "../calc/tab/a1_mis"
    "../calc/tab/rd2008y"
    "../calc/tab/rd2012y"
    "beamer"
    "beamer10"
    "xcolor"
    "natbib"
    "multicol"
    "booktabs"
    "wasysym"
    "graphicx"
    "color"
    "lmodern"
    "array")
   (TeX-add-symbols
    "emph"
    "newblock")
   (LaTeX-add-bibliographies
    "/data/Copy/1-src/lit/Literature")))

