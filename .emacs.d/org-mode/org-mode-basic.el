;;; Basic Org Mode Settings

(use-package org
  :init
  ;; Show the formatted text of *bold*, /italics/, but not the
  ;; characters used to mark then out.
  (setq org-hide-emphasis-markers t)

  ;; Show any source blocks in their native mode.
  (setq org-src-fontify-natively t)

  ;; Org mode modules I want to use.
  (setq org-modules '(org-bbdb))

  (eval-after-load 'org
    '(org-load-modules-maybe t))

  ;; Use CSS for any HTML output.
  (setq org-export-htmlize-output-type 'css)

  :config
  (eval-after-load 'org-agenda
    '(bind-key "i" 'org-agenda-clock-in org-agenda-mode-map))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . nil)
     (R . t)))
  (defun luminousmonkey-org-confirm-babel-eval (lang body)
    (not (or (string= lang "latex")
             (string= lang "R"))))
  (setq org-confirm-babel-evaluate 'luminousmonkey-org-confirm-babel-eval)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)))

(setq org-latex-pdf-process
  '("xelatex -interaction nonstopmode %f"
    "xelatex -interaction nonstopmode %f")) ;; for multiple passes

(setq org-latex-with-hyperref nil)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("luminousmonkey-org-article"
                "\\documentclass[12pt,a4paper]{article}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{graphicx}
\\defaultfontfeatures{Mapping=tex-text}
\\setromanfont{Equity Text B}
\\usepackage{geometry}
\\geometry{a4paper, textwidth=6.5in, textheight=10in,
           marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
\\title{}
        [NO-DEFAULT-PACKAGES]
        [NO-PACKAGES]"
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")
                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(provide 'org-mode-basic)
