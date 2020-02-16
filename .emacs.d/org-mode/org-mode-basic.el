;;; Basic Org Mode Settings

(use-package org-bullets
  :after org
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list
   '("◉" "○")))

(use-package org
  :init
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
  (defun luminousmonkey-org-confirm-babel-eval (lang body)
    (not (or (string= lang "latex")
             (string= lang "R"))))
  (setq org-confirm-babel-evaluate 'luminousmonkey-org-confirm-babel-eval)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link))
  :custom
  (org-src-window-setup 'current-window)
  (org-babel-load-languages
   '((emacs-lisp . t)
     (R . t)))
  (org-confirm-babel-evaluate nil)
  (org-catch-invisible-edits 'show)
  :custom-face
  (variable-pitch ((t (:family "ETBookOT"))))
  (org-done ((t (:strike-through t :weight bold))))
  (org-headline-done ((t (:strike-through t))))
  (org-level-1 ((t (:height 1.3 :weight bold))))
  (org-level-2 ((t (:height 1.2 :weight bold))))
  (org-level-3 ((t (:height 1.1 :weight bold)))))

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

(defun luminousmonkey/style-org ()
  (setq line-spacing 0.2)
  (variable-pitch-mode +1)
  (mapc
   (lambda (face) ;; Other fonts with fixed-pitch.
     (set-face-attribute face nil :inherit 'fixed-pitch))
   (list 'org-code
         'org-link
         'org-block
         'org-table
         'org-verbatim
         'org-block-begin-line
         'org-block-end-line
         'org-meta-line
         'org-document-info-keyword)))

(add-hook 'org-mode-hook #'luminousmonkey/style-org)

(setq org-startup-indented nil
      org-hide-leading-stars nil
      org-hide-emphasis-markers nil
      org-pretty-entities nil
      org-adapt-indentation nil)

(provide 'org-mode-basic)
