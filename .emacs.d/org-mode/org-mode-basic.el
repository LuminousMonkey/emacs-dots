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
  :bind (("C-c r" . org-capture)
	 ("C-c a" . org-agenda))
  )

(provide 'org-mode-basic)
