;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package yasnippet
  :blackout ((yas-global-mode . t)
             (yas-minor-mode . t))
  :init
  (yas-global-mode t)
  :config
  (use-package yasnippet-snippets)
  ;; Adding yasnippet support to company
  ;;(with-eval-after-load "company"
  ;;  (add-to-list 'company-backends '(company-yasnippet)))
  )

(provide 'programming-yasnippet)
