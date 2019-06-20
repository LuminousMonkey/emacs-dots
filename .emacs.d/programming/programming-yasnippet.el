;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  ;; Adding yasnippet support to company
  (add-to-list 'company-backends '(company-yasnippet))
  (yas-global-mode t)
  :diminish yas-minor-mode)

(provide 'programming-yasnippet)
