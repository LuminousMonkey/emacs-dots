;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package rainbow-mode
  :blackout rainbow-mode
  :hook
  (css-mode . rainbow-mode)
  (scss-mode . rainbow-mode))

(provide 'programming-html)
