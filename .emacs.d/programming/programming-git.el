;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))

;; Mark uncommitted changes in the fringe.
(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode t)
  :diminish git-gutter-mode)

(provide 'programming-git)
