;; -*- coding: utf-8; lexical-binding: t; -*-

;; Projectile
(use-package projectile
  :blackout projectile-mode
  :demand t
  :commands projectile-global-mode
  :config
  (projectile-global-mode))

(use-package helm-projectile
  :bind ("C-c C-f" . helm-projectile-find-file)
  :config
  (helm-projectile-on))

(provide 'programming-project)
