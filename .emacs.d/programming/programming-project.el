;; -*- coding: utf-8; lexical-binding: t; -*-

;; Projectile
(use-package projectile
  :demand t
  :commands projectile-global-mode
  :config
  (projectile-global-mode)
  :diminish projectile-mode)

(use-package helm-projectile
  :bind ("C-c C-f" . helm-projectile-find-file)
  :config
  (helm-projectile-on))

(provide 'programming-project)
