;; -*- coding: utf-8; lexical-binding: t; -*-

;; Auto complete
(use-package company
  :init (global-company-mode)
  :blackout company-mode
  :commands (company-mode company-complete-common)
  :bind (("C-<tab>" . company-complete-common)
         :map company-mode-map
         ([remap completion-at-point] . company-complete-common)
         ([remap complete-symbol] . company-complete-common))
  :config
  ;; Nicer Keybindings
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer))

(provide 'core-completion)
