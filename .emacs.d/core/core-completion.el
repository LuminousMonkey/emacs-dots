;; -*- coding: utf-8; lexical-binding: t; -*-

;; Auto complete
(use-package company
  :defer 5
  :diminish
  :commands (company-mode company-complete-common)
  :bind (("C-<tab>" . company-complete-common)
         :map company-mode-map
         ([remap completion-at-point] . company-complete-common)
         ([remap complete-symbol] . company-complete-common))
  :config
  ;; Nicer Keybindings
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)

  (global-company-mode 1))

(provide 'core-completion)
