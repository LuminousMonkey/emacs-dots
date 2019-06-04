;; -*- coding: utf-8; lexical-binding: t; -*-

;; Auto complete
(use-package company
  :defer 5
  :diminish
  :commands (company-mode company-indent-or-complete-common)
  :bind (("C-<tab>" . company-indent-or-complete-common)
         :map company-mode-map
         ([remap completion-at-point] . company-indent-or-complete-common)
         ([remap complete-symbol] . company-indent-or-complete-common))
  :config
  ;; Nicer Keybindings
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)

  (global-company-mode 1))

(provide 'core-completion)
