;; These are the global type keybindings.
;; Mode-based keybindings are done via use-package.

;; This gets replaced later by Helm, but have it here if the config
;; runs into problems.
(define-key global-map (kbd "C-x C-m") 'execute-extended-command)
(define-key global-map (kbd "C-w ") 'backward-kill-word)
(define-key global-map (kbd "C-x C-k") 'kill-region)
(define-key global-map (kbd "C-c C-k") 'kill-region)

(provide 'core-keybindings)
