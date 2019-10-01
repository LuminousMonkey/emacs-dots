(use-package kickasm-mode
  :straight (kickasm-mode :type git :host github :repo "mweidhagen/kickasm-mode")
  :init
  (setq kickasm-mnemonic-indent 2)
  (setq kickasm-command-start-indent 0))

(provide 'programming-6502)
