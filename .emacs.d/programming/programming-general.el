;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package aggressive-indent
  :blackout aggressive-indent-mode
  :config
  (global-aggressive-indent-mode +1)
  :custom
  (aggressive-indent-excluded-modes
   '(bibtex-mode
     cider-repl-mode
     c-mode
     c++-mode
     coffee-mode
     comint-mode
     conf-mode
     Custom-mode
     diff-mode
     doc-view-mode
     dos-mode
     erc-mode
     jabber-chat-mode
     haml-mode
     intero-mode
     haskell-mode
     interative-haskell-mode
     haskell-interactive-mode
     image-mode
     makefile-mode
     makefile-gmake-mode
     minibuffer-inactive-mode
     nix-mode
     netcmd-mode
     python-mode
     sass-mode
     slim-mode
     special-mode
     shell-mode
     snippet-mode
     eshell-mode
     tabulated-list-mode
     term-mode
     TeX-output-mode
     text-mode
     yaml-mode
     scala-mode)))

;; Guess indenting and try to preserve.
(use-package dtrt-indent
  :blackout t
  :config
  (dtrt-indent-mode +1))

(provide 'programming-general)
