(use-package tuareg
  :defer t)

(use-package merlin
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  :config
  (setq merlin-command 'opam))

(use-package ocp-indent
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'ocp-indent-caml-mode-setup))

(use-package utop
  :diminish (utop-minor-mode . " µTop")
  :defer t
  :init
  (progn
    (add-hook 'tuareg-mode-hook 'utop-minor-mode))
  :config
  (setq utop-command "opam config exec -- utop -emacs"))

(provide 'programming-ocaml)
