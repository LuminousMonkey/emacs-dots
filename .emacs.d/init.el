;; Note: Emacs can be slow to start unless the host has a FQDN.

;; Remove all the GUI cruft. This is done as soon as possible to try
;; and reduce the time they're shown at all.
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; No start up or scratch messages, I don't read them.
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq ns-use-srgb-colorspace nil)

;; Use local packages.
(eval-and-compile
  (mapc
   #'(lambda (path)
       (push (expand-file-name path user-emacs-directory) load-path))
   '("site-lisp" "site-lisp/use-package" "override" "lisp")))

(eval-and-compile
  (defvar use-package-verbose t)
  (require 'cl)
  (require 'use-package))

;; These are needed by auto-compile and must be ahead of it.
(use-package dash :defer t :load-path "lib/dash")
(use-package packed :defer t :load-path "lib/packed")

(use-package auto-compile
  :load-path "lib/auto-compile"
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(eval-and-compile
  (push (expand-file-name "lib" user-emacs-directory) load-path))

(use-package let-alist :defer t)
(use-package popwin :defer t :load-path "lib/popwin")
(use-package s :defer t :load-path "lib/s")

(use-package org :load-path "override/org-mode")

;; Make adding hooks to modes a little nicer to specify.
(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

;; Always follow symbolic links to source controlled files.
;; I used VC with my dotfiles, and the prompt (since I now use org-babel) is
;; annoying.
(setq vc-follow-symlinks t)
(org-babel-load-file (expand-file-name "monkey.org" user-emacs-directory))
