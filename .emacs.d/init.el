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
   '("site-lisp" "override" "lisp")))

(package-initialize)

(setq package-enable-at-startup nil)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-verbose t)
  (setq use-package-always-ensure t)
  (require 'cl)
  (require 'use-package))

(setq load-prefer-newer t)

;; These are needed by auto-compile and must be ahead of it.
(use-package dash :defer t)
(use-package packed :defer t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(eval-and-compile
  (push (expand-file-name "lib" user-emacs-directory) load-path))

(use-package let-alist :defer t)
(use-package popwin :defer t :load-path "lib/popwin")
(use-package s :defer t :load-path "lib/s")

(use-package org :ensure t)

;; Make adding hooks to modes a little nicer to specify.
(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

;; Always follow symbolic links to source controlled files.
;; I used VC with my dotfiles, and the prompt (since I now use org-babel) is
;; annoying.
(setq vc-follow-symlinks t)
(org-babel-load-file (expand-file-name "monkey.org" user-emacs-directory))
