;;;; Package --- Emacs initialisation of LuminousMonkey
;;; Commentary:
;; Emacs initialisation starting point.

;; Note: Emacs can be slow to start unless the host has a FQDN.

;; I've copied bits of this config from:
;; https://github.com/mrvdb/emacs-config/

;;; Code:

;; Remove all the GUI cruft. This is done as soon as possible to try
;; and reduce the time they're shown at all.
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; No start up or scratch messages, I don't read them.
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; I keep my configs using dotdotdot, and I use hardlinks.
;; Make sure Emacs perserves hardlinks.
(setq backup-by-copying-when-linked t)

(package-initialize)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents) ;; Just to make sure (note that refresh
                             ;; only runs if use-package is NOT
                             ;; installed)
  (package-install 'use-package)
  (eval-when-compile
    (setq use-package-verbose t)
    (setq use-package-always-ensure t)
    (require 'use-package)))

;; These are needed by auto-compile and must be ahead of it.
(use-package dash :defer t)
(use-package packed :defer t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(use-package org)

(use-package diminish)

(use-package exec-path-from-shell)

;; ASpell on Windows
(if (eq system-type 'windows-nt)
    (add-to-list 'exec-path "c:/Program Files (x86)/Aspell/bin/"))

;; Make adding hooks to modes a little nicer to specify.
(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

;; Always follow symbolic links to source controlled files.
;; I used VC with my dotfiles, and the prompt (since I now use org-babel) is
;; annoying.
(setq vc-follow-symlinks t)
(org-babel-load-file (expand-file-name "monkey.org" user-emacs-directory))
