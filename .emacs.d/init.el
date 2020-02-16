;;;; Package --- Emacs initialisation of LuminousMonkey
;;; Commentary:
;; Emacs initialisation starting point.

;; Note: Emacs can be slow to start unless the host has a FQDN.

;; I've copied bits of this config from:
;; https://github.com/mrvdb/emacs-config/

;;; Code:

;; (package-initialize)

;; -*- lexical-binding: t -*-
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(use-package blackout
  :straight (blackout :host github :repo "raxod502/blackout")
  :demand t)

(use-package el-patch
         :straight t)

;; Personal Information
(setq user-full-name "Mike Alded"
      user-mail-address "mike.aldred@luminousmonkey.org")

;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.
(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package autorevert
  :straight nil
  :blackout t
  :hook
  (dired-mode . auto-revert-mode)
  :config
  (global-auto-revert-mode +1)
  :custom
  (auto-revert-verbose nil))

;; Make it easier to answer questions.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make the selection work like most people expect.
(delete-selection-mode t)
(transient-mark-mode t)

;; Sentences end in a single space
(setq sentence-end-double-space nil)

;; Code Style
;; Default indentation
(setq-default tab-width 2)
(setq-default js-indent-level 2)

;; Tab indentation is a disease; a cancer of this planet.
(setq-default indent-tabs-mode nil)

(setq-default truncate-lines t)

(defun luminousmonkey/truncate-lines-hook ()
  (setq truncate-lines nil))

(add-hook 'text-mode-hook 'luminousmonkey/truncate-lines-hook)

(setq create-lockfiles nil)

; Make sure we always use UTF-8.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq require-final-newline nil)
(setq mode-require-final-newline nil)

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)

;; Make compilation buffers scroll to follow the output, but stop scrolling
;; at the first error.
(setq compilation-scroll-output 'first-error)

(load-file (concat (file-name-directory load-file-name)
           "core/core-load-paths.el"))

;; Remove all the GUI cruft. This is done as soon as possible to try
;; and reduce the time they're shown at all.
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; No start up or scratch messages, I don't read them.
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; I keep my configs using dotdotdot, and I use hardlinks.
;; Make sure Emacs perserves hardlinks.
(setq backup-by-copying-when-linked t)

;; Fix MAC Alt handling
(setq mac-option-modifier 'meta)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)

;; Keybindings done early here, just if something stuffs up.
(require 'core-keybindings)

;;; Prevent Emacs-provided Org from being loaded

;; The following is a temporary hack until straight.el supports
;; building Org, see:
;;
;; * https://github.com/raxod502/straight.el/issues/211
;; * https://github.com/raxod502/radian/issues/410
;;
;; There are three things missing from our version of Org: the
;; functions `org-git-version' and `org-release', and the feature
;; `org-version'. We provide all three of those ourself, therefore.

;; Package `git' is a library providing convenience functions for
;; running Git.
(use-package git)

(defun org-git-version ()
  "The Git version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)
(provide 'org-mode-holidays)

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
(straight-use-package 'org)

(require 'core-funcs)

;; Theme, etc
(require 'core-appearance)

;; Visual Bell only
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; Packages
(require 'core-navigation)

;; Winner Mode
;; Enable going back to previous Emacs layouts.
(use-package winner
  :defer t)

;; Always newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

(use-package ethan-wspace
  :blackout " ☐"
  :commands global-ethan-wspace-mode
  :init
  (progn
    (global-ethan-wspace-mode)
    (setq mode-require-final-newline nil)))

(use-package smartparens
  :blackout " ⚖"
  :init
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1))
  :config
  (progn
    (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

    (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
    (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

    (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
    (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
    (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

    (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
    (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

    (define-key smartparens-mode-map (kbd "C-M-n") 'sp-forward-hybrid-sexp)
    (define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-hybrid-sexp)

    (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
    (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

    (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

    (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
    (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

    (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
    (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
    (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

    (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
    (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

    (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
    (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

    (define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner)
    (define-key smartparens-mode-map (kbd "M-i") 'sp-change-enclosing)

    (setq sp-base-key-bindings 'sp)
    (setq sp-highlight-pair-overlay nil)

    (sp-with-modes `(c-mode c++-mode emacs-lisp-mode js-mode java-mode clojure-mode clojurescript-mode))

    (sp-pair "(" ")" :wrap "M-(")
    (sp-pair "{" "}" :wrap "M-{")
    (sp-pair "[" "]" :wrap "M-[")

    ;; Use it everywhere
    (smartparens-global-strict-mode t)
    (show-smartparens-global-mode t)))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode +1))

(require 'server)

(defadvice server-create-window-system-frame
    (after set-window-system-frame-colours ())
  "Set custom frame colours when create the first frame on a display"
  (message "Running after frame-initialize")
  (setup-window-system))

(ad-activate 'server-create-window-system-frame)
(add-hook 'after-make-frame-functions 'setup-window-system)

;; (use-package spaceline
;;   :init
;;   (setq powerline-default-separator 'wave)
;;   (require 'spaceline-config)
;;   :config
;;   (spaceline-emacs-theme)
;;   (spaceline-helm-mode)
;;   (setup-window-system))

;; Start Org Mode
(require 'org-mode-basic)
(require 'org-mode-organisation)
(require 'org-mode-templates)
(require 'org-mode-download)

;; Programming config
(require 'programming-general)
(require 'programming-git)
(require 'programming-project)
(require 'programming-clojure)
(require 'programming-flycheck)
(require 'programming-ocaml)
(require 'programming-commonlisp)
(require 'programming-yasnippet)
(require 'programming-6502)
(require 'programming-html)

;; Spelling config
(require 'core-spelling)

;; Autocomplete
(require 'core-completion)

;; Writing config
(require 'core-fountain)

;; Email
;; (require 'gnus-load)

;; Social
(require 'social-twitter)
(require 'social-blog)

;; Use Windows browser when under WSL.
(setq-default sysTypeSpecific  system-type)

(cond
 ;; If type is "gnu/linux", override to "wsl/linux" if it's WSL.
 ((eq sysTypeSpecific 'gnu/linux)
  (when (string-match "Linux.*Microsoft.*Linux"
                      (shell-command-to-string "uname -a"))

    (setq-default sysTypeSpecific "wsl/linux") ;; for later use.
    (setq
     cmdExeBin"/mnt/c/Windows/System32/cmd.exe"
     cmdExeArgs '("/c" "start" "") )
    (setq
     browse-url-generic-program cmdExeBin
     browse-url-generic-args cmdExeArgs
     browse-url-browser-function 'browse-url-generic))))

;; Pretty Code Snippets for sharing.
(use-package carbon-now-sh)

(use-package ess
  :init (require 'ess-site))
