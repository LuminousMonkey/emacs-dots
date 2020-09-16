;; LuminousMonkey Emacs config
;; -*- coding: utf-8; lexical-binding: t; -*-
;;
;; This is my Emacs config, there are many like it, but this one is
;; mine.
;;
;; Ideas taken from many sources, as are most Emacs configs I
;; think. I'm going back to trying an org-mode based config again, so
;; this init file will just do all the early setup to get that
;; underway.
;;
;; Note: Emacs can be slow to start unless the host has a FQDN.

;; Start up speed improvement.  Set the GC threshold large, then reset
;; it back to normal once we're done.
(let* ((normal-gc-cons-threshold (* 20 1024 1024))
       (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold (* 29 1024 1024)))))

;; I keep my configs using dotdotdot, and I use hardlinks.
;; Make sure Emacs perserves hardlinks.
(setq backup-by-copying-when-linked t)

;; First thing is first, we need to get the package management, etc,
;; all sorted, because I want to make sure I use recent versions of
;; org-mode.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
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

(use-package dynamic-fonts
  :commands (dynamic-fonts-setup)
  :init
  (progn
    (setq
     dynamic-fonts-preferred-monospace-fonts
     '("Triplicate T4c" "Fira Code Medium" "PragmataPro" "Consolas" "Monaco" "Menlo"
       "DejaVu Sans Mono"
       "Droid Sans Mono Pro" "Droid Sans Mono" "Inconsolata" "Source Code Pro"
       "Lucida Console" "Envy Code R" "Andale Mono" "Lucida Sans Typewriter"
       "Lucida Typewriter" "Panic Sans" "Bitstream Vera Sans Mono"
       "Excalibur Monospace" "Courier New" "Courier" "Cousine" "Lekton"
       "Ubuntu Mono" "Liberation Mono" "BPmono" "Anonymous Pro"
       "ProFontWindows")
     dynamic-fonts-preferred-monospace-point-size 18
     dynamic-fonts-preferred-proportional-fonts
     '("PT Sans" "Lucida Grande" "Segoe UI" "DejaVu Sans" "Bitstream Vera"
       "Tahoma" "Verdana" "Helvetica" "Arial Unicode MS" "Arial")
     dynamic-fonts-preferred-proportional-point-size 11)

    (defvar my-monospaced-font "PragmataPro-11.8")
    (defvar my-variable-pitch-font "Pt Sans-13")
    ;; (defvar my-variable-pitch-font "Input Sans Compressed-11.8")
    ;; (defvar my-monospaced-font "Input Mono Compressed-11.8")

    ;; Attach to the 'focus-in-hook, it will remove itself first time it is run,
    ;; since it's only needed once.
    (defun my-set-fonts  ()
      (interactive)
      (when window-system
        (condition-case nil
            (progn
              (set-face-attribute 'default nil :font my-monospaced-font)
              ;; (set-face-attribute 'default nil :font my-monospaced-font :width 'ultra-condensed :weight 'normal )
              (set-face-attribute 'fixed-pitch nil :font my-monospaced-font)
              (set-face-attribute 'variable-pitch nil :font my-variable-pitch-font))
          (error
           (progn
             (message
              "Setting default fonts failed, running dynamic-fonts-setup...")
             (dynamic-fonts-setup)))))
      (remove-hook 'focus-in-hook #'my-set-fonts))
    (add-hook 'focus-in-hook #'my-set-fonts t)))

(use-package blackout
  :straight (blackout :host github :repo "raxod502/blackout")
  :demand t)

(use-package el-patch)

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
(straight-use-package 'org-plus-contrib)

;; Always follow symbolic links to source controlled files.
;; I used VC with my dotfiles, and the prompt (since I now use org-babel) is
;; annoying.
(setq vc-follow-symlinks t)

;; Personal Information
(setq user-full-name "Mike Aldred"
      user-mail-address "mike.aldred@luminousmonkey.org")

;; Disable certain byte compiler warnings to cut down on the noise. This is a
;; personal choice and can be removed if you would like to see any and all byte
;; compiler warnings.
(setq byte-compile-warnings '(not free-vars unresolved noruntime
                                  lexical make-local))

;; Defaults I consider to be sane for basic operation.
;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; Make the selection work like most people expect.
(delete-selection-mode t)
(transient-mark-mode t)

(setq create-lockfiles nil)

                                        ; Make sure we always use UTF-8.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)

;; Make compilation buffers scroll to follow the output, but stop scrolling
;; at the first error.
(setq compilation-scroll-output 'first-error)

(setq-default
 mac-option-modifier 'meta		    ; Fix Mac Alt handling
 auto-window-vscroll nil		    ; Lighten vertical scroll
 cursor-in-non-selected-windows nil         ; Hide the cursor in inactive windows
 display-time-default-load-average nil	    ; Don't display load average
 display-time-format "%H:%M"		    ; Format the time string
 fill-column 80			            ; Set width for automatic line breaks
 help-window-select t		            ; Focus new help windows when opened
 indent-tabs-mode nil		            ; Stop using tabs to indent
 mouse-yank-at-point t		            ; Yank at point rather than pointer
 ns-use-srgb-colorspace nil		    ; Don't use sRGB colors
 ;;   recenter-positions '(5 top bottom)	    ; Set re-centering positions
 scroll-conservatively most-positive-fixnum ; Always scroll by one line
 scroll-margin 10                           ; Add a margin when scrolling vertically
 select-enable-clipboard t                  ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil              ; End a sentence after a dot and a space
 show-trailing-whitespace nil	            ; Display trailing whitespaces
 split-height-threshold nil                 ; Disable vertical window splitting
 split-width-threshold nil                  ; Disable horizontal window splitting
 tab-width 4                                ; Set width for tabs
 uniquify-buffer-name-style 'forward        ; Uniquify buffer names
 ;;   window-combination-resize t                ; Resize windows proportionally
 x-stretch-cursor t                         ; Stretch cursor to the glyph width
 )

(display-time-mode 1)

                                        ; Home directory always
(cd "~/")

                                        ; Iterate through CamelCase words
(global-subword-mode 1)

(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region

;; Garbage collect on focus-out, Emacs /should/ feel snappier.
(add-hook 'focus-out-hook #'garbage-collect)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Initial Keybindings
;;Initial keybindings that I want fairly early in the config, because I'm used
;;to them.
(define-key global-map (kbd "C-x C-m") 'execute-extended-command)
(define-key global-map (kbd "C-w ") 'backward-kill-word)
(define-key global-map (kbd "C-x C-k") 'kill-region)
(define-key global-map (kbd "C-c C-k") 'kill-region)

;; Learning Emacs
;; Discovery, so I can find out little modes and things that I don't know or
;; practice.
(use-package discover
  :config (global-discover-mode 1))

;; Basic Appearance
;; Remove the toolbar and the scrollbar, keep the menu though, I might discover
;; new features I didn't know.
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq-default
 inhibit-startup-screen t
 initial-scratch-message "")

;; Theme
;; I like to get into the theme as quickly as possible, if something goes wrong,
;; I want my font, and my colours.
(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (with-eval-after-load 'flycheck
    (set-face-attribute 'flycheck-error nil
                        :underline `(:color ,(doom-color 'error)
                                            :style line))
    (set-face-attribute 'flycheck-info nil
                        :underline `(:color ,(doom-color
                                              'highlight) :style line))
    (set-face-attribute 'flycheck-info nil
                        :underline `(:color ,(doom-color
                                              'highlight) :style line))
    (set-face-attribute 'flycheck-warning nil
                        :underline `(:color ,(doom-color
                                              'warning) :style line))))

;; Modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  )

;; Highlights
(global-hl-line-mode +1)

(use-package volatile-highlights
  :blackout volatile-highlights-mode
  :config
  (volatile-highlights-mode +1))

;; Client Settings

;; I use an Emacs daemon, so some settings don't get set on the client from the
;; Emacs server startup. So we define a function here that will run when a
;; client starts to get it setup.
(defun luminousmonkey/run-client-settings (&rest frame)
  (if (window-system)
      (progn
        (setq doom-modeline-icon (display-graphic-p)))))

(require 'server)

(defadvice server-create-window-system-frame
    (after set-window-system-frame-colours ())
  "Set custom font, etc, when we create the first frame on
display"
  (message "Running after frame-initialize")
  (luminousmonkey/run-client-settings))

(ad-activate 'server-create-window-system-frame)
(add-hook 'after-make-frame-functions 'luminousmonkey/run-client-settings)

;; Navigation / Inline
;; Smarter C-a
(defun monkey/beginning-of-line-dwim ()
  "Move point to first non-whitespace character, or beginning of
line."
  (interactive "^")
  (let ((origin (point)))
    (beginning-of-line)
    (and (= origin (point))
         (back-to-indentation))))

(global-set-key [remap move-beginning-of-line] #'monkey/beginning-of-line-dwim)

(org-babel-load-file (expand-file-name "monkey.org" user-emacs-directory))
