;;;; Package --- Emacs initialisation of LuminousMonkey
;;; Commentary:
;; Emacs initialisation starting point.

;; Note: Emacs can be slow to start unless the host has a FQDN.

;; I've copied bits of this config from:
;; https://github.com/mrvdb/emacs-config/

;;; Code:

;; (package-initialize)

(setq require-final-newline nil)

(load-file (concat (file-name-directory load-file-name)
		   "core/core-load-paths.el"))

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

(straight-use-package 'use-package)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)

;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.
(use-package no-littering
  :demand t)

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

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
(straight-use-package 'org)

(use-package el-patch
	     :straight t)

(require 'core-funcs)

;; Shamelessly ripped from Spacemacs.
(defun luminousmonkey/set-default-font (plists)
  "Set the font given the passed PLISTS.

PLISTS has either the form (\"fontname\" :prop1 val1 :prop2 val2 ...)
or is a list of such. The first font that can be found will be used.

The return value is nil if no font was found, truthy otherwise."
  (unless (listp (car plists))
    (setq plists (list plists)))
  (catch 'break
    (dolist (plist plists)
      (when (find-font (font-spec :name (car plist)))
	(let* ((font (car plist))
	       (props (cdr plist))
	       (scale (plist-get props :powerline-scale))
	       (font-props (luminousmonkey/mplist-remove
			    (luminousmonkey/mplist-remove props :powerline-scale)
			    :powerline-offset))
	       (fontspec (apply 'font-spec :name font font-props)))
	  (set-frame-font fontspec nil t)
	  (push `(font . ,(frame-parameter nil 'font)) default-frame-alist)
	  (setq-default powerline-scale scale)
	  (setq-default powerline-height (luminousmonkey/compute-powerline-height))
	  ;; Fallback font for Unicode characters used.
	  (pcase system-type
	    (`gnu/linux
	     (setq fallback-font-name "NanumGothic")
	     (setq fallback-font-name2 "NanumGothic"))
	    (`darwin
	     (setq fallback-font-name "Arial Unicode MS")
	     (setq fallback-font-name2 "Arial Unicode MS"))
	    (`windows-nt
	     (setq fallback-font-name "MS Gothic")
	     (setq fallback-font-name2 "Lucida Sans Unicode"))
	    (`cygwin
	     (setq fallback-font-name "MS Gothic")
	     (setq fallback-font-name2 "Lucida Sans Unicode"))
	    (other
	     (setq fallback-font-name nil)
	     (setq fallback-font-name2 nil)))
	  (when (and fallback-font-name fallback-font-name2)
	    (let* ((fallback-props (luminousmonkey/mplist-remove
				    (luminousmonkey/mplist-remove font-props :size)
				    :height))
		   (fallback-spec (apply 'font-spec
					 :name fallback-font-name
					 fallback-props))
		   (fallback-spec2 (apply 'font-spec
					  :name fallback-font-name2
					  fallback-props)))
	      ;; Window numbers.
	      (set-fontset-font "fontset-default"
				'(#x2776 . #x24fe) fallback-spec nil 'prepend)
	      ;; Mode-line circled letters.
	      (set-fontset-font "fontset-default"
				'(#x24b6 . #x24fe) fallback-spec nil 'prepend)
              ;; Mode-line additional characters
              (set-fontset-font "fontset-default"
                                '(#x2295 . #x22a1) fallback-spec nil 'prepend)
	      ;; New version lighter
	      (set-fontset-font "fontset-default"
				'(#x2190 . #x2200) fallback-spec2 nil 'prepend))))
	(throw 'break t)))
    nil))

(defun luminousmonkey/compute-powerline-height ()
  "Return an adjusted powerline height."
  (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
		   powerline-scale 1)))
    (truncate (* scale (frame-char-height)))))

(use-package monokai-theme
  :demand t)

;; Visual Bell only
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; Packages
(use-package diminish)

(require 'core-navigation)

(use-package ethan-wspace
  :diminish " ☐"
  :commands global-ethan-wspace-mode
  :init
  (progn
    (global-ethan-wspace-mode)
    (setq mode-require-final-newline nil)))

(use-package smartparens
  :diminish " ⚖"
  :init
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1))
  :config
  (progn
    (setq sp-highlight-pair-overlay nil)

    (define-key sp-keymap (kbd "M-k") #'sp-raise-sexp)

    (define-key sp-keymap (kbd "C-(") #'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-{") #'sp-forward-barf-sexp)

    (sp-with-modes `(c-mode c++-mode emacs-lisp-mode js-mode java-mode clojure-mode clojurescript-mode))

    (sp-pair "(" ")" :wrap "M-(")
    (sp-pair "{" "}" :wrap "M-{")
    (sp-pair "[" "]" :wrap "M-[")

    ;; Use it everywhere
    (smartparens-global-strict-mode t)
    (show-smartparens-global-mode t)))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(load-theme 'monokai 'no-confirm)

;; Be sure we have a fallback font for symbols. Also, could be running
;; emacs client, which doesn't appear to set the 'window-system. So,
;; attach to the hooks.

;; https://www.emacswiki.org/emacs/SettingFrameColorsForEmacsClient
(defun setup-window-system (&rest frame)
  (if (window-system)
      (progn
	(set-frame-size (selected-frame) 120 60)
        (luminousmonkey/set-default-font '("Fira Code"
				  :size 18
				  :weight normal
				  :width normal
				  :powerline-scale 1.4))
	;; Also fix powerline
	(powerline-reset))))

(require 'server)

(defadvice server-create-window-system-frame
    (after set-window-system-frame-colours ())
  "Set custom frame colours when create the first frame on a display"
  (message "Running after frame-initialize")
  (setup-window-system))

(ad-activate 'server-create-window-system-frame)
(add-hook 'after-make-frame-functions 'setup-window-system)

(use-package spaceline
  :init
  (setq powerline-default-separator 'wave)
  (require 'spaceline-config)
  :config
  (spaceline-emacs-theme)
  (spaceline-helm-mode)
  (setup-window-system))

;; Start Org Mode
(require 'org-mode-basic)
(require 'org-mode-organisation)
(require 'org-mode-templates)

;; Programming config
(require 'programming-clojure)
(require 'programming-flycheck)
(require 'programming-ocaml)
(require 'programming-commonlisp)

;; Spelling config
(require 'core-spelling)

;; Writing config
(require 'core-fountain)

;; Email
;; (require 'gnus-load)

;; Social
(require 'social-twitter)
