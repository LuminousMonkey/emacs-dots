;; -*- coding: utf-8; lexical-binding: t; -*-
;; LuminousMonkey Emacs config
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

;; Load built-in utility libraries
(require 'cl-lib)
(require 'map)
(require 'subr-x)

(defmacro radian-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. PLACE should be sharp-quoted.
DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "Radian: advice `%S' not documented'" name))
  (unless (and (listp place)
               (= 2 (length place))
               (eq (nth 0 place) 'function)
               (symbolp (nth 1 place)))
    (error "Radian: advice `%S' does not sharp-quote place `%S'" name place))
  `(progn
     ;; You'd think I would put an `eval-and-compile' around this. It
     ;; turns out that doing so breaks the ability of
     ;; `elisp-completion-at-point' to complete on function arguments
     ;; to the advice. I know, right? Apparently this is because the
     ;; code that gets the list of lexically bound symbols at point
     ;; tries to `macroexpand-all', and apparently macroexpanding
     ;; `eval-and-compile' goes ahead and evals the thing and returns
     ;; only the function symbol. No good. But the compiler does still
     ;; want to know the function is defined (this is a Gilardi
     ;; scenario), so we pacify it by `eval-when-compile'ing something
     ;; similar (see below).
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for `%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (eval-when-compile
       (declare-function ,name nil))
     (advice-add ,place ',where #',name)
     ',name))

(defmacro radian-defhook (name arglist hooks docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOKS is a list of hooks to which to
add the function, or just a single hook. DOCSTRING and BODY are
as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (error "Symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (error "Radian: no docstring provided for `radian-defhook'"))
  (let ((hooks-str (format "`%S'" (car hooks))))
    (dolist (hook (cdr hooks))
      (setq hooks-str (format "%s\nand `%S'" hooks-str hook)))
    `(progn
       (defun ,name ,arglist
         ,(format "%s\n\nThis function is for use in %s."
                  docstring hooks-str)
         ,@body)
       (dolist (hook ',hooks)
         (add-hook hook ',name)))))

(defmacro radian-operating-system-p (os)
  "Return non-nil if OS corresponds to the current operating system.
Allowable values for OS (not quoted) are `macOS', `osx',
`windows', `linux', `unix'."
  (pcase os
    (`unix `(not (memq system-type '(ms-dos windows-nt cygwin))))
    ((or `macOS `osx) `(eq system-type 'darwin))
    (`linux `(not (memq system-type
                        '(darwin ms-dos windows-nt cygwin))))
    (`windows `(memq system-type '(ms-dos windows-nt cygwin)))))

(defmacro radian-with-operating-system (os &rest body)
  "If OS corresponds to the current operating system, eval and return BODY.
If not, return nil.
Allowable values for OS (not quoted) are `macOS', `osx',
`windows', `linux', `unix'."
  (declare (indent 1))
  `(when (radian-operating-system-p ,os)
     ,@body))

(defmacro radian-flet (bindings &rest body)
  "Temporarily override function definitions using `cl-letf*'.
BINDINGS are composed of `defun'-ish forms. NAME is the function
to override. It has access to the original function as a
lexically bound variable by the same name, for use with
`funcall'. ARGLIST and BODY are as in `defun'.
\(fn ((defun NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  `(cl-letf* (,@(cl-mapcan
                 (lambda (binding)
                   (when (memq (car binding) '(defun lambda))
                     (setq binding (cdr binding)))
                   (cl-destructuring-bind (name arglist &rest body) binding
                     (list
                      `(,name (symbol-function #',name))
                      `((symbol-function #',name)
                        (lambda ,arglist
                          ,@body)))))
                 bindings))
     ,@body))

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

; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
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

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;; This package lets us customise the display of major and minor modes
;; on the mode line.
(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)

;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.
(use-package no-littering
  :demand t)

;;; Prevent Emacs-provided Org from being loaded

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
;;
;; Use my mirror of Org because the upstream has *shockingly*
;; atrocious uptime (namely, the entire service will just go down for
;; more than a day at a time on a regular basis). Unacceptable because
;; it keeps breaking Radian CI.
(straight-use-package
 '(org :host github :repo "emacs-straight/org-mode" :local-repo "org"))

;;; el-patch

;; Package `el-patch' provides a way to override the definition of an
;; internal function from another package by providing an s-expression
;; based diff which can later be validated to ensure that the upstream
;; definition has not changed.
(use-package el-patch)

;; Only needed at compile time, thanks to Jon
;; <https://github.com/raxod502/el-patch/pull/11>.
(eval-when-compile
  (require 'el-patch))

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

;; Make sure we always use UTF-8.
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Automatically save buffers before launching M-x compile and friends,
;; instead of asking you if you want to save.
(setq compilation-ask-about-save nil)

;; Make compilation buffers scroll to follow the output, but stop scrolling
;; at the first error.
(setq compilation-scroll-output 'first-error)

;; Iterate through CamelCase words
(global-subword-mode 1)

(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region

;; Garbage collect on focus-out, Emacs /should/ feel snappier.
(add-hook 'focus-out-hook #'garbage-collect)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Keybindings

;; Package `bind-key' provides a macro by the same name (along with
;; `bind-key*' and `unbind-key') which provides a much prettier API
;; for manipulating keymaps than `define-key' and `global-set-key' do.
;; It's also the same API that `:bind' and similar keywords in
;; `use-package' use.
(use-package bind-key
  :demand t)

(bind-key* "C-x C-m" #'execute-extended-command)
(bind-key* "C-w" #'backward-kill-word)
(bind-key* "C-x C-k" #'kill-region)
(bind-key* "C-c C-k" #'kill-region)

(defvar radian-keymap (make-sparse-keymap)
  "Keymap for Radian commands that should be put under a prefix.
This keymap is bound under \\[radian-keymap].")

(bind-key* "M-P" radian-keymap)

(defmacro radian-bind-key (key-name command &optional predicate)
  "Bind a key in `radian-keymap'.
KEY-NAME, COMMAND, and PREDICATE are as in `bind-key'."
  `(bind-key ,key-name ,command radian-keymap ,predicate))

;; Package `which-key' displays the key bindings and associated
;; commands following the currently-entered key prefix in a popup.
(use-package which-key
  :demand t
  :config

  ;; We configure it so that `which-key' is triggered by typing C-h
  ;; during a key sequence (the usual way to show bindings). See
  ;; <https://github.com/justbur/emacs-which-key#manual-activation>.
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay most-positive-fixnum)
  (setq which-key-idle-secondary-delay 1e-100)

  (which-key-mode +1)

  :blackout t)

;; Basic Appearance
;; Remove the toolbar and the scrollbar, keep the menu though, I might discover
;; new features I didn't know.
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq-default
 inhibit-startup-screen t
 initial-scratch-message "")

;; Package `selectrum' is an incremental completion and narrowing
;; framework. Like Ivy and Helm, which it improves on, Selectrum
;; provides a user interface for choosing from a list of options by
;; typing a query to narrow the list, and then selecting one of the
;; remaining candidates. This offers a significant improvement over
;; the default Emacs interface for candidate selection.
(use-package selectrum
  :straight (:host github :repo "raxod502/selectrum")
  :defer t
  :init

  ;; This doesn't actually load Selectrum.
  (selectrum-mode +1))

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.
(use-package prescient
  :config

  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1)

  ;; The default settings seem a little forgetful to me. Let's try
  ;; this out.
  (setq prescient-history-length 1000))

;; Package `selectrum-prescient' provides intelligent sorting and
;; filtering for candidates in Selectrum menus.
(use-package selectrum-prescient
  :straight (:host github :repo "raxod502/prescient.el"
                   :files ("selectrum-prescient.el"))
  :demand t
  :after selectrum
  :config

  (selectrum-prescient-mode +1))

;; Package `transpose-frame' provides simple commands to mirror,
;; rotate, and transpose Emacs windows: `flip-frame', `flop-frame',
;; `transpose-frame', `rotate-frame-clockwise',
;; `rotate-frame-anticlockwise', `rotate-frame'.
(use-package transpose-frame
  :bind* (("s-t" . #'transpose-frame)))

;; Feature `ibuffer' provides a more modern replacement for the
;; `list-buffers' command.
(use-feature ibuffer
  :bind (([remap list-buffers] . #'ibuffer)))

;; Theme
(add-to-list 'default-frame-alist '(font . "Fira Code Medium-16"))

;; I like to get into the theme as quickly as possible, if something goes wrong,
;; I want my font, and my colours.
(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-molokai t)
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
  :hook (after-init . doom-modeline-mode))

;; Package `projectile' keeps track of a "project" list, which is
;; automatically added to as you visit Git repositories, Node.js
;; projects, etc. It then provides commands for quickly navigating
;; between and within these projects.
(use-package projectile
  :defer 1
  :bind-keymap* (("C-c p" . projectile-command-map))
  :config

  ;; Use Selectrum (via `completing-read') for Projectile instead of
  ;; IDO.
  (setq projectile-completion-system 'default)

  ;; When switching projects, give the option to choose what to do.
  ;; This is a way better interface than having to remember ahead of
  ;; time to use a prefix argument on `projectile-switch-project'
  ;; (because, and please be honest here, when was the last time you
  ;; actually remembered to do that?).
  (setq projectile-switch-project-action 'projectile-commander)

  (def-projectile-commander-method ?\C-m
    "Find file in project."
    (call-interactively #'find-file))

  ;; Enable the mode again now that we have all the supporting hooks
  ;; and stuff defined.
  (projectile-mode +1)

  (defun radian--projectile-indexing-method-p (method)
    "Non-nil if METHOD is a safe value for `projectile-indexing-method'."
    (memq method '(native alien)))

  (put 'projectile-indexing-method 'safe-local-variable
       #'radian--projectile-indexing-method-p)

  ;; Can't bind M-r because some genius bound ESC. *Never* bind ESC!
  (dolist (key '("C-r" "R"))
    (bind-key key #'projectile-replace-regexp projectile-command-map))

  :blackout t)

;;; Saving files

;; Don't make backup files.
(setq make-backup-files nil)

;; Don't make autosave files.
(setq auto-save-default nil)

;; Don't make lockfiles.
(setq create-lockfiles nil)

(setq-default
 mac-option-modifier 'meta              ; Fix Mac Alt handling
 auto-window-vscroll nil                ; Lighten vertical scroll
 cursor-in-non-selected-windows nil     ; Hide the cursor in inactive windows
 display-time-default-load-average nil  ; Don't display load average
 display-time-format "%H:%M"		    ; Format the time string
 fill-column 80                         ; Set width for automatic line breaks
 help-window-select t		            ; Focus new help windows when opened
 indent-tabs-mode nil		            ; Stop using tabs to indent
 mouse-yank-at-point t		            ; Yank at point rather than pointer
 ns-use-srgb-colorspace nil             ; Don't use sRGB colors
 ;;   recenter-positions '(5 top bottom)	    ; Set re-centering positions
 scroll-conservatively most-positive-fixnum ; Always scroll by one line
 scroll-margin 10                       ; Add a margin when scrolling vertically
 select-enable-clipboard t              ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil          ; End a sentence after a dot and a space
 show-trailing-whitespace nil           ; Display trailing whitespaces
 split-height-threshold nil             ; Disable vertical window splitting
 split-width-threshold nil              ; Disable horizontal window splitting
 tab-width 4                            ; Set width for tabs
 uniquify-buffer-name-style 'forward    ; Uniquify buffer names
 ;;   window-combination-resize t                ; Resize windows proportionally
 x-stretch-cursor t                     ; Stretch cursor to the glyph width
 )

;; Feature `newcomment' provides commands for commenting and
;; uncommenting code, and editing comments.
(use-feature newcomment
  :bind (([remap default-indent-new-line] . #'radian-continue-comment))
  :config

  (defun radian-continue-comment ()
    "Continue current comment, preserving trailing whitespace.
This differs from `default-indent-new-line' in the following way:
If you have a comment like \";; Some text\" with point at the end
of the line, then running `default-indent-new-line' will get you
a new line with \";; \", but running it again will get you a line
with only \";;\" (no trailing whitespace). This is annoying for
inserting a new paragraph in a comment. With this command, the
two inserted lines are the same."
    (interactive)
    ;; `default-indent-new-line' uses `delete-horizontal-space'
    ;; because in auto-filling we want to avoid the space character at
    ;; the end of the line from being put at the beginning of the next
    ;; line. But when continuing a comment it's not desired.
    (cl-letf (((symbol-function #'delete-horizontal-space) #'ignore))
      (default-indent-new-line))))

;; Feature `outline' provides major and minor modes for collapsing
;; sections of a buffer into an outline-like format.
(use-feature outline
  :demand t
  :config

  (define-globalized-minor-mode global-outline-minor-mode
    outline-minor-mode outline-minor-mode)

  (global-outline-minor-mode +1)

  :blackout outline-minor-mode)

;; Package `undo-tree' replaces the default Emacs undo system, which
;; is poorly designed and hard to use, with a much more powerful
;; tree-based system. In basic usage, you don't even have to think
;; about the tree, because it acts like a conventional undo/redo
;; system. Bindings are C-/, M-/, and C-x u.
(use-package undo-tree
  :demand t
  :bind (;; By default, `undo' (and by extension `undo-tree-undo') is
         ;; bound to C-_ and C-/, and `undo-tree-redo' is bound to
         ;; M-_. It's logical to also bind M-/ to `undo-tree-redo'.
         ;; This overrides the default binding of M-/, which is to
         ;; `dabbrev-expand'.
         :map undo-tree-map
         ("M-/" . #'undo-tree-redo))
  :config

  (global-undo-tree-mode +1)

  ;; Disable undo-in-region. It sounds like a cool feature, but
  ;; unfortunately the implementation is very buggy and usually causes
  ;; you to lose your undo history if you use it by accident.
  (setq undo-tree-enable-undo-in-region nil)

  :blackout t)

;;; Navigation

;; Feature `subword' provides a minor mode which causes the
;; `forward-word' and `backward-word' commands to stop at
;; capitalization changes within a word, so that you can step through
;; the components of CamelCase symbols one at a time.
(use-feature subword
  :demand t
  :config

  (global-subword-mode +1)

  :blackout t)

;; Package `ctrlf' provides a replacement for `isearch' that is more
;; similar to the tried-and-true text search interfaces in web
;; browsers and other programs (think of what happens when you type
;; ctrl+F).
(use-package ctrlf
  :straight (:host github :repo "raxod502/ctrlf")
  :init

  (ctrlf-mode +1))

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(use-package visual-regexp
  :bind (([remap query-replace] . #'vr/query-replace)))

;; Key chords
(use-package use-package-chords
  :config (key-chord-mode 1))

(require 'use-package-chords)

;; Line and word jumping
(use-package avy
  :demand t
  :chords (("jj" . avy-goto-word-1)
           ("jl" . avy-goto-line))
  :bind (("C-:" . avy-goto-char))
  :config
  (setq avy-keys
        (nconc (number-sequence ?a ?z)
               (number-sequence ?A ?Z)
               (number-sequence ?1 ?9)
               '(?0)))
  (setq avy-all-windows 'all-frames)
  (with-eval-after-load "isearch"
    (define-key isearch-mode-map (kbd "C-;") 'avy-isearch)))

;;;; Automatic delimiter pairing

;; Package `smartparens' provides an API for manipulating paired
;; delimiters of many different types, as well as interactive commands
;; and keybindings for operating on paired delimiters at the
;; s-expression level. It provides a Paredit compatibility layer.
(use-package smartparens
  :demand t
  :config

  ;; Load the default pair definitions for Smartparens.
  (require 'smartparens-config)

  ;; Enable Smartparens functionality in all buffers.
  (smartparens-global-mode +1)

  ;; When in Paredit emulation mode, Smartparens binds M-( to wrap the
  ;; following s-expression in round parentheses. By analogy, we
  ;; should bind M-[ to wrap the following s-expression in square
  ;; brackets. However, this breaks escape sequences in the terminal,
  ;; so it may be controversial upstream. We only enable the
  ;; keybinding in windowed mode.
  (when (display-graphic-p)
    (setf (map-elt sp-paredit-bindings "M-[") #'sp-wrap-square))

  ;; Set up keybindings for s-expression navigation and manipulation
  ;; in the style of Paredit.
  (sp-use-paredit-bindings)

  ;; Highlight matching delimiters.
  (show-smartparens-global-mode +1)

  ;; Prevent all transient highlighting of inserted pairs.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (setq sp-cancel-autoskip-on-backward-movement nil)

  ;; Disable Smartparens in Org-related modes, since the keybindings
  ;; conflict.

  (use-feature org
    :config

    (add-to-list 'sp-ignore-modes-list #'org-mode))

  (use-feature org-agenda
    :config

    (add-to-list 'sp-ignore-modes-list #'org-agenda-mode))

  ;; Make C-k kill the sexp following point in Lisp modes, instead of
  ;; just the current line.
  (bind-key [remap kill-line] #'sp-kill-hybrid-sexp smartparens-mode-map
            (apply #'derived-mode-p sp-lisp-modes))

  (defun radian--smartparens-indent-new-pair (&rest _)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  ;; The following is a really absurdly stupid hack that I can barely
  ;; stand to look at. It needs to be fixed.
  ;;
  ;; Nevertheless, I can't live without the feature it provides (which
  ;; should really come out of the box IMO): when pressing RET after
  ;; inserting a pair, add an extra newline and indent. See
  ;; <https://github.com/Fuco1/smartparens/issues/80#issuecomment-18910312>.

  (defun radian--smartparens-pair-setup (mode delim)
    "In major mode MODE, set up DELIM with newline-and-indent."
    (sp-local-pair mode delim nil :post-handlers
                   '((radian--smartparens-indent-new-pair "RET")
                     (radian--smartparens-indent-new-pair "<return>"))))

  (radian--smartparens-pair-setup #'prog-mode "(")
  (radian--smartparens-pair-setup #'prog-mode "[")
  (radian--smartparens-pair-setup #'prog-mode "{")
  (radian--smartparens-pair-setup #'python-mode "\"\"\"")
  (radian--smartparens-pair-setup #'latex-mode "\\[")
  (radian--smartparens-pair-setup #'markdown-mode "```")

  ;; It's unclear to me why any of this is needed.
  (radian--smartparens-pair-setup #'json-mode "[")
  (radian--smartparens-pair-setup #'json-mode "{")
  (radian--smartparens-pair-setup #'tex-mode "{")

  ;; Deal with `protobuf-mode' not using `define-minor-mode'.
  (radian--smartparens-pair-setup #'protobuf-mode "{")

  ;; Work around https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  ;; Quiet some silly messages.
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (cdr (assq key sp-message-alist)) nil))

  :blackout t)

;;;; Code reformatting

;; Package `apheleia' implements a sophisticated algorithm for
;; applying code formatters asynchronously on save without moving
;; point or modifying the scroll position.
(use-package apheleia
  :straight (:host github :repo "raxod502/apheleia")
  :init

  (apheleia-global-mode +1)

  ;; We need to do this both before and after Apheleia is loaded
  ;; because the autoloading is set up such that the minor mode
  ;; definition is evaluated twice.
  (blackout 'apheleia-mode)

  :blackout t)

;; Package `lsp-mode' is an Emacs client for the Language Server
;; Protocol <https://langserver.org/>. It is where we get all of our
;; information for completions, definition location, documentation,
;; and so on.
(use-package lsp-mode
  :init

  (defcustom radian-lsp-disable nil
    "If non-nil, then LSP is not allowed to be enabled.
For use in file-local variables."
    :type 'boolean
    :safe #'booleanp)

  (radian-defhook radian--lsp-enable ()
    after-change-major-mode-hook
    "Enable `lsp-mode' for most programming modes.
Do this on `after-change-major-mode-hook' instead of
`prog-mode-hook' and `text-mode-hook' because we want to make
sure regular mode hooks get a chance to run first, for example to
set LSP configuration (see `lsp-python-ms')."
    (when (derived-mode-p #'prog-mode #'text-mode)
      (unless (or radian-lsp-disable
                  (null buffer-file-name)
                  (derived-mode-p
                   ;; `lsp-mode' doesn't support Elisp, so let's avoid
                   ;; triggering the autoload just for checking that, yes,
                   ;; there's nothing to do for the *scratch* buffer.
                   #'emacs-lisp-mode
                   ;; Disable for modes that we currently use a specialized
                   ;; framework for, until they are phased out in favor of
                   ;; LSP.
                   #'clojure-mode
                   #'ruby-mode))
        (lsp))))

  :config

  ;; As per <https://github.com/emacs-lsp/lsp-mode#performance>.
  (setq read-process-output-max (* 1024 1024))

  (defun radian--advice-lsp-mode-silence (format &rest args)
    "Silence needless diagnostic messages from `lsp-mode'.
This is a `:before-until' advice for several `lsp-mode' logging
functions."
    (or
     ;; Messages we get when trying to start LSP (happens every time
     ;; we open a buffer).
     (member format `("No LSP server for %s(check *lsp-log*)."
                      "Connected to %s."
                      ,(concat
                        "Unable to calculate the languageId for current "
                        "buffer. Take a look at "
                        "lsp-language-id-configuration.")
                      ,(concat
                        "There are no language servers supporting current "
                        "mode %s registered with `lsp-mode'.")))
     ;; Errors we get from gopls for no good reason (I can't figure
     ;; out why). They don't impair functionality.
     (and (stringp (car args))
          (or (string-match-p "^no object for ident .+$" (car args))
              (string-match-p "^no identifier found$" (car args))))))

  (dolist (fun '(lsp-warn lsp--warn lsp--info lsp--error))
    (advice-add fun :before-until #'radian--advice-lsp-mode-silence))

  ;; If we don't disable this, we get a warning about YASnippet not
  ;; being available, even though it is. I don't use YASnippet anyway,
  ;; so don't bother with it.
  (setq lsp-enable-snippet nil)

  (radian-defhook radian--lsp-teardown ()
    kill-emacs-hook
    "Ignore the LSP server getting killed.
If we don't do this, then when killing Emacs we may be prompted
with whether we want to restart the LSP server that has just been
killed (which happens during Emacs shutdown)."
    (setq lsp-restart nil))

  ;; Looks like `lsp-mode' doesn't know about LaTeX yet.
  (add-to-list 'lsp-language-id-configuration '(latex-mode . "latex"))

  ;; Also, it has a bunch of regexps which are completely wrong.
  (setq lsp-language-id-configuration
        (mapcar
         (lambda (link)
           (if (and (stringp (car link))
                    (string-match "\\`\\.\\*\\.\\(.+\\)\\'" (car link)))
               (cons
                (format "\\.%s\\'" (match-string 1 (car link))) (cdr link))
             link))
         lsp-language-id-configuration))

  ;; Disable LSP reformatting your code as you type. We use Apheleia
  ;; for that instead.
  (setq lsp-enable-on-type-formatting nil)

  :blackout " LSP")

(use-package ccls
  :defer t
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :custom
  (ccls-executable (executable-find "ccls")) ; Add ccls to path if you haven't done so
  (ccls-sem-highlight-method 'font-lock)
  (ccls-enable-skipped-ranges nil)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection (cons ccls-executable ccls-args))
    :major-modes '(c-mode c++-mode cuda-mode objc-mode)
    :server-id 'ccls-remote
    :multi-root nil
    :remote? t
    :notification-handlers
    (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
            ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
    :initialization-options (lambda () ccls-initialization-options)
    :library-folders-fn nil)))

(use-package modern-cpp-font-lock
  :diminish t
  :init (modern-c++-font-lock-global-mode t))

;;;; Autocompletion

;; Package `company' provides an in-buffer autocompletion framework.
;; It allows for packages to define backends that supply completion
;; candidates, as well as optional documentation and source code. Then
;; Company allows for multiple frontends to display the candidates,
;; such as a tooltip menu. Company stands for "Complete Anything".
(use-package company
  :defer 0.5
  :init

  (defvar radian--company-backends-global
    '(company-capf
      company-files
      (company-dabbrev-code company-keywords)
      company-dabbrev)
    "Values for `company-backends' used everywhere.
If `company-backends' is overridden by Radian, then these
backends will still be included.")

  :bind (;; Remap the standard Emacs keybindings for invoking
         ;; completion to instead use Company. You might think this
         ;; could be put in the `:bind*' declaration below, but it
         ;; seems that `bind-key*' does not work with remappings.
         ([remap completion-at-point] . #'company-manual-begin)
         ([remap complete-symbol] . #'company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         :map company-active-map

         ;; Make TAB always complete the current selection, instead of
         ;; only completing a common prefix.
         ("<tab>" . #'company-complete-selection)
         ("TAB" . #'company-complete-selection)

         ;; When was the last time you used the C-s binding for
         ;; searching candidates? It conflicts with buffer search,
         ;; anyway. Same for the scroll commands.
         ("C-s" . nil)
         ([remap scroll-down-command] . nil)
         ([remap scroll-up-command] . nil)

         ;; The following are keybindings that only take effect if the
         ;; user has explicitly interacted with Company. Note that
         ;; `:map' from above is "sticky", and applies also below: see
         ;; https://github.com/jwiegley/use-package/issues/334#issuecomment-349473819.

         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company, instead of always
         ;; doing so.
         ("<return>" . #'company-complete-selection)
         ("RET" . #'company-complete-selection)

         ;; We then make <up> and <down> abort the completions menu
         ;; unless the user has interacted explicitly. Note that we
         ;; use `company-select-previous' instead of
         ;; `company-select-previous-or-abort'. I think the former
         ;; makes more sense since the general idea of this `company'
         ;; configuration is to decide whether or not to steal
         ;; keypresses based on whether the user has explicitly
         ;; interacted with `company', not based on the number of
         ;; candidates.
         ;;
         ;; Note that M-p and M-n work regardless of whether explicit
         ;; interaction has happened yet, and note also that M-TAB
         ;; when the completions menu is open counts as an
         ;; interaction.
         ("<up>" . #'company-select-previous)
         ("<down>" . #'company-select-next))

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. We
          ;; already remapped those bindings to `company-manual-begin'
          ;; above. Here we make sure that they definitely invoke
          ;; `company-manual-begin' even if a minor mode binds M-TAB
          ;; directly.
          ("M-TAB" . #'company-manual-begin))

  :config

  ;; Make completions display twice as soon.
  (setq company-idle-delay 0.15)

  ;; Make completions display when you have only typed one character,
  ;; instead of three.
  (setq company-minimum-prefix-length 1)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (setq company-tooltip-align-annotations t)

  (defvar-local radian--company-buffer-modified-counter nil
    "Last return value of `buffer-chars-modified-tick'.
Used to ensure that Company only initiates a completion when the
buffer is modified.")

  (radian-defadvice radian--advice-company-complete-on-change ()
    :override #'company--should-begin
    "Make Company trigger a completion when the buffer is modified.
This is in contrast to the default behavior, which is to trigger
a completion when one of a whitelisted set of commands is used.
One specific improvement this brings about is that you get
completions automatically when backspacing into a symbol."
    (let ((tick (buffer-chars-modified-tick)))
      (unless (equal tick radian--company-buffer-modified-counter)
        ;; Only trigger completion if previous counter value was
        ;; non-nil (i.e., don't trigger completion just as we're
        ;; jumping to a buffer for the first time).
        (prog1 (and radian--company-buffer-modified-counter
                    (not (and (symbolp this-command)
                              (string-match-p
                               "^\\(company-\\|undo-\\|undo$\\)"
                               (symbol-name this-command)))))
          (setq radian--company-buffer-modified-counter tick)))))

  (radian-defadvice radian--advice-company-update-buffer-modified-counter ()
    :after #'company--should-continue
    "Make sure `radian--company-buffer-modified-counter' is up to date.
If we don't do this on `company--should-continue' as well as
`company--should-begin', then we may end up in a situation where
autocomplete triggers when it shouldn't. Specifically suppose we
delete a char from a symbol, triggering autocompletion, then type
it back, but there is more than one candidate so the menu stays
onscreen. Without this advice, saving the buffer will cause the
menu to disappear and then come back after `company-idle-delay'."
    (setq radian--company-buffer-modified-counter
          (buffer-chars-modified-tick)))

  (global-company-mode +1)

  :blackout t)

;; Package `company-prescient' provides intelligent sorting and
;; filtering for candidates in Company completions.
(use-package company-prescient
  :demand t
  :after company
  :config

  ;; Use `prescient' for Company menus.
  (company-prescient-mode +1))

;; Package `company-lsp' provides a Company backend for `lsp-mode'.
;; It's configured automatically by `lsp-mode'.
(use-package company-lsp
  :init

  (use-feature lsp
    :config

    (radian-defadvice radian--company-lsp-setup (&rest _)
      :after #'lsp
      "Disable `company-prescient' sorting by length in some contexts.
Specifically, disable sorting by length if the LSP Company
backend returns fuzzy-matched candidates, which implies that the
backend has already sorted the candidates into a reasonable
order."
      (setq-local company-prescient-sort-length-enable
                  (cl-dolist (w lsp--buffer-workspaces)
                    (when (thread-first w
                            (lsp--workspace-client)
                            (lsp--client-server-id)
                            (memq '(jsts-ls mspyls bash-ls texlab ts-ls))
                            (not))
                      (cl-return t)))))))

;;;; Syntax checking and code linting

;; Package `flycheck' provides a framework for in-buffer error and
;; warning highlighting. We kind of don't use it because we use
;; `lsp-ui' instead, but internally `lsp-ui' actually hacks Flycheck
;; to behave differently, so it is a dependency. We just don't enable
;; Flycheck anywhere else and rely on `lsp-ui' to handle things when
;; appropriate. However, interestingly, Flycheck is not marked as a
;; dependency of `lsp-ui', hence this declaration.
(use-package flycheck
  :blackout t)

;; Package `lsp-ui' provides a pretty UI for showing diagnostic
;; messages from LSP in the buffer using overlays. It's configured
;; automatically by `lsp-mode'.
(use-package lsp-ui
  :bind (("C-c f" . #'lsp-ui-sideline-apply-code-actions))
  :config

  (radian-defadvice radian--advice-lsp-ui-apply-single-fix
      (orig-fun &rest args)
    :around #'lsp-ui-sideline-apply-code-actions
    "Apply code fix immediately if only one is possible."
    (radian-flet ((defun completing-read (prompt collection &rest args)
                    (if (= (safe-length collection) 1)
                        (car collection)
                      (apply completing-read prompt collection args))))
      (apply orig-fun args)))

  (use-feature lsp-mode
    :config

    ;; With `lsp-ui', there's no need for the ElDoc integration
    ;; provided by `lsp-mode', and in fact for Bash it is very
    ;; annoying since all the hover information is multiline.
    (setq lsp-eldoc-enable-hover nil)))

;; Feature `lsp-ui-doc' from package `lsp-ui' displays documentation
;; in a child frame when point is on a symbol.
(use-feature lsp-ui-doc
  :config

  ;; https://github.com/emacs-lsp/lsp-ui/issues/414
  (add-to-list 'lsp-ui-doc-frame-parameters '(no-accept-focus . t))

  (radian-defadvice radian--advice-lsp-ui-doc-allow-multiline (func &rest args)
    :around #'lsp-ui-doc--render-buffer
    "Prevent `lsp-ui-doc' from removing newlines from documentation."
    (radian-flet ((defun replace-regexp-in-string
                      (regexp rep string &rest args)
                    (if (equal regexp "`\\([\n]+\\)")
                        string
                      (apply replace-regexp-in-string
                             regexp rep string args))))
      (apply func args))))

;;; Language support
;;;; Lisp languages

;; Feature `lisp-mode' provides a base major mode for Lisp languages,
;; and supporting functions for dealing with Lisp code.
(use-feature lisp-mode
  :init

  (add-to-list 'safe-local-variable-values
               '(lisp-indent-function . common-lisp-indent-function)))

;;;; Version control

;; Feature `vc-hooks' provides hooks for the Emacs VC package. We
;; don't use VC, because Magit is superior in pretty much every way.
(use-feature vc-hooks
  :config

  ;; Disable VC. This improves performance and disables some annoying
  ;; warning messages and prompts, especially regarding symlinks. See
  ;; https://stackoverflow.com/a/6190338/3538165.
  (setq vc-handled-backends nil))

;; Feature `smerge-mode' provides an interactive mode for visualizing
;; and resolving Git merge conflicts.
(use-feature smerge-mode
  :blackout t)

;; Package `with-editor' provides infrastructure for using Emacs as an
;; external editor for programs like Git. It is used by Magit.
(use-package with-editor
  :config/el-patch

  ;; Make sure that `with-editor' always starts a server with a
  ;; nonstandard name, instead of using the default one, so that
  ;; emacsclient from a tty never picks it up (which messes up the
  ;; color theme).
  (defun with-editor--setup ()
    (if (or (not with-editor-emacsclient-executable)
            (file-remote-p default-directory))
        (push (concat with-editor--envvar "=" with-editor-sleeping-editor)
              process-environment)
      ;; Make sure server-use-tcp's value is valid.
      (unless (featurep 'make-network-process '(:family local))
        (setq server-use-tcp t))
      ;; Make sure the server is running.
      (unless (process-live-p server-process)
        (el-patch-splice 2
          (when (server-running-p server-name)
            (setq server-name (format "server%s" (emacs-pid)))
            (when (server-running-p server-name)
              (server-force-delete server-name))))
        (server-start))
      ;; Tell $EDITOR to use the Emacsclient.
      (push (concat with-editor--envvar "="
                    (shell-quote-argument with-editor-emacsclient-executable)
                    ;; Tell the process where the server file is.
                    (and (not server-use-tcp)
                         (concat " --socket-name="
                                 (shell-quote-argument
                                  (expand-file-name server-name
                                                    server-socket-dir)))))
            process-environment)
      (when server-use-tcp
        (push (concat "EMACS_SERVER_FILE="
                      (expand-file-name server-name server-auth-dir))
              process-environment))
      ;; As last resort fallback to the sleeping editor.
      (push (concat "ALTERNATE_EDITOR=" with-editor-sleeping-editor)
            process-environment))))

;; Package `transient' is the interface used by Magit to display
;; popups.
(use-package transient
  :config

  ;; Allow using `q' to quit out of popups, in addition to `C-g'. See
  ;; <https://magit.vc/manual/transient.html#Why-does-q-not-quit-popups-anymore_003f>
  ;; for discussion.
  (transient-bind-q-to-quit))

;; Package `magit' provides a full graphical interface for Git within
;; Emacs.
(use-package magit
  :bind (;; This is the primary entry point for Magit. Binding to C-x
         ;; g is recommended in the manual:
         ;; https://magit.vc/manual/magit.html#Getting-Started
         ("C-x g" . #'magit-status)
         ;; Alternate transient entry point; binding recommended in
         ;; <https://magit.vc/manual/magit.html#Transient-Commands>.
         ("C-x M-g" . #'magit-dispatch)
         ;; Completing the trio of bindings in `magit-file-mode-map'.
         ("C-c M-g" . #'magit-file-dispatch))

  :init

  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))

  :config/el-patch

  ;; Prevent Emacs asking if we're sure we want to exit, if a
  ;; Magit-spawned git-credential-cache process is running.
  (defun magit-maybe-start-credential-cache-daemon ()
    "Maybe start a `git-credential-cache--daemon' process.
If such a process is already running or if the value of option
`magit-credential-cache-daemon-socket' is nil, then do nothing.
Otherwise start the process passing the value of that options
as argument."
    (unless (or (not magit-credential-cache-daemon-socket)
                (process-live-p magit-credential-cache-daemon-process)
                (memq magit-credential-cache-daemon-process
                      (list-system-processes)))
      (setq magit-credential-cache-daemon-process
            (or (--first (let* ((attr (process-attributes it))
                                (comm (cdr (assq 'comm attr)))
                                (user (cdr (assq 'user attr))))
                           (and (string= comm "git-credential-cache--daemon")
                                (string= user user-login-name)))
                         (list-system-processes))
                (condition-case nil
                    (el-patch-wrap 2
                      (with-current-buffer
                          (get-buffer-create " *git-credential-cache--daemon*")
                        (start-process "git-credential-cache--daemon"
                                       (el-patch-swap
                                         " *git-credential-cache--daemon*"
                                         (current-buffer))
                                       magit-git-executable
                                       "credential-cache--daemon"
                                       magit-credential-cache-daemon-socket)
                        (el-patch-add
                          (set-process-query-on-exit-flag
                           (get-buffer-process (current-buffer)) nil))))
                  ;; Some Git implementations (e.g. Windows) won't have
                  ;; this program; if we fail the first time, stop trying.
                  ((debug error)
                   (remove-hook
                    'magit-credential-hook
                    #'magit-maybe-start-credential-cache-daemon)))))))

  :config

  ;; The default location for git-credential-cache is in
  ;; ~/.config/git/credential. However, if ~/.git-credential-cache/
  ;; exists, then it is used instead. Magit seems to be hardcoded to
  ;; use the latter, so here we override it to have more correct
  ;; behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (let* ((xdg-config-home (or (getenv "XDG_CONFIG_HOME")
                                (expand-file-name "~/.config/")))
           (socket (expand-file-name "git/credential/socket" xdg-config-home)))
      (setq magit-credential-cache-daemon-socket socket)))

  ;; Don't try to save unsaved buffers when using Magit. We know
  ;; perfectly well that we need to save our buffers if we want Magit
  ;; to see them.
  (setq magit-save-repository-buffers nil)

  (transient-append-suffix
    'magit-merge "-n"
    '("-u" "Allow unrelated" "--allow-unrelated-histories"))

  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  (transient-append-suffix 'magit-fetch "-t"
    '("-u" "Unshallow" "--unshallow")))

;; Feature `magit-diff' from package `magit' handles all the stuff
;; related to interactive Git diffs.
(use-feature magit-diff
  :config

  (radian-defadvice radian--magit-diff-revert-before-smerge (buf _pos)
    :before #'magit-diff-visit-file--setup
    "Before calling `smerge-start-session', try to revert buffer.
This is necessary because it's possible that the file being
visited has changed on disk (due to merge conflict, for example)
but it was already visited, and hasn't been autoreverted yet
(because it hasn't been visible in a window, for example). But
`smerge-start-session', which is called by Magit while jumping
you to the file, will not wait for an autorevert. It will just
see that there aren't any conflict markers in the file and
disable itself. Sad."
    (with-current-buffer buf
      (auto-revert-handler))))

;; Feature `git-commit' from package `magit' provides the commit
;; message editing capabilities of Magit.
(use-feature git-commit
  :config

  ;; Max length for commit message summary is 50 characters as per
  ;; https://chris.beams.io/posts/git-commit/.
  (setq git-commit-summary-max-length 50))

;; Package `emacsql-sqlite' is a dependency of Forge which is used to
;; interact with the SQLite database that Forge uses to keep track of
;; information about pull requests.
(use-feature emacsql-sqlite
  :init

  ;; Put the EmacSQL binary in the repository, not the build dir. That
  ;; way we don't have to recompile it every time packages get rebuilt
  ;; by straight.el. See
  ;; <https://github.com/raxod502/straight.el/issues/274> for not
  ;; having to use the internal function `straight--dir'.
  (setq emacsql-sqlite-data-root (straight--repos-dir "emacsql"))

  :config

  (radian-defadvice radian--advice-emacsql-no-compile-during-compile
      (&rest _)
    :before-until #'emacsql-sqlite-ensure-binary
    "Prevent EmacSQL from trying to compile stuff during byte-compilation.
This is a problem because Forge tries to get EmacSQL to compile
its binary at load time, which is bad (you should never do
anything significant at package load time) since it breaks CI."
    byte-compile-current-file))

;; Package `forge' provides a GitHub/GitLab/etc. interface directly
;; within Magit.
(use-package forge)

;; Feature `forge-core' from package `forge' implements the core
;; functionality.
(use-feature forge-core
  :config

  (radian-defadvice radian--forge-get-repository-lazily (&rest _)
    :before-while #'forge-get-repository
    "Make `forge-get-repository' return nil if the binary isn't built yet.
This prevents having EmacSQL try to build its binary (which may
be annoying, inconvenient, or impossible depending on the
situation) just because you tried to do literally anything with
Magit."
    (file-executable-p emacsql-sqlite-executable))

  (radian-defadvice radian--forge-build-binary-lazily (&rest _)
    :before #'forge-dispatch
    "Make `forge-dispatch' build the binary if necessary.
Normally, the binary gets built as soon as Forge is loaded, which
is terrible UX. We disable that above, so we now have to manually
make sure it does get built when we actually issue a Forge
command."
    (unless (file-executable-p emacsql-sqlite-executable)
      (emacsql-sqlite-compile 2))))

;; Package `git-gutter' adds a column to the left-hand side of each
;; window, showing which lines have been added, removed, or modified
;; since the last Git commit.
(use-package git-gutter
  :commands (git-gutter:previous-hunk
             git-gutter:next-hunk
             radian-git-gutter:beginning-of-hunk
             git-gutter:end-of-hunk
             git-gutter:revert-hunk)
  :init

  (radian-bind-key "v p" #'git-gutter:previous-hunk)
  (radian-bind-key "v n" #'git-gutter:next-hunk)
  (radian-bind-key "v a" #'radian-git-gutter:beginning-of-hunk)
  (radian-bind-key "v e" #'git-gutter:end-of-hunk)
  (radian-bind-key "v k" #'git-gutter:revert-hunk)

  ;; Disable in Org mode, as per
  ;; <https://github.com/syl20bnr/spacemacs/issues/10555> and
  ;; <https://github.com/syohex/emacs-git-gutter/issues/24>.
  ;; Apparently, the mode-enabling function for global minor modes
  ;; gets called for new buffers while they are still in
  ;; `fundamental-mode', before a major mode has been assigned. I
  ;; don't know why this is the case, but adding `fundamental-mode'
  ;; here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode org-mode))

  (radian-defhook radian--git-gutter-load ()
    find-file-hook
    "Load `git-gutter' when initially finding a file."
    (require 'git-gutter)
    (remove-hook 'find-file-hook #'radian--git-gutter-load))

  :config

  ;; Don't prompt when reverting hunk.
  (setq git-gutter:ask-p nil)

  (global-git-gutter-mode +1)

  (defun radian-git-gutter:beginning-of-hunk ()
    "Move to beginning of current diff hunk."
    (interactive)
    (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
      (let ((lines (- (git-gutter-hunk-start-line it) (line-number-at-pos))))
        ;; This will move backwards since lines will be negative.
        (forward-line lines))))

  ;; Shuffle around all the hooks. `git-gutter' puts itself on a bunch
  ;; of different things, but not exactly the right things. Remove all
  ;; its meddling, and then do the right thing (run on window or
  ;; buffer switch after a top-level command, after a buffer revert,
  ;; and after Apheleia runs).

  (remove-hook 'post-command-hook #'git-gutter:post-command-hook)
  (ad-deactivate #'quit-window)
  (ad-deactivate #'switch-to-buffer)

  (defvar radian--git-gutter-last-buffer-and-window nil
    "Cons of current buffer and selected window before last command.
This is used to detect when the current buffer or selected window
changes, which means that `git-gutter' needs to be re-run.")

  (radian-defhook radian--git-gutter-on-buffer-or-window-change ()
    post-command-hook
    "Update `git-gutter' when current buffer or selected window changes."
    (let ((new (cons (current-buffer) (selected-window))))
      (unless (equal new radian--git-gutter-last-buffer-and-window)
        (setq radian--git-gutter-last-buffer-and-window new)
        ;; Sometimes the current buffer has not gotten updated yet
        ;; after switching window, for example after `quit-window'.
        (with-current-buffer (window-buffer)
          (when git-gutter-mode
            (when buffer-file-name
              (unless (file-remote-p buffer-file-name)
                (git-gutter))))))))

  (use-feature autorevert
    :config

    (radian-defhook radian--git-gutter-after-autorevert ()
      after-revert-hook
      "Update `git-gutter' after the buffer is autoreverted."
      (when git-gutter-mode
        (git-gutter))))

  (use-feature apheleia
    :config

    (radian-defhook radian--git-gutter-after-apheleia ()
      apheleia-post-format-hook
      "Update `git-gutter' after Apheleia formats the buffer."
      (when git-gutter-mode
        (git-gutter))))

  :blackout git-gutter-mode)

;; Package `git-gutter-fringe' integrates with `git-gutter' to make
;; the gutter display use the window fringe rather than a column of
;; text.
;;
;; Note that we only even put the package on the load path if
;; `git-gutter-fringe' fringe is defined. The function might not be
;; defined if Emacs was not built with X/Cocoa support, and if that's
;; the case, then loading it will cause errors (and besides that, will
;; break `git-gutter' since the fringe stuff is not available).
;; However, we do need to load the package in order to byte-compile
;; this configuration. That's okay since it's only done in a
;; subprocess (so it won't break `git-gutter') but we still need to
;; fix the errors in that case. Hence the `eval-when-compile'.
(straight-register-package 'git-gutter-fringe)
(when (fboundp 'define-fringe-bitmap)
  (eval-when-compile
    (unless (fboundp 'define-fringe-bitmap)
      (fset 'define-fringe-bitmap #'ignore))
    (unless (boundp 'overflow-newline-into-fringe)
      (setq overflow-newline-into-fringe t)))
  (use-package git-gutter-fringe
    :demand t
    :after git-gutter
    :init

    (use-feature git-gutter
      :config

      ;; This function is only available when Emacs is built with
      ;; X/Cocoa support, see e.g.
      ;; <https://github.com/pft/mingus/issues/5>. If we try to
      ;; load/configure `git-gutter-fringe' without it, we run into
      ;; trouble.
      (when (fboundp 'define-fringe-bitmap)
        (require 'git-gutter-fringe)))

    :config

    (fringe-helper-define 'radian--git-gutter-blank nil
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........")

    (radian-defadvice radian--advice-git-gutter-remove-bitmaps
        (func &rest args)
      :around #'git-gutter-fr:view-diff-infos
      "Disable the cutesy bitmap pluses and minuses from `git-gutter-fringe'.
Instead, display simply a flat colored region in the fringe."
      (radian-flet ((defun fringe-helper-insert-region
                        (beg end _bitmap &rest args)
                      (apply fringe-helper-insert-region
                             beg end 'radian--git-gutter-blank args)))
        (apply func args)))))

;;;; External commands

;; Feature `compile' provides a way to run a shell command from Emacs
;; and view the output in real time, with errors and warnings
;; highlighted and hyperlinked.
(use-feature compile
  :init

  (radian-bind-key "m" #'compile)

  :config

  ;; By default, run from root of current Git repository.
  (setq compile-command "git exec make ")

  ;; Automatically scroll the Compilation buffer as output appears,
  ;; but stop at the first error.
  (setq compilation-scroll-output 'first-error)

  ;; Don't ask about saving buffers when invoking `compile'. Try to
  ;; save them all immediately using `save-some-buffers'.
  (setq compilation-ask-about-save nil)

  ;; Actually, don't bother saving buffers at all. That's dumb. We
  ;; know to save our buffers if we want them to be updated on disk.
  (setq compilation-save-buffers-predicate
        (lambda ()))

  (radian-defadvice radian--advice-compile-pop-to-buffer (buf)
    :filter-return #'compilation-start
    "Pop to compilation buffer on \\[compile]."
    (prog1 buf
      (when-let ((win (get-buffer-window buf)))
        (select-window win)))))

;; C++ Formatting
(defconst monkey-big-fun-c-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
  "Monkey's Big Fun C++ Style")

(defun monkey-big-fun-c-hook ()
  (c-add-style "BigFun" monkey-big-fun-c-style t)

  ;; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  ;; Newline indents, semi-colon doesn't
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))

  ;; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)

  ;; Abbrevation expansion
  (abbrev-mode 1))

(add-hook 'c-mode-common-hook 'monkey-big-fun-c-hook)

;; Appearance
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
