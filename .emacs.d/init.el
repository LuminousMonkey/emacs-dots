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
