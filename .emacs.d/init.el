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

;; Taken from: https://github.com/raxod502/radian/blob/develop/emacs/init.el
(defvar luminous--init-file-loaded-p nil
  "Non-nil if the init-file has already been loaded.
This is important for Emacs 27 and above, since our early
init-file just loads the regular init-file, which would lead to
loading the init-file twice if it were not for this variable.")

(when (not (and (not after-init-time) luminous--init-file-loaded-p))
  ;; If already loaded, do nothing. But still allow re-loading, just
  ;; do it only once during init.
  (setq luminous--init-file-loaded-p t)
  (defvar luminous-minimum-emacs-version "25.2"
    "LuminousMonkey isn't going to support any Emacs version below this.")
  (defvar luminous-local-init-file
    (expand-file-name "init.local.el" user-emacs-directory)
    "File for local customisations.")

  ;; Prevent package.el from modifying this file.
  (setq package-enable-at-startup nil)

  ;; Prevent Custom from modifying this file.
  (setq custom-file (expand-file-name
                     (format "custom-%d-%d.el" (emacs-pid) (random))
                     temporary-file-directory))

  ;; Make sure we are running a modern enough Emacs, otherwise abort
  ;; init.
  (if (version< emacs-version luminous-minimum-emacs-version)
      (error (concat "You gotta run Emacs %s, "
                     "but you are running Emacs %s")
             luminous-minimum-emacs-version emacs-version)

    (let ((link-target
           ;; We may be loading init.el in batch mode, in which case
           ;; `user-init-file' is nil. In that case, we should have
           ;; some backup options to try.
           (or user-init-file
               load-file-name
               buffer-file-name)))

      (defvar luminous-lib-file (expand-file-name
				 "luminous.el"
				 (file-name-directory link-target))
        "File containing my main configuration.
This file is loaded by init.el.")

      (unwind-protect
          ;; Load the main Luminous configuration code. Disable
          ;; `file-name-handler-alist' to improve load time.
          ;;
          ;; Make sure not to load an out-of-date .elc file. Since
          ;; we byte-compile asynchronously in the background after
          ;; init succeeds, this case will happen often.
          (let ((file-name-handler-alist nil)
                (load-prefer-newer t)
                (stale-bytecode t))
            (catch 'stale-bytecode
              ;; We actually embed the contents of the local
              ;; init-file directly into the compiled luminous.elc, so
              ;; that it can get compiled as well (and its
              ;; macroexpansion can use packages that Luminous only
              ;; loads at compile-time). So that means we have to go
              ;; the slow path if the local init-file has been
              ;; updated more recently than the compiled luminous.elc.
                (when (file-newer-than-file-p
                       luminous-local-init-file
                       (concat luminous-lib-file "c"))
                  (throw 'stale-bytecode nil))
                (load
                 (file-name-sans-extension luminous-lib-file)
                 nil 'nomessage)
                (setq stale-bytecode nil))
              (when stale-bytecode
                ;; Don't bother trying to recompile, unlike in
                ;; straight.el, since we are going to handle that
                ;; later, asynchronously.
                (ignore-errors
                  (delete-file (concat luminous-lib-file "c")))
                (load luminous-lib-file nil 'nomessage 'nosuffix)))
          (run-hooks 'luminous--finalise-init-hook)))))
