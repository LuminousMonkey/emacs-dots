;; -*- coding: utf-8; lexical-binding: t; -*-
;; LuminousMonkey Emacs config
;;
;; This file is loaded before package.el is initialized, and before
;; the first graphical frame is initialized, by Emacs 27 (but not by
;; any previous version of Emacs). Implimented by the guy who I copied
;; this config from:
;; https://github.com/raxod502/radian/blob/develop/emacs/early-init.el
;;
;; If the early init-file is available, we actually execute our entire
;; init process within it, by just loading the regular init-file.
;; (That file takes care of making sure it is only loaded once.)

(defun luminous--advice-fix-display-graphic-p (func &optional display)
  "Fix `display-graphic-p' so it works while loading the early init-file."
  (if display
      (funcall func display)
    ;; `display-graphic-p' lies by returning nil, but
    ;; `initial-window-system' tells the truth (it is nil only if we
    ;; are actually in a tty environment).
    initial-window-system))

(advice-add #'display-graphic-p :around
            #'luminous--advice-fix-display-graphic-p)

(defun luminous--advice-fix-xw-display-color-p (func &optional display)
  "Fix `xw-display-color-p' so it works while loading the early init-file."
  (if (or display after-init-time)
      (funcall func display)
    ;; Make an educated guess.
    initial-window-system))

(advice-add #'xw-display-color-p :around
            #'luminous--advice-fix-xw-display-color-p)

(defun luminous--advice-disable-x-resource-application ()
  "Disable `x-apply-session-resources'.
Now, `x-apply-session-resources' normally gets called before
reading the init-file. However if we do our initialization in the
early init-file, before that function gets called, then it may
override some important things like the cursor color. So we just
disable it, since there's no real reason to respect X
resources.")

(advice-add #'x-apply-session-resources :override
            #'luminous--advice-disable-x-resource-application)

;; Load the regular init-file.
(load
 (expand-file-name "init.el" user-emacs-directory) nil 'nomessage 'nosuffix)

;; Avoid messing with things more than necessary.
(advice-remove #'display-graphic-p #'luminous--advice-fix-display-graphic-p)
(advice-remove #'xw-display-color-p #'luminous--advice-fix-xw-display-color-p)
