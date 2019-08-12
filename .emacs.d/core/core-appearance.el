;; -*- coding: utf-8; lexical-binding: t; -*-

;; Emacs Appearance

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

(load-theme 'monokai 'no-confirm)

;; Be sure we have a fallback font for symbols. Also, could be running
;; emacs client, which doesn't appear to set the 'window-system. So,
;; attach to the hooks.

;; https://www.emacswiki.org/emacs/SettingFrameColorsForEmacsClient
(defun setup-window-system (&rest frame)
  (if (window-system)
      (progn
    (set-frame-size (selected-frame) 120 60)
    (luminousmonkey/set-default-font '("Fira Code Medium"
                                       :size 18
                                       :weight normal
                                       :width normal
                                       :powerline-scale 1.4))
    ;; Also fix powerline
    (powerline-reset))))

;; Keep a clock in the modeline.
(display-time-mode 1)

(provide 'core-appearance)
