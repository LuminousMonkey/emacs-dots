;; -*- coding: utf-8; lexical-binding: t; -*-

;; Make PgUp/Dn move the point.
(setq scroll-error-top-bottom t)

;; Smart home key.
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; Consider CamelCase chunks as words when navigating.
(global-subword-mode 1)

;; Key chords.
;; Key chords allows you to press two keys in quick succession to run
;; a command.
(use-package use-package-chords
  :config (key-chord-mode 1))

(require 'use-package-chords)

(use-package ace-jump-mode
  :config
  (setq ace-jump-mode-submode-list
        '(ace-jump-char-mode
          ace-jump-word-mode
          ace-jump-line-mode))
  :chords (("jj" . ace-jump-word-mode)
       ("jl" . ace-jump-line-mode)
       ("jZ" . ace-jump-zap-to-char)))

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

(use-package ace-window
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("C-x o" . ace-window)
  :chords (("jw" . ace-window)))

;; Helm updates how you can navigate for options in the minibuffer, etc.
(use-package helm
  :diminish helm-mode
  :init
  (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    (setq helm-idle-delay 0.0
      helm-input-idle-delay 0.01
      helm-yas-display-key-on-candidate t
      helm-quick-update t
      helm-M-x-requires-pattern nil
      helm-ff-skip-boring-files t)
    (helm-mode)
    (ido-mode -1)
  :bind (("M-x" . helm-M-x)
     ("C-x C-m" . helm-M-x)
     ("C-x C-f" . helm-find-files))
  :chords (("FF" . helm-find-files))
  :config
  (helm-autoresize-mode t)
  (setq-default helm-display-header-line nil
                helm-autoresize-min-height 10
                helm-autoresize-max-height 35
                helm-split-window-in-side-p t

                helm-M-x-fuzzy-match t
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-apropos-fuzzy-match t)
  (set-face-attribute 'helm-source-header nil :height 0.75))

;; Enable fuzzy matching in Helm navigation.
(use-package helm-flx
  :config
  (with-eval-after-load "helm"
    (require 'helm-flx)
    (helm-flx-mode 1)))

;; Set up a couple of tweaks from helm-ext.
(use-package helm-ext
  :config
  (helm-ext-ff-enable-skipping-dots t)
  (helm-ext-ff-enable-auto-path-expansion t))

;; Describe Bindings
(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
     ("C-h w" . helm-descbinds)))

;; (use-package key-chord
;;   :config
;;   (progn
;;     (fset 'key-chord-define 'luminousmonkey/key-chord-define)
;;     (setq key-chord-one-key-delay 0.16)
;;     ;; k can be bound too
;;     (key-chord-define-global "uu" 'undo)))

(provide 'core-navigation)
