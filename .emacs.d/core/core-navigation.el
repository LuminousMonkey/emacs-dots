
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

(use-package ace-window
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ("C-x o" . ace-window)
  :chords (("jw" . ace-window)))

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    (setq helm-idle-delay 0.0
	  helm-input-idle-delay 0.01
	  helm-yas-display-key-on-candidate t
	  helm-quick-update t
	  helm-M-x-requires-pattern nil
	  helm-ff-skip-boring-files t)
    (helm-mode)
    (ido-mode -1))
  :bind (("M-x" . helm-M-x)
	 ("C-x C-m" . helm-M-x)
	 ("C-x C-f" . helm-find-files))
  :chords (("FF" . helm-find-files)))

;; (use-package key-chord
;;   :config
;;   (progn
;;     (fset 'key-chord-define 'luminousmonkey/key-chord-define)
;;     (setq key-chord-one-key-delay 0.16)
;;     ;; k can be bound too
;;     (key-chord-define-global "uu" 'undo)))

(provide 'core-navigation)
