;; -*- coding: utf-8; lexical-binding: t; -*-

;; Clojure Programming
(use-package clojure-mode
  :mode (("\\.edn$" . clojure-mode))
  :init
  (progn
    (use-package clojure-snippets)
    (use-package clojure-mode-extra-font-locking)
    (use-package cider
      :diminish " ç"
      :init
      (progn
	(add-hook 'cider-mode-hook 'eldoc-mode)
	(add-hook 'cider-repl-mode-hook 'subword-mode))
      :config
      (progn
	(setq nrepl-log-messages t
	      cider-popup-stacktraces-in-repl t
	      cider-repl-display-in-current-window t
	      cider-repl-use-clojure-font-lock nil
	      cider-prompt-save-file-on-load 'always-save
	      cider-font-lock-dynamically '(macro core function var)
	      nrepl-hide-special-buffers t
	      cider-overlays-use-font-lock t
	      clojure-use-backtracking-indent t
	      cider-repl-history-file (expand-file-name "nrepl-history" user-emacs-directory)
	      cider-auto-select-error-buffer nil
	      cider-prompt-save-file-on-load nil
	      cider-repl-use-pretty-printing t
	      cider-repl-display-help-banner nil)
	(add-to-list 'same-window-buffer-names "*cider*")))

    (setq clojure--prettify-symbols-alist
	  '(("fn" . ?λ)
	    ("not=" . ?≠)
	    ("identical?" . ?≡)
	    ("<=" . ?≤)
	    (">=" . ?≥)
	    ("->" . (?- (Br . Bc) ?- (Br . Bc) ?>))
	    ("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
			   (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
			   (Bc . Bl) ?- (Br . Br) ?>))))

    (add-hook 'clojure-mode-hook 'prettify-symbols-mode)
    (add-hook 'cider-repl-mode-hook '(lambda () (setq scroll-conservatively 101)))

    (use-package flycheck-clojure
      :init
      (progn
	(eval-after-load 'flycheck '(flycheck-clojure-setup))))))

(provide 'programming-clojure)
