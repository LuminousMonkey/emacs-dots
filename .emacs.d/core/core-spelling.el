;; -*- coding: utf-8; lexical-binding: t; -*-
;; Lexical binding it what you would expect!

;; avoid spell-checking doublon (double word) in certain major modes
(defvar flyspell-check-doublon t
  "Check doublon (double word) when calling `flyspell-highlight-incorrect-region'.")
(make-variable-buffer-local 'flyspell-check-doublon)

(use-package flyspell
  :straight nil
  :blackout " ⓒ"
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :config
  (progn
    (setq ispell-program-name "aspell"
      ispell-dictionary "british"
      ispell-extra-args '("--sug-mode=ultra")
      ispell-silently-savep t)
    (add-hook 'ispell-initialize-spellchecker-hook
          (lambda ()
        (setq ispell-base-dicts-override-alist
              '((nil ; default
             "[A-Za-z]" "[^A-Za-z]" "[']" t
             ("-d" "en_GB" "--encoding=utf-8") nil utf-8)
            ("australian"
             "[A-Za-z]" "[^A-Za-z]" "[']" t
             ("-d" "en_AU" "--encoding=utf-8") nil utf-8)
            ("american" ; Yankee English
             "[A-Za-z]" "[^A-Za-z]" "[']" t
             ("-d" "en_US" "--encoding=utf-8") nil utf-8)
            ("british" ; British English
             "[A-Za-z]" "[^A-Za-z]" "[']" t
             ("-d" "en_GB" "--encoding=utf-8") nil utf-8)))))))

(provide 'core-spelling)
