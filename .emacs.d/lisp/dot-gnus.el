;; My Gnus config
(eval-when-compile
  (require 'cl))
(require 'use-package)

(eval-and-compile
  (push (expand-file-name "override/bbdb/lisp" user-emacs-directory)
        load-path))

(require 'gnus)
(require 'message)
(require 'bbdb)
(require 'bbdb-gnus)
(require 'bbdb-message)

(gnus-delay-initialize)

(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'gnus 'message)
(setq bbdb-pop-up-window-size 0.15)
(setq bbdb-mua-pop-up-window-size 0.15)
(setq bbdb-mua-update-interactive-p '(query . create))
(setq bbdb-message-all-addresses t)

(add-hook 'gnus-summary-mode-hook
          (lambda ()
            (define-key gnus-summary-mode-map (kbd ";") 'bbdb-mua-edit-field)))
(setq bbdb-file "~/Dropbox/bbdb")

(provide 'dot-gnus)
