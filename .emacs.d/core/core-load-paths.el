(defun add-to-load-path (dir) (add-to-list 'load-path dir))

(defvar luminousmonkey-start-directory
  user-emacs-directory
  "LuminousMonkey start directory.")
(defconst luminousmonkey-core-directory
  (expand-file-name (concat luminousmonkey-start-directory "core/"))
  "LuminousMonkey core directory.")
(defconst luminousmonkey-programming-directory
  (expand-file-name (concat luminousmonkey-start-directory "programming/"))
  "LuminousMonkey programming directory.")
(defconst luminousmonkey-org-directory
  (expand-file-name (concat luminousmonkey-start-directory "org-mode/"))
  "LuminousMonkey org-mode config directory.")
(defconst luminousmonkey-social-directory
  (expand-file-name (concat luminousmonkey-start-directory "social/"))
  "LuminousMonkey social config directory.")
(defconst luminousmonkey-gnus
  (expand-file-name (concat luminousmonkey-start-directory "override/gnus/lisp/"))
  "Gnus directory.")
(defconst luminousmonkey-lilypond
  (expand-file-name (concat luminousmonkey-start-directory "override/lilypond/"))
  "Lilypond Emacs directory.")

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")

(mapc 'add-to-load-path
      `(
        ,luminousmonkey-core-directory
        ,luminousmonkey-programming-directory
        ,luminousmonkey-org-directory
        ,luminousmonkey-social-directory
        ,luminousmonkey-gnus
        ,luminousmonkey-lilypond
        ))

(provide 'core-load-paths)
