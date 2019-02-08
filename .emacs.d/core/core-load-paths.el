(defun add-to-load-path (dir) (add-to-list 'load-path dir))

(defvar luminousmonkey-start-directory
  user-emacs-directory
  "LuminousMonkey start directory.")
(defconst luminousmonkey-core-directory
  (expand-file-name (concat luminousmonkey-start-directory "core/"))
  "LuminousMonkey core directory.")

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")

(mapc 'add-to-load-path
      `(
        ,luminousmonkey-core-directory
        ))

(provide 'core-load-paths)
