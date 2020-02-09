;;; My templates for Org Mode

(defvar luminousmonkey/org-basic-task-template "* TODO %^{Task}
  SCHEDULED: %^t
  :PROPERTIES:
  :Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
  :END:
  %?
" "Basic task data")

(defun capture-report-date-file (path)
  (let ((name (replace-regexp-in-string
               " " "-"
               (downcase
                (read-string "File Title: ")))))
    (expand-file-name
     (format "%s-%s.org"
             (format-time-string "%Y-%m-%d")
             name) path)))

(setq org-capture-templates
      `(("t" "Tasks" entry
         (file+headline org-tasks-file "Tasks")
         ,luminousmonkey/org-basic-task-template)
        ("T" "Quick task" entry
         (file+headline org-tasks-file "Tasks")
         "* TODO %^{Task}"
         :immediate-finish t)
        ("j" "Journal entry" plain
         (file+datetree org-journal-file)
         "%K = %a\n%i\n%?\n"
         :unnarrowed t)
        ("B" "Blog Entry" plain
         (file (capture-report-date-file
                "~/Projects/monkey-blog/posts/"))
         "#+title: %^{Title}
#+date: %T\n\n%?"
         )
        ("m" "TODO from Mail" entry
         (file+headline org-tasks-file "Tasks")
         "* TODO %?, Link: %a")))

(provide 'org-mode-templates)
