;;; Org Mode Organisation

;; Config for my actual organisation in org mode.

(if (eq system-type 'windows-nt)
    (setq org-directory "~/../../Dropbox/GTD")
  (setq org-directory "~/Dropbox/GTD"))

(setq org-birthdays-file (concat org-directory "/birthdays.org"))

(setq org-personal-file (concat org-directory "/personal.org"))
(setq org-work-file (concat org-directory "/work.org"))

(setq org-tasks-file (concat org-directory "/tasks.org"))
(setq org-journal-file (concat org-directory "/journal.org"))

;; Org Agenda
(setq org-agenda-files
      (delq nil
	    (mapcar (lambda (x) (and (file-exists-p x) x))
		    `(,org-tasks-file
		      ,org-journal-file
		      ,org-personal-file
		      ,org-birthdays-file
		      ,org-work-file))))

;; Agenda settings, two days at a time, see log entries, but not
;; scheduled items that are finished. Use a timegrid.
(setq org-agenda-span 2)
(setq org-agenda-sticky nil)
(setq org-agenda-inhibit-startup t)
(setq org-agenda-use-tag-inheritance t)
(setq org-agenda-show-log t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
(setq org-agenda-time-grid
      '((daily today require-timed)
        "----------------"
        (800 1000 1200 1400 1600 1800)))
(setq org-columns-default-format "%50ITEM %12SCHEDULED %TODO %3PRIORITY %Effort{:} %TAGS")

;; Start weeks on a Saturday, since I'm done for work on the Friday.
(setq org-agenda-start-on-weekday 6)

(provide 'org-mode-organisation)
