;; -*- coding: utf-8; lexical-binding: t; -*-
;; Holiday Settings for Org Mode

;; Some Australian holidays go to the next Monday if on a weekend.
(defun next-week-day-holiday (in-day in-month in-year)
  (let* ((holiday-date (list in-month in-day in-year))
	 (current-day (calendar-day-of-week holiday-date)))
    (if (or (= 6 current-day) (= 0 current-day))
	(+ (1+ (mod current-day 5))
	   (calendar-absolute-from-gregorian holiday-date))
      (calendar-absolute-from-gregorian holiday-date))))

;; Australia Public Holidays
(setq holiday-local-holidays
      '((holiday-fixed 1 1     "New Years Day")
	(holiday-sexp (next-week-day-holiday 1 1 year)
		      "New Years Day Holiday")
	(holiday-fixed 1 26    "Australia Day")
	(holiday-sexp (next-week-day-holiday 26 1 year)
		      "Australia Day Holiday")
      (holiday-fixed 3 4     "Labour Day")
      (holiday-easter-etc -2 "Good Friday")
      (holiday-easter-etc +1 "Easter Monday")
      (holiday-fixed 4 25    "Anzac Day")
      (holiday-float 6 1 1   "Queens Birthday")
      (holiday-float 10 1 4  "Labour Day")
      (holiday-fixed 12 25   "Christmas Day")
      (holiday-fixed 12 26   "Boxing Day")
      (holiday-sexp (next-week-day-holiday 26 12 year)
		    "Boxing Day Holiday")))

(setq org-agenda-include-diary t)

(setq holiday-general-holidays nil)
(setq holiday-christian-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq holiday-bahai-holidays nil)
(setq holiday-oriental-holidays nil)

(provide 'org-mode-holidays)
