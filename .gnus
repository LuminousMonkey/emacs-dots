;; Do not save .newsrc, we do not use a newsreader
(setq gnus-nntp-server nil
      gnus-read-active-file nil
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      gnus-check-new-newsgroups nil)

(setq gnus-select-method '(nnnil ""))

;; http://www.cataclysmicmutation.com/2010/11/multiple-gmail-accounts-in-gnus/
(setq gnus-secondary-select-methods
      '((nnmaildir "Fastmail"
                  (directory "~/.mail/mike-fastmail.com"))
        (nnmaildir "OldEmail"
                   (directory "~/.mail/mike-gmail.com"))
        (nnmaildir "Work"
                   (directory "~/.mail/mike-directcommunications.com.au"))
        (nnmaildir "Uni"
                   (directory "~/.mail/mike-student.curtin.edu.au"))
        (nnmaildir "Uni-Staff"
                   (directory "~/.mail/mike-staff.curtin.edu.au"))))

(setq gnus-parameters
      '(("^nnmaildir.*Work:.*"
         (posting-style
          (name "Mike Aldred")
          (address "mike.aldred@directcommunications.com.au")
          (organization "Direct Communications")
          (signature-file "~/.directcomms_sig")))
        ("^nnmaildir.*Fastmail:.*"
         (posting-style
          (name "Mike Aldred")
          (address "mike.aldred@luminousmonkey.org")
          (gcc "nnmaildir+Fastmail:Sent")))
        ("^nnmaildir.*OldEmail:.*"
         (posting-style
          (name "Mike Aldred")
          (address "mike.aldred@gmail.com")))
        ("^nnmaildir.*Uni:.*"
         (posting-style
          (name "Mike Aldred")
          (address "michael.aldred@student.curtin.edu.au")
          (gcc "nnmaildir+Uni:Sent")))
        ("^nnmaildir.*Uni-Staff:.*"
         (posting-style
          (name "Mike Aldred")
          (address "michael.aldred@curtin.edu.au")
          (gcc "nnmaildir+Uni-Staff:Sent Items")))))

;; mark Gcc (group Cc) messages as read
(setq gnus-gcc-mark-as-read t)

;; Format the lines in the folder view.
(setq gnus-summary-line-format
      (concat "%*%z%U%R %~(max-right 17)~(pad-right 17)&user-date;  "
              "%~(max-right 20)~(pad-right 20)f %B%s\n"))

(setq gnus-subscribe-newsgroup-method 'gnus-subscribe-alphabetically)

;; Do not save .newsrc, we do not use a newsreader
(setq gnus-save-newsrc-file nil)
(setq gnus-read-newsrc-file nil)
(setq gnus-always-read-dribble-file t)

;; GNUS Group
(setq gnus-group-default-list-level (- gnus-level-subscribed 1))
(setq gnus-group-sort-function '(gnus-group-sort-by-alphabet
                                 gnus-group-sort-by-level))

;; Taken from https://github.com/jd/emacs.d/blob/master/gnus.conf.el
(defun jd:gnus-image-or-space (string image image-p)
  (let ((image (create-image image)))
    (if (display-images-p)
        (if image-p
            (propertize string 'display
                        (append image
                                '(:ascent center)))
          (propertize " " 'display `(space . (:width ,(car (image-size image))))))
      (if image-p
          string
        " "))))

(defun gnus-user-format-function-e (dummy)
  (jd:gnus-image-or-space (char-to-string gnus-unread-mark) "~/.emacs.d/icons/email.png"
                          (> (string-to-number gnus-tmp-number-of-unread) 0)))

(defun gnus-user-format-function-M (dummy)
  (jd:gnus-image-or-space (char-to-string gnus-ticked-mark) "~/.emacs.d/icons/important.png"
                          (cdr (assq 'tick gnus-tmp-marked))))

;; Update to get nicer names:
;; http://stackoverflow.com/questions/22150745/changing-the-display-name-of-a-gnus-group

(setq group-name-map '(("nnmaildir+OldEmail:INBOX" . "Gmail-Inbox")
                       ("nnmaildir+Work:INBOX" . "Work-Inbox")
                       ("nnmaildir+Work:All Mail" . "Work-Archive")
                       ("nnmaildir+Work:Backlog" . "Work-Backlog")
                       ("nnmaildir+Work:Sent Mail" . "Work-Sent")
                       ("nnmaildir+Work:org-archive" . "Work-Org-Archive")
                       ("nnmaildir+Fastmail:INBOX" . "Monkey-Inbox")
                       ("nnmaildir+Uni:INBOX" . "Uni-Inbox")
                       ("nnmaildir+Uni-Staff:INBOX" . "Uni-Staff-Inbox")))

(setq gnus-group-line-format "%ue%uM %S%p[%5t][%L]\t%P%5y:%B%(%uG%)%O\n")

(setq gnus-permanently-visible-groups "^Work")

(defun gnus-user-format-function-G (arg)
  (let ((mapped-name (assoc gnus-tmp-group group-name-map)))
    (if (null mapped-name)
        gnus-tmp-group
      (cdr mapped-name))))

;; gnus-sum
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
(setq gnus-ignored-from-addresses (regexp-opt
                                   '("mike.aldred@luminousmonkey.org"
                                     "mike.aldred@gmail.com"
                                     "mike.aldred@directcommunications.com.au"
                                     "michael.aldred@student.curtin.edu.au"
                                     "09831542@student.curtin.edu.au"
                                     "michael.aldred@curtin.edu.au")))
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-number
                                   gnus-thread-sort-by-subject
                                   gnus-thread-sort-by-date
                                   gnus-thread-sort-by-total-score))
(setq gnus-auto-select-subject 'unread) ; Select unread article on group entering
(setq gnus-summary-stop-at-end-of-message t)
(setq gnus-summary-to-prefix "→"
      gnus-summary-newsgroup-prefix "⇶"
      ;; Marks
      gnus-ticked-mark ?⚑
      gnus-dormant-mark ?⚐
      gnus-expirable-mark ?♻
      gnus-read-mark ?✓
      gnus-del-mark ?✗
      gnus-killed-mark ?☠
      gnus-replied-mark ?↺
      gnus-forwarded-mark ?↪
      gnus-cached-mark ?☍
      gnus-recent-mark ?✩
      gnus-unseen-mark ?★
      gnus-unread-mark ?✉
      gnus-score-over-mark ?↑           ; ↑ ☀
      gnus-score-below-mark ?↓         ; ↓ ☂
      gnus-sum-thread-tree-false-root " ◌ "
      gnus-sum-thread-tree-single-indent "◎ "
      gnus-sum-thread-tree-indent "   "
      gnus-sum-thread-tree-root "● "
      gnus-sum-thread-tree-leaf-with-other "├─▶ "
      gnus-sum-thread-tree-single-leaf     "└─▶ " ; "╰─►"
      gnus-sum-thread-tree-vertical        "│ ")

(setq gnus-sorted-header-list
      '("^From:" "^To:" "^Newsgroups:" "^Cc:" "^Subject:" "^Summary:" "^Keywords:" "^Followup-To:" "^Date:" "^Organization:"))
(setq gnus-face-properties-alist
      '((pbm . (:face gnus-x-face :ascent center))
        (png . (:ascent center))))
(setq gnus-treat-from-picon nil)
(setq gnus-treat-newsgroups-picon nil)
(setq gnus-treat-mail-picon nil)
(setq gnus-treat-from-gravatar 'head)
(setq gnus-treat-mail-gravatar 'head)
(setq gnus-treat-body-boundary nil)    ; No body/header separator

(setq gnus-blocked-images nil)          ; HTML rendering

;; gnus-win
(gnus-add-configuration
 ;; two panes side-by-side
 '(article (horizontal 1.0
                       (summary .5 point)
                       (article 1.0))))
(gnus-add-configuration
 ;; two panes side-by-side
 '(summary (horizontal 1.0
                       (summary 1.0 point))))
;; Vertical display when replying
(gnus-add-configuration '(reply (vertical 1.0
                                          (horizontal 1.0
                                                      (message .5 point)
                                                      (article 1.0))
                                          (summary .2))))
(gnus-add-configuration '(reply-yank (vertical 1.0
                                               (horizontal 1.0
                                                           (message .5 point)
                                                           (article 1.0))
                                               (summary .2))))
(gnus-add-configuration '(forward (vertical 1.0
                                            (horizontal 1.0
                                                        (message .5 point)
                                                        (article 1.0))
                                            (summary .2))))

(setq send-mail-function 'smtpmail-send-it)
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;;(setq sendmail-program "msmtp")
