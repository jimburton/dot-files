(setq smtpmail-default-smtp-server "smtp.brighton.ac.uk")
(require 'smtpmail)
(require 'bbdb)

(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-message)
(gnus-registry-initialize)
(setq gnus-select-method                     '(nnmaildir "mymailbox" (directory "~/Mail/"))
      nnmail-crosspost                       nil
      nnmail-resplit-incoming                t
      gnus-read-active-file                  'some
      gnus-message-archive-group             "archive"
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      gnus-thread-hide-subtree               t
      gnus-thread-ignore-subject             t
      gnus-summary-line-format               "%R%z%I%d: %-23,23f %s\n"
      gnus-save-newsrc-file                  nil
      gnus-save-killed-list                  nil
      gnus-check-new-newsgroups              nil
      gnus-buttonized-mime-types             (quote ("multipart/signed" "multipart/alternative" ))
      gnus-always-read-dribble-file          t
      gnus-novice-user                       nil
      gnus-fetch-old-headers                 t
      bbdb-complete-name-full-completion     t
      bbdb-completion-type                   'primary-or-name
      bbdb-complete-name-allow-cycling       t
      bbdb-offer-save                        1
      bbdb-use-pop-up                        nil
      bbdb-electric-p                        t
      bbdb-popup-target-lines                1)


;;prefer plain text
(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(defun my-alter-summary-map ()
  (local-set-key "d" '(lambda () (interactive)
			(gnus-summary-delete-article)
			(message "Article deleted")
			(next-line))))

(add-hook 'gnus-summary-mode-hook 'my-alter-summary-map)
(add-hook 'gnus-group-mode-hook   'gnus-topic-mode)
(add-hook 'gnus-startup-hook      'bbdb-insinuate-gnus)
