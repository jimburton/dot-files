;;; package -- Mail config
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent
      mu4e-sent-folder   "/Sent"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder  "/Trash"
      mu4e-refile-folder "/Archive"
      mu4e-sent-messages-behavior 'delete
      mu4e-get-mail-command "offlineimap -o"
      mu4e-update-interval 300
      mu4e-compose-signature-auto-include nil
      mu4e-view-use-gnus t
      gnus-blocked-images "http")

(use-package mu4e-marker-icons
  :ensure t
  :init (mu4e-marker-icons-mode 1))
(use-package mu4e-alert
  :ensure t
  :init (mu4e-marker-icons-mode 1))
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
;(add-to-list 'mu4e-bookmarks
;  '( :name  "lilleys"
;     :query "list:lilleyslist.googlegroups.com"
					;     :key   ?l))
(setq mu4e-maildir-shortcuts
  '( (:maildir "/INBOX"   :key  ?i)
     (:maildir "/Sent"    :key  ?s)
     (:maildir "/Drafts"  :key  ?d)
     (:maildir "/Archive" :key  ?a)))
(require 'mu4e-icalendar)
(mu4e-icalendar-setup)
(add-to-list 'mu4e-bookmarks
	     '( :name  "Flagged messages"
		       :query "flag:flagged"
		       :key   ?f)
	     t)

(use-package mu4e-column-faces
  :after mu4e
  :config (mu4e-column-faces-mode))
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   smtpmail-default-smtp-server "localhost"
   smtpmail-smtp-server "localhost"
   smtpmail-stream-type 'plain
   smtpmail-smtp-service 1025)

(provide 'init-mail)
