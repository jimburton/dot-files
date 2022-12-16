;;; package --- init file
;;; Commentary:
;;; My location for external packages.
;(set-face-attribute 'default nil :height 100)

;;;;;;;;;;;;;;;;
;; put these commands in a cheat-sheet
;;;;;;;;;;;;;;;;
;;
;; Jumping to locations in a buffer
;;
;; Jump to the mark, there is only one mark at a time
;;
;; C-SPC C-SPC             Set the mark and keep region highlighting off
;; C-u C-SPC               Jump back to last mark
;;
;; Jump to (any number of) registers, which die with the session
;;
;; C-x r <SPC> R           Save position of point in register R
;; C-x r j R               Jump to register R
;;
;; Bookmarks -- any number, long names, last across sessions
;;
;; C-x r m <RET>           Set the bookmark for the visited file, at point.
;; C-x r m BOOKMARK <RET>  Set the bookmark named BOOKMARK at point
;; C-x r b BOOKMARK <RET>  Jump to the bookmark named BOOKMARK
;; C-x r l                 List all bookmarks (`list-bookmarks').
;; M-x bookmark-save       Save all the current bookmark values in the default bookmark file.
;;
;;
;;;;;;;;;;;;
;; 
;; Saving text in registers:
;;
;; `C-x r s r` stores a copy of the text of the region into the register
;; named `r`.  Given a numeric argument, `C-x r s r` deletes the text from
;; the buffer as well.
;;
;; `C-x r i r` inserts in the buffer the text from register `r`.  Normally
;; it leaves point before the text and places the mark after, but with
;; a numeric argument (C-u) it puts point after the text and the mark
;; before.
;;

;;; Code:
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

(require 'package)
(add-to-list 'package-archives
	 '("melpa" . "https://melpa.org/packages/")
         ;'("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
	 )

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq inhibit-splash-screen        t
      make-backup-files            nil
      use-dialog-box               nil
      bookmark-save-flag           1
      fill-column                  95
      x-select-enable-clipboard    t
      browse-url-browser-function  'browse-url-firefox
      european-calendar-style      t
      appt-audible                 t
      appt-display-format          'window
      canlock-password             "f06cedf552aff05ea3bb02ae875b8571cdc1bdc1"
      case-fold-search             t
      column-number-mode           t
      current-language-environment "Latin-1"
      dabbrev-case-fold-search     nil
      default-input-method         "latin-1-prefix"
      display-time-mode            t
      mail-signature               t
      undo-limit                   100000
      org-log-done                 'time
      org-image-actual-width       nil
      user-mail-address	           "j.burton@brighton.ac.uk"
      user-full-name	           "James Burton"
      message-signature-file       "~/.signature"
      send-mail-function	   'smtpmail-send-it
      message-send-mail-function   'smtpmail-send-it
      smtpmail-smtp-server	   "localhost"
      smtpmail-stream-type         'ssl
      company-selection-wrap-around t
      default-frame-alist          '((cursor-color . "#8b8989")))

(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
	    ;; Set dired-x global variables here.  For example:
	    ;; (setq dired-guess-shell-gnutar "gtar")
	    ;; (setq dired-x-hands-off-my-keys nil)
	    ))
(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; Set dired-x buffer-local variables here.  For example:
	    ;; (dired-omit-mode 1)
	    ))

(menu-bar-mode -1)
(global-set-key [(control f1)] 'menu-bar-mode)
(defun toggle-mode-line ()
  "Toggle the modeline on and off."
  (interactive)
  (setq mode-line-format
	(if (equal mode-line-format nil)
	    (default-value 'mode-line-format)) )
  (redraw-display))
(global-set-key [(control f2)] 'toggle-mode-line)
(toggle-mode-line)
(transient-mark-mode)
(winner-mode 1)
(global-set-key (kbd "M-g g") 'goto-line)
(tool-bar-mode 0)
(ido-mode 1)
(ido-everywhere t)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(prefer-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)
(auto-image-file-mode 1)
(column-number-mode t)
(line-number-mode t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(display-time)

(global-set-key (kbd "C-c o") 'occur)

(add-hook 'dired-load-hook (function
			    (lambda () (load "dired-x"))))

;;resizing windows
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)
(global-set-key (kbd "C-x m")       'browse-url-at-point)

;;save scripts as executable
(add-hook 'after-save-hook
          '(lambda
	     ()
             (progn
               (and (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (save-match-data
                          (looking-at "^#!"))))
                    (shell-command (concat "chmod u+x " buffer-file-name))
                    (message (concat "Saved as script: " buffer-file-name))))))

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

(require 'printing)		; load printing package
(setq pr-path-alist
      '((unix      "." "~/bin" ghostview mpage PATH)
	(ghostview "/usr/bin")
	(mpage     "")))
(setq pr-txt-name      'prt)
(setq pr-txt-printer-alist
      '((prt "lpr" nil "prt")))
(setq pr-ps-name       'lps)
(setq pr-ps-printer-alist
      '((lps_06b "lps" nil "-P" "lps_06b")))

(pr-update-menus t)		; update now printer and utility menus

(autoload 'php-mode "php-mode.el" "Php mode." t)
(setq auto-mode-alist (append '(("/*.\.php[345]?$" . php-mode)) auto-mode-alist))
(add-hook 'php-mode-hook '(lambda ()
			    (progn
			      (setq fill-column 95)
			      (auto-fill-mode 1)
			      (parenthesis-register-keys "[(<'\"" php-mode-map))))

(setq frame-title-format
      '(buffer-file-name "%b - %f" ; File buffer
        (dired-directory dired-directory ; Dired buffer
         (revert-buffer-function "%b" ; Buffer Menu
				 ("%b - Dir: " default-directory))))) ; Plain buffer

(require 'my-functions)
(global-set-key (kbd "C-c C-f")       'jb/fetchmail-wake-or-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Longer domain-specific config is in separate init files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'init-packages)
;(require 'init-mail)
(require 'init-latex)
(require 'init-haskell)

;(require 'my-site-specific)

;;; init.el ends here.
