;;; My location for external packages.
;(set-face-attribute 'default nil :height 100)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))

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
;; C-x r s r stores a copy of the text of the region into the register named r. Given a numeric argument, C-x r s r deletes the text from the buffer as well.
;;
;; C-x r i r inserts in the buffer the text from register r. Normally it leaves point before the text and places the mark after, but with a numeric argument (C-u) it puts point after the text and the mark before. 
;;

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

(use-package exec-path-from-shell
	     :ensure t)
(exec-path-from-shell-initialize)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(setq inhibit-splash-screen        t
      make-backup-files            nil
      use-dialog-box               nil
      bookmark-save-flag           1
      fill-column                  95
      x-select-enable-clipboard    t
      browse-url-browser-function 'eww-browse-url
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
      user-mail-address	           "j.burton@brighton.ac.uk"
      user-full-name	           "James Burton"
      message-signature-file       "~/.signature"
      send-mail-function	   'smtpmail-send-it
      message-send-mail-function   'smtpmail-send-it
      smtpmail-smtp-server	   "localhost"
      smtpmail-stream-type         'ssl
      mu4e-maildir                 "~/Mail"   
      mu4e-sent-folder             "/archive"
      mu4e-drafts-folder           "/drafts"
      mu4e-trash-folder            "/trash"
      mu4e-refile-folder           "/archive"
      company-selection-wrap-around t
      default-frame-alist          '((cursor-color . "#8b8989")))

(use-package vlf
  :ensure t)
(use-package doom-modeline
  :ensure t)
(doom-modeline-mode 1)

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
(use-package diminish
	     :ensure t)

(require 'font-lock)
(require 'linum)
(show-paren-mode t)
(setq paren-priority 'close)
(if (load "mwheel" t)
    (mwheel-install))
(autoload 'magit-status "magit" nil t)
(global-set-key (kbd "C-x g") 'magit-status)
(autoload 'wget "wget" "wget interface for Emacs." t)
(require 'parenthesis)
(add-to-list  'parenthesis-func-alist '(parenthesis-insert-dollar "$" "$" nil))
(parenthesis-init)


(add-hook 'dired-load-hook (function
			    (lambda () (load "dired-x"))))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook '(lambda ()
			    (progn
			      (setq fill-column 95)
			      (auto-fill-mode 1)
			      (parenthesis-register-keys "[(<$" org-mode-map))))

;; org presentations
(defalias 'list-buffers 'ibuffer)
(use-package org-present
  :ensure t)
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-hide-cursor)
                 (org-present-read-only)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

;;resizing windows
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)
(global-set-key (kbd "C-x m")       'browse-url-at-point)

;;erc
(require 'erc)
(add-hook 'erc-text-matched-hook 'erc-beep-on-match)
(setq erc-modules                '(autojoin button fill irccontrols 
					    match netsplit noncommands 
					    pcomplete readonly ring 
					    scrolltobottom services 
					    smiley stamp track)
      erc-nick                   "titusg"
      erc-prompt-for-channel-key nil
      erc-send-whitespace-lines  nil
      erc-user-full-name         "Titus Groan"
      erc-warn-about-blank-lines t)

;;smex
(use-package smex
  :ensure t)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(require 'dash)

(use-package which-key
  :ensure t)
(which-key-mode)
(use-package flycheck
  :ensure t
  :defer 2
  :diminish
  :init (global-flycheck-mode)
  :custom
  (flycheck-display-errors-delay .3))
(use-package flycheck-haskell
  :ensure t)
(use-package lsp-mode
  :hook ((haskell-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(setq lsp-keymap-prefix "C-c C-l")
(use-package lsp-ui
  :ensure t)
(use-package lsp-haskell
  :ensure t)
(use-package haskell-mode
  :ensure t)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
(lsp-modeline-code-actions-mode)
(setq lsp-modeline-diagnostics-enable t
      lsp-modeline-code-actions-mode t
      lsp-headerline-breadcrumb-enable nil
      company-minimum-prefix-length 1
      company-idle-delay 0.0
      lsp-ui-sideline-show-code-actions nil
      lsp-ui-doc-show-with-cursor nil)
(define-key lsp-command-map (kbd "i") 'lsp-ui-imenu)
(define-key lsp-command-map (kbd "d") 'lsp-ui-doc-show)
(define-key lsp-command-map (kbd "q") 'lsp-ui-doc-hide)
(define-key lsp-command-map (kbd ".") 'completion-at-point)


(use-package company-ghci
  :ensure t)
(push 'company-ghci company-backends)
(add-hook 'haskell-mode-hook 'company-mode)
;;; To get completions in the REPL
(add-hook 'haskell-interactive-mode-hook 'company-mode)

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
(setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
(add-to-list 'exec-path my-cabal-path))
(eval-after-load 'haskell-mode
  '(progn
     (parenthesis-register-keys "{[(<'\"" haskell-mode-map)))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    (((output-pdf has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open"))) t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"))
 '(column-number-mode t)
 '(company-ghc-show-info t)
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(display-time-mode t)
 '(fci-rule-color "#073642")
 '(flycheck-display-errors-delay 0.3)
 '(global-font-lock-mode t nil (font-lock))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(mpc-browser-tags (quote (Artist Album Playlist)))
 '(mpc-frame-alist (quote ((name . "MPC") (tool-bar-lines) (font . "Sans"))))
 '(mpc-mpd-music-directory "/home/jim/music")
 '(package-selected-packages
   (quote
    (org-present mu4e-alert mu4e-marker-icons mu4e-conversation excorporate dash dash-functional which-keyg company-ghci company-mode flymake-hlint flymake-haskell-multi flycheck-haskell flycheck lsp-haskell lsp-ui lsp-mode auctex yasnippet vlf ghc all-the-icons doom-modeline bbdb company diminish use-package exec-path-from-shell bongo intero neotree haskell-mode which-key undo-tree smex rainbow-delimiters pandoc-mode markdown-mode magit hindent csv-mode company-ghc color-theme-sanityinc-solarized browse-kill-ring)))
 '(safe-local-variable-values (quote ((TeX-master . t) (TeX-master . main))))
 '(save-place-mode t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(weblogger-config-alist
   (quote
    (("burton-library" "http://jim.sdf-eu.org/motd/wordpress/xmlrpc.php" "jim" "" "1")))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX, AUCTeX and math-mode
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auctex
  :defer t
  :ensure t)
;(load "auctex-autoloads.el" nil t t)
;(load "preview.el" nil t t)

(setq reftex-bibpath-environment-variables '("/home/jb259/texmf/bib/bibtex/")
      reftex-default-bibliography '("/home/jb259/texmf/bib/bibtex/jimburton.bib")
      TeX-file-recurse                     t
      TeX-macro-private                    '("/home/jb259/texmf/")
      reftex-format-cite-function 
      '(lambda (key fmt)
	 (let ((cite (replace-regexp-in-string "%l" key fmt)))
	   (if (or (= ?~ (string-to-char fmt))
		   (member (preceding-char) '(?\ ?\t ?\n ?~)))
	       cite
	     (concat "~" cite))))
      TeX-auto-save                        t
      TeX-parse-self                       t
      reftex-plug-into-AUCTeX              t
      LaTeX-math-list                      '(("v" "vdash")
					     ("V" "vDash")
					     ("e" "eta"))
      TeX-outline-extra                    '(("[ \t]*\\\\\\(bib\\)?item\\b" 7)
					     ("\\\\bibliography\\b" 2)
					     ("\\\\frame\\b" 2))
      TeX-PDF-mode                         t
      TeX-output-view-style                '(("." "." "evince %o ")))
(setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
(setq TeX-view-program-selection '((output-pdf "Evince")))
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-start-server t)

;; So that RefTeX also recognizes \addbibresource. Note that you
;; can't use $HOME in path for \addbibresource but that "~"
;; works.
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

(setq reftex-external-file-finders
'(("tex" . "/usr/bin/kpsewhich -format=.tex %f")
  ("bib" . "/usr/bin/kpsewhich -format=.bib %f")))

(setq-default TeX-master nil)
;; minor modes for LaTex
(add-hook 'LaTeX-mode-hook '(lambda () 
			      (progn
				(turn-on-reftex)
				(setq TeX-PDF-mode t)
				(LaTeX-math-mode  1)
				(flyspell-mode    1)
				(auto-fill-mode   1)
				(setq fill-column 95)
				(parenthesis-register-keys "{([$" LaTeX-mode-map)
				(define-key LaTeX-mode-map (kbd "C-#") 'jb/wc-latex)
				(outline-minor-mode t)
				(setq latex-mode-map LaTeX-mode-map)
				(add-to-list 'TeX-command-list 
					     '("images" "utils.py svg2pdf"
					       TeX-run-command nil t))
				(add-to-list 'TeX-command-list 
					     '("make" "latexmk -pdf %s"
					       TeX-run-TeX nil t
					       :help "Run Latexmk on file")))))

(use-package browse-kill-ring
  :ensure t)
(browse-kill-ring-default-keybindings)

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

;(add-hook 'after-init-hook 'global-company-mode)
;(global-set-key (kbd "M-/") 'company-complete)
;(with-eval-after-load 'company
;  (add-to-list 'company-backends 'company-ghc))

(use-package solarized-theme
  :ensure t)
(load-theme 'solarized-light) 

(use-package yasnippet
  :ensure t);;
(yas-global-mode 1)

(use-package undo-tree
  :ensure t);;
(global-undo-tree-mode)

(use-package neotree
  :ensure t)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-theme 'arrow)

(require 'my-functions)
(global-set-key (kbd "C-c C-f")       'jb/fetchmail-wake-or-start)

;;;;;;;;;;;;;;;;;;;;;;
;; MAIL
;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent
      mu4e-drafts-folder "/[work].Drafts"
      mu4e-sent-folder   "/[work].Sent Mail"
      mu4e-trash-folder  "/[work].Trash"
      mu4e-sent-messages-behavior 'delete
      mu4e-get-mail-command "offlineimap -o"
      mu4e-update-interval 300)

(use-package mu4e-marker-icons
  :ensure t
  :init (mu4e-marker-icons-mode 1))
(use-package mu4e-alert
  :ensure t
  :init (mu4e-marker-icons-mode 1))
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   smtpmail-default-smtp-server "localhost"
   smtpmail-smtp-server "localhost"
   smtpmail-stream-type 'plain
   smtpmail-smtp-service 1025)

;(require 'my-site-specific)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal)))))
