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
;;;;;;;;;;;;;;;;
;;
;; Using abbrev-mode.
;; put point after the text you want to expand to. e.g
;;
;; \bigcup\limits_{}| where | is point
;; Type C-u <num of words before point to capture> C-x a g
;; M-x write-abbrevs-file


(require 'package)
(add-to-list 'package-archives 
         '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

(package-initialize)
(exec-path-from-shell-initialize)
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
      mu4e-maildir                 "~/Mail"   
      mu4e-sent-folder             "/archive"
      mu4e-drafts-folder           "/drafts"
      mu4e-trash-folder            "/trash"
      mu4e-refile-folder           "/archive")

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
(defun toggle-mode-line () "toggles the modeline on and off"
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
(add-hook 'diary-hook 'appt-make-list)
(diary 0)
(appt-activate 12)
(global-set-key (kbd "C-c o") 'occur)
(use-package diminish
	     :ensure t)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/emms/")
(require 'emms-setup)
(emms-standard)
(setq emms-playlist-default-major-mode 'emms-playlist-mode)
(when (fboundp 'emms-cache)
  (emms-cache 1))
(setq emms-player-list
      '(emms-player-mpg321
        emms-player-ogg123
        emms-player-mplayer))

(setq emms-playlist-buffer-name "*Music*")
(setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
(setq emms-source-file-default-directory "~/Music")

(require 'emms-browser)
(require 'emms-mode-line)
(emms-mode-line 1)
(require 'emms-playing-time)
(emms-playing-time 1)
(emms-browser-make-filter "all" 'ignore)
(require 'emms-info-libtag)
(setq emms-info-functions '(emms-info-libtag))

(require 'mu4e)
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
(require 'undo-tree)
(global-undo-tree-mode)

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

(defalias 'list-buffers 'ibuffer)

;;resizing windows
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)
(global-set-key (kbd "C-x m")       'browse-url-at-point)

(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(setq ac-auto-start 3)
(define-key ac-complete-mode-map "\M-/" 'ac-stop)

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
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(add-hook 'haskell-mode-hook #'hindent-mode)
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
(setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
(add-to-list 'exec-path my-cabal-path))
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'haskell-interactive-mode))
(eval-after-load 'haskell-mode
  '(progn
     (parenthesis-register-keys "[(<'\"" haskell-mode-map)))

;;save scripts as executable
(add-hook 'after-save-hook
          '(lambda ()
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
     (output-html "xdg-open"))))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#002b36"))
 '(company-ghc-show-info t)
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(fci-rule-color "#073642")
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
    (diminish use-package exec-path-from-shell bongo intero neotree haskell-mode which-key undo-tree smex rainbow-delimiters pandoc-mode markdown-mode magit hindent csv-mode company-ghc color-theme-sanityinc-solarized browse-kill-ring ac-haskell-process)))
 '(safe-local-variable-values (quote ((TeX-master . main))))
 '(save-place-mode t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(smtpmail-smtp-server "smtp.brighton.ac.uk")
 '(smtpmail-smtp-service 465)
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

(load "auctex-autoloads.el" nil t t)
(load "preview.el" nil t t)

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

(require 'browse-kill-ring)
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

(global-set-key (kbd "M-/") 'hippie-expand)

(load-theme 'sanityinc-solarized-dark) 

(require 'yasnippet);;
(yas-global-mode 1)

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-theme 'arrow)

(require 'my-functions)
;(require 'my-site-specific)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
