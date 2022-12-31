;;; package -- init-packages.el.
;;; Commentary:
;;; set up the packages I'm using from ELPA/MELPA
;;; Code:
(use-package solarized-theme
  :ensure t)
(load-theme 'solarized-light)

(use-package magit
  :ensure t)

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

(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'haskell-mode-hook #'rainbow-delimiters-mode)

(use-package browse-kill-ring
  :ensure t)
(browse-kill-ring-default-keybindings)

(use-package smex
  :ensure t)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package dash
  :ensure t)

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

;; Org

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(add-hook 'org-mode-hook #'(lambda ()
			     (progn
			       (setq fill-column 80
				     org-src-fontify-natively t)
			       (auto-fill-mode 1)
			       (parenthesis-register-keys "[(<$" org-mode-map))))
;; org presentations
(defalias 'list-buffers 'ibuffer)
(use-package org-present
  :ensure t)
(use-package epresent
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

;;;;;;;;;;;;;;;;

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
(global-set-key (kbd "C-c g") 'magit-file-dispatch)
(autoload 'wget "wget" "wget interface for Emacs." t)
(require 'parenthesis)
(add-to-list  'parenthesis-func-alist '(parenthesis-insert-dollar "$" "$" nil))
(parenthesis-init)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)
	      ("C-c p" . projectile-command-map)))
(use-package
  vlf
  :ensure t)
(use-package doom-modeline
  :ensure t)
(doom-modeline-mode 1)

(use-package exec-path-from-shell
	     :ensure t)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'init-packages)
;;; init-packages.el ends here.
