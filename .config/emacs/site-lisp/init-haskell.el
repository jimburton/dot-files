;;; package --- init-haskell.el
;;; Commentary:
;;; Set up haskell packages.
;;; Code:
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
      lsp-ui-sideline-show-code-actions t
      lsp-ui-doc-show-with-cursor t
      lsp-haskell-process-path-hie "haskell-language-server-wrapper")
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

(provide 'init-haskell)
;;; init-haskell.el ends here.
