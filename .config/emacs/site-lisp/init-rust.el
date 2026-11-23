;;; package --- init-rust.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Set up rust packages.
;;; Code:

;; Enable Eglot (built-in LSP client)
(use-package eglot
  :ensure t
  :hook ((rustic-mode . eglot-ensure)
         ;; Automatically turn on inline hints when Eglot connects
         (eglot-managed-mode . eglot-inlay-hints-mode))
  :config
  ;; Define rust-analyzer as the LSP server for Rust
  (add-to-list 'eglot-server-programs '((rust-mode rustic-mode) . ("rust-analyzer")))
  (keymap-set eglot-mode-map "C-c ." #'eglot-code-actions))

(setq-default eglot-workspace-configuration
              '(:rust-analyzer
                (:inlayHints
                 (:typeHints (:enable t)
                  :parameterHints (:enable t)
                  :chainingHints (:enable t)
                  :closingBraceHints (:enable t)))))
(use-package eldoc
  :init
  ;; Show documentation floating/in minibuffer instantly or on demand
  (setq eldoc-echo-area-use-multiline-p 3  ; Allow up to 3 lines in minibuffer
        eldoc-idle-delay 0.2))             ; Delay before docs show (in seconds)

;; Enable Rustic mode for advanced Cargo/Rust integration
(use-package rustic
  :ensure t
  :custom
  ;; Let rustic handle format-on-save natively using rustfmt/eglot
  (rustic-format-on-save t)
  (rustic-format-trigger 'on-save)
  (rustic-lsp-client 'eglot)
  :hook (rustic-mode . electric-pair-local-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  :init
  (global-corfu-mode))

;; Add clean completion behavior settings to Emacs natively
(use-package emacs
  :init
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold nil))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'init-rust)
;;; init-rust.el ends here.
