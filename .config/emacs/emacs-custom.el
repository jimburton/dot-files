;;; package --- Emacs custom variables set using customize features.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-pdf has-no-display-manager) "dvi2tty")
     ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi")
     (output-pdf "Evince") (output-html "xdg-open")) t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(column-number-mode t)
 '(company-ghc-show-info t)
 '(custom-enabled-themes '(sanityinc-solarized-dark))
 '(custom-safe-themes
   '("2b0fcc7cc9be4c09ec5c75405260a85e41691abb1ee28d29fcd5521e4fca575b"
     "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
     "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347"
     "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358"
     "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4"
     "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328"
     default))
 '(display-time-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type 'cabal-repl)
 '(haskell-tags-on-save t)
 '(mpc-browser-tags '(Artist Album Playlist))
 '(mpc-frame-alist '((name . "MPC") (tool-bar-lines) (font . "Sans")))
 '(mpc-mpd-music-directory "/home/jim/music")
 '(package-selected-packages
   '(all-the-icons auctex browse-kill-ring company-ghci corfu diminish
		   doom-modeline elpy epresent exec-path-from-shell
		   flycheck-haskell kind-icon lsp-haskell lsp-ui magit
		   mu4e-alert mu4e-column-faces mu4e-marker-icons
		   neotree orderless org-present pdf-tools projectile
		   rainbow-delimiters rustic smex solarized-theme
		   tree-sitter typescript-mode undo-tree use-package
		   vlf which-key yafolding))
 '(safe-local-variable-values '((TeX-master . t) (TeX-master . main)))
 '(save-place-mode t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(weblogger-config-alist
   '(("burton-library" "http://jim.sdf-eu.org/motd/wordpress/xmlrpc.php"
      "jim" "" "1"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'emacs-custom)
;;; emacs-custom.el ends here.

