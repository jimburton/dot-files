;;; package --- Emacs custom variables set using customize features.
;;; Commentary:
;;; Code:

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
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(column-number-mode t)
 '(company-ghc-show-info t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(display-time-mode t)
 '(fci-rule-color "#eee8d5")
 '(flycheck-display-errors-delay 0.3)
 '(global-font-lock-mode t nil (font-lock))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (quote
    ("#ffffff9fcfae" "#dba8fffffff9" "#ffffcffdbf71" "#dcdcc53effff" "#ffffff9fcfae" "#ffffcffdbf71" "#dba8fffffff9")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#b3c34d" . 20)
     ("#6ccec0" . 30)
     ("#74adf5" . 50)
     ("#e1af4b" . 60)
     ("#fb7640" . 70)
     ("#ff699e" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#e1af4b" "#fb7640" "#ff6849" "#ff699e" "#8d85e7" "#74adf5" "#6ccec0" "#b3c34d")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(lsp-ui-doc-border "#586e75")
 '(mpc-browser-tags (quote (Artist Album Playlist)))
 '(mpc-frame-alist (quote ((name . "MPC") (tool-bar-lines) (font . "Sans"))))
 '(mpc-mpd-music-directory "/home/jim/music")
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (flx-ido projectile latex-preview-pane pdf-tools chess solarized-theme mu4e-column-faces mu4e-overview mu4e-alert mu4e-marker-icons aria2 epresent org-present excorporate dash dash-functional which-keyg company-ghci company-mode flymake-hlint flymake-haskell-multi flycheck-haskell flycheck lsp-haskell lsp-ui lsp-mode auctex yasnippet vlf ghc all-the-icons doom-modeline bbdb company diminish use-package exec-path-from-shell bongo intero neotree haskell-mode which-key undo-tree smex rainbow-delimiters pandoc-mode markdown-mode magit hindent csv-mode company-ghc color-theme-sanityinc-solarized browse-kill-ring)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(safe-local-variable-values (quote ((TeX-master . t) (TeX-master . main))))
 '(save-place-mode t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#ca7966842090")
     (60 . "#c05678c91534")
     (80 . "#b58900")
     (100 . "#a6088eed0000")
     (120 . "#9e3a91a70000")
     (140 . "#9629943b0000")
     (160 . "#8dc696ae0000")
     (180 . "#859900")
     (200 . "#76f09b6145e9")
     (220 . "#6cd69caa5b9d")
     (240 . "#5f5f9e06701f")
     (260 . "#4c1b9f788425")
     (280 . "#2aa198")
     (300 . "#3003984faf4d")
     (320 . "#2f7093e9bae0")
     (340 . "#2c5a8f79c670")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weblogger-config-alist
   (quote
    (("burton-library" "http://jim.sdf-eu.org/motd/wordpress/xmlrpc.php" "jim" "" "1"))))
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal)))))

(provide 'emacs-custom)
;;; emacs-custom.el ends here.

