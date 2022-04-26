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

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
	  #'TeX-revert-document-buffer)

(provide 'init-latex)
