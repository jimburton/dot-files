;;; package --- Little helper functions.
;;;
;;; Commentary:
;;;
;;; Allow undo of scrolling
;;;
;;; Code taken from "Writing GNU Emacs Extensions" by Bob Glickstein,
;;; (O'Reilly & Assoc., 1997)

;;; Code:

(defvar unscroll-point (make-marker)
  "Cursor position for next call to \\[unscroll].")
(defvar unscroll-window-start (make-marker)
  "Window start for next call to \\[unscroll].")
(defvar unscroll-hscroll nil
  "Horizontal scroll for next call to \\[unscroll].")

(defun unscroll ()
  "Revert to last position before the start of scrolling."
  (interactive)
  (goto-char unscroll-point)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))


(put 'scroll-up 'unscrollable t)
(put 'scroll-down 'unscrollable t)
(put 'scroll-left 'unscrollable t)
(put 'scroll-right 'unscrollable t)


(defun unscroll-maybe-remember ()
  "Maybe remember where we started scrolling."
  (if (not (get last-command 'unscrollable))
      (progn
	(set-marker unscroll-point (point))
	(set-marker unscroll-window-start (window-start))
	(setq unscroll-hscroll (window-hscroll)))))


(defadvice scroll-up (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-down (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-left (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(defadvice scroll-right (before remember-for-unscroll activate compile)
  "Remember where we started from, for `unscroll'."
  (unscroll-maybe-remember))

(global-set-key (kbd "C-<") 'unscroll)

;; from http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
(require 'term)
(defun visit-ansi-term ()
  "If the current buffer is:
1) a running `ansi-term' named *ansi-term*, rename it.
2) a stopped `ansi-term', kill it and create a new one.
3) a non `ansi-term', go to an already running `ansi-term'
or start a new one while killing a defunct one."
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))

(global-set-key (kbd "<f2>") 'visit-ansi-term)

;; open files as root
;; from http://www.emacswiki.org/cgi-bin/wiki/TrampMode
(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "Open a file as the root user.
Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
         ;; use a separate history list for "root" files.
         (file-name-history find-file-root-history)
         (name (or buffer-file-name default-directory))
         (tramp (and (tramp-tramp-file-p name)
                     (tramp-dissect-file-name name)))
         path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-path tramp)
            dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(global-set-key [(control x) (control r)] 'find-file-root)

;;wc
(defun jb/wc-latex ()
  "Count words in a buffer disregarding LaTeX macro names and environments etc.
Counts the words in the region if the mark is
active, or in the whole buffer if not.  Requires external programs
wc and untex."
  (interactive)
  (jb/shell-command-on-region-or-buffer "untex -a -e-o - | wc -w"))

(defun jb/wc ()
  "Count words in a buffer. Counts the words in the region if the
mark is active, or in the whole buffer if not.  Requires external
program wc."
  (interactive)
  (jb/shell-command-on-region-or-buffer "wc -w"))
  
(defun jb/shell-command-on-region-or-buffer (cmd)
  "Run the command CMD on region if mark is active or whole buffer if not."
  (interactive)
  (let* ((begin (if mark-active (region-beginning) (point-min)))
	(end (if mark-active (region-end) (point-max))))
    (shell-command-on-region begin end cmd)))

(global-set-key (kbd "C-#") 'jb/wc)

(defun jb/define-keys (kmap pairs)
  "Define a series of keybindings in KMAP.  PAIRS is an list of (KEYBINDING .  COMMAND) pairs."
  (mapc #'(lambda (pair)
	    (define-key kmap (car pair) (cdr pair)))
	pairs))

(defun jb/org-to-beamer-evince ()
  "Run pandoc on an org file to produce beamer slides.
Requires pandoc and LaTeX distribution."
  (interactive)
  (let* ((input (buffer-file-name))
	 (base (file-name-base (buffer-file-name)))
	 (dir (file-name-directory (buffer-file-name)))
	 (output (format "%s%s.pdf" dir base))
	 (pandoc-cmd (format "pandoc -t beamer %s -o %s" input output))
	 (view-cmd (format "evince %s &" output)))
    (progn
      (message (format "Generating beamer slides: %s" pandoc-cmd))
      (shell-command pandoc-cmd)
      (shell-command view-cmd))))

(defun jb/org-to-beamer ()
  "Run pandoc on an org file to produce beamer slides.
Requires pandoc and LaTeX distribution."
  (interactive)
  (let* ((input (buffer-file-name))
	 (base (file-name-base (buffer-file-name)))
	 (dir (file-name-directory (buffer-file-name)))
	 (output (format "%s%s.pdf" dir base))
	 (pandoc-cmd (format "pandoc --pdf-engine=xelatex -V theme:Frankfurt -V mainfont=\"DejaVu Sans\" -V sansfont=\"DejaVu Sans Mono\" -t beamer %s -o %s" input output)))
    (progn
      (message (format "Generating beamer slides: %s" pandoc-cmd))
      (shell-command pandoc-cmd))))

(provide 'my-functions)
;;; my-functions.el ends here
