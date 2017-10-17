
(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(add-hook
 'after-init-hook
 (lambda ()
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))

(require 'cl-lib)
(require 'package)
(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(let* ((packages
        '(auto-compile         ; automatically compile Emacs Lisp libraries
          exec-path-from-shell ; sets exec-path and $PATH from shell (OS X)
          idle-require         ; load emacs-lisp libraries while Emacs is idle
          git-gutter-fringe    ; Fringe version of git-gutter.el
          org))                  ; Outline-based notes management and organizer
       ;; Remove all packages already installed
       (packages (cl-remove-if 'package-installed-p packages)))
  (when packages
    (ignore-errors (package-refresh-contents)
                   (mapc 'package-install packages)
                   ;; This package is only relevant for Mac OS X.
                   (when (memq window-system '(mac ns))
                     (package-install 'exec-path-from-shell)))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'idle-require)             ; Need in order to use idle-require

(dolist (feature
         '(auto-compile))           ; auto-compile .el files
  (idle-require feature))

(setq idle-require-idle-delay 5)
(idle-require-mode 1)

(setq ;;auto-revert-interval 1            ; Refresh buffers fast
      ;;custom-file (make-temp-file "")   ; Discard customization's
      ;;default-input-method "TeX"        ; Use TeX when toggling input method
      echo-keystrokes 0.1               ; Show keystrokes asap
      inhibit-startup-message t         ; No splash screen please
      initial-scratch-message nil       ; Clean scratch buffer
      ;;recentf-max-saved-items 100       ; Show more recent files
      ring-bell-function 'ignore        ; Quiet
      sentence-end-double-space nil)    ; No double space
;; Some mac-bindings interfere with Emacs bindings.
;; (when (boundp 'mac-pass-command-to-system)
;;   (setq mac-pass-command-to-system nil))

(setq-default ;fill-column 80                    ; Maximum line width
              truncate-lines t                  ; Don't fold lines
              indent-tabs-mode nil              ; Use spaces instead of tabs
              split-width-threshold 160)         ; Split verticly by default

(setq initial-frame-alist
      '(
        (width . 125) ; character
        (height . 60) ; lines
        ))

(setq default-frame-alist
      '(
        (width . 122) ; character
        (height . 58) ; lines
        ))

(fset 'yes-or-no-p 'y-or-n-p)

(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

(set-language-environment "UTF-8")

(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text
           scroll-bar-mode              ; No scroll bars either
           blink-cursor-mode))          ; The blinking cursor gets old
  (funcall mode 0))

(dolist (mode
         '(column-number-mode           ; Show column number in mode line
           delete-selection-mode        ; Replace selected text
           show-paren-mode))            ; Highlight matching parentheses
  (funcall mode 1))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
logical line.  This is useful, e.g., for use with
`visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; Handy key definition
(define-key global-map "\C-\M-Q" 'unfill-region)

(require 'git-gutter-fringe)

(dolist (p '((git-gutter:added    . "#0c0")
             (git-gutter:deleted  . "#c00")
             (git-gutter:modified . "#c0c")))
  (set-face-foreground (car p) (cdr p))
  (set-face-background (car p) (cdr p)))

(setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                       ("delta" . ?Δ)
                                       ("gamma" . ?Γ)
                                       ("phi" . ?φ)
                                       ("psi" . ?ψ)))

(use-package wc-mode
  :ensure t)

(use-package org
  :config
  (setq org-startup-indented t))

(define-key org-mode-map "\M-q" 'toggle-truncate-lines)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-deadline-warning-days 0)

(use-package org-ref
  :ensure t)

(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
      org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

(setq bibtex-completion-bibliography "~/Dropbox/bibliography/references.bib"
      bibtex-completion-library-path "~/Dropbox/bibliography/bibtex-pdfs"
      bibtex-completion-notes-path "~/Dropbox/bibliography/helm-bibtex-notes")

;; open pdf with system pdf viewer (works on mac)
;;(setq bibtex-completion-pdf-open-function
;;  (lambda (fpath)
;;    (start-process "open" "*open*" "open" fpath)))

;; alternative
(setq bibtex-completion-pdf-open-function 'org-open-file)
