(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Directory beneath which additional per-user Emacs-specific
  files are placed.
  Various programs in Emacs store information in this directory.
  Note that this should end with a directory separator.
  See also `locate-user-emacs-file'."))

;; Homebrew
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Custom programs
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/:~/bin"))
(setq exec-path (append exec-path '("~/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Whitespace handling
(setq-default sentence-end-double-space nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; Ido
(require 'ido)
(ido-mode t)
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)
(ido-mode 1)

;; Display column numbers
(column-number-mode)

; Bind meta
(global-set-key (kbd "<ESC> <ESC>") 'dabbrev-expand)

; Backup / autosave to ~/.emacs.d/ to prevent polluting project directories
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

; Install many a packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(package-install 'color-theme-solarized)
(package-install 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
  (global-fci-mode 1)
(package-install 'markdown-mode)
(package-install 'yaml-mode)
(package-install 'rust-mode)
(package-install 'coffee-mode)
(package-install 'diff-hl)
(package-install 'python-mode)
(package-install 'js2-mode)
(package-install 'json-mode)
(package-install 'git-commit)
(package-install 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (python . t)
   (sh     . t)
   ))
(setf org-babel-default-header-args:org '((:exports . "both")))
(setq org-directory "~/org")
(setq org-babel-python-command "/usr/local/bin/python3")

;; osx copy and paste and cut
(defun pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

(global-set-key (kbd "C-c c") 'pbcopy)
(global-set-key (kbd "C-c v") 'pbpaste)
(global-set-key (kbd "C-c x") 'pbcut)

;; gist a region
(defun gist ()
  (interactive)
  (call-process-region (point) (mark) "gist")
  (setq deactivate-mark t))

(global-set-key (kbd "C-c g") 'gist)


;; js mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; indent with spaces
(setq js2-mode-hook
      '(lambda () (progn
                    (set-variable 'indent-tabs-mode nil))))

;; end / home
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "<home>") 'beginning-of-line)
