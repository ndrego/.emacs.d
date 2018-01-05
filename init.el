
;;; Package --- init.el

;;; Commentary:

;;; Code:
;; By default load up what was last here
(require 'desktop)
(desktop-read)

(setq desktop-buffers-not-to-save "^$")
(setq desktop-files-not-to-save "^$")
(desktop-save-mode 1)

(let ((default-directory "~/.emacs.d/"))
      (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

;; Tramp nastiness
(setq tramp-ssh-controlmaster-options "-o ControlPath=%%C -o ControlMaster=auto -o ControlPersist=no")

(require 'use-package)
(use-package package
  :config
  (mapc (lambda(p) (push p package-archives))
        '(("marmalade" . "http://marmalade-repo.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)
(package-refresh-contents)
(package-initialize)

(setq use-package-always-ensure t)

;; (add-to-list 'load-path "/usr/local/Cellar/doxymacs/1.8.0/share/emacs/site-lisp/")
;; (add-hook 'c-mode-common-hook
;;   (lambda ()
;;     (require 'doxymacs)
;;     (doxymacs-mode t)
;;     (doxymacs-font-lock)))
;; (setq doxymacs-doxygen-style "Qt")

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (modern-c++-font-lock-global-mode t))

(use-package flymake-python-pyflakes
  :config
  (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flymake-python-pyflakes-executable "flake8"))

;(Setq comint-maximum-buffer-size 50000)
(setq default-tab-width 4)

;;magit
(use-package magit
  :pin melpa
  :commands magit-status 
  :bind ("C-c C-m" . magit-status))

;;(require 'magit-gh-pulls)
;;(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;;unique buffer names
(use-package uniquify
  :ensure nil
  :config
  (setq 
   uniquify-buffer-name-style 'post-forward
   uniquify-separator ":"))

(use-package ido
  :config
  (ido-mode t))

;; Get some column numbers up
(setq column-number-mode t)

;; This puts the name of the buffer in the title bar
(setq frame-title-format "%b")

;; Show paren matching
(use-package paren
  :config
  (show-paren-mode 1))

;; Syntax highlighting!
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; Get rid off annoying backup files
(add-hook 'dired-load-hook
	  (function (lambda ()
		      (load "dired-x"))))

;(setq initial-dired-omit-files-p t)
(setq make-backup-files nil)

;; Change tabs to spaces
(setq-default indent-tabs-mode nil)
(setq c-default-style "gnu"
      c-basic-offset 2)
(c-set-offset 'case-label '+)

;; (add-hook 'verilog-mode-hook '(lambda ()
;;     (add-hook 'local-write-file-hooks (lambda()
;;        (untabify (point-min) (point-max))))))

;; (remove-hook 'verilog-mode-hook '(lambda ()
;;     (add-hook 'local-write-file-hooks (lambda()
;;        (untabify (point-min) (point-max))))))

(add-hook 'python-mode-hook
           (lambda ()
	     (setq indent-tabs-mode nil)
;	     (setq python-indent 4)
             (setq outline-regexp "def\\|class ")))

;; Change "HOME" and "END" keys to beg and end of buffer
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end]  'end-of-buffer)

;; Set Ctrl+G to "goto-line"
(global-set-key "\C-g" 'goto-line)

;; Set Ctrl+T to tab a whole region
(global-set-key "\C-t" 'indent-region)

;; Allow for commenting of whole regions
(define-key global-map "\C-c\C-c" 'comment-region)
(define-key global-map "\C-c\C-u" 'uncomment-region)

(set-cursor-color "white")

(setq tramp-default-method "ssh")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(git-gutter:update-interval 2)
 '(global-subword-mode t)
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(package-selected-packages
   (quote
    (flymd markdown-preview-mode doctags whitespace-cleanup-mode w3m use-package tabbar slack origami modern-cpp-font-lock magit-gh-pulls helm-swoop helm-dash go-guru github-issues github-clone git-gutter flymake-python-pyflakes flycheck)))
 '(safe-local-variable-values
   (quote
    ((flycheck-clang-language-standard . "c++11")
     (eval progn
           (c-set-offset
            (quote case-label)
            (quote +))))))
 '(show-paren-mode t))

(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path)
  (push "/usr/local/bin" exec-path))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#141312" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "unknown" :family "Monaco")))))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command 
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

(defadvice find-tag (around refresh-etags activate)
   "Rerun etags and reload tags if tag not found and redo find-tag.              
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
    ad-do-it
      (error (and (buffer-modified-p)
          (not (ding))
          (y-or-n-p "Buffer is modified, save it? ")
          (save-buffer))
         (er-refresh-etags extension)
         ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently          
    (visit-tags-table default-directory nil)))


(global-set-key [f5]  'revert-buffer)


(defun revert-all-buffers ()
          "Refreshes all open buffers from their respective files"
          (interactive)
          (let* ((list (buffer-list))
                 (buffer (car list)))
            (while buffer
              (when (and (buffer-file-name buffer) 
                         (not (buffer-modified-p buffer)))
                (set-buffer buffer)
                (revert-buffer t t t))
              (setq list (cdr list))
              (setq buffer (car list))))
          (message "Refreshed open files"))


(global-set-key [f6]  'revert-all-buffers)

;(setq c-default-style "linux"
;         c-basic-offset 2)

;; go mode
;(setq load-path (cons "/usr/local/go/misc/emacs" load-path))
(use-package go-mode
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

;; Git Gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  (custom-set-variables
   '(git-gutter:update-interval 2)))

;; Dash docs
(defun go-doc ()
  (interactive)
  (setq-local helm-dash-docsets '("Go")))

(add-hook 'go-mode-hook 'go-doc)

(load "/usr/local/Cellar/clang-format/2017-11-14/share/clang/clang-format.el")
(global-set-key [C-M-tab] 'clang-format-region)

(provide 'emacs)
;;; .emacs ends here



