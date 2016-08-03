
;;; Commentary:

;;; Code:
;; By default load up what was last here
(desktop-read)

(desktop-save-mode 1)

(let ((default-directory "~/.emacs.d/"))
      (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")

;(Setq comint-maximum-buffer-size 50000)
;(setq default-tab-width 4)

;;magit
(require 'magit)
(global-set-key (kbd "C-c C-m") 'magit-status)

;;(require 'magit-gh-pulls)
;;(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;;unique buffer names
(require 'uniquify) 
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

(require 'ido)
(ido-mode t)

;; Get some column numbers up
(setq column-number-mode t)

;; This puts the name of the buffer in the title bar
(setq frame-title-format "%b")

;; Show paren matching
(require 'paren)
(show-paren-mode 1)

;; Syntax highlighting!
(global-font-lock-mode 1)
(setq font-lock-maximum-decoration t)

;; Get rid off annoying backup files
(add-hook 'dired-load-hook
	  (function (lambda ()
		      (load "dired-x"))))

;(setq initial-dired-omit-files-p t)
(setq make-backup-files nil)

;; CPerl-Mode!
(defalias 'perl-mode 'cperl-mode)

;; Change tabs to spaces
(setq indent-tabs-mode nil)

;; (add-hook 'verilog-mode-hook '(lambda ()
;;     (add-hook 'local-write-file-hooks (lambda()
;;        (untabify (point-min) (point-max))))))

;; (remove-hook 'verilog-mode-hook '(lambda ()
;;     (add-hook 'local-write-file-hooks (lambda()
;;        (untabify (point-min) (point-max))))))

;; Set tab-spacing to 4
;(setq c-basic-offset 4)
(setq tab-width 4)

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
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
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
(setq load-path (cons "/usr/local/go/misc/emacs" load-path))
(require 'go-mode)
(add-hook 'before-save-hook #'gofmt-before-save)

;; Git Gutter
(global-git-gutter-mode +1)
(custom-set-variables
 '(git-gutter:update-interval 2))

;; Dash docs
(defun go-doc ()
  (interactive)
  (setq-local helm-dash-docsets '("Go")))

(add-hook 'go-mode-hook 'go-doc)

(provide 'emacs)
;;; .emacs ends here



