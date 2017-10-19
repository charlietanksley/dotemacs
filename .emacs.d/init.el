;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'cl)
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))

(package-initialize)
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

;;; Ubiquitous packages
(eval-when-compile
  (require 'use-package))

(use-package dash
  :ensure t)
(use-package f
  :ensure t)
(use-package git
  :ensure t)
(use-package s
  :ensure t)

;;;;;;;;;;;;;;;;;;
;; Basic config ;;
;;;;;;;;;;;;;;;;;;

;;; Sounds, colors, and shapes (layout)

;; A bell is annoying to everyone.
(setq visible-bell 1)

; Use as much screen real estate as possible for actual content.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode 1)

;; Show me the columns
(column-number-mode 1)

;; indicate empty lines at the end of a file
(defun turn-on-indicate-empty-lines ()
  (setq indicate-empty-lines t))
(add-hook 'prog-mode-hook 'turn-on-indicate-empty-lines nil t)

;; let me narrow regions!
(put 'narrow-to-region 'disabled nil)

;; newlines at end, please
(setq require-final-newline t)

;;;;;;;;;;;
;; Files ;;
;;;;;;;;;;;

;; Backups aren't super-helpful
(setq make-backup-files nil)

;; keep pace with files on disk
(global-auto-revert-mode t)

;; Customizations should live out of the way
(setq custom-file "~/.emacs.d/init.d/emacs-custom.el")
(load custom-file 'noerror)

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;; Not quite file specific, but I mostly use these interactive commands with
;; files, so it can go here. :)
(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;
;; Characters ;;
;;;;;;;;;;;;;;;;

;; Tabs and spaces
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)

(require 'whitespace)
(setq whitespace-style
      '(face trailing lines-tail tabs)
      whitespace-line-column 80)
(global-whitespace-mode 1)

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;

; Set my command keys to meta keys.
(setq mac-command-modifier 'meta)
(setq mac-right-option-modifier 'control)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modes and packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Ivy
(use-package swiper
  :ensure t)
(use-package counsel
  :ensure t)
(use-package ivy
  :ensure t
  :init (ivy-mode 1)
  :bind (("C-s" . swiper)
	     ("M-x" . counsel-M-x)
	     ("C-x C-f" . counsel-find-file)
	     ("C-c C-r" . ivy-resume))
  :config (progn
            (setq ivy-use-virtual-buffers t)
            (setq ivy-count-format "(%d/%d) ")))

;;;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config (progn
            (setq magit-remote-ref-format 'remote-slash-name)
            (add-hook 'magit-log-mode-hook 'turn-on-auto-fill)))

;;; Projectile
(use-package projectile
  :init (projectile-global-mode 1)
  :ensure t
  :config
  (progn
    ;; This next setting is a hack from:
    ;; https://github.com/bbatsov/projectile/issues/1183
    (setq projectile-mode-line
         '(:eval (format " Projectile[%s]"
                        (projectile-project-name))))
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'projectile-completion-fn)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

;;; Python
(use-package anaconda-mode
  :defer t
  :ensure t
  ;:mode (("\\.py$" . python-mode))
  :config (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

(use-package pyvenv
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;
;;; Misc functions ;;;
;;;;;;;;;;;;;;;;;;;;;;

;;Clean up buffers
(defun esk-untabify-buffer ()
 (interactive)
 (untabify (point-min) (point-max)))

(defun esk-indent-buffer ()
 (interactive)
 (indent-region (point-min) (point-max)))

(defun esk-cleanup-buffer ()
 "Perform a bunch of operations on the whitespace content of a buffer."
 (interactive)
 (esk-indent-buffer)
 (esk-untabify-buffer)
 (delete-trailing-whitespace))

;;Manage my emacs
(defun open-init-file ()
 (interactive)
 (find-file "~/dotfiles/dotemacs/.emacs.d/init.el"))
