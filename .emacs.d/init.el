;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'cask "~/.cask/cask.el")
(cask-initialize)

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

;;;;;;;;;;;
;; Files ;;
;;;;;;;;;;;

;; Backups aren't super-helpful
(setq make-backup-files nil)

;; Customizations should live out of the way
(setq custom-file "~/.emacs.d/init.d/emacs-custom.el")
(load custom-file 'noerror)

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
(add-hook 'prog-mode-hook 'whitespace-mode)

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;

; Set my command keys to meta keys.
(setq mac-command-modifier 'meta)
(setq mac-right-option-modifier 'super)

;;;;;;;;;;;
;; Modes ;;
;;;;;;;;;;;
(require 'use-package)

;;; Magit
(use-package magit
  :bind ("C-x g" . magit-status)
  :config (progn
            (setq magit-remote-ref-format 'remote-slash-name)
            (add-hook 'magit-log-mode-hook 'turn-on-auto-fill)))

;;; Org mode
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;; Ruby mode
(use-package enh-ruby-mode
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
            (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))))
