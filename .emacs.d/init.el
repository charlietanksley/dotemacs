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

;;;;;;;;;;;;;;;;
;; Characters ;;
;;;;;;;;;;;;;;;;

;; Tabs and spaces
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)

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


