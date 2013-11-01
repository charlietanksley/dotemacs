;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;; Ubiquitous packages
(require 'use-package)
(use-package f)
(use-package dash)
(use-package s)

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

;; Show me the columns
(column-number-mode 1)

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


;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes and packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;; Ack
(use-package ack-and-a-half
  :config
  (progn
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

;;; Circe
(use-package circe)

;;; Hippie expand
(global-set-key
 (kbd "C-.")
 'hippie-expand)

;;; Ido
(use-package ido
  :init (ido-mode 1)
  :config
  (progn
    (setq
     ido-auto-merge-work-directories-length nil
     ido-case-fold t
     ido-everywhere t
     ido-enable-prefix nil
     ido-enable-flex-matching t
     ido-create-new-buffer 'always
     ido-max-prospects 10
     ido-file-extensions-order '(".rb" ".el" ".js" ".coffee"))
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

;;; Javascript
(use-package js3-mode
  :config
  (progn
    '(js3-auto-indent-p t)
    '(js3-enter-indents-newline t)
    '(js3-indent-on-enter-key t))
  :mode (("\\.js$" . js3-mode)))

;;; Magit
(use-package magit
  :bind ("C-x g" . magit-status)
  :config (progn
            (setq magit-remote-ref-format 'remote-slash-name)
            (add-hook 'magit-log-mode-hook 'turn-on-auto-fill)))

;;; Org mode
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;; Popwin
(use-package popwin
  :config (setq display-buffer-function 'popwin:display-buffer))

;;; Projectile
(use-package projectile
  :init (projectile-global-mode 1)
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'projectile-completion-fn)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

;;; Ruby mode
(use-package enh-ruby-mode
  :config
  (progn
      (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode)))
  :mode (("\\.rb$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))

;;; Smex
(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex))

(use-package switch-window
  :bind ("C-x i" . switch-window))

;;; YAML
(use-package yaml-mode
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
            (add-hook 'yaml-mode-hook
                      '(lambda ()
                         (define-key yaml-mode-map "\C-m" newline-and-indent)))))

;;; Web-mode
(use-package web-mode)

;;; Windmove
(use-package windmove
  :config (windmove-default-keybindings 'shift))

;;; Window numbering
(use-package window-numbering)
