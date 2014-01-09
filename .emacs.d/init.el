;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;; Install repos from git
(setq git-install-pacakge-dir "~/.emacs.d/packages/")

(defun git-install-packages (lst)
  (mapcar 'git-install lst)
  (mapcar 'git-install-add-to-load-path lst))

(defun git-install-dir (item)
  (let ((package-name (f-base (f-filename item))))
    (f-join git-install-pacakge-dir (f-base (f-filename item)))))

(defun git-install (item)
  (let ((install-dir (git-install-dir item)))
    (unless (f-exists? install-dir)
      (git-clone item install-dir))))

(defun git-install-add-to-load-path (item)
  (let ((dir (git-install-dir item)))
    (add-to-list 'load-path dir)))

(setq git-package-list '("https://github.com/plexus/chruby.el.git"))

(git-install-packages git-package-list)

;;; Ubiquitous packages
(require 'use-package)
(use-package dash)
(use-package f)
(use-package git)
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

;; indicate empty lines at the end of a file
(defun turn-on-indicate-empty-lines ()
  (setq indicate-empty-lines t))
(add-hook 'prog-mode-hook 'turn-on-indicate-empty-lines nil t)


;; let me narrow regions!
(put 'narrow-to-region 'disabled nil)

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
(setq mac-right-option-modifier 'super)

; Shells at my fingertips
(global-set-key (kbd "C-x m") 'eshell)

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

;;; Auctex
(use-package auctex
  :config
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)
    (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (setq TeX-PDF-mode t)))

;;; Chruby
(use-package chruby
  :config
  (progn
    (chruby "ruby-2.0.0-p247")))

;;; Circe
(use-package circe
  :config
  (progn
    (if (file-exists-p "~/.private.el")
        (load-file "~/.private.el")
      (setq circe-network-options
            `(("Freenode"
               :nick "charlietanksley"
               :channels ("#bnr"
                          "#bnrpython"
                          "#emacs")
               :nickserv-password ,freenode-password))))
    (setq circe-reduce-lurker-spam t)
    (add-hook 'circe-chat-mode-hook 'my-circe-prompt)
    (defun my-circe-prompt ()
      (lui-set-prompt
       (concat (propertize (concat (buffer-name) ">")
                           'face 'circe-prompt-face)
               " ")))))

;;; CoffeeScript
(use-package coffee-mode
  :config
  (progn
    (setq coffee-tab-width 2)
    (setq whitespace-action '(auto-cleanup))
    (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))))

;;; Current story
(use-package current-story)

;;; Flycheck
(use-package flycheck)

;;; Erlang
(use-package erlang)

;;; Haskell
(use-package haskell-mode
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)))

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

(use-package js-comint
  :config
  (progn
    (setq inferior-js-program-command "node --interactive")
    ;; http://stackoverflow.com/questions/13862471/using-node-js-with-js-comint-in-emacs
    (setq inferior-js-mode-hook
          (lambda ()
            ;; We like nice colors
            (ansi-color-for-comint-mode-on)
            ;; Deal with some prompt nonsense
            (add-to-list
             'comint-preoutput-filter-functions
             (lambda (output)
               (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))))))

;;; Magit
(use-package magit
  :bind ("C-x g" . magit-status)
  :config (progn
            (setq magit-remote-ref-format 'remote-slash-name)
            (add-hook 'magit-log-mode-hook 'turn-on-auto-fill)))

;;; Org mode
(use-package org
  :config
  (progn
    (add-hook 'org-mode-hook 'turn-on-auto-fill)
    (define-key global-map "\C-cc" 'org-capture)
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "~/Dropbox/org/tasks.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
            ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
             "* %?\nEntered on %U\n  %i\n  %a")))))

;;; Markdown
(use-package markdown-mode
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)
         ("\\.text$" . markdown-mode)))

;;; Pandoc
(use-package pandoc-mode
  :config
  (progn
    (add-hook 'markdown-mode-hook 'turn-on-pandoc)))

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

;;; Rainbow delimiters
(use-package rainbow-delimiters
  :config
  (progn
    (global-rainbow-delimiters-mode)))

;;; Rainbow mode (css)
(use-package rainbow-mode)

;;; Ruby mode
(use-package ruby-mode
  :config
  (progn
    (setq ruby-deep-indent-paren nil)
    (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode)))
  :mode (("\\.rb$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Guardfile$" . ruby-mode)))

;;; Scala-mode2
(use-package scala-mode2)

;;; Scss
(use-package scss-mode
  :config
  (progn
    (setq css-indent-offset 2)
    (setq scss-compile-at-save nil))
  :mode (("\\.scss$" . scss-mode)))

;;; Slim
(use-package slim-mode)

;;; Smartparens
(use-package smartparens
  :config
  (progn
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)
    (define-key sp-keymap (kbd "M-s") 'sp-splice-sexp)
    (define-key sp-keymap (kbd "C-M-<right>") 'sp-forward-slurp-sexp)
    (define-key sp-keymap (kbd "C-M-<left>") 'sp-forward-barf-sexp)
    (sp-pair "'" nil :actions :rem)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands and functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Open with marked
(defun my-open-with-marked (filename)
  (interactive)
  (start-process "Marked" nil "/Applications/Marked.app/Contents/MacOS/Marked" filename))

(defun my-open-current-file-with-marked ()
  (interactive)
  (my-open-with-marked (buffer-file-name)))

;;; Clean up buffers
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

;;; Manage my emacs
(defun open-init-file ()
  (interactive)
  (find-file "~/dotfiles/dotemacs/.emacs.d/init.el"))

(defun open-cask-file ()
  (interactive)
  (find-file "~/dotfiles/dotemacs/.emacs.d/Cask"))
