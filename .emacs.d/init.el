;; * Bootstrapping
;;
;; Commands:
;;   * org-global-cycle - to close everything
;;   * f/b - like p/n but moves at same level
;;
;;
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; https://jamiecollinson.com/blog/my-emacs-config/#bootstrap-use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; * Semi-literate config
;;
;; I like the idea of a literate config because mine is always such a
;; nightmare, but I don't want to deal with really learning orgmode
;; right now. By using outshine I think I'll maybe get the best of
;; both worlds?
(use-package outshine
  :ensure t
  :init (progn
          (defvar outline-minor-mode-prefix "\M-#")
          (setq outshine-use-speed-commands t))
  :config (progn
            (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
            (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)))

;; In org and outshine modes, `navi` makes `j` open up a nav panel
;; when at the start of a line.
(use-package navi-mode
  :ensure t)

;; * Basic config
;; ** Sounds, colors, and shapes (layout)

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

;; newlines at end, please
(setq require-final-newline t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq global-linum-mode 1)

;; ** Files

;; I hate the way my backups work. Let's try someone else's in the
;; hopes that it isn't so crappy?
;; https://jamiecollinson.com/blog/my-emacs-config/#preferences
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old
    )

;; keep pace with files on disk
(global-auto-revert-mode t)

;; Customizations should live out of the way
(setq custom-file "~/.emacs.d/init.d/emacs-custom.el")
(load custom-file 'noerror)

;; Makes keeping track of all those models.py a little cleaner
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;; ** Characters

;; Tabs and spaces
(setq-default indent-tabs-mode nil)
(setq standard-indent 2)

(require 'whitespace)
(setq whitespace-style
      '(face trailing lines-tail tabs)
      whitespace-line-column 120)
(global-whitespace-mode 1)

;; ** Keys

; Set my command keys to meta keys.
(setq mac-command-modifier 'meta)
(setq mac-right-option-modifier 'super)


;; * Advanced settings
;; let me narrow regions!
(put 'narrow-to-region 'disabled nil)

;; * General packages
;; These are the packages that I'll use no matter the major mode.
;; **  Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config (progn
            (setq magit-remote-ref-format 'remote-slash-name)
            (add-hook 'magit-log-mode-hook 'turn-on-auto-fill)))

;; ** Smex

;; This is only barely configured because it'll actually be used by
;; Council below
(use-package smex
  :ensure t)

;; ** Ivy
;; Steals a few settings from https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (progn
    (ivy-mode t)
    (setq ivy-initial-inputs-alist nil
          ivy-use-virtual-buffers t
          ivy-count-format "%d/%d ")))

;; ** Counsel
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)))

;; ** Swiper
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))


;; ** Projectile
(use-package projectile
  :ensure t
  :init(progn
         (projectile-mode +1))
  :bind (("C-c p" . projectile-command-map))
  :config (progn
            (setq projectile-enable-caching t
                  projectile-require-project-root nil
                  projectile-completion-system 'ivy)
            (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

;; This is broken right now so just ignore it.
;; (use-package counsel-projectile
;;   :ensure t
;;   :config
;;   (progn
;;     (add-hook 'after-init-hook 'counsel-projectile-mode)))

;; ** Ag
(use-package ag
  :ensure t)

;; ** Exec path from shell
(use-package exec-path-from-shell
  :ensure t
  :init (progn
          (exec-path-from-shell-initialize)))
;; ** Editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
;; * Language specific packages
;; ** Lua
(use-package lua-mode
  :ensure t
  :config (progn
            (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))
  :mode (("\\.lua$" . lua-mode)))


;; ** Python

;; Hrm. So it is unclear to me what exactly this brings to the table. :/
(use-package anaconda-mode
  :ensure t
  :config (progn
            (add-hook 'python-mode-hook 'anaconda-mode)
            (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

(use-package py-autopep8
  :ensure t)

;; The bit about installing jedi correctly from here worked for me, I
;; think. Note that I *did not* do the complicated copying files over
;; part:
;; https://github.com/tkf/emacs-jedi/issues/293#issuecomment-361843980
(use-package jedi
  :ensure t
  :config (progn
            (add-hook 'python-mode-hook 'jedi:setup)
            (setq jedi:complete-on-dot t)))

;; I tried using https://github.com/tkf/emacs-jedi-direx becuase I
;; really like the idea but it is broken. :/

;; ** Rust
(use-package rust-mode
  :ensure t
  :config (progn
            (setq rust-format-on-save t))
  :mode (("\\.rs$" . rust-mode)))

;; ** Haskell
(use-package haskell-mode
  :ensure t
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))
  :mode (("\\.hs$" . haskell-mode)))

;; ** YAML
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

;; * Old configs
;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

;; (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
;; (cask-initialize)

;; ;;; Install repos from git
;; (setq git-install-pacakge-dir "~/.emacs.d/packages/")

;; (defun git-install-packages (lst)
;;   (mapcar 'git-install lst)
;;   (mapcar 'git-install-add-to-load-path lst))

;; (defun git-install-dir (item)
;;   (let ((package-name (f-base (f-filename item))))
;;     (f-join git-install-pacakge-dir (f-base (f-filename item)))))

;; (defun git-install (item)
;;   (let ((install-dir (git-install-dir item)))
;;     (unless (f-exists? install-dir)
;;       (git-clone item install-dir))))

;; (defun git-install-add-to-load-path (item)
;;   (let ((dir (git-install-dir item)))
;;     (add-to-list 'load-path dir)))

;; (setq git-package-list '("https://github.com/plexus/chruby.el.git"))

;; (git-install-packages git-package-list)

;; ;;; Ubiquitous packages
;; (use-package dash)
;; (use-package f)
;; (use-package git)
;; (use-package s)

;; ; Shells at my fingertips
;; (global-set-key (kbd "C-x m") 'eshell)

;; ;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Modes and packages ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;; Ack
;; (use-package ack-and-a-half
;;   :config
;;   (progn
;;     (defalias 'ack 'ack-and-a-half)
;;     (defalias 'ack-same 'ack-and-a-half-same)
;;     (defalias 'ack-find-file 'ack-and-a-half-find-file)
;;     (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

;; ;;; Auctex
;; ;;(use-package auctex
;; ;;  :config
;; ;;  (progn
;; ;;    (setq TeX-auto-save t)
;; ;;    (setq TeX-parse-self t)
;; ;;    (setq-default TeX-master nil)
;; ;;    (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
;; ;;    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; ;;    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; ;;    (setq TeX-PDF-mode t)))

;; ;;; C
;; (setq c-default-style "linux"
;;           c-basic-offset 8)

;; ;;; Cider
;; (use-package cider
;;   :config
;;   (progn
;;     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;;     (setq nrepl-hide-special-buffers t)))

;; ;;; Circe
;; (use-package circe
;;   :config
;;   (progn
;;    (setq circe-reduce-lurker-spam t)
;;     (add-hook 'circe-chat-mode-hook 'my-circe-prompt)
;;     (defun my-circe-prompt ()
;;       (lui-set-prompt
;;        (concat (propertize (concat (buffer-name) ">")
;;                            'face 'circe-prompt-face)
;;                " ")))))

;; ;;; Clojure-mode
;; (use-package clojure-mode)

;; ;;; CoffeeScript
;; (use-package coffee-mode
;;   :config
;;   (progn
;;     (setq coffee-tab-width 2)
;;     (setq whitespace-action '(auto-cleanup))
;;     (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))))

;; ;;; Current story
;; (use-package current-story)

;; ;;; Flycheck
;; (use-package flycheck
;;   :config
;;   (progn
;;     (add-hook 'after-init-hook #'global-flycheck-mode)))

;; ;;; Erlang
;; (use-package erlang)

;; ;;; ElasticSearch
;; (use-package es-mode
;;   :config
;;   (progn
;;     (add-to-list 'auto-mode-alist '("\\.es$" . es-mode))))

;; ;;; ESS (Emacs Speaks Statistics)
;; (use-package ess)
;; (use-package ess-R-data-view)
;;     ;; (ess-disable-smart-S-assign)))


;; ;;; Polymode (mostly for R markdown stuff)
;; (use-package polymode
;;   :init
;;   (progn
;;     (require 'poly-R)
;;     (require 'poly-markdown)

;;     ;;; MARKDOWN
;;     (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;     ;;; R modes
;;     (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
;;     (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
;;     (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))))

;; ;;; Make my shell work
;; (use-package exec-path-from-shell
;;   :config
;;   (progn
;;     (exec-path-from-shell-initialize)))

;; ;;; Go
;; (use-package go-mode
;;   :config
;;   (progn
;;     (add-hook 'before-save-hook 'gofmt-before-save)))

;; ;;; Haskell
;; (use-package haskell-mode
;;   :config
;;   (progn
;;     (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;     (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)))

;; ;;; Hippie expand
;; (global-set-key
;;  (kbd "C-.")
;;  'hippie-expand)

;; ;;; Ido
;; (use-package ido
;;   :init (ido-mode 1)
;;   :config
;;   (progn
;;     (setq
;;      ido-auto-merge-work-directories-length nil
;;      ido-case-fold t
;;      ido-everywhere t
;;      ido-enable-prefix nil
;;      ido-enable-flex-matching t
;;      ido-create-new-buffer 'always
;;      ido-max-prospects 10
;;      ido-file-extensions-order '(".rb" ".el" ".js" ".coffee"))
;;     (add-to-list 'ido-ignore-files "\\.DS_Store")))

;; ;;; J
;; (use-package j-mode)

;; ;;; Javascript
;; (use-package js3-mode
;;   :config
;;   (progn
;;     '(js3-auto-indent-p t)
;;     '(js3-enter-indents-newline t)
;;     '(js3-indent-on-enter-key t))
;;   :mode (("\\.js$" . js3-mode)))

;; (use-package js-comint
;;   :config
;;   (progn
;;     (setq inferior-js-program-command "node --interactive")
;;     ;; http://stackoverflow.com/questions/13862471/using-node-js-with-js-comint-in-emacs
;;     (setq inferior-js-mode-hook
;;           (lambda ()
;;             ;; We like nice colors
;;             (ansi-color-for-comint-mode-on)
;;             ;; Deal with some prompt nonsense
;;             (add-to-list
;;              'comint-preoutput-filter-functions
;;              (lambda (output)
;;                (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))))))

;; ;;; Lua
;; (use-package lua-mode
;;   :config
;;   (progn
;;     (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;;     (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))))

;; ;;; Magit
;; (use-package magit
;;   :bind ("C-x g" . magit-status)
;;   :config (progn
;;             (setq magit-remote-ref-format 'remote-slash-name)
;;             (add-hook 'magit-log-mode-hook 'turn-on-auto-fill)))

;; ;;; Org mode
;; (use-package org
;;   :config
;;   (progn
;;     (add-hook 'org-mode-hook 'turn-on-auto-fill)
;;     (define-key global-map "\C-cc" 'org-capture)
;;     (setq org-capture-templates
;;           '(("t" "Todo" entry (file+headline "~/Dropbox/org/tasks.org" "Tasks")
;;              "* TODO %?\n  %i\n  %a")
;;             ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
;;              "* %?\nEntered on %U\n  %i\n  %a")))))

;; ;;; Markdown
;; (use-package markdown-mode
;;   :mode (("\\.markdown$" . markdown-mode)
;;          ("\\.md$" . markdown-mode)
;;          ("\\.text$" . markdown-mode)))

;; ;;; Nodejs-repl
;; (use-package nodejs-repl)

;; ;;; Pandoc
;; (use-package pandoc-mode
;;   :config
;;   (progn
;;     (add-hook 'markdown-mode-hook 'turn-on-pandoc)))

;; ;;; Popwin
;; (use-package popwin
;;   :config (setq display-buffer-function 'popwin:display-buffer))

;; ;;; Projectile
;; (use-package projectile
;;   :init (projectile-global-mode 1)
;;   :config
;;   (progn
;;     (setq projectile-enable-caching t)
;;     (setq projectile-require-project-root nil)
;;     (setq projectile-completion-system 'projectile-completion-fn)
;;     (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

;; ;;; Rainbow delimiters
;; ;;(use-package rainbow-delimiters
;; ;;  :config
;; ;;  (progn
;; ;;    (global-rainbow-delimiters-mode)))

;; ;;; Rainbow mode (css)
;; (use-package rainbow-mode)

;; ;;; Ruby mode
;; (use-package ruby-mode
;;   :config
;;   (progn
;;     (setq ruby-deep-indent-paren nil)
;;     (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode)))
;;   :mode (("\\.rb$" . ruby-mode)
;;          ("\\.rake$" . ruby-mode)
;;          ("\\.gemspec$" . ruby-mode)
;;          ("\\.ru$" . ruby-mode)
;;          ("Rakefile$" . ruby-mode)
;;          ("Gemfile$" . ruby-mode)
;;          ("Capfile$" . ruby-mode)
;;          ("Guardfile$" . ruby-mode)))

;; (use-package rust-mode
;;   :config
;;   (progn
;;     (add-to-list 'load-path "/path/to/rust-mode/")
;;     (autoload 'rust-mode "rust-mode" nil t)
;;     (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))))

;; ;;; Scala-mode2
;; (use-package scala-mode2)

;; ;;; Scss
;; (use-package scss-mode
;;   :config
;;   (progn
;;     (setq css-indent-offset 2)
;;     (setq scss-compile-at-save nil))
;;   :mode (("\\.scss$" . scss-mode)))

;; ;;; Scheme
;; (setq scheme-program-name "csi")

;; ;;; Slim
;; (use-package slim-mode)

;; ;;; Smartparens
;; (use-package smartparens
;;   :config
;;   (progn
;;     (smartparens-global-mode t)
;;     (show-smartparens-global-mode t)
;;     (define-key sp-keymap (kbd "M-s") 'sp-splice-sexp)
;;     (define-key sp-keymap (kbd "C-M-<right>") 'sp-forward-slurp-sexp)
;;     (define-key sp-keymap (kbd "C-M-<left>") 'sp-forward-barf-sexp)
;;     (setq sp-autoescape-string-quote nil)
;;     (sp-pair "'" nil :actions :rem)))


;; (use-package switch-window
;;   :bind ("C-x i" . switch-window))

;; ;;; YAML
;; (use-package yaml-mode
;;   :config (progn
;;             (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;             (add-hook 'yaml-mode-hook
;;                       '(lambda ()
;;                          (define-key yaml-mode-map "\C-m" newline-and-indent)))))

;; ;;; Yard (ruby)
;; (use-package yard-mode
;;   :config
;;   (progn
;;     (add-hook 'ruby-mode-hook 'yard-mode)
;;     (add-hook 'ruby-mode-hook 'eldoc-mode)))

;; ;;; Vagrant
;; (use-package "vagrant")

;; ;;; Web-mode
;; (use-package web-mode)

;; ;;; Windmove
;; (use-package windmove
;;   :config (windmove-default-keybindings 'shift))

;; ;;; Window numbering
;; (use-package window-numbering)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Commands and functions ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;; Open with marked
;; (defun my-open-with-marked (filename)
;;   (interactive)
;;   (start-process "Marked" nil "/Applications/Marked.app/Contents/MacOS/Marked" filename))

;; (defun my-open-current-file-with-marked ()
;;   (interactive)
;;   (my-open-with-marked (buffer-file-name)))

;; ;;; Clean up buffers
;; (defun esk-untabify-buffer ()
;;   (interactive)
;;   (untabify (point-min) (point-max)))

;; (defun esk-indent-buffer ()
;;   (interactive)
;;   (indent-region (point-min) (point-max)))

;; (defun esk-cleanup-buffer ()
;;   "Perform a bunch of operations on the whitespace content of a buffer."
;;   (interactive)
;;   (esk-indent-buffer)
;;   (esk-untabify-buffer)
;;   (delete-trailing-whitespace))

;; ;;; Manage my emacs
;; (defun open-init-file ()
;;   (interactive)
;;   (find-file "~/dotfiles/dotemacs/.emacs.d/init.el"))

;; (defun open-cask-file ()
;;   (interactive)
;;   (find-file "~/dotfiles/dotemacs/.emacs.d/Cask"))
;; (put 'downcase-region 'disabled nil)

;; * Experimental packages
;; Packages I want to try but I'm not confident I'll actually stick with

;; ** Ivy-hydra

;; So I *think* that ivy itself installs hydra and then this makes
;; hydra have a slightly different look/feel/function? See
;; https://github.com/abo-abo/hydra for more.
(use-package ivy-hydra
  :ensure t)

;; ** Which-key

;; https://github.com/justbur/emacs-which-key
;; I'm not certain this one is useful. I should explore it some.
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (add-hook 'after-init-hook 'which-key-mode))




;; ** Expand Region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; ** Focus

;; Turn off highlighting for everythign but the region under the cursor?
(use-package focus
    :ensure t)

;; ** Time machine

;; Move through versions of a file
(use-package git-timemachine
  :ensure t)

;; ** Dumb jump

;; Apparently this is a decent tag alternative?
(use-package dumb-jump
    :ensure t
    :diminish dumb-jump-mode
    :bind (("C-c n d g" . dumb-jump-go)
           ("C-c n d b" . dumb-jump-back)
           ("C-c n d q" . dumb-jump-quick-look)))

;; ** Counsel-dash
(use-package counsel-dash
  :ensure t
  :bind (("C-c n d d" . counsel-dash)
         ("C-c n d a" . counsel-dash-activate-docset))
  :config (progn
            (setq helm-dash-browser-func 'eww)))

;; ** Deadgrep
(use-package deadgrep
  :ensure t
  :bind (("C-c n s r" . deadgrep)))

;; Keep packages up to date
;; (use-package auto-package-update
;;   :ensure t
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (auto-package-update-maybe))

;; ** Avy
(use-package avy
  :ensure t
  :bind (("C-c j c" . avy-goto-char-2)
         ("C-c j w" . avy-goto-char-timer)
         ("C-c j l" . avy-goto-line)))

;; ** Lispy
;; https://github.com/abo-abo/lispy#list-commands-overview
;; This seems cool but maybe conflicts with outshine?
(use-package lispy
  :ensure t
  :config
  (progn (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))))

;; ** Ipy
;; https://github.com/abo-abo/lpy
;; Does't seem to be in melpa yet
;; (use-package lpy
;;   :ensure t)

;; ** Ace-window
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :config
  (progn (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

;; ** Dimmer
(use-package dimmer
  :ensure t
  :config
  (progn (dimmer-mode)
         (setq dimmer-fraction 0.25)))

;; ** Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

;; ** Flycheck
(use-package flycheck
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode)))

;; ** Web-mode
(use-package web-mode
  :ensure t
  :config
  (progn
    (setq web-mode-content-types-alist
          '(("jsx"  . "/Users/charlietanksley/git/monetate-server//ui/js/.*/.\\.js[x]?\\'")))))

;; ** Smartparens
(use-package smartparens
  :ensure t
  :config
  (progn
    (add-hook 'js-mode-hook #'smartparens-mode)
    (add-hook 'web-mode-hook #'smartparens-mode)
    (add-hook 'python-mode-hook #'smartparens-mode)))

;; ** rjsx-mode
(use-package rjsx-mode
  :ensure t)

;; ** markdown
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; ** Haskell
(use-package intero
  :ensure t
  :config
  (progn
    (add-hook 'haskell-mode-hook 'intero-mode)))

;; ** Elixir
(use-package elixir-mode
  :ensure t
  :config
  (progn
    (add-hook 'elixir-mode-hook
              (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))))

(use-package alchemist
  :ensure t
  :config
  (progn
    (add-hook 'elixir-mode-hook 'alchemist-mode)))

;; ** Indent tools
(use-package indent-tools
  :ensure t
  :bind (("C-c >" . indent-tools-hydra/body)))


;; ** Arduino
(use-package arduino-mode
  :ensure t)

(use-package company-arduino
  :ensure t
  :config
  (progn
    (add-hook 'irony-mode-hook 'company-arduino-turn-on)
    (add-hook 'arduino-mode-hook 'irony-mode)))

;; ** Irony (C/C++)
(use-package irony
  :ensure t
  :config
  (progn
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

;; * Org
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (ditaa . t)
   (gnuplot . t)
   (latex . t)))

(setq org-ditaa-jar-path "/usr/local/bin/ditaa")
(setq org-babel-ditaa-java-cmd " ")
(setq org-babel-default-header-args:ditaa
      '((:results . "file")
        (:exports . "results")
        (:java . " ")))
(setq org-ditaa-jar-option " ")


(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


;; (use-package gnuplot
;;   :ensure t)
(setq org-use-speed-commands t)
(setq org-log-done 'time)
(setq org-agenda-files '("/Users/charlietanksley/org"))

(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "CANCELED")))

(setq org-capture-templates
      '(("r" "Retrospective" entry (file+olp "~/org/retrospectives.org" "Retrospectives" "2018 Q3")
         "*** <%(org-read-date nil nil \"-Mon\")> - <%(org-read-date nil nil \"-Fri\")>
**** OKR Status
***** Reduce time spent solving algorithmic challenges by 50\%
%?
***** Reduce mistakes in work programming by 90\%
***** Increase (2 sprint average) velocity by 300\%
**** Health Metrics
***** Stress:
***** Time management:
**** Last week
**** This week")
        ("p" "PR" entry (file "~/org/prs.org")
         "* TODO %? [0/13]
:PROPERTIES:
:ORDERED:  t
:END:
  - [ ] Claim
  - [ ] Write PR description
  - [ ] Understand the problem
  - [ ] Write tests
  - [ ] Write the code
  - [ ] Document UAT test script
  - [ ] Documentation
  - [ ] Clean commits
  - [ ] Request review
  - [ ] Respond to review
  - [ ] Clean commits
  - [ ] Merge
  - [ ] Update changelog")))

(global-set-key "\C-ca" 'org-agenda)

(provide 'init)
;;; init.el ends here


