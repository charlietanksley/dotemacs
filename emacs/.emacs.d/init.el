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
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))

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
            (add-hook 'outline-minor-mode-hook 'outshine-mode)
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

;; hoping this helps with some wild cpu-usage I'm seeing:
;; https://github.com/gregsexton/origami.el/issues/39
(use-package nlinum
  :ensure t
  :config (global-nlinum-mode 1))

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
            (add-hook 'magit-log-mode-hook 'turn-on-auto-fill)
            (add-hook 'magit-mode-hook (lambda () (global-whitespace-mode -1)))))

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
          ivy-re-builders-alist '(
                                  (counsel-projectile-find-file . ivy--regex-fuzzy)
                                  (counsel-find-file . ivy--regex-fuzzy)
                                  (t . ivy--regex-plus))
          ivy-use-virtual-buffers t
          enable-recursive-minibuffers t
          ivy-count-format "%d/%d ")))

;; ** Counsel
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)))
;; TODO turn on counsel mode?

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

;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; This doesn't seem to actually do anything...
(use-package counsel-projectile
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'counsel-projectile-mode)))

;; ** Exec path from shell
(use-package exec-path-from-shell
  :ensure t
  :init (progn
          (exec-path-from-shell-initialize)))
;; ** Editorconfig
;; TODO this isn't set up right. I never actually use it?
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; ** Fish shell
(use-package fish-mode
  :ensure t)
;; * Language specific packages
;; ** COBOL
(use-package cobol-mode
  :ensure t)

;; ** Geiser (Scheme)
(use-package geiser
  :ensure t)

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

;; https://dotnet.microsoft.com/download
;; https://github.com/andrew-christianson/lsp-python-ms
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((python-mode . lsp))
  :bind (("C-c n l d" . lsp-find-definition)
         ("C-c n l r" . lsp-find-references)))

;; https://emacs-lsp.github.io/lsp-mode/page/lsp-solargraph/
;; https://github.com/joaotavora/eglot
(use-package eglot
  :ensure t
  :config
  (progn
    (add-hook 'ruby-mode-hook 'eglot-ensure)))

(use-package lsp-python-ms
  :load-path "~/dotemacs/packages/lsp-python-ms"
  :ensure nil
  :hook (python-mode . lsp)
  :config
  ;; for executable of language server, if it's not symlinked on your PATH
  (setq lsp-python-ms-executable
        "~/bin/Microsoft.Python.LanguageServer"))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; (use-package robe
;;   :ensure t
;;   :hook ((ruby-mode . robe)))

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

;; ** JavaScript
(use-package prettier-js
  :ensure t
  :config (progn
            (add-hook 'js-mode-hook 'prettier-js-mode)))

;; ** Clojure
(use-package clojure-mode
  :ensure t
  :mode (("\\.clj$" . clojure-mode)))

(use-package cider
  :ensure t)

;; ** Racket/Scheme
(use-package racket-mode
  :ensure t
  :mode (("\\.rkt$" . racket-mode))
  :config (progn
            (setq racket-program "/Users/charlietanksley/.asdf/shims/racket")))

(use-package flymake-racket
  :ensure t
  :commands (flymake-racket-add-hook)
  :init
  (add-hook 'scheme-mode-hook #'flymake-racket-add-hook)
  (add-hook 'racket-mode-hook #'flymake-racket-add-hook))

;; ** Ruby
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
         ("Capfile$" . ruby-mode)))

(use-package rubocopfmt
  :ensure t
  :hook ((ruby-mode . rubocopfmt-mode)))

(use-package inf-ruby
  :ensure t)

(use-package rspec-mode
  :ensure t)

;; ** SQL
(use-package sqlformat
  :ensure t
  :init
  (setq sqlformat-command 'pgformatter))

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
  :hook (
         (emacs-lisp-mode . (lambda () (lispy-mode 1)))
         (clojure-mode . (lambda () (lispy-mode 1)))
         (scheme-mode . (lambda () (lispy-mode 1)))))



;; ** Ace-window
(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window))
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
  :hook (ruby-mode . (lambda () (add-to-list 'flycheck-disabled-checkers 'ruby-reek)))
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
;; TODO Do I want to configure this to be useful for Ruby?
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

;; ** Whitespace cleanup mode
(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode 1))

;; ** Indent tools
(use-package indent-tools
  :ensure t
  :bind (("C-c >" . indent-tools-hydra/body)))

;; ** Code folding with Origami
(use-package origami
  :ensure t
  :init (global-origami-mode 1)
  :bind (("C-c f o a" . origami-open-all-nodes)
         ("C-c f o n" . origami-open-node)
         ("C-c f c n" . origami-close-node)
         ("C-c f c a" . origami-close-all-nodes)))

;; ** Git-link
(use-package git-link
  :ensure t)

;; ** Autocomplete stuff
(use-package auto-complete
  :ensure t
  :config
  (progn
    (ac-config-default)
    (add-to-list 'ac-modes 'ruby-mode)))
;; * Org

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (gnuplot . t)
   (latex . t)
   (plantuml . t)
   (python . t)
   (ruby . t)
   (sql . t)))

(require 'ox-md)
(use-package ox-gfm
  :ensure t)

(setq org-use-speed-commands t)
(setq org-log-done 'time)
(setq org-agenda-files '("/Users/charlietanksley/org"))

(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "CANCELED")))

;; ** Plantuml

;; brew install plantuml
;; find /usr/local/Cellar/ -name plantuml.jar
(setq org-plantuml-jar-path
      (expand-file-name "/usr/local/Cellar//plantuml/1.2020.15/libexec/plantuml.jar"))

;; needed for org export to html
(use-package htmlize
  :ensure t)

(use-package plantuml-mode
  :ensure t
  :mode (("\\.plantuml\\'" . plantuml-mode))
  :init (progn
          (setq plantuml-default-exec-mode 'jar)
          (setq plantuml-jar-path
                (expand-file-name "/usr/local/Cellar//plantuml/1.2020.15/libexec/plantuml.jar"))))

;; * Functions
;; ** adjust-font-size
(defun adjust-font-size (n)
  "Set font size to N, as passed in via interactive function call."
  (interactive "n Font size: ")
  (set-font-size n))

(defun set-font-size (size)
  "Set font size to SIZE."
  (set-face-attribute 'default nil
                      :family "Menlo" :height (* size 10) :weight 'normal))

;; * Wrapping up

;; If asdf is installed, we want it accessable for things like
;; flycheck.
;;
;; I do not understand why this has to come late in the file, but it
;; does. Possibly it just has to come after exec-path-from-shell.
(if (file-exists-p "/Users/charlietanksley/.asdf")
    (setq exec-path (append '("/Users/charlietanksley/.asdf/shims" "/Users/charlietanksley/.asdf/bin")
                            exec-path))
  nil)

(provide 'init)
;;; init.el ends here




(setq frame-title-format (quote (:eval (if (buffer-file-name) "%b — %f" "%b"))))

;; TODO Look into:
;; - lsp-mode, eglot (language servers)
;; - Gdb, dap-mode (debuggers)
