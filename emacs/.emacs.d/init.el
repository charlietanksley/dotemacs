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

(global-linum-mode 1)

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
          ivy-re-builders-alist '((t . ivy--regex-fuzzy))
          ivy-use-virtual-buffers t
          enable-recursive-minibuffers t
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

;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; This doesn't seem to actually do anything...
(use-package counsel-projectile
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'counsel-projectile-mode)))

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

;; https://dotnet.microsoft.com/download
;; https://github.com/andrew-christianson/lsp-python-ms
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((python-mode . lsp))
  :bind (("C-c n l d" . lsp-find-definition)
         ("C-c n l r" . lsp-find-references)))

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
  :config
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (add-hook 'ruby-mode-hook (lambda () (setq flycheck-disabled-checkers '(ruby-reek))))))

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

;; ** Whitespace cleanup mode
(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode 1))

;; * Org

;; Remove the built-in org
;; From https://github.com/glasserc/etc/commit/3af96f2c780a35d35bdf1b9ac19d80fe2e6ebbf8
(eval-when-compile
  (require 'cl))
(setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))
(package-built-in-p 'org)
(setq package--builtins (assq-delete-all 'org package--builtins))
(use-package org
  :ensure t
  :pin org)



;;(org-babel-do-load-languages
;; 'org-babel-load-languages
;; '((python . t)
;;   (C . t)
;;   (ditaa . t)
;;   (gnuplot . t)
;;   (latex . t))
;;)

;;(use-package ox-gfm
;;  :ensure t)
;;
;;;; We need org-tempo for <s TAB
;;(require 'org-tempo)
;;
;;;;(setq org-ditaa-jar-path "/usr/local/bin/ditaa")
;;;;(setq org-babel-ditaa-java-cmd " ")
;;;;(setq org-babel-default-header-args:ditaa
;;;;      '((:results . "file")
;;;;        (:exports . "results")
;;;;        (:java . " ")))
;;;;(setq org-ditaa-jar-option " ")
;;
;;
;;(require 'ox-latex)
;;(add-to-list 'org-latex-packages-alist '("" "minted"))
;;(setq org-latex-listings 'minted)
;;
;;(setq org-latex-pdf-process
;;      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;;
;;
;;;; (use-package gnuplot
;;;;   :ensure t)
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


