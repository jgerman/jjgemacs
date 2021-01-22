(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq mac-pass-command-to-system 't)

(define-key global-map [?\s-v] 'yank)
(define-key global-map [?\s-c] 'kill-ring-save)
(define-key global-map [?\s-x] 'kill-region)

(setq select-enable-clipboard t)

(defconst *install-dir* "~/jjgemacs/")

(defun find-config ()
  "Edit config.org"
  (interactive)
  (find-file (concat *install-dir* "settings.org")))

(global-set-key (kbd "C-c I") 'find-config)

(load-file (concat *install-dir* "sensible-defaults.el"))
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

(set-window-scroll-bars (minibuffer-window) nil nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package doom-themes
  :straight t
  :config
  (progn
    (setq doom-themes-enable-bold t
	  doom-themes-enable-italic t)
    (load-theme 'doom-vibrant t)))

(use-package hl-line
  :straight t
  :config
  (global-hl-line-mode))

(use-package rainbow-delimiters
  :straight t)

(use-package all-the-icons
  :straight t)

(use-package projectile
  :straight  t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(global-set-key (kbd "C-c p p") #'projectile-switch-project)
(global-set-key (kbd "C-c p f") #'projectile-find-file)
(global-set-key (kbd "C-c p t") #'projectile-toggle-between-implementation-and-test)

(use-package ag
  :straight t
  :commands (ag ag-regexp ag-project))

(global-set-key (kbd "C-c p s g") #'ag-project)

(use-package company
  :straight t)
(global-company-mode)

(defun helm-hide-minibuffer-maybe ()
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(use-package helm
  :straight t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list))
  :bind (:map helm-map
              ("M-i" . helm-previous-line)
              ("M-k" . helm-next-line)
              ("M-I" . helm-previous-page)
              ("M-K" . helm-next-page)
              ("M-h" . helm-beginning-of-buffer)
              ("M-H" . helm-end-of-buffer)
              ("TAB" . helm-execute-persistent-action)
              ("<tab>" . helm-execute-persistent-action)
              ("C-z" . helm-select-action))
  :config (progn
            (setq helm-buffers-fuzzy-matching t)
            (setq helm-echo-input-in-header-line t)
            (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
            (helm-mode 1)))

(use-package helm-descbinds
  :straight t
  :bind ("C-h b" . helm-descbinds))

(use-package helm-swoop
  :straight t
  :bind (("M-m" . helm-swoop)
         ("M-M" . helm-swoop-back-to-last-point))
  :init
  (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map))

(use-package helm-ag
  :straight helm-ag
  :bind ("M-p" . helm-projectile-ag)
  :commands (helm-ag helm-projectile-ag)
  :init (setq helm-ag-insert-at-point 'symbol
              helm-ag-command-option "--path-to-ignore ~/.agignore"))

 (use-package helm-company
   :straight t)

(use-package helm-projectile
  :straight t
  :config
  (helm-projectile-on))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents . 5)
                          (projects . 10)
                          (bookmarks . 5))))

(use-package key-chord
  :straight t
  :config (key-chord-mode 1))

(use-package popwin
  :straight t
  :config
  (setq display-buffer-alist '((popwin:display-buffer-condition popwin:display-buffer-action))))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-minor-modes (featurep 'minions)))

(use-package minions
  :straight t
  :config (minions-mode 1))

(save-place-mode 1)
(set-default 'truncate-lines 1)
(setq ring-bell-function 'ignore)
(winner-mode t)
(blink-cursor-mode -1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq fci-rule-column 100)

(require 'paren)
;;(set-face-background 'show-paren-match "#aaaaaa")
(set-face-foreground 'show-paren-match "#f54949")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*", temporary-file-directory t)))

(use-package restclient
  :straight (restclient :type git :host github :repo "pashky/restclient.el"))

(use-package restclient-jq
  :straight (restclient-jq :type git :host github :repo "pashky/restclient.el"))

(add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode))

(use-package ace-window
  :straight t)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(key-chord-define-global "ww" 'ace-select-window)

(require 'org-tempo)

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))

(add-hook 'org-mode-hook 'org-indent-mode)

(use-package ob-go
:straight t)
  ;; TODO TEMPORARY I need a solution for managing non-package elisp files
;; this requires manually putting the restclient code in the search path
;; TODO this can probably be a gitsubmodule
(load-file (concat *install-dir* "ob-restclient.el"))
(require 'ob-restclient)
(require 'ob-go)
(require 'ob-clojure)
(require 'ob-js)
(require 'ob-groovy)

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages
                                                       '((shell		.	t)
                                                         (lisp		.	t)
                                                         (clojure	.	t)
                                                         (sql		.	t)
                                                         (python	.	t)
                                                         (go		.	t)
                                                         (js		.	t)
                                                         (restclient	.	t)
                                                         (groovy        .       t))))

(setq org-babel-clojure-backend 'cider)

(setq org-log-done 'time)

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))

(use-package git-gutter+
  :straight t
  :init (global-git-gutter+-mode))

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package which-key
 :straight t)
(which-key-mode)
(which-key-setup-minibuffer)

(use-package avy
  :straight t
  :bind (("C-'" . avy-goto-char-timer)
         ("C-:" . avy-goto-line)))

(key-chord-define-global "sf" 'avy-goto-char-2)

(require 'midnight)
(midnight-delay-set 'midnight-delay "12:00am")

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package deft
  :straight t
  :config
  (setq deft-directory "~/notes")
  (setq def-recursive t)
  (setq deft-auto-save-interval 300.0)
  (setq deft-org-mode-title-prefix t)
  (setq deft-use-filename-as-title t)
  (setq deft-default-extension "org"))

(use-package ox-hugo
  :straight t
  :after ox)

(use-package howdoyou
  :straight t)

(use-package free-keys
  :straight t)

(use-package smartparens
  :straight t)

(require 'smartparens-config)

(global-set-key (kbd "C-)") 'sp-forward-slurp-sexp) ;; this shouldn't be always set but for now if it gets me going
(global-set-key (kbd "M-s") 'sp-splice-sexp)

(use-package json-mode
  :straight t)

(use-package yaml-mode
  :straight t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . markdown-mode))

(use-package slime
  :straight t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  ;;(setq slime-contribs '(slime-fancy slime-asdf)) ;;taking out temporarily for clisp
  (setq slime-lisp-implementations
        '((sbcl ("/usr/local/bin/sbcl"))
          (clisp ("/usr/local/bin/clisp")))))

(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'smartparens-mode)
(add-hook 'lisp-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook 'lisp-mode-hook #'eldoc-mode)
(add-hook 'lisp-mode-hook #'subword-mode)

(add-hook 'slime-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook #'smartparens-mode)
(add-hook 'slime-repl-mode-hook #'turn-on-smartparens-strict-mode)

(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'subword-mode)

(use-package clojure-mode
  :straight t
  :config
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'turn-on-smartparens-mode)
  (add-hook 'clojure-mode-hook #'turn-on-smartparens-strict-mode))

(use-package cider
  :straight t
  :config
  (setq nrepl-log-messages t)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'smartparens-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'turn-on-smartparens-strict-mode))

(use-package clj-refactor
  :straight t)

(defun my-clj-refactor-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clj-refactor-hook)

(use-package groovy-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode)))

(defun my-go-mode-hook ()
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
   (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(defun auto-complete-for-go ()
  (auto-complete-mode 1))

(use-package go-mode
  :straight t
  :config
  (add-hook 'go-mode-hook 'my-go-mode-hook)
  (add-hook 'go-mode-hook 'auto-complete-for-go))

(defun xml-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region
     (mark) (point)
     "xmllint --format --encode utf-8 -"
     (buffer-name) t)))

(defun decode-jwt ()
  (interactive)
  (let ((new-buff (get-buffer-create "decoded-jwt.js")))
    (save-excursion
      (shell-command-on-region
       (mark) (point)
       "jq -R 'split(\".\") | .[1] | @base64d | fromjson'"
       new-buff)
      (with-current-buffer new-buff
        (funcall 'javascript-mode))
      (switch-to-buffer new-buff))))

;; taken from here: https://www.emacswiki.org/emacs/EshellMultipleEshellBuffers#:~:text=Multi%2Deshell,-multi%2Deshell.&text=It%20maintains%20a%20ring%20of,buffer%20in%20the%20shell%20ring.
(defun eshell-new ()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(setq custom-file (concat *install-dir* "custom.el"))
(load custom-file)
