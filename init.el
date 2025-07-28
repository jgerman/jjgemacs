;; early init handles disabling the built in package management
;;

;; bootstrap straight

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package org :straight t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MAc
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq select-enable-clipboard t)
(setq mac-pass-command-to-system nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some constants that are highly subjective to my setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I keep all my checked out projects under a development directory, including my config
;; and third party repos I clone
(defconst *development-dir* "~/development/")
(defconst *install-dir* "~/development/jgerman/jjgemacs/") ;; TODO change me when I merge this to main

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; I'm going to call these core settings for now, stuff I want in my emacs
;; config no matter what (for the most part)
;;
;; Things that should go in this section are:
;;   - fonts and font sizes
;;   - emacs core settings (gc threshold, turning on and off ui elements)
;;   - core window management (ace-window, pop-win)
;;   - basically anything that I don't intend to change or remove
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I still use sensible defaults to bootstrap my emacs configs
(load-file (concat *development-dir* "hrs/sensible-defaults.el/sensible-defaults.el"))
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)


;; Fonts and sizes

(defvar *my-font-size* 120)
(when (= (display-pixel-width) 1920)
  (setq *my-font-size* 110))
(when (= (display-pixel-width) 3456)
  (setq *my-font-size* 100))
(when (= (display-pixel-width) 3840)
  (setq *my-font-size* 120)) ;; changing this for the mac probably screws up linux.. why??
(when (= (display-pixel-width) 2560)
  (setq *my-font-size* 110))
(when (= (display-pixel-width) 1600)
  (setq *my-font-size* 120))
(message (concat "Setting font to: " (number-to-string *my-font-size*)))
(defvar *my-font* "")
(setq *my-font* "Hack")
(set-face-attribute 'default nil :font *my-font* :height *my-font-size*)
(set-frame-font *my-font* nil t)

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Hack" :height 180 :weight thin))))
 '(fixed-pitch ((t (:family "Hack" :height 120)))))

 (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  (add-hook 'org-mode-hook 'variable-pitch-mode)

;; how do we treat tabs?
(setq-default indent-tabs-mode nil)

;; I set this based on the advice from lsp-mode's performance page
;; currently using eglot but seems like a good thing to keep high
(setq gc-cons-threshold 100000000)


;; convenient way to get to the config
(defun find-config ()
  (interactive)

  (find-file (concat *install-dir* "init.el")))

(global-set-key (kbd "C-c I") 'find-config)

;; why did this get unbound? ok maybe it was the mac os upgrade had to turn off the touchbar screenshot in settings
(global-set-key (kbd "M-^") 'join-line)

;; Some basic changes
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)
(save-place-mode 1)
(set-default 'truncate-lines 1)
(setq ring-bell-function 'ignore)
(winner-mode t)
(blink-cursor-mode -1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq fci-rule-column 100)
(set-window-scroll-bars (minibuffer-window) nil nil)
(setq native-comp-async-report-warnings-errors nil)

(setq custom-file (concat *install-dir* "custom.el"))
(load custom-file)

;; TODO double check that this is doing what I think it's doing
;; Having trouble with long file names, for now disabling auto save and backup
;; there was also a reddit post about how doom hashes the name to make it shorter... look into that
;; https://wilkesley.org/~ian/xah/emacs/emacs_set_backup_into_a_directory.html
(setq make-backup-files nil)
(setq auto-save-default nil)
;;(setq backup-directory-alist
;;      `((".*" . , temporary-file-directory)))


(setq auto-save-file-name-transforms
      `((".*", temporary-file-directory t)))

;; I use this a lot but I think I'm overwriting completion at point here? This
;; may need to change
(global-set-key "\M-/" 'hippie-expand)

;; Started using general to define keymaps based on a blog post
;; If it's convenient I'll convert all custom mappings to use it
(use-package general
  :straight t)

;; manage how pop up windows behave
(use-package popwin
  :straight t
  :config
  (setq display-buffer-alist '((popwin:display-buffer-condition popwin:display-buffer-action))))

;; convenient window switching
(use-package ace-window
  :straight t)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(global-set-key (kbd "C-x o") 'ace-select-window)

;; window movement
(global-set-key (kbd "C-M-S-s-j") 'windmove-down)
(global-set-key (kbd "C-M-S-s-<down>") 'windmove-down)

(global-set-key (kbd "C-M-S-s-k") 'windmove-up)
(global-set-key (kbd "C-M-S-s-<up>") 'windmove-up)

(global-set-key (kbd "C-M-S-s-h") 'windmove-left)
(global-set-key (kbd "C-M-S-s-<left>") 'windmove-left)

(global-set-key (kbd "C-M-S-s-l") 'windmove-right)
(global-set-key (kbd "C-M-S-s-<right>") 'windmove-right)

;; ensure shell
(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; control how eldoc behaves
(setq eldoc-documentation-function
      (lambda ()
        (when (eql last-command-event 32)
          (let (eldoc-documentation-function)
            (eldoc-print-current-symbol-info)))))

;; attempting to solve eglot not starting based on reddit post I saw
;; this fixes the problem that ai was seeing after installing emacs-plus@30
(straight-use-package '(project :type built-in))
(straight-use-package '(xref :type built-in))

;; taken from here: https://wwvw.emacswiki.org/emacs/EshellMultipleEshellBuffers#:~:text=Multi%2Deshell,-multi%2Deshell.&text=It%20maintains%20a%20ring%20of,buffer%20in%20the%20shell%20ring.
(defun eshell-new ()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Theming
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; doom vibrant was looking too washed out
;; doom-ayu-dark isn't bad but I can't see the comments
;; doom-ir-black is a strong contender still good
;; doom-laserwave is ok
;; material dark is decent
;; meltbus is monocrhomatic but I kind of like it
;; molokai isn't bad, but don't like the full line, comments still too dark, like the variable font sizes for headings
;; doom monokai octagon ok
;; monokai pro
;; monokai spectrum
;; moonlight
;; nord might be too contrasty but I kinda like the highlighted current line
;; nova contrasty but not bad
;; oceanic-next
;; one
;; outrun-electric is good except the line numbers are invisible except the current
;; wilmersdorf not bad but same issue with background too light gray

;; doom-vibrant was my last most common
;; doom-ir-black but with variable font sizes? and brighter comments

(use-package doom-themes
  :straight t
  :config
  (progn
    (setq doom-themes-enable-bold t
	  doom-themes-enable-italic t
	  doom-vibrant-brighter-modeline t
	  doom-vibrant-brighter-comments nil
          doom-ir-black-brighter-comments t
          doom-ir-black-brighter-modeline t)
    ;;(load-theme 'doom-vibrant)
      (load-theme 'doom-ir-black)))

;; wash out the background a bit
;; (custom-set-faces
;;  '(default ((t (:background "#1c1b1b")))))

;; (use-package solarized-theme
;;   :straight t
;;   :config
;;   (setq solarized-distinct-fringe-background nil)
;;   (setq x-underline-at-descent-line nil)
;;   (setq solarized-high-contrast-mode-line nil)
;;   (load-theme 'solarized-dark t))


;; Highlight the current paren in bold red
(require 'paren)
(set-face-foreground 'show-paren-match "#f54949")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-minor-modes (featurep 'minions))
  (setq doom-modeline-hud t)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-vcs-max-length 25))



(use-package hl-line
  :straight t
  :config
  (global-hl-line-mode))

(use-package minions
  :straight t
  :config (minions-mode 1))

(use-package rainbow-delimiters
  :straight t)

;; on a new install this requires setting M-x install-all-the-icons
(use-package all-the-icons
  :straight t)

(use-package all-the-icons-completion
  :straight t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Use dashboard as a landing for new frames
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents . 5)
                          (projects . 10)
                          (bookmarks . 5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Magit is really core, but I'm putting it in its own section
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))


;; control how magit opens
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Git Time Machine
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package git-timemachine
  :straight t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Generic modes that I want across emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs
  :straight t
  :init
  (setq treemacs-project-follow-mode t)
  (setq treemacs-display-current-project-exclusively t))

(global-set-key [f5] 'treemacs)

(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . markdown-mode))

(use-package yaml-mode
  :straight t
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; (use-package git-gutter+
;;   :straight t
;;   :init (global-git-gutter+-mode))

(use-package which-key
  :straight t)
(which-key-mode)
(which-key-setup-minibuffer)

(use-package avy
  :straight t
  :bind (("C-'" . avy-goto-char)
         ("C-:" . avy-goto-line)))

(use-package dockerfile-mode
  :straight t
  :mode "\\Dockerfile*//")

;; restclient is great when I need it
(use-package restclient
  :straight (restclient :type git :host github :repo "pashky/restclient.el"))

(use-package restclient-jq
  :straight (restclient-jq :type git :host github :repo "pashky/restclient.el"))

(add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode))

;; I need to start checking my snippets in, and learning to better manage
;; yasnippet in general
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package free-keys
  :straight t)

;; I still use this though I think either consult or embark might have a better way
(use-package multiple-cursors
  :straight t)

(use-package graphql-mode
  :straight t)

;; if I get rid of kmonad I have to get rid of this too it's already iffy due to
;; the way I type, I've had actions occurring on screen I don't want
;; ultimately home row mods would be nice though...
;; (use-package kbd-mode
;;   :straight (keyboard-mode :type git :host github :repo "kmonad/kbd-mode"))

;; (add-to-list 'auto-mode-alist '("\\.kbd\\'" . restclient-mode))

;; (use-package meow
;;   :straight t)

;; (defun meow-setup ()
;;   (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;   (meow-motion-overwrite-define-key
;;    '("j" . meow-next)
;;    '("k" . meow-prev)
;;    '("<escape>" . ignore))
;;   (meow-leader-define-key
;;    ;; SPC j/k will run the original command in MOTION state.
;;    '("j" . "H-j")
;;    '("k" . "H-k")
;;    ;; Use SPC (0-9) for digit arguments.
;;    '("1" . meow-digit-argument)
;;    '("2" . meow-digit-argument)
;;    '("3" . meow-digit-argument)
;;    '("4" . meow-digit-argument)
;;    '("5" . meow-digit-argument)
;;    '("6" . meow-digit-argument)
;;    '("7" . meow-digit-argument)
;;    '("8" . meow-digit-argument)
;;    '("9" . meow-digit-argument)
;;    '("0" . meow-digit-argument)
;;    '("/" . meow-keypad-describe-key)
;;    '("?" . meow-cheatsheet))
;;   (meow-normal-define-key
;;    '("0" . meow-expand-0)
;;    '("9" . meow-expand-9)
;;    '("8" . meow-expand-8)
;;    '("7" . meow-expand-7)
;;    '("6" . meow-expand-6)
;;    '("5" . meow-expand-5)
;;    '("4" . meow-expand-4)
;;    '("3" . meow-expand-3)
;;    '("2" . meow-expand-2)
;;    '("1" . meow-expand-1)
;;    '("-" . negative-argument)
;;    '(";" . meow-reverse)
;;    '("," . meow-inner-of-thing)
;;    '("." . meow-bounds-of-thing)
;;    '("[" . meow-beginning-of-thing)
;;    '("]" . meow-end-of-thing)
;;    '("a" . meow-append)
;;    '("A" . meow-open-below)
;;    '("b" . meow-back-word)
;;    '("B" . meow-back-symbol)
;;    '("c" . meow-change)
;;    '("d" . meow-delete)
;;    '("D" . meow-backward-delete)
;;    '("e" . meow-next-word)
;;    '("E" . meow-next-symbol)
;;    '("f" . meow-find)
;;    '("g" . meow-cancel-selection)
;;    '("G" . meow-grab)
;;    '("h" . meow-left)
;;    '("H" . meow-left-expand)
;;    '("i" . meow-insert)
;;    '("I" . meow-open-above)
;;    '("j" . meow-next)
;;    '("J" . meow-next-expand)
;;    '("k" . meow-prev)
;;    '("K" . meow-prev-expand)
;;    '("l" . meow-right)
;;    '("L" . meow-right-expand)
;;    '("m" . meow-join)
;;    '("n" . meow-search)
;;    '("o" . meow-block)
;;    '("O" . meow-to-block)
;;    '("p" . meow-yank)
;;    '("q" . meow-quit)
;;    '("Q" . meow-goto-line)
;;    '("r" . meow-replace)
;;    '("R" . meow-swap-grab)
;;    '("s" . meow-kill)
;;    '("t" . meow-till)
;;    '("u" . meow-undo)
;;    '("U" . meow-undo-in-selection)
;;    '("v" . meow-visit)
;;    '("w" . meow-mark-word)
;;    '("W" . meow-mark-symbol)
;;    '("x" . meow-line)
;;    '("X" . meow-goto-line)
;;    '("y" . meow-save)
;;    '("Y" . meow-sync-grab)
;;    '("z" . meow-pop-selection)
;;    '("'" . repeat)
;;    '("<escape>" . ignore)))

;;(require 'meow)
;;(meow-setup)

;;(meow-global-mode 1)

;; leaving meow in the file for now but it - doesn't appear to play well with
;; smart parens (using the fake cursors) - doesn't evaluate symbols correctly in
;; clojure (keywords specifically and only in beacon mode which is odd) - I
;; don't think I need it for movement, my current thinking is to bind C-c l as a
;; leader for common lisp editing and movement commands: smartparens, consult
;; imenu, avy line, lispy-ace-window
;;
;; However, when editing normal text I can see meow being useful for recording
;; macros across lines maybe? So that small bit may be something I steal. If I
;; can turn on meow to make a selection across lines then use beacon just do to
;; the editing maybe?
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dired ranger
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package dired-ranger
;;  :straight t
;;  :bind (:map dired-mode-map
;;             ("W" . dired-ranger-copy)
;;              ("X" . dired-ranger-move)
;;              ("Y" . dired-ranger-paste)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Dirvish
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; setting this here due to getting errors opening dired after installing this
;; this issue seems to relate: https://github.com/d12frosted/homebrew-emacs-plus/issues/383
(setq insert-directory-program "gls" dired-use-ls-dired t)
(setq dired-listing-switches "-al --group-directories-first")

(use-package dirvish
  :straight t
  :config
  (dirvish-override-dired-mode)
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dirvish-hide-details nil)
  (setq dired-listing-switches
        "-lah --almost-all --human-readable --group-directories-first --no-group"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Epubs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nov
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Org Mode Setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'org-mode-hook 'turn-on-auto-fill)
(setq org-confirm-babel-evaluate nil)

(require 'org-tempo)

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode))

(add-hook 'org-mode-hook 'org-indent-mode)

;; org-babel
(use-package ob-kotlin
  :straight (ob-kotlin :type git :host github :repo "zweifisch/ob-kotlin"))

(use-package ob-typescript
  :straight t)

(use-package ob-go
  :straight t)

(require 'ob-go)
(require 'ob-clojure)
(require 'ob-js)
(require 'ob-kotlin)
(require 'ob-typescript)

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell		.	t)
                                 (lisp		.	t)
                                 (clojure	.	t)
                                 (sql		.	t)
                                 (python	.	t)
                                 (go		.	t)
                                 (sql           .       t)
                                 (kotlin        .       t)
                                 (typescript    .       t)
                                 (scheme        .       t)
                                 (js		.	t)
                                 (dot . t))))

(setq org-babel-clojure-backend 'cider)

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/pkb/")
  (org-agenda-files '("~/Documents/pkb/" "~/Documents/pkb/daily/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . competion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-setup))

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

(use-package consult-org-roam
  :straight t)
;; '(("d" "default" plain
;;    "%?"
;;    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
;;    :unnarrowed t))


;; exports
(use-package ox-slack
  :straight t)

(require 'ox-slack)
(require 'ox-md)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Core Stack
;;
;; This section has the core stack for completion. This is the major change from my last
;; init, I was using helm and company mode extensively prior to this.
;;
;; This switch is to a core stack of: vertico, orderless (easily swappable), consult, corfu
;;
;; marginalia is included to enhance these modes, and I'm including embark on
;; the core stack as well based on the little I've used it so far.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
		     :includes (vertico-indexed
				vertico-flat
				vertico-grid
				vertico-mouse
				vertico-quick
				vertico-buffer
				vertico-repeat
				vertico-reverse
				vertico-directory
				vertico-multiform
				vertico-unobtrusive
				vertico-buffer))

  :general
  (:keymaps 'vertico-map
            ;; Vertico-directory which makes typing file paths in the minibuffer
            ;; more convenient. Use it to get a sense of what these do
            "<backspace>" #'vertico-directory-delete-char
	    "C-l" #'vertico-directory-delete-word
            "C-w" #'vertico-directory-delete-word
            "C-<backspace>" #'vertico-directory-delete-word
            "RET" #'vertico-directory-enter)
  :init
  (vertico-mode)
;;  (vertico-buffer-mode)
  (vertico-multiform-mode)

  (setq vertico-multiform-commands
	'((consult-imenu buffer indexed)))

  (setq vertico-multiform-categories
	'(;;(file grid)
	  (consult-grep buffer)))

  :custom
  ;; Show more candidates
  (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize nil)

  :config
  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
		(setq cand (funcall orig cand prefix suffix index _start))
		(concat
		 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
		 cand)))

  )


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :init
  (savehist-mode))

;; A few more useful configurations (I got this from a blog, I've never seen
;; use-package emacs before TODO)
(use-package emacs
  :straight t
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


;; Completion style
;; Trying orderless, but potentially may try prescient

(use-package orderless
  :straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; Works well with vertico
(use-package marginalia
  :straight t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; Example configuration for Consult TODO go through these bindings figure out
;; what they do and whether or not I want them potentially group consult
;; commands under a single key leader so I can get to what I want with which-key
;; I've already commented out the M-s ones due to an error from use-package
(use-package consult
  :straight
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ;;("M-s d" . consult-find)
         ;;("M-s D" . consult-locate)
         ;;("M-s g" . consult-grep)
         ;;("M-s G" . consult-git-grep)
         ;;("M-s r" . consult-ripgrep)
         ;;("M-s l" . consult-line)
         ;;("M-s L" . consult-line-multi)
         ;;("M-s k" . consult-keep-lines)
         ;;("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;;("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; KB based right click context menus sign me up.. TODO dig deeper into embark
;; to learn the more sophisticated uses
(use-package embark
  :straight t
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Provide cap with a popup
(use-package corfu
  :straight t
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
   (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary 'separator)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)    ;; Disable candidate preselectino
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
   :init
   (global-corfu-mode))

(use-package cape
  :straight t
  :defer 10
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  ;; taken from the cape configurations
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))


;; A few more useful configurations...
(use-package emacs
  :straight t
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tree Sitter and Combobulate
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Attempt at pointing to the manually built tree-sitter grammar
;; if this works it can probably be moved into the treesitter use-package config
(setq treesit-extra-load-path '( "~/development/sogaiu/tree-sitter-clojure/dist"))

;; crashes emacs
(use-package clojure-ts-mode
  :straight (clojure-ts-mode
             :type git
             :host github
             :repo "clojure-emacs/clojure-ts-mode"))

;; do we need to use package if treesitter is already built in?
(use-package treesit
  :straight (treesit
             :type git
             :host github
             :repo "tree-sitter/tree-sitter")
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional. Combobulate works in both xxxx-ts-modes and
  ;; non-ts-modes.

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (mp-setup-install-grammars))

(use-package combobulate
  :straight (combobulate
             :type git
             :host github
             :repo "mickeynp/combobulate")
    :custom
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (combobulate-key-prefix "C-c o")
    :hook ((prog-mode . combobulate-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CSV
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package csv-mode
  :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tree Sitter and Combobulate
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package kubed
  :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EJC SQL
;;
;; Use jdbc to connect to dbs from emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ejc-sql
  :straight t

  :commands
  (ejc-create-connection
   ejc-connect
   ejc-set-column-width-limit
   ejc-set-max-rows)

  :init
  (setq ejc-set-rows-limit 1000
        ejc-result-table-imple 'orgtbl-mode
        ejc-use-flx t
        ejc-flx-threshold 3
        nrepl-sync-request-timeout 30)

  ;; auto complete
  ;; (add-hook 'ejc-sql-minor-mode-hook
  ;;           (lambda ()
  ;;             (auto-complete-mode t)
  ;;             (ejc-ac-setup)))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AWS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; not working yet
(use-package aws-mode
  :straight (aws-mode
             :type git
             :host github
             :repo "snowiow/aws.el")
  :custom
  (aws-vault nil)
  (aws-output "json"))

(use-package s3ed
  :straight t)

(use-package axe
  :straight t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Terminal Emulation (trial)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; God Mode (trial)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package god-mode
;;   :straight t
;;   :bind
;;   (("<escape>" . god-local-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Trying to avoid the filename too long issue
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun my-shorten-auto-save-file-name (&rest args)
  (let ((buffer-file-name
         (when buffer-file-name (sha1 buffer-file-name))))
    (apply args)))

(advice-add 'make-auto-save-file-name :around
            #'my-shorten-auto-save-file-name)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Coding Config
;;
;; This section should have the bulk of the config for languages, starting with
;; some cross language stuff then the specific language packages
;;
;; As of right now I'm trying out eglot instead of lsp-mode, if I switch back
;; LSP will be set up and configured here as well
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (use-package lsp-mode
;;   :straight t
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook
;;   (clojure-mode . lsp)
;;   (lsp-mode . lsp-enable-which-key-integration)
;;   :commands lsp)

;; (use-package lsp-ui
;;   :straight t
;;   :commands lsp-ui)

;; (use-package lsp-treemacs
;;   :straight t
;;   :commands lsp-treemacs
;;   :init
;;   (setq lsp-treemacs-sync-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Experimental gtel setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gptel
  :straight t
  :config
  (setq
   gptel-model 'qwen2.5-coder
   gptel-default-mode 'org-mode
   gptel-backend (gptel-make-ollama "Ollama"
                                    :host "localhost:11434"
                                    :stream t
                                    :models '(qwen2.5-coder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Coding Config
;;
;; This section should have the bulk of the config for languages, starting with
;; some cross language stuff then the specific language packages
;;
;; This is the LSP setup since I'm still not sure which I want to go with...
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this was the first attempt, completions weren't working quite right
;; (use-package lsp-mode
;;   :straight t
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setq lsp-keymap-precix "C-c l")
;;   (setq read-process-output-max (* 1024 1024))
;;   :custom
;;   (lsp-idle-delay 0.6)
;;   :config
;;   (lsp-enable-which-key-integration)
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package lsp-ui
;;   :straight t
;;   :commands lsp-ui-mode)


;; (set lsp-completion-provider :none)
;; (defun corfu-lsp-setup ()
;;   (setq-local completion-styles '(orderless)
;;               completion-category-defaults nil))

;; (add-hook 'lsp-mode-hook #'corfu-lsp-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General setup
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smart-shift
  :straight (smart-shift :type git :host github :repo "hbin/smart-shift"))

(use-package dtrt-indent
  :straight (dtrt-indent :type git :host github :repo "jscheid/dtrt-indent"))

(use-package smartparens
  :straight t)

(require 'smartparens-config)

;; I can't seem to get the hang of turning this on for lisps all the time but I
;; intend to use bits of the package (like lispy-avy)
(use-package lispy
  :straight t)

;; (eval-after-load "lispy"
;;   '(progn
;;      ;; I'd like square brackets to do their standard insert
;;      (define-key lispy-mode-map (kbd "C-[") 'lispy-backward)
;;      (define-key lispy-mode-map (kbd "C-]") 'lispy-forward)))

;; temporary, I should either bind this in the modes I want or make another attempt at using lispy
(global-set-key (kbd "C-c C-d") 'lispy-describe-inline)

(use-package repl-toggle
  :straight (repl-toggle :type git :host github :repo "tomterl/repl-toggle")
  :init
  (setq rtog/fullscreen nil)
  (setq rtog/mode-repl-alist '((emacs-lisp-mode . ielm))))

;; why do I have this? what problem did it solve?
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cli2eli
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cli2eli
  :straight (cli2eli :type git :host github :repo "nohzafk/cli2eli" :branch "main" ))

(require 'cli2eli)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Common Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sly
  :straight t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl"))

(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'smartparens-mode)
(add-hook 'lisp-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook 'lisp-mode-hook #'eldoc-mode)
(add-hook 'lisp-mode-hook #'subword-mode)

(use-package sly-overlay
  :straight (sly-overlay
             :type git
             :host github
             :repo "jgerman/sly-overlay"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'turn-on-smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'subword-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Clojure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-)") 'sp-forward-slurp-sexp)
(global-set-key (kbd "M-s") 'sp-splice-sexp)
(use-package clojure-mode
  :straight t
  :config
  (add-hook 'clojure-mode-hook #'smartparens-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'turn-on-smartparens-mode)
  (add-hook 'clojure-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'clojure-mode-hook #'eglot-ensure)
  ;;(add-hook 'clojure-mode-hook #'lispy-mode)
 :bind (("C-!" . lispy-ace-paren)
       ("C-c l l" . lispy-ace-paren)
       ("C-c l g" . avy-goto-line)

       ("C-c l i" . consult-imenu)
       ("C-c l r" . raise-sexp)
       ("C-c l s" . sp-forward-slurp-sexp)
       ("C-c l b" . sp-forward-barf-sexp)
       ("C-c l n" . eglot-rename)
       ("C-c l a" . eglot-code-actions)
    ("C-c p s g" . consult-ripgrep)))

(use-package cider
  :straight t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'smartparens-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'turn-on-smartparens-strict-mode))

;; is this even necessary anymore with eglot/lsp?
(use-package clj-refactor
  :straight t)

(defun my-clj-refactor-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

  (add-hook 'clojure-mode-hook #'my-clj-refactor-hook)

;; set babashka files to open in clojure mode
(add-to-list 'auto-mode-alist '("\\.bb\\'" . clojure-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; cider-storm test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package cider-storm
;;  :straight (cider-storem :type git :host github :repo "jpmonettas/cider-storm"))
;;(define-key cider-mode-map (kbd "C-c C-f") 'cider-storm-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Schemes
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package geiser-guile
  :straight t)

(use-package geiser-mit
  :straight t)

(use-package geiser-racket
  :straight t)

(use-package geiser
  :straight t
  :custom
  (geiser-active-implementations '(racket guile mit))
  (geiser-set-default-implementation 'mit))

;;(use-package racket-mode
;;  :straight t)

;;(add-hook 'racket-mode-hook #'smartparens-mode)
;;(add-hook 'racket-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'racket-mode-hook #' turn-on-smartparens-strict-mode)

(add-hook 'geiser-mode-hook #'smartparens-mode)
(add-hook 'geiser-mode-hook #'rainbow-delimiters-mode)
(add-hook 'geiser-mode-hook #' turn-on-smartparens-strict-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Golang - this is two jobs removed, haven't used it in forever
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LUA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lua-mode
  :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pico 8
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; not sure what we're going to do here, I've cloned the repo locally but the project
;; is 2 years stale so I may need to fork it and turn it into a proper package
;; requires lua

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Kotlin
;;
;; Virtually no setup here, my old config used lsp-mode for kotlin, I have no idea how well
;; eglot works with it. I may need to run both eglot and lsp-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package kotlin-mode
  :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Typescript
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :straight t
  :config (setq typescript-indent-level 2)
  :init
  (add-hook 'typerscript-mode-hook #'smartparens-mode)
  (add-hook 'typescript-mode-hook  #'turn-on-smartparens-strict-mode)
  (add-hook 'typescript-mode-hook (lambda ()
                                    (local-set-key (kbd "C-c C-z") 'rtog/goto-buffer-fun))))

(use-package ts-comint
  :straight t
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
              (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
              (local-set-key (kbd "C-c b") 'ts-send-buffer)
              (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
              (local-set-key (kbd "C-c l") 'ts-load-file-and-go))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Python -- next time I write python code maybe it'll be time to move this to eglot?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elpy
  :straight t
  :init
  (elpy-enable)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-virtualenv-path 'current))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rust
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rustic
  :straight t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Misc Tools and Utils
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package uuidgen
  :straight (uuidgen :type git :host github :repo "kanru/uuidgen-el"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Howardism DM Tools
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (expand-file-name "~/development/howardabrams/emacs-rpgdm"))
(add-to-list 'load-path (expand-file-name "~/development/howardabrams/emacs-ironsworn"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; OpenScad
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package scad-mode
  :straight t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Experimenting with hydras
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some sloppy crap that's mainly for my current job
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tw-project-p ()
  (string-match-p (regexp-quote "development/tradeswell") default-directory))

(defun tw-branch-git-subj ()
  (let ((prj (car (split-string (magit-get-current-branch) "\/"))))
    (concat "[" (upcase prj) "] ")))

(defun my-git-commit-insert-branch ()
  (when (tw-project-p)
    (insert (tw-branch-git-subj))))

(add-hook 'git-commit-setup-hook 'my-git-commit-insert-branch)

;;(load "~/.emacs-dbs.el")
;;(load "~/development/tradeswell/tradeswell-emacs/tradeswell.el" t)

;; I usually just do this via dmenu
;; I should namespace the functions in it and change the name
(load (locate-user-emacs-file "github.el") nil :nomessage)

(load (locate-user-emacs-file "my-projects.el") nil :nomessage)

;; (concat *install-dir* "custom.el")
(when (file-exists-p "~/.tradeswell/.onepassword.el")
  (load "~/.tradeswell/.onepassword.el" nil :nomessage))

 (when (file-exists-p (concat *development-dir* "tradeswell/tradeswell-emacs/tradeswell-dbs.el"))
   (load (concat *development-dir* "tradeswell/tradeswell-emacs/tradeswell-dbs.el"))
   (tw/create-db-connections))

;; since I'm not using helm find file adding this advice
;; TODO change this so it asks first
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Random Code that should be pulled into a package
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defun csvcut-headers ()
;;   (interactive)
;;   (let* ((csv-file (read-file-name "Select CSV: "))
;;          (buff-name (concat "*" (file-name-base csv-file) " headers*"))
;;          (buff (get-buffer-create buff-name)))
;;     buff-name))

(defun csvcut-headers ()
  (interactive)
  (let* ((csv-file (buffer-file-name))
         (buff-name (concat "*" (file-name-base csv-file) " headers*"))
         (buff (get-buffer-create buff-name))
         (headers (shell-command-to-string (concat "csvcut -n " csv-file))))
    (with-current-buffer buff-name
      (erase-buffer)
      (insert headers))
    (switch-to-buffer buff)))

(defun json-to-clojure ()
  (interactive)
  )

;; get now
(defun now ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; generate a random currency float
(defun random-digit ()
  (interactive)
  (let* ((digits "0123456789")
         (i (% (abs (random)) (length digits))))
    (substring digits i (1+ i))))

(defun random-dollar-amt ()
  (interactive)
  (insert
   (concat
    (random-digit)
    (random-digit)
    "."
    (random-digit)
    (random-digit))))

(defun copy-buffer-filename ()
  (interactive)
  (kill-new (buffer-file-name)))

;; auto repl for babel?
;; how are the ejc-sql buffers auto hidden?
(cider-jack-in '(:project-dir "/Users/jgerman/development/jgerman/emacs-utility-project/"))

(defun wrap-region-with-string ()
  "Prompt for a string and insert it at the beginning and end of the current region."
  (interactive)
  (let* ((input-str (read-string "Enter the string to wrap with: "))
         (start (region-beginning))
         (end (region-end)))
    ;; Insert the string at the beginning of the region
    (goto-char start)
    (insert input-str)

    ;; Move to the end of the region and insert the string again
    (goto-char (+ end (length input-str)))
    (insert input-str)))

;; requires jet to be on the path
;; rewrite this usihng raw elisp instead
;; select a region
;; C-u, M-| 'jet --from json --to edn --keywordize'
;; then I typically C-c <space> to reformat
(defalias 'json-region-to-edn
   (kmacro "C-u M-| j e t SPC - - f r o m SPC j s o n SPC - - t o SPC c l <backspace> <backspace> e d n SPC - - k e y w o r d i z e <return> C-M-SPC M-% , <return> <return> ! C-c SPC"))

;; solo rpgs in emacs
;; (use-package rpgdm-ironsworn
;;   :straight (:local-repo "~/development/howardabrams/emacs-rpgdm")
;;   :init
;;   (setq rpgdm-ironsworn-project (expand-file-name "~/development/howardabrams/emacs-rpgdm")))

;; (use-package emacs-ironsworn
;;   :straight (:local-repo "~/development/howardabrams/emacs-ironsworn")
;;   :init
;;   (setq rpgdm-ironsworn-project (expand-file-name "~/development/howardabrams/emacs-ironsworn")))

;; (use-package emacs-ironsworn
;;   :straight (:local-repo "~/development/howardabrams/emacs-ironsworn")
;;   :init
;;   (setq rpgdm-ironsworn-project (expand-file-name "~/development/howardabrams/emacs-ironsworn")))

;; (use-package egme
;;   :straight (egme :type git :host github :repo "CategoryQ/EmacsGME"))
