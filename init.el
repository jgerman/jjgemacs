;; loading straight in settings doesn't work because I want the latest version
;; of org, and by the time I get to settings I've already required org so we
;; will setup straight and require org here prior to the tangling the
;; settings.org file


;; early init handles disabling the built in package management
;;

;; bootstrap straight

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

;; we also need to pull in org mode now

(use-package org :straight t)

;;(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
		   user-emacs-directory))
