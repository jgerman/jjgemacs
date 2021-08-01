(defun github-choose-repo ()
  (interactive)
  (completing-read "Repo?: "
   (split-string
    (shell-command-to-string "github_repos.py") "\n")))

(defun github-get-config ()
  (json-parse-string
   (with-temp-buffer
     (insert-file-contents "~/.github_explorer")
     (buffer-string))))

(defun github-get-base-dir ()
  (gethash "base_dir" (github-get-config)))

(defun github-clone ()
  (interactive)
  (let* ((repo (github-choose-repo))
         (parts (split-string repo "/"))
         (org (car parts))
         (proj (cadr parts))
         (clone-dir (concat (github-get-base-dir)
                            "/"
                            (downcase org)
                            "/"
                            proj)))
    (if (file-directory-p clone-dir)
        (message "Project already exists.")
      (shell-command (concat "git clone git@github.com:" repo " " clone-dir) "*clone-output*"))))

;; (defun github-clone ()
;;   (interactive)
;;   (let* ((repo (github-choose-repo))
;;          (parts (split-string repo "/"))
;;          (org (car parts))
;;          (proj (cadr parts))
;;          (clone-dir (concat (github-get-base-dir)
;;                             "/"
;;                             (downcase org)
;;                             "/"
;;                             proj)))
;;     (if (file-directory-p clone-dir)
;;         (message "Project has already been with.")
;;       (with-output-to-temp-buffer "*clone-output*"
;;         (insert (shell-command (concat "git clone git@github.com:" repo " " clone-dir) "*clone-output*"))
;;         (pop-to-buffer "*clone-output*")
;;         (special-mode)))))
;; special mode doesn't completely kill the buffer, I'd like to do that eventually
(provide 'github)

;; this kinda works
;; (with-output-to-temp-buffer "*clone-buffer*" (print (shell-command-to-string "ls -al")))
