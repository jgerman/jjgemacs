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
        (message "Project has already been cloned.")
      (shell-command (concat "git clone git@github.com:" repo " " clone-dir)))))
(provide 'github)
