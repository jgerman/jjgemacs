(defvar *base-dir* (concat (getenv "HOME") "/development")) ;; really should be defined globally somewhere

(defun mp/test ()
  (interactive)
  (message *base-dir*))

(defun mp/new-project ()
  (interactive)
  (let* ((org (read-string "Org? [jgerman]: " nil  nil "jgerman"))
         (project (read-string "Project? "))
         (filename (read-string "Initial File? "))
         (location (concat *base-dir* "/" org "/" project "/" filename)))
    (make-empty-file location)
    (find-file location)))

(defun mp/new-clj-project ()
  (interactive)
  (let* ((org (read-string "Org? [jgerman]: " nil  nil "jgerman"))
         (project (read-string "Project?: "))
         (templates (read-string "Templates?: "  nil nil ""))
         (location (concat *base-dir* "/" org "/"))
         (default-directory location))
    (shell-command (concat "lein new " templates " " project))
    (find-file (concat location project "/project.clj"))))



(provide 'my-projects)
