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

(defun mp/new-lein-project ()
  (interactive)
  (let* ((org (read-string "Org? [jgerman]: " nil  nil "jgerman"))
         (project (read-string "Project?: "))
         (templates (read-string "Templates?: "  nil nil ""))
         (location (concat *base-dir* "/" org "/"))
         (default-directory location))
    (shell-command (concat "lein new " templates " " project))
    (find-file (concat location project "/project.clj"))))

;; Uses deps-new installed as a tool:
;; https://github.com/seancorfield/deps-new
(defun mp/new-clj-project ()
  (interactive)
  (let* ((org (read-string "Org? [jgerman]: " nil  nil "jgerman"))
         (location (concat *base-dir* "/" org "/"))
         (project (read-string "Project?: "))
         (prj-name (concat org "/" project))
         (template (completing-read "Alias? "
                                    '("app" "lib" "scratch")))
         (default-directory location))
    (shell-command (concat "clojure -Tnew " template " :name " prj-name))
    (find-file (concat location project "/deps.edn"))))


(defun mp/new-rust-project ()
  (interactive)
  (let* ((org (read-string "Org? [jgerman]: " nil  nil "jgerman"))
         (project (read-string "Project? "))
         (location (concat *base-dir* "/" org "/" project)))
    (shell-command (concat "cargo new " location))
    (find-file (concat location "/src/main.rs"))))

(provide 'my-projects)
