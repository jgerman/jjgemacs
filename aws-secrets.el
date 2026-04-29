;;; aws-secrets.el --- Browse AWS Secrets Manager secrets -*- lexical-binding: t; -*-

;; Browse and inspect AWS Secrets Manager secrets from Emacs.
;; Requires the AWS CLI (`aws`) to be installed and configured.
;;
;; Usage:
;;   M-x aws-secrets/browse   - List secrets, select one, view its values
;;   M-x aws-secrets/clear-cache - Clear cached secrets list and values
;;
;; Buffer keybindings:
;;   n / TAB / <down>   - Move to next value
;;   p / S-TAB / <up>   - Move to previous value
;;   c                  - Copy value at point to kill ring
;;   j                  - Copy entire secret as JSON to kill ring
;;   e                  - Copy entire secret as EDN (Clojure map) to kill ring
;;   q                  - Quit buffer
;;
;; Customization (M-x customize-group RET aws-secrets):
;;   `aws-secrets-quit-after-copy'     - Quit buffer after copy (default: t)
;;   `aws-secrets-secrets-list-ttl'    - Cache TTL for secrets list in seconds (default: 300)
;;   `aws-secrets-secret-value-ttl'    - Cache TTL for secret values in seconds (default: 300)
;;   `aws-secrets-edn-kebab-case'      - Convert keys to kebab-case in EDN output (default: nil)
;;                                       NOTE: kebab-case conversion is currently broken
;;                                       and disabled by default.
;;
;; Faces:
;;   `aws-secrets-title-face'     - Secret name header (blue, bold)
;;   `aws-secrets-separator-face' - Separator lines (blue)
;;   `aws-secrets-key-face'       - Key names (green)

(require 'json)

(defvar-local aws-secrets--buffer-json nil
  "The raw secret alist for the current buffer, used for JSON copy.")

(defvar-local aws-secrets--value-column nil
  "Column where values start, used to keep cursor aligned.")

(defcustom aws-secrets-quit-after-copy t
  "When non-nil, quit the secret buffer after copying a value."
  :type 'boolean
  :group 'aws-secrets)

(defcustom aws-secrets-secrets-list-ttl 300
  "Time in seconds to cache the secrets list.
When non-nil, reuse the cached list if it is younger than this many seconds.
When nil, always fetch a fresh list."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Always refresh" nil))
  :group 'aws-secrets)

(defcustom aws-secrets-secret-value-ttl 300
  "Time in seconds to cache a retrieved secret value.
When non-nil, reuse the cached value if the same secret is requested
within this many seconds.  When nil, always fetch fresh."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Always refresh" nil))
  :group 'aws-secrets)

(defcustom aws-secrets-edn-kebab-case nil
  "When non-nil, convert secret keys to kebab-case in EDN output.
For example, DB_HOST becomes db-host."
  :type 'boolean
  :group 'aws-secrets)

(defface aws-secrets-title-face
  '((t :foreground "cornflower blue" :weight bold))
  "Face for the secret name title.")

(defface aws-secrets-separator-face
  '((t :foreground "cornflower blue"))
  "Face for separator lines.")

(defface aws-secrets-key-face
  '((t :foreground "green"))
  "Face for secret key names.")

(defvar aws-secrets-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `aws-secrets-mode'.")

(defun aws-secrets--quit ()
  "Kill the current AWS Secrets buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(define-derived-mode aws-secrets-mode special-mode "AWS-Secrets"
  "Major mode for viewing AWS Secrets Manager secret values.

\\{aws-secrets-mode-map}"
  (define-key aws-secrets-mode-map (kbd "q") #'aws-secrets--quit)
  (define-key aws-secrets-mode-map (kbd "TAB") #'aws-secrets--next-value)
  (define-key aws-secrets-mode-map (kbd "<backtab>") #'aws-secrets--prev-value)
  (define-key aws-secrets-mode-map (kbd "n") #'aws-secrets--next-value)
  (define-key aws-secrets-mode-map (kbd "p") #'aws-secrets--prev-value)
  (define-key aws-secrets-mode-map (kbd "<down>") #'aws-secrets--next-value)
  (define-key aws-secrets-mode-map (kbd "<up>") #'aws-secrets--prev-value)
  (define-key aws-secrets-mode-map (kbd "c") #'aws-secrets--copy-current-value)
  (define-key aws-secrets-mode-map (kbd "j") #'aws-secrets--copy-as-json)
  (define-key aws-secrets-mode-map (kbd "e") #'aws-secrets--copy-as-edn))

(defun aws-secrets--run-cli (args)
  "Run aws ARGS via shell, returning raw output.
Stderr is redirected to /dev/null."
  (shell-command-to-string (format "aws %s 2>/dev/null" args)))

(defvar aws-secrets--list-cache nil
  "Cached list of secret names.")

(defvar aws-secrets--list-cache-time nil
  "Time when `aws-secrets--list-cache' was last populated.")

(defun aws-secrets--list-secret-names ()
  "Return a list of secret name strings from AWS Secrets Manager.
Uses cached results if `aws-secrets-secrets-list-ttl' is set and the
cache is still fresh."
  (if (and aws-secrets-secrets-list-ttl
           aws-secrets--list-cache
           aws-secrets--list-cache-time
           (< (float-time (time-subtract (current-time) aws-secrets--list-cache-time))
              aws-secrets-secrets-list-ttl))
      aws-secrets--list-cache
    (condition-case err
        (let* ((output (aws-secrets--run-cli "secretsmanager list-secrets"))
               (json (json-read-from-string output))
               (secrets (alist-get 'SecretList json))
               (names (mapcar (lambda (s) (alist-get 'Name s)) secrets)))
          (setq aws-secrets--list-cache names
                aws-secrets--list-cache-time (current-time))
          names)
      (error (user-error "Failed to list secrets: %s" (error-message-string err))))))

(defun aws-secrets/clear-cache ()
  "Clear the secrets list and secret value caches."
  (interactive)
  (setq aws-secrets--list-cache nil
        aws-secrets--list-cache-time nil
        aws-secrets--value-cache nil
        aws-secrets--value-cache-time nil)
  (message "AWS Secrets caches cleared"))

(defvar aws-secrets--value-cache nil
  "Cached secret value as (name . parsed-value).")

(defvar aws-secrets--value-cache-time nil
  "Time when `aws-secrets--value-cache' was last populated.")

(defun aws-secrets--get-secret-value (secret-name)
  "Fetch the secret value for SECRET-NAME.
Returns a parsed alist if the SecretString is JSON, or the raw string otherwise.
Uses cached result if `aws-secrets-secret-value-ttl' is set and the cache
is fresh and matches SECRET-NAME."
  (if (and aws-secrets-secret-value-ttl
           aws-secrets--value-cache
           aws-secrets--value-cache-time
           (string= secret-name (car aws-secrets--value-cache))
           (< (float-time (time-subtract (current-time) aws-secrets--value-cache-time))
              aws-secrets-secret-value-ttl))
      (cdr aws-secrets--value-cache)
    (condition-case err
        (let* ((output (aws-secrets--run-cli
                        (format "secretsmanager get-secret-value --secret-id %s"
                                (shell-quote-argument secret-name))))
               (json (json-read-from-string output))
               (secret-string (alist-get 'SecretString json))
               (value (condition-case nil
                          (json-read-from-string secret-string)
                        (error secret-string))))
          (setq aws-secrets--value-cache (cons secret-name value)
                aws-secrets--value-cache-time (current-time))
          value)
      (error (user-error "Failed to get secret %s: %s" secret-name (error-message-string err))))))

(defun aws-secrets--insert-colored (text face)
  "Insert TEXT with FACE applied."
  (let ((start (point)))
    (insert text)
    (put-text-property start (point) 'face face)))

(defun aws-secrets--format-secret-buffer (secret-name secret-value)
  "Populate current buffer with formatted SECRET-NAME and SECRET-VALUE.
Values are tagged with text properties for navigation and copying."
  (erase-buffer)
  (aws-secrets--insert-colored (format "AWS Secret: %s\n" secret-name) 'aws-secrets-title-face)
  (aws-secrets--insert-colored "================================\n\n" 'aws-secrets-separator-face)
  (if (and (listp secret-value) (consp (car secret-value)))
      (let* ((pairs (mapcar (lambda (pair)
                              (cons (symbol-name (car pair))
                                    (format "%s" (cdr pair))))
                            secret-value))
             (max-key-len (apply #'max (mapcar (lambda (p) (length (car p))) pairs)))
             (val-col (+ 2 max-key-len 3))) ;; "  " + key + " : "
        (setq aws-secrets--value-column val-col)
        (dolist (pair pairs)
          (let ((padded-key (string-pad (car pair) max-key-len)))
            (insert "  ")
            (aws-secrets--insert-colored padded-key 'aws-secrets-key-face)
            (insert " : ")
            (let ((start (point)))
              (insert (cdr pair))
              (put-text-property start (point) 'aws-secrets-value (cdr pair)))
            (insert "\n"))))
    (setq aws-secrets--value-column 0)
    (let ((start (point)))
      (insert (format "%s" secret-value))
      (put-text-property start (point) 'aws-secrets-value (format "%s" secret-value))
      (insert "\n")))
  (aws-secrets--insert-colored "\n================================\n" 'aws-secrets-separator-face)
  (insert "n/TAB/down : next value    p/S-TAB/up : prev value\n")
  (insert "c : copy value at point    j : copy secret as JSON\n")
  (insert "e : copy secret as EDN     q : quit\n"))

(defun aws-secrets--goto-value-column ()
  "Move point to the value column on the current line, if it has a value."
  (beginning-of-line)
  (let ((col (or aws-secrets--value-column 0)))
    (move-to-column col))
  (get-text-property (point) 'aws-secrets-value))

(defun aws-secrets--next-value ()
  "Move point to the next secret value field."
  (interactive)
  (let ((found nil))
    (save-excursion
      (forward-line 1)
      (while (and (not (eobp)) (not found))
        (when (aws-secrets--goto-value-column)
          (setq found (point)))
        (unless found (forward-line 1))))
    (when found (goto-char found))))

(defun aws-secrets--prev-value ()
  "Move point to the previous secret value field."
  (interactive)
  (let ((found nil))
    (save-excursion
      (forward-line -1)
      (while (and (not (bobp)) (not found))
        (when (aws-secrets--goto-value-column)
          (setq found (point)))
        (unless found (forward-line -1))))
    (when found (goto-char found))))

(defun aws-secrets--copy-current-value ()
  "Copy the secret value at point to the kill ring."
  (interactive)
  (let ((val (get-text-property (point) 'aws-secrets-value)))
    (if val
        (progn
          (kill-new val)
          (message "Copied: %s" val)
          (when aws-secrets-quit-after-copy
            (aws-secrets--quit)))
      (message "No value at point"))))

(defun aws-secrets--copy-as-json ()
  "Copy the entire secret as JSON to the kill ring."
  (interactive)
  (if aws-secrets--buffer-json
      (let ((json-str (json-encode aws-secrets--buffer-json)))
        (kill-new json-str)
        (message "Copied secret as JSON")
        (when aws-secrets-quit-after-copy
          (aws-secrets--quit)))
    (message "No JSON data available for this secret")))

(defun aws-secrets--to-kebab-case (str)
  "Convert STR from camelCase, PascalCase, or snake_case to kebab-case."
  (downcase
   (replace-regexp-in-string
    "_" "-"
    (replace-regexp-in-string
     "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1-\\2"
     (replace-regexp-in-string
      "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1-\\2" str)))))

(defun aws-secrets--alist-to-edn (alist)
  "Convert ALIST to an EDN map string.
When `aws-secrets-edn-kebab-case' is non-nil, keys are converted to kebab-case."
  (let ((pairs (mapcar (lambda (pair)
                         (let ((key (symbol-name (car pair))))
                           (when aws-secrets-edn-kebab-case
                             (setq key (aws-secrets--to-kebab-case key)))
                           (format ":%s %S" key (cdr pair))))
                       alist)))
    (format "{%s}" (mapconcat #'identity pairs "\n "))))

(defun aws-secrets--copy-as-edn ()
  "Copy the entire secret as an EDN map to the kill ring."
  (interactive)
  (if (and aws-secrets--buffer-json (listp aws-secrets--buffer-json))
      (let ((edn-str (aws-secrets--alist-to-edn aws-secrets--buffer-json)))
        (kill-new edn-str)
        (message "Copied secret as EDN")
        (when aws-secrets-quit-after-copy
          (aws-secrets--quit)))
    (message "No structured data available for this secret")))

(defun aws-secrets/browse ()
  "Browse AWS Secrets Manager secrets.
Lists secrets with completing-read, then displays the selected secret's value."
  (interactive)
  (let* ((names (aws-secrets--list-secret-names))
         (_ (unless names (user-error "No secrets found")))
         (selected (completing-read "AWS Secret: " names nil t))
         (value (aws-secrets--get-secret-value selected))
         (buf (get-buffer-create (format "*AWS Secret: %s*" selected))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (aws-secrets-mode)
        (setq aws-secrets--buffer-json value)
        (aws-secrets--format-secret-buffer selected value)
        (goto-char (point-min))
        (aws-secrets--next-value)))
    (switch-to-buffer buf)))

(provide 'aws-secrets)
;;; aws-secrets.el ends here
