;;; mode-visual-bell.el --- Visual indicators for active minor modes -*- lexical-binding: t; -*-

;; Provides a header-line banner + cursor change to make it obvious
;; when a specific minor mode is active in a buffer.
;;
;; Usage:
;;
;;   (mode-visual-bell-register 'my-minor-mode
;;     :label "MY MODE"
;;     :bg "#4a0020"
;;     :fg "#ff6699"
;;     :cursor-color "#ff6699"
;;     :cursor-type 'box)
;;
;; This sets up hooks so that whenever `my-minor-mode' is toggled on,
;; the buffer gets a colored header line and a distinct cursor.  When
;; the mode is toggled off, everything reverts.
;;
;; Multiple modes can be registered independently.

(defvar-local mode-visual-bell--saved-header-line nil
  "Saved `header-line-format' to restore when the mode is deactivated.")

(defvar-local mode-visual-bell--saved-cursor-color nil
  "Saved cursor color to restore when the mode is deactivated.")

(defvar-local mode-visual-bell--saved-cursor-type nil
  "Saved `cursor-type' to restore when the mode is deactivated.")

(defvar-local mode-visual-bell--active-mode nil
  "The mode symbol currently driving the visual indicator in this buffer.")

(defun mode-visual-bell--activate (mode label bg fg cursor-color cursor-type)
  "Activate visual indicators for MODE in the current buffer."
  (setq mode-visual-bell--saved-header-line header-line-format
        mode-visual-bell--saved-cursor-color (face-background 'cursor nil t)
        mode-visual-bell--saved-cursor-type cursor-type
        mode-visual-bell--active-mode mode)
  (setq header-line-format
        (propertize (format " %s " label)
                    'face `(:background ,bg :foreground ,fg :weight bold)))
  (when cursor-color
    (set-face-attribute 'cursor (selected-frame) :background cursor-color))
  (when cursor-type
    (setq-local cursor-type cursor-type)))

(defun mode-visual-bell--deactivate ()
  "Remove visual indicators and restore prior state in the current buffer."
  (setq header-line-format mode-visual-bell--saved-header-line)
  (when mode-visual-bell--saved-cursor-color
    (set-face-attribute 'cursor (selected-frame)
                        :background mode-visual-bell--saved-cursor-color))
  (when mode-visual-bell--saved-cursor-type
    (setq-local cursor-type mode-visual-bell--saved-cursor-type))
  (setq mode-visual-bell--active-mode nil))

(defun mode-visual-bell--make-hook (mode label bg fg cursor-color cursor-type)
  "Return a function that toggles visual indicators based on MODE's state."
  (lambda ()
    (if (symbol-value mode)
        (mode-visual-bell--activate mode label bg fg cursor-color cursor-type)
      (mode-visual-bell--deactivate))))

;;;###autoload
(defun mode-visual-bell-register (mode &rest plist)
  "Register visual indicators for minor MODE.

PLIST accepts the following keys:
  :label        — String shown in the header line (default: mode name uppercased)
  :bg           — Background color for the header line (default: \"#4a0020\")
  :fg           — Foreground color for the header line (default: \"#ff6699\")
  :cursor-color — Cursor color when mode is active (default: same as :fg)
  :cursor-type  — Cursor type when mode is active (default: box)"
  (let* ((label        (or (plist-get plist :label)
                           (upcase (replace-regexp-in-string
                                    "-mode\\'" ""
                                    (symbol-name mode)))))
         (bg           (or (plist-get plist :bg) "#4a0020"))
         (fg           (or (plist-get plist :fg) "#ff6699"))
         (cursor-color (or (plist-get plist :cursor-color) fg))
         (cursor-type  (or (plist-get plist :cursor-type) 'box))
         (hook-sym     (intern (format "%s-hook" mode)))
         (hook-fn      (mode-visual-bell--make-hook
                        mode label bg fg cursor-color cursor-type)))
    (add-hook hook-sym hook-fn)))

(provide 'mode-visual-bell)

;;; mode-visual-bell.el ends here
