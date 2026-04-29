;;; date-picker.el --- Insert ISO 8601 date via calendar -*- lexical-binding: t; -*-

(require 'calendar)

(defvar jg/date-picker--buffer nil
  "Buffer where the selected date should be inserted.")

(defun jg/date-picker--insert-date-with-time (time-str)
  "Insert the date under cursor in ISO 8601 format with TIME-STR into the original buffer."
  (let ((date (calendar-cursor-to-date t)))
    (when date
      (let* ((month (nth 0 date))
             (day (nth 1 date))
             (year (nth 2 date))
             (formatted (format "%04d-%02d-%02dT%sZ" year month day time-str))
             (buf jg/date-picker--buffer))
        (calendar-exit)
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (insert formatted)))))))

(defun jg/date-picker--insert-date ()
  "Insert the date under cursor in ISO 8601 format (start of day)."
  (interactive)
  (jg/date-picker--insert-date-with-time "00:00:00"))

(defun jg/date-picker--insert-date-eod ()
  "Insert the date under cursor in ISO 8601 format (end of day)."
  (interactive)
  (jg/date-picker--insert-date-with-time "23:59:59"))

(defun jg/date-picker ()
  "Open the calendar and insert a selected date in ISO 8601 format."
  (interactive)
  (setq jg/date-picker--buffer (current-buffer))
  (calendar)
  (local-set-key (kbd "RET") #'jg/date-picker--insert-date)
  (local-set-key (kbd "M-RET") #'jg/date-picker--insert-date-eod))

(provide 'date-picker)
;;; date-picker.el ends here
