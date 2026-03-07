;;; clojure-tap-inspector.el --- Clojure tap> inspector for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; A tap> receiver that displays tapped values in a list buffer and
;; delegates to CIDER's inspector for drill-down.

;;; Code:

(require 'cider-client)
(require 'cider-inspector)

(defvar clj-tap--init-code
  "(do
  (ns jjg.tap-inspector)
  (defonce taps (atom []))
  (defonce tap-fn
    (let [f (fn [v]
              (swap! taps conj {:value v
                                :timestamp (java.time.LocalDateTime/now)
                                :type (type v)}))]
      (add-tap f)
      f)))"
  "Clojure code to initialize the tap inspector.")

(defvar clj-tap--clear-code
  "(reset! jjg.tap-inspector/taps [])"
  "Clojure code to clear all tapped values.")

(defvar clj-tap--stop-code
  "(do (remove-tap @jjg.tap-inspector/tap-fn)
     (ns-unmap 'jjg.tap-inspector 'tap-fn)
     (ns-unmap 'jjg.tap-inspector 'taps))"
  "Clojure code to stop the tap handler and clean up.")

(defun clj-tap--eval-sync (code)
  "Evaluate Clojure CODE via nREPL and return the value string."
  (let ((response (cider-nrepl-sync-request:eval code)))
    (if (nrepl-dict-get response "err")
        (error "Tap inspector eval error: %s" (nrepl-dict-get response "err"))
      (nrepl-dict-get response "value"))))

;;;###autoload
(defun clj-tap-start ()
  "Start the tap> inspector by evaluating init code in the REPL."
  (interactive)
  (clj-tap--eval-sync clj-tap--init-code)
  (message "Tap inspector started"))

;;;###autoload
(defun clj-tap-stop ()
  "Stop the tap> inspector and remove the tap handler."
  (interactive)
  (condition-case err
      (progn
        (clj-tap--eval-sync clj-tap--stop-code)
        (message "Tap inspector stopped"))
    (error (message "Error stopping tap inspector: %s" (error-message-string err)))))

;;;###autoload
(defun clj-tap-show ()
  "Show all tapped values in CIDER's inspector."
  (interactive)
  (cider-inspect-expr "@jjg.tap-inspector/taps" "jjg.tap-inspector"))

;;;###autoload
(defun clj-tap-clear ()
  "Clear all tapped values and refresh the buffer."
  (interactive)
  (clj-tap--eval-sync clj-tap--clear-code)
  (message "Taps cleared"))

(provide 'clojure-tap-inspector)
;;; clojure-tap-inspector.el ends here
