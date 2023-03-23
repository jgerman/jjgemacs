(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1aa4243143f6c9f2a51ff173221f4fd23a1719f4194df6cef8878e75d349613d" "c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" default))
 '(eldoc-echo-area-use-multiline-p nil)
 '(eldoc-minor-mode-string nil)
 '(global-corfu-mode t)
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   '((cider-clojure-cli-global-options . -A:env/repl:test)
     (cider-clojure-cli-global-options . -A:dev:test)
     (cider-clojure-cli-global-options . -A:dev)
     (cider-clojure-cli-global-options . -A:env/repl)
     (cider-clojure-cli-global-options . -A:env/test:env/dev)
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(corfu-current ((t (:background "dim gray" :foreground "#bbc2cf")))))
