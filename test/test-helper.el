;; Don't load old byte-compiled versions!
(setq load-prefer-newer t)

;; Load drdv-2048
(require 'f)
(add-to-list 'load-path (f-parent (f-dirname load-file-name)))
(require 'drdv-2048)
