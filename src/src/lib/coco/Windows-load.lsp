
;; Directory of CoCo ('COCOHOME'):
;; (setf *cocohome* "/usr/lib/R/library/CoCo")

;; Directory of object-file (now 'COCOLIB'):
(setf *cocolib* "/usr/lib/R/library/CoCo/lib/coco")

;; Directory of lsp-files (now 'XCOCOLIB'):
(setf *xlisp-cocolib* "/usr/lib/R/library/CoCo/lsp")

;; Name of object-file, with extension:
(setf *coco-object-name* "wXcoco.dll")

(expand 5)

(load (concatenate 'string *xlisp-cocolib* "/cocofiles.lsp"))

(load (concatenate 'string *xlisp-cocolib* "/tail-windows.lsp"))

