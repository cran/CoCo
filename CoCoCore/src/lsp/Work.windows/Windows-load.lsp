
;; Directory of CoCo ('COCOHOME'):
;; (setf *cocohome* "C:/Programmer/R/rw1061/library/CoCo")

;; Directory of object-file (now 'COCOLIB'):
(setf *cocolib* "C:/Programmer/R/rw1061/library/CoCo/lib/coco")

;; Directory of lsp-files (now 'XCOCOLIB'):
(setf *xlisp-cocolib* "C:/Programmer/R/rw1061/library/CoCo/lsp")

;; Name of object-file, with extension:
(setf *coco-object-name* "wXcoco.dll")

;; Name of entry point:
(setf *coco-entry-name* "CoCo")

(expand 5)

(load (concatenate 'string *xlisp-cocolib* "/cocofiles.lsp"))

(load (concatenate 'string *xlisp-cocolib* "/tail-windows.lsp"))

