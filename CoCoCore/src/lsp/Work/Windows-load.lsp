
;; Directory of CoCo ('COCOHOME'):
;; (setf *cocohome* "C:/COMPILERS/Cygwin/home/jhb/lib/CoCo.1.5.1")

;; Directory of object-file (now 'COCOLIB'):
(setf *cocolib* "C:/COMPILERS/Cygwin/home/jhb/lib/CoCo.1.5.1/lib/coco")

;; Directory of lsp-files (now 'XCOCOLIB'):
(setf *xlisp-cocolib* "C:/COMPILERS/Cygwin/home/jhb/lib/CoCo.1.5.1/lsp")

;; Name of object-file, with extension:
(setf *coco-object-name* "wXcoco")

(expand 5)

(load (concatenate 'string *xlisp-cocolib* "/cocofiles.lsp"))

(load (concatenate 'string *xlisp-cocolib* "/tail-windows.lsp"))

