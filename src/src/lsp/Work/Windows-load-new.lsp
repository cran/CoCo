
;; Directory of CoCo ('COCOHOME'):
;; (setf *cocohome* "C:/COMPILERS/Cygwin/home/jhb/CoCo/source.2002.okt.18")

;; Directory of object-file (now 'COCOLIB'):
(setf *cocolib* "C:/COMPILERS/Cygwin/home/jhb/CoCo/source.2002.okt.18/lib/coco")

;; Directory of lsp-files (now 'XCOCOLIB'):
(setf *xlisp-cocolib* "C:/COMPILERS/Cygwin/home/jhb/CoCo/source.2002.okt.18/lsp")

;; Name of object-file, with extension:
(setf *coco-object-name* "wXcoco")

(expand 5)

(load (concatenate 'string *xlisp-cocolib* "/cocofiles.lsp"))

(load (concatenate 'string *xlisp-cocolib* "/tail-windows.lsp"))

