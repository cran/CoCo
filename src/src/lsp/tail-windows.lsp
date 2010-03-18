
(load (concatenate 'string *xlisp-cocolib* "/oldcfun.lsp"))

(load (concatenate 'string *xlisp-cocolib* "/cocoshlib.lsp"))

(load (concatenate 'string *xlisp-cocolib* "/fix-windows.lsp"))

(defun get-lib-and-tmp ()
  (let ((result (concatenate 'string "@" *cocolib* "@"
			     "C:\\windows\\TEMP" "@")))
			     ;;; "C:\\COMPILERS\\Cygwin\\tmp"
    result)
  )

(defun get-lib-and-tmp ()
  (let ((result (concatenate 'string "@" *cocolib* "@"
			     ;;; "C:\\windows\\TEMP" "@"
			     "C:\\Programmer\\Cygwin\\tmp"
			     )))
    result)
  )
