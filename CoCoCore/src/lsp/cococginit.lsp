(defun default-cococg-init ()
 (let ((default-file (concatenate 'string *xlisp-cocolib*
                      "/" "Examples/TestMips.lsp")))
  (if (ok-or-cancel-dialog
       (concatenate 'string 
        "Welcome to Xlisp+CoCoCg. ~%"
        " ~%"
        "No cococginit.lsp file found, using default example. ~%"
        "Copy the default example lsp-file ~%"
        default-file "~%"
        "to cococginit.lsp (.cococginit.lsp) in the current directory ~%"
        "or (cococginit.lsp) .cococginit.lsp in your home-directory, ~%"
        "edit the resulting file, and restart xlisp+coco. ~%"
        "In this ordered list of the four (.)-cococginit.lsp files ~%"
        "the first found will be loaded when starting xlisp+coco. ~%"
        "Or 'touch cococginit.lsp' (to make the empty file) and ~%"
        "avoid this dialog box and loading of the default example ~%"
        "when starting xlisp+coco. ~%"
        ))
   (load default-file))))

;; (trace send)

; (trace return-edge-list)
; (trace split-string)
; (trace to-string)
; (trace return-default-vertices)
; (trace :return-names)
; (trace :return-name-list)
; (trace :return-name-list-string)

(default-cococg-init)

; (load "TestMips")
