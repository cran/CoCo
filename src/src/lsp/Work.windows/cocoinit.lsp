(defun default-coco-init ()
 (let ((default-file (concatenate 'string *xlisp-cocolib*
                      "/" "Examples/TestGraph.lsp")))
  (if (ok-or-cancel-dialog
       (concatenate 'string 
        "Welcome to Xlisp+CoCo. ~%"
        " ~%"
        "No cocoinit.lsp file found, using default example. ~%"
        "Copy the default example lsp-file ~%"
        default-file "~%"
        "to cocoinit.lsp (.cocoinit.lsp) in the current directory ~%"
        "or (cocoinit.lsp) .cocoinit.lsp in your home-directory, ~%"
        "edit the resulting file, and restart xlisp+coco. ~%"
        "In this ordered list of the four (.)-cocoinit.lsp files ~%"
        "the first found will be loaded when starting xlisp+coco. ~%"
        "Or 'touch cocoinit.lsp' (to make the empty file) and ~%"
        "avoid this dialog box and loading of the default example ~%"
        "when starting xlisp+coco. ~%"
        ))
   (load default-file))))

(trace return-edge-list)
(trace split-string)
(trace split-string-chr)
(trace to-string)
(trace return-default-vertices)
(trace :return-names)
(trace :return-name-list)
(trace :return-name-list-string)

(default-coco-init)

