
;;; Get some room, allocate 2000 segments:

(expand 200)

;;; `require' is not working?
;;; Neither with this:

;;; (def *default-path* (concatenate 'string *xlisp-cocolib* "/src/"))


;;; Load interface function for CoCo:

(if (not (boundp '*coco-apix-version*)) ;; cocoinit.lsp
    (progn
      (load (concatenate 'string *xlisp-cocolib* "/src/" "cocofunc.lsp"))
      (load (concatenate 'string *xlisp-cocolib* "/src/" "cocoapix.lsp"))
      (load (concatenate 'string *xlisp-cocolib* "/src/" "cocoinit.lsp"))
      ))


;;; Load the CoCo object prototype and its methods:

(if (not (boundp '*coco-proto-version*)) ;; cocometh.lsp
    (load (concatenate 'string *xlisp-cocolib* "/src/" "cocometh.lsp"))
  )


;;; Load the CoCo graph stuffs:

(if (not (boundp '*coco-graph-window-proto-version*)) ;; cocograph.lsp
    (progn
     (load (concatenate 'string *xlisp-cocolib* "/src/" "independence.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/" "draggraph.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "vertices.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "edges.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "tests.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "blocks.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/" "assocdiag.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/" "cocograph.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocomenu.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocodialogs.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocooptions.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocokey.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocotest.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocomanager.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"     "plotcontrols.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"     "toggle.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cococontrols.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "graphtotex.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocodyns.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "factorgraph.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/" "resampling.lsp"))
     (load (concatenate 'string *xlisp-cocolib* "/src/" "split.lsp"))
     ))
