
;;; Get some room, allocate 2000 segments:

(expand 200)

;;; `require' is not working?
;;; Neither with this:

;;; (def *default-path* (concatenate 'string *xlisp-cocolib* "/src/"))


;;; Load interface function for CoCo:

(if (not (boundp '*coco-apix-version*)) ;; cocoinit.lsp
    (progn
      (load (concatenate 'string *xlisp-cocolib* "/src/" "cocofunc"))
      (load (concatenate 'string *xlisp-cocolib* "/src/" "cocoapix"))
      (load (concatenate 'string *xlisp-cocolib* "/src/" "cocoinit"))
      ))


;;; Load the CoCo object prototype and its methods:

(if (not (boundp '*coco-proto-version*)) ;; cocometh.lsp
    (load (concatenate 'string *xlisp-cocolib* "/src/" "cocometh"))
  )


;;; Load the CoCo graph stuffs:

(if (not (boundp '*coco-graph-window-proto-version*)) ;; cocograph.lsp
    (progn
     (load (concatenate 'string *xlisp-cocolib* "/src/" "independence"))
     (load (concatenate 'string *xlisp-cocolib* "/src/" "draggraph"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "vertices"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "edges"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "tests"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "blocks"))
     (load (concatenate 'string *xlisp-cocolib* "/src/" "assocdiag"))
     (load (concatenate 'string *xlisp-cocolib* "/src/" "cocograph"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocomenu"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocodialogs"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocooptions"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocokey"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocotest"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocomanager"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"     "plotcontrols"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"     "toggle"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cococontrols"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "graphtotex"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "cocodyns"))
     (load (concatenate 'string *xlisp-cocolib* "/src/"   "factorgraph"))
     (load (concatenate 'string *xlisp-cocolib* "/src/" "resampling"))
     (load (concatenate 'string *xlisp-cocolib* "/src/" "split"))
     ))
