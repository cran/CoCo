
(load "tstinit.lsp")

(print (send x :backward))

(print (send x :backward
	     :recursive T :coherent T :headlong T
	     :follow T :least-significant T :separators nil :edges T))

(print (send x :backward :sorted T
	     :recursive T :coherent T :headlong T
	     :follow T :least-significant T :separators nil :edges T))

(print (send x :backward :only T :sorted T
	     :recursive T :coherent T :headlong T
	     :follow T :least-significant T :separators nil :edges T))

(print (send x :backward :only T :reversed T :sorted T
	     :recursive T :coherent T :headlong T
	     :follow T :least-significant T :separators nil :edges T))

(print (send x :backward :only T
	     :recursive T :coherent T :headlong T
	     :follow T :least-significant T :separators nil :edges T))

(print (send x :backward :short T
	     :recursive T :coherent T :headlong T
	     :follow T :least-significant T :separators nil :edges T))


(print (send x :backward :only T :sorted T
	     :recursive T :coherent T :headlong T
	     :follow nil :least-significant T :separators nil :edges T))

(print (send x :backward :only T :sorted T
	     :recursive T :coherent nil :headlong nil
	     :follow T :least-significant T :separators nil :edges T))

(print (send x :backward :only T :sorted T
	     :recursive T :coherent T :headlong nil
	     :follow T :least-significant T :separators nil :edges T))

(print (send x :backward :only T :sorted T
	     :recursive T :coherent nil :headlong T
	     :follow T :least-significant T :separators nil :edges T))


(print (send x :backward :only T :sorted T
	     :recursive T :coherent nil :headlong nil
	     :follow T :least-significant nil :separators nil :edges T))

(print (send x :backward :only T :sorted T
	     :recursive T :coherent T :headlong T
	     :follow T :least-significant nil :separators nil :edges T))


(print (send x :backward :only T :sorted T
	     :recursive T :coherent T :headlong T
	     :follow T :least-significant T :separators nil
	     :edges 'interactions))

(print (send x :backward :only T :sorted T
	     :recursive T :coherent T :headlong T
	     :follow T :least-significant T :separators T :edges T))

(def model-4 (send reinis-coco-object :make-model "."))

(print (send x :forward))

(print (send x :forward
	     :recursive T :coherent T :headlong T
	     :all-significant T :separators nil :edges T))

(print (send x :forward :sorted T
	     :recursive T :coherent T :headlong T
	     :all-significant T :separators nil :edges T))

(print (send x :forward :only T :sorted T
	     :recursive T :coherent T :headlong T
	     :all-significant T :separators nil :edges T))

(print (send x :forward :only T :reversed T :sorted T
	     :recursive T :coherent T :headlong T
	     :all-significant T :separators nil :edges T))

(print (send x :forward :only T
	     :recursive T :coherent T :headlong T
	     :all-significant T :separators nil :edges T))

(print (send x :forward :short T
	     :recursive T :coherent T :headlong T
	     :all-significant T :separators nil :edges T))


(print (send x :forward :only T :sorted T
	     :recursive T :coherent nil :headlong nil
	     :all-significant T :separators nil :edges T))

(print (send x :forward :only T :sorted T
	     :recursive T :coherent T :headlong nil
	     :all-significant T :separators nil :edges T))

(print (send x :forward :only T :sorted T
	     :recursive T :coherent nil :headlong T
	     :all-significant T :separators nil :edges T))


(print (send x :forward :only T :sorted T
	     :recursive T :coherent nil :headlong nil
	     :all-significant nil :separators nil :edges T))

(print (send x :forward :only T :sorted T
	     :recursive T :coherent T :headlong T
	     :all-significant nil :separators nil :edges T))


(print (send x :forward :only T :sorted T
	     :recursive T :coherent T :headlong T
	     :all-significant T :separators nil
	     :edges 'interactions))

(print (send x :forward :only T :sorted T
	     :recursive T :coherent T :headlong T
	     :all-significant T :separators T :edges T))


(print (send x :fix-edges "ABC"))
(send x :status 'fix)
(print (send x :fix-edges 'what))
(print (send x :and-fix-edges 'what))
(print (send x :return-fix 'fix))

(print (send x :and-fix-edges "CD"))
(send x :status 'fix)
(print (send x :fix-edges 'what))
(print (send x :and-fix-edges 'what))
(print (send x :return-fix 'fix))

(print (send x :and-fix-edges "DEF"))
(send x :status 'fix)
(print (send x :fix-edges 'what))
(print (send x :and-fix-edges 'what))
(print (send x :return-fix 'fix))

(print (send x :fix-edges "BCDE"))
(send x :status 'fix)
(print (send x :fix-edges 'what))
(print (send x :and-fix-edges 'what))
(print (send x :return-fix 'fix))

