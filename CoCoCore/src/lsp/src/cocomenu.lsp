
;;; Copyright 1992, 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This is a funstion for adding a menu to a coco-graph-window


(defun add-coco-graph-menu (x)
  (let ((help-item
         (send menu-item-proto :new "Help for interaction by the mouse ... "
	       :action #'(lambda () (help-dialog x))))

	(graph-item
	 (send menu-item-proto :new "The graph window ..."
	       :action #'(lambda () (graph-dialog x))))

	(manager-item
         (send menu-item-proto :new "Model Manager ..."
	       :action #'(lambda () (manager-dialog x))))

	(option-item
	 (send menu-item-proto :new "Options in the CoCo object ..."
	       :action #'(lambda () (options-dialog x))))

	(model-description-item
         (send menu-item-proto :new "Description of Models ..."
	       :action #'(lambda () (model-description-dialog x))))

	(model-item
         (send menu-item-proto :new "Models (creating graphs) ..."
	       :action #'(lambda () (model-dialog x))))

	(split-item
         (send menu-item-proto :new "Split models ..."
	       :action #'(lambda () (split-dialog x))))

	(block-item
         (send menu-item-proto :new "Block recursive models ..."
	       :action #'(lambda () (blocks-dialog x))))

	(values-item
         (send menu-item-proto :new "Description of fitted values ..."
	       :action #'(lambda () (values-dialog x))))

	(test-item
         (send menu-item-proto :new "Tests ..."
	       :action #'(lambda () (test-dialog x))))
	
	(stepwise-item
         (send menu-item-proto :new "Stepwise model selection ..."
	       :action #'(lambda () (stepwise-dialog x))))
	
	(eh-item
         (send menu-item-proto :new "The EH procedure ..."
	       :action #'(lambda () (eh-dialog x))))
	
	(coco-graph-menu (send menu-proto :new "CoCo-Graph")))
     
    (send coco-graph-menu :append-items
	  help-item graph-item manager-item option-item
	  model-description-item model-item split-item block-item
	  values-item test-item stepwise-item eh-item)

    (if (kind-of-p x manager-proto)
	(progn
	  (send test-item :enabled nil)
	  (send stepwise-item :enabled nil)
	  (if (not (send x :slot-value 'identification))
	      (progn
		(send manager-item :enabled nil)
		(send option-item :enabled nil)
		(send model-description-item :enabled nil)
		(send model-item :enabled nil)
		(send split-item :enabled nil)
		(send block-item :enabled nil)
		(send values-item :enabled nil)
		(send test-item :enabled nil)
		(send stepwise-item :enabled nil)
		(send eh-item :enabled nil)))))

    (send x :menu coco-graph-menu)))

(defun modal-text (text)
  (concatenate 'string text
	       (to-string (repeat " " (max 0 (- 40 (length text)))) ""))
  )

(defun toggle-text (text)
  (concatenate 'string text
	       (to-string (repeat " " (max 0 (- 10 (length text)))) ""))
  )


(provide "cocomenu")
