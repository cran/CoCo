
;;; Copyright 1995 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU
;;; library public licence.  This program comes with NO WARANTEE.
;;; See file COPYRIGHT for details.


;;; This function creates a dialog window for creating a model manager.

;;; Update managers?

(defun manager-dialog (x)

  (let* ((dismiss-manager
	  (send modal-button-proto :new (modal-text "Dismiss dialog window")
		:action #'(lambda () (send manager-dialog-window :remove))))

	 (end-item
	  (send modal-button-proto :new
		(if (kind-of-p x manager-proto)
		    (modal-text "End for CoCo-object of Manager")
		  (modal-text "End CoCo-object of graph"))
		:action #'(lambda ()
			    (send x :end)
			    (send manager-dialog-window :remove))))

	 (manager-item
	  (send modal-button-proto :new
		(if (kind-of-p x manager-proto)
		    (modal-text "Copy Manager for CoCo-object of graph")
		  (modal-text "Create Manager for CoCo-object of graph"))
		:action #'(lambda ()
			    (send x :return-manager
				  :location (send x :location)))))

	 (show-manager-item
	  (send modal-button-proto :new
		(if (or T (kind-of-p x manager-proto))
		    (modal-text "Show Manager for this CoCo-object"))
		:action #'(lambda ()
			    (dolist (i (send manager-proto :slot-value
					     'instances-manager) T)
				    (if (equalp 
					 (send i :slot-value 'identification)
					 (send x :slot-value 'identification))
					(send i :show-window))))))

	 (show-all-manager-item
	  (send modal-button-proto :new
		(if (or T (kind-of-p x manager-proto))
		    (modal-text "Show Manager for all CoCo-objects"))
		:action #'(lambda () (send *manager* :show-window))))

	 (show-family-item
	  (send modal-button-proto :new (modal-text "`1': Show family")
		:action #'(lambda () (send x :show-family :redraw T
					   :offsprings T :ancestors T))))

	 (show-offsprings-item
	  (send modal-button-proto :new (modal-text "`2': Show offsprings")
		:action #'(lambda () (send x :show-family :redraw T
					   :offsprings T :ancestors nil))))

	 (show-ancestors-item
	  (send modal-button-proto :new (modal-text "`3': Show ancestors")
		:action #'(lambda () (send x :show-family :redraw T
					   :offsprings nil :ancestors T))))

	 (hide-family-item
	  (send modal-button-proto :new (modal-text "`4': Hide family")
		:action #'(lambda () (send x :hide-family :redraw T
					   :offsprings T :ancestors T))))
	 
	 (hide-offsprings-item
	  (send modal-button-proto :new (modal-text "`5': Hide offsprings")
		:action #'(lambda () (send x :hide-family :redraw T
					   :offsprings T :ancestors nil))))

	 (hide-ancestors-item
	  (send modal-button-proto :new (modal-text "`6': Hide ancestors")
		:action #'(lambda () (send x :hide-family :redraw T
					   :offsprings nil :ancestors T))))

	 (close-family-item
	  (send modal-button-proto :new (modal-text "`7': Close family")
		:action #'(lambda () (send x :close-family :redraw T
					   :offsprings T :ancestors T))))

	 (close-offsprings-item
	  (send modal-button-proto :new (modal-text "`8': Close offsprings")
		:action #'(lambda () (send x :close-family :redraw T
					   :offsprings T :ancestors nil))))

	 (close-ancestors-item
	  (send modal-button-proto :new (modal-text "`9': Close ancestors")
		:action #'(lambda () (send x :close-family :redraw T
					   :offsprings nil :ancestors T))))

	 (update-managers-item
	  (send modal-button-proto :new
		(modal-text "`>': Update managers")
		:action #'(lambda () (send x :update-managers))))
	
	 (update-screen-item
	  (send modal-button-proto :new
		(modal-text "`<': Update screen (unnecessary)")
		:action #'(lambda ()
			    (dolist (i (send manager-proto :slot-value
					     'instances-manager) T)
				    (send i :update-screen)))))
	
	 (help-item
	  (send text-item-proto :new
		"`O': Open; `H': Hide; `C': Close;"))

	 (if (kind-of-p x manager-proto)
	     (progn
	       (send manager-item :enabled nil)))

	 (manager-dialog-window
	  (send modal-dialog-proto :new
		(list
		 (list dismiss-manager)
		 manager-item
		 show-manager-item
		 show-all-manager-item
		 show-family-item show-offsprings-item show-ancestors-item
		 hide-family-item hide-offsprings-item hide-ancestors-item
		 close-family-item close-offsprings-item close-ancestors-item
		 update-managers-item update-screen-item help-item
		 end-item)))))
  )
