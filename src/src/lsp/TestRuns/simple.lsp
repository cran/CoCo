
(def x (make-coco))

(print (def X-print-formats        
	    (send x :set-print-formats     'what )))
(print (def X-table-formats        	   
	    (send x :set-table-formats     'what )))
(print (def X-test-formats         	   
	    (send x :set-test-formats      'what )))
(print (def X-page-formats         	   
	    (send x :set-page-formats      'what )))
(print (def X-paging-lenght        	   
	    (send x :set-paging-lenght     'what )))

(print X-print-formats)
(print X-table-formats)
(print X-test-formats )
(print X-page-formats )
(print X-paging-lenght)
	                          
(send x :status 'formats)

	                          
(print (def X-ips-stop-criterion   
	(send x :set-ips-stop-criterion    'what )))
(print (def X-ips-epsilon          	   
	(send x :set-ips-epsilon           'what )))
(print (def X-ips-max-iterations   	   
	(send x :set-ips-max-iterations    'what )))

(print X-ips-stop-criterion   )
(print X-ips-epsilon          )
(print X-ips-max-iterations   )
	                          
(send x :status 'ips)

	                          
(print (def X-em-initial           
	(send x :set-em-initial           'what )))
(print (def X-em-epsilon           	  
	(send x :set-em-epsilon           'what )))
(print (def X-em-max-iterations    	  
	(send x :set-em-max-iterations    'what )))
	                          
(print X-em-initial         )
(print X-em-epsilon         )
(print X-em-max-iterations  )
	                          
(send x :status  'em)
	   
                       
(print (def X-algorithm            
	(send x :set-algorithm            'what )))
	                          	  
(print (def X-test                 	  
	(send x :set-test                 'what )))
(print (def X-power-lambda         	  
	(send x :set-power-lambda         'what )))
(print (def X-ic                   	  
	(send x :set-ic                   'what )))
	                          
(print (def X-acceptance           
	(send x :set-acceptance           'what )))
(print (def X-rejection            	  
	(send x :set-rejection            'what )))
(print (def X-components           	  
	(send x :set-components           'what )))
(print (def X-separators           	  
	(send x :set-separators           'what )))
(print (def X-asymptotic           	  
	(send x :set-asymptotic           'what )))
	                          
(print (def X-exact-test           
	(send x :set-exact-test           'what )))
(print (def X-seed                 	  
	(send x :set-seed                 'what )))
(print (def X-number-of-tables     	  
	(send x :set-number-of-tables     'what )))
(print (def X-exact-epsilon        	  
	(send x :set-exact-epsilon        'what )))

(print (def X-list-of-number-of-tables
	(send x :set-list-of-number-of-tables 'what  )))
	                          
(print X-algorithm            )
(print X-test                 )
(print X-power-lambda         )
(print X-ic                   )
(print X-acceptance           )
(print X-rejection            )
(print X-components           )
(print X-separators           	  )
(print X-asymptotic           	  )
(print X-exact-test               )
(print X-seed                 	  )
(print X-number-of-tables     	  )
(print X-exact-epsilon        	  )
(print X-list-of-number-of-tables )

(send x :status 'tests)
(send x :status 'exact)


(print (send x :set-print-formats         (list 10 4)))
(print (send x :set-table-formats         (list 10 6 2 2)))
(print (send x :set-test-formats          (list 9 4 7 5)))
(print (send x :set-page-formats          (list 127 65)))
(print (send x :set-paging-lenght         (list 22)))

(send x :status 'formats)

(print (send x :set-ips-stop-criterion    'cell))
(print (send x :set-ips-epsilon           0.0000001))
(print (send x :set-ips-max-iterations    100))

(send x :status 'ips)

(print (send x :set-em-initial            'uniform))
(print (send x :set-em-epsilon            0.001))
(print (send x :set-em-max-iterations     100))

(send x :status 'em)

(print (send x :set-algorithm            'a))

(print (send x :set-test                 'lr))
(print (send x :set-power-lambda         0.666667))
(print (send x :set-ic                   'aic  2.0))

(print (send x :set-acceptance           0.05))
(print (send x :set-rejection            0.025))
(print (send x :set-components           0.01))
(print (send x :set-separators           0.001))
(print (send x :set-asymptotic           0.25))

(print (send x :set-exact-test           'on))
(print (send x :set-seed                 'random))
(print (send x :set-number-of-tables     1000))
(print (send x :set-exact-epsilon        0.0000001))

(print (send x :set-list-of-number-of-tables
                                         (list 20 2 1000 5 200 8 100 20 20)))

(send x :status 'tests)
(send x :status 'exact)
