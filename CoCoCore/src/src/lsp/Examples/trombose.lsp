(setf x (make-coco))
(send x :set-data-file "../../Datasets/trombose.dat")
(send x :read-specification)
(send x :set-read 'subset "0XABOPZSRdefghikN")
(send x :reject-cases "@" (list 2)) ; Error?
(send x :or-reject-cases "D" (list 3)) ; Error?
;(send x :or-reject-cases "D" (list 5))
(send x :read-observations)
(send x :exclude-missing)
(send x :set-exact-test 'all)
(send x :set-ordinal "0XAOPSRdef0ghikNZ")

(setf m1 (send x :make-graph
	      :model "[[ANRS][APR][Xk][OX][OS][NX][BRS][0k][0O][Xe][Xhi][BZk][de][fg]]"
	      :location (list  50  50) :title "Forward 1"))

; (send m1 :vertex-position #\O '(-45 40 0)           )
; (send m1 :vertex-position #\N '(-45 10 0)           )
; (send m1 :vertex-position #\X '(-35 25 0)           )
; (send m1 :vertex-position #\d '(-22.5 -20 40)       )
; (send m1 :vertex-position #\f '(-25 -40 -20)        )
; (send m1 :vertex-position #\h '(-22.5 -35 -20)      )
; (send m1 :vertex-position #\A '(-15 30 0)           )
; (send m1 :vertex-position #\B '(-5 7.5 0)           )
; (send m1 :vertex-position #\P '(5 20 0)             )
; (send m1 :vertex-position #\R '(5 30 0)             )
; (send m1 :vertex-position #\S '(5 40 0)             )
; (send m1 :vertex-position #\e '(15 10 40)           )
; (send m1 :vertex-position #\g '(15 -40 0)           )
; (send m1 :vertex-position #\i '(15 -35 -40)         )
; (send m1 :vertex-position #\k '(35 -30 0)           )
; (send m1 :vertex-position #\Z '(25 -5 0)            )
; (send m1 :vertex-position #\0 '(45 10 0)            )

; (send m1 :vertex-position #\O '(-47.5 42.5 0)       )
; (send m1 :vertex-position #\N '(-45 -20 0)          )
; (send m1 :vertex-position #\X '(-35 20 0)           )
; (send m1 :vertex-position #\d '(-22.5 -20 40)       )
; (send m1 :vertex-position #\f '(-27.5 -20 -20)      )
; (send m1 :vertex-position #\h '(-27.5 -40 -20)      )
; (send m1 :vertex-position #\A '(-17.5 10 0)         )
; (send m1 :vertex-position #\B '(-5 -10 0)           )
; (send m1 :vertex-position #\P '(2.5 2.5 0)          )
; (send m1 :vertex-position #\R '(7.5 5 0)            )
; (send m1 :vertex-position #\S '(7.5 10 0)           )
; (send m1 :vertex-position #\e '(15 -20 40)          )
; (send m1 :vertex-position #\g '(15 -35 0)           )
; (send m1 :vertex-position #\i '(12.5 -40 -40)       )
; (send m1 :vertex-position #\Z '(27.5 10 0)          )
; (send m1 :vertex-position #\k '(35 -30 0)           )
; (send m1 :vertex-position #\0 '(45 42.5 0)          )

(send m1 :vertex-position #\O '(-45 40 0)         )
(send m1 :vertex-position #\N '(-45 -20 0)        )
(send m1 :vertex-position #\X '(-35 10 0)         )
(send m1 :vertex-position #\d '(-25 -20 40)       )
(send m1 :vertex-position #\f '(-25 -30 -20)      )
(send m1 :vertex-position #\h '(-25 -40 -20)      )
(send m1 :vertex-position #\A '(-17.5 10 0)       )
(send m1 :vertex-position #\B '(-5 -10 0)         )
(send m1 :vertex-position #\P '(2.5 30 0)         )
(send m1 :vertex-position #\R '(5 20 0)           )
(send m1 :vertex-position #\S '(5 10 0)           )
(send m1 :vertex-position #\e '(15 -20 40)        )
(send m1 :vertex-position #\g '(15 -30 0)         )
(send m1 :vertex-position #\i '(15 -40 -40)       )
(send m1 :vertex-position #\Z '(25 0 0)           )
(send m1 :vertex-position #\k '(35 -20 0)         )
(send m1 :vertex-position #\0 '(45 40 0)          )

(send m1 :static nil)
(send m1 :item-color 'vertex-label 'red)

;(setf m1d (send m1 :return-child-coco-graph-window
;	      :model "[[ANRS][NRSX][BRSX][BOSX][BOXk][0Ok][APR][BZk][Xe][de][Xhi][fg]]"
;	      :location (list 550  50) :title "Forward 1 with FillIn"))

;(setf m2 (send m1 :return-child-coco-graph-window
;	      :model "[[ANRS][APR][NSX][BOSX][BOXk][0Ok][BZk][Xe][de][Xh][hi][fg]]"
;	      :location (list  50  100) :title "Backward 1"))
;
;(setf m3 (send m1 :return-child-coco-graph-window
;	      :model "[[ANRS][APR][NSX][BOSX][BOXk][0Ok][BZk][BXe][de][Xh][hi][fg]]"
;	      :location (list  50  150) :title "Forward 2"))
;
;(setf m4 (send m1 :return-child-coco-graph-window
;	      :model "[[ANS][ARS][APR][NSX][BOSX][BOXk][0Ok][BXe][Zk][de][Xh][hi][fg]]"
;	      :location (list  50  200) :title "Backward 2"))
;
(setf m4 (send m1 :return-child-coco-graph-window
	      :model "[[hi][Xh][fg][Zf][de][Zk][0Ok][BXek][BOXk][BOSX][NSX][APR][ARS][ANS]]"
	      :location (list  50  250) :title "; Forward 3"))

(setf m5 (send m4 :define-blocks "NO,X,dfh,A,B,PRS,egi,Z,k,0"))

(send m5 :vertex-position #\O '(-45 40 0)         )
(send m5 :vertex-position #\N '(-45 -20 0)        )
(send m5 :vertex-position #\X '(-35 10 0)         )
(send m5 :vertex-position #\d '(-25 -20 40)       )
(send m5 :vertex-position #\f '(-25 -30 -20)      )
(send m5 :vertex-position #\h '(-25 -40 -20)      )
(send m5 :vertex-position #\A '(-17.5 10 0)       )
(send m5 :vertex-position #\B '(-5 -10 0)         )
(send m5 :vertex-position #\P '(2.5 30 0)         )
(send m5 :vertex-position #\R '(5 20 0)           )
(send m5 :vertex-position #\S '(5 10 0)           )
(send m5 :vertex-position #\e '(15 -20 40)        )
(send m5 :vertex-position #\g '(15 -30 0)         )
(send m5 :vertex-position #\i '(15 -40 -40)       )
(send m5 :vertex-position #\Z '(25 0 0)           )
(send m5 :vertex-position #\k '(35 -20 0)         )
(send m5 :vertex-position #\0 '(45 40 0)          )

(send m5 :vertex-label #\O "O: Koen"	)
(send m5 :vertex-label #\N "N: Alder"	)
(send m5 :vertex-label #\X "X: Diagnose"	) ;;, (u,v,w,x,y,z)}
(send m5 :vertex-label #\d "d: Non steroidt anti-inflammatorisk praeparat")
(send m5 :vertex-label #\d "d: NS anti-inflammatorisk praep."	)
(send m5 :vertex-label #\f "f: Acetylsalicyl syre"	)
(send m5 :vertex-label #\h "h: Steroider"	)
(send m5 :vertex-label #\A "A: Komponenttype (A,D)"	)
(send m5 :vertex-label #\B "B: Profylakseform (B,M)"	)
(send m5 :vertex-label #\P "P: Operations tid"	)
(send m5 :vertex-label #\R "R: Peroperativ bloedning"	)
(send m5 :vertex-label #\S "S: Postoperativ bloedning"	)
(send m5 :vertex-label #\e "e: Non steroidt anti-inflammatorisk praeparat")
(send m5 :vertex-label #\e "e: NS anti-inflammatorisk praep."	)
(send m5 :vertex-label #\g "g: Acetylsalicyl syre"	)
(send m5 :vertex-label #\i "i: Steroider"	)
(send m5 :vertex-label #\Z "Z: Venetrombose (Z,Q)"	)
(send m5 :vertex-label #\k "k: Anti koagulerende behandling"	)
(send m5 :vertex-label #\0 "0: Ossification"	)
(send m5 :vertex-label-position #\0 `(-10 -5 0))
(send m5 :vertex-label-position #\k `(-20  5 0))
(send m5 :vertex-label-arrow #\0 T)
(send m5 :vertex-label-arrow #\k T)

(setf m6 (send m5 :return-child-coco-graph-window
	      :model "[[OX][NX][AN][BS][BZk][0k][0O][AZk][AOS][ARS][APR][Be][de][fg][fh][hi]]"
	      :location (list  50  300) :title "; Final 1"))

;(setf m7 (send m5 :return-child-coco-graph-window
;	      :model "[[OX][NX][AN][BS][BZk][0Z][0O][AZk][AOS][ARS][APR][Be][de][fg][fh][hi]]"
;	      :location (list  50  350) :title "; Final 2"))

(send m6 :size 550 700)
