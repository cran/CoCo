(def graph-3b (send model-3 :make-graph :title "Hustage"))

;(send graph-3b :vertex-label #\N "N: Nummer" :redraw nil)
(send graph-3b :vertex-label #\p
 "p: ZIP-code, Postnummer" :redraw nil)
(send graph-3b :vertex-label #\A
 "A: Owners age, Ejeres Alder" :redraw nil)
(send graph-3b :vertex-label #\k
 "k: Sex, Koen" :redraw nil)
(send graph-3b :vertex-label #\h
 "h: Owner/Rent, Husejer" :redraw nil)
(send graph-3b :vertex-label #\i
 "i: Income, Indkomst (hustand)" :redraw nil)
(send graph-3b :vertex-label #\e
 "e: Occupation, Erhverv" :redraw nil)
(send graph-3b :vertex-label #\y
 "y: Construction year, Aar" :redraw nil)
(send graph-3b :vertex-label #\b
 "b: Current cover type, Belaegning (nuvaerende)" :redraw nil)
(send graph-3b :vertex-label #\a
 "a: Age of current roof, Alder (belaegning)" :redraw nil)
(send graph-3b :vertex-label #\u
 "u: Replacement, Udskiftning (paataenkt)" :redraw nil)
(send graph-3b :vertex-label #\g
 "g: Reason for replacement,Grund (udskiftning)" :redraw nil)
(send graph-3b :vertex-label #\n
 "n: New coat type, Ny belaegning" :redraw nil)
;(send graph-3b :vertex-label #\X "X: " :redraw nil)
;(send graph-3b :vertex-label #\Y "Y: " :redraw nil)
;(send graph-3b :vertex-label #\Z "Z: " :redraw nil)
(send graph-3b :vertex-label #\1
 "1: Cheap, Billig" :redraw nil)
(send graph-3b :vertex-label #\2
 "2: Tightness, Taet" :redraw nil)
(send graph-3b :vertex-label #\3
 "3: Robustness, Robust" :redraw nil)
(send graph-3b :vertex-label #\4
 "4: Nice, Paent" :redraw nil)
(send graph-3b :vertex-label #\5
 "5: Survivaltime, Levetid" :redraw nil)
(send graph-3b :vertex-label #\6
 "6: Maintenance, Vedligeholdelse" :redraw nil)
(send graph-3b :vertex-label #\7
 "7: Quality, Kvalitet" :redraw nil)
(send graph-3b :vertex-label #\8
 "8: Look, Udseende" :redraw nil)
(send graph-3b :vertex-label #\9
 "9: No, Ved ikke" :redraw nil)
(send graph-3b :vertex-label #\0
 "0: Other, Andet" :redraw nil)
(send graph-3b :vertex-label #\r
 "r: Consultation, Raadgivning" :redraw nil)
(send graph-3b :vertex-label #\f
 "f: Free of asbestos, Asbestfri" :redraw nil)

;(send graph-3b :vertex-label #\N "N: Nummer" :redraw nil)
(send graph-3b :vertex-label #\p
 "p: ZIP-code" :redraw nil)
(send graph-3b :vertex-label #\A
 "A: Owners age" :redraw nil)
(send graph-3b :vertex-label #\k
 "k: Sex" :redraw nil)
(send graph-3b :vertex-label #\h
 "h: Owner/Rent" :redraw nil)
(send graph-3b :vertex-label #\i
 "i: Income" :redraw nil)
(send graph-3b :vertex-label #\e
 "e: Occupation" :redraw nil)
(send graph-3b :vertex-label #\y
 "y: Construction year" :redraw nil)
(send graph-3b :vertex-label #\b
 "b: Current cover type" :redraw nil)
(send graph-3b :vertex-label #\a
 "a: Age of current roof" :redraw nil)
(send graph-3b :vertex-label #\u
 "u: Replacement" :redraw nil)
(send graph-3b :vertex-label #\g
 "g: Reason for replacement" :redraw nil)
(send graph-3b :vertex-label #\n
 "n: New coat type" :redraw nil)
;(send graph-3b :vertex-label #\X "X: " :redraw nil)
;(send graph-3b :vertex-label #\Y "Y: " :redraw nil)
;(send graph-3b :vertex-label #\Z "Z: " :redraw nil)
(send graph-3b :vertex-label #\1
 "1: Cheap" :redraw nil)
(send graph-3b :vertex-label #\2
 "2: Tightness" :redraw nil)
(send graph-3b :vertex-label #\3
 "3: Robustness" :redraw nil)
(send graph-3b :vertex-label #\4
 "4: Nice" :redraw nil)
(send graph-3b :vertex-label #\5
 "5: Survivaltime" :redraw nil)
(send graph-3b :vertex-label #\6
 "6: Maintenance" :redraw nil)
(send graph-3b :vertex-label #\7
 "7: Quality" :redraw nil)
(send graph-3b :vertex-label #\8
 "8: Look" :redraw nil)
(send graph-3b :vertex-label #\9
 "9: No reason" :redraw nil)
(send graph-3b :vertex-label #\0
 "0: Other" :redraw nil)
(send graph-3b :vertex-label #\r
 "r: Consultation" :redraw nil)
(send graph-3b :vertex-label #\f
 "f: Free of asbestos" :redraw nil)

(send graph-3b :vertex-position #\p '(-20 40 0))
(send graph-3b :vertex-position #\A '(20 20 0))
(send graph-3b :vertex-position #\k '(40 40 0))
(send graph-3b :vertex-position #\h '(20 -20 0))
(send graph-3b :vertex-position #\i '(40 20 0))
(send graph-3b :vertex-position #\e '(20 40 0))
(send graph-3b :vertex-position #\y '(10 30 0))
(send graph-3b :vertex-position #\b '(30 0 0))
(send graph-3b :vertex-position #\a '(-20 20 0))
(send graph-3b :vertex-position #\u '(-40 20 0))
(send graph-3b :vertex-position #\g '(-40 40 0))
(send graph-3b :vertex-position #\n '(-30 0 0))
(send graph-3b :vertex-position #\1 '(0 5 0))
(send graph-3b :vertex-position #\2 '(0 10 0))
(send graph-3b :vertex-position #\3 '(30 -40 0))
(send graph-3b :vertex-position #\4 '(-30 -10 0))
(send graph-3b :vertex-position #\5 '(-30 -40 0))
(send graph-3b :vertex-position #\6 '(-40 -20 0))
(send graph-3b :vertex-position #\7 '(0 -20 0))
(send graph-3b :vertex-position #\8 '(30 -10 0))
(send graph-3b :vertex-position #\9 '(-20 -20 0))
(send graph-3b :vertex-position #\0 '(40 -40 0))
(send graph-3b :vertex-position #\r '(-40 0 0))
(send graph-3b :vertex-position #\f '(40 0 0))

(send graph-3b :vertex-label-position #\6 '(-5 -5 0))
(send graph-3b :vertex-label-position #\3 '(-5 -5 0))
(send graph-3b :vertex-label-position #\0 '(-5  5 0))
(send graph-3b :vertex-label-position #\r '(-5 -5 0))
(send graph-3b :vertex-label-position #\u '(-5 -5 0))
(send graph-3b :vertex-label-position #\b '(-20 -5 0))
(send graph-3b :vertex-label-position #\i '(-5 -5 0))
(send graph-3b :vertex-label-position #\k '(-5 -5 0))
(send graph-3b :vertex-label-position #\g '(-5 -10 0))
(send graph-3b :vertex-label-position #\A '(-5 5 0))
(send graph-3b :vertex-label-position #\i '(-10 -5 0))
(send graph-3b :vertex-label-position #\f '(-10 5 0))
(send graph-3b :vertex-label-position #\b '(-30 -5 0))
(send graph-3b :vertex-label-position #\8 '(-5 -5 0))

(send graph-3b :item-color 'vertex-label 'red)

(send graph-3b :vertex-label-arrow #\6 T)
(send graph-3b :vertex-label-arrow #\3 T)
(send graph-3b :vertex-label-arrow #\0 T)
(send graph-3b :vertex-label-arrow #\r T)
(send graph-3b :vertex-label-arrow #\u T)
(send graph-3b :vertex-label-arrow #\b T)
(send graph-3b :vertex-label-arrow #\i T)
(send graph-3b :vertex-label-arrow #\k T)
(send graph-3b :vertex-label-arrow #\g T)
(send graph-3b :vertex-label-arrow #\A T)
(send graph-3b :vertex-label-arrow #\i T)
(send graph-3b :vertex-label-arrow #\f T)
(send graph-3b :vertex-label-arrow #\b T)
(send graph-3b :vertex-label-arrow #\8 T)

(send graph-3b :size 550 700)

(send graph-3b :vertex-position #\a '(-20 40 0) )
(send graph-3b :vertex-position #\p '(-40 40 0) )
(send graph-3b :vertex-position #\u '(-10 5 0)  )
(send graph-3b :vertex-position #\g '(-20 20 0) )
(send graph-3b :vertex-position #\1 '(-40 10 0) )
(send graph-3b :vertex-position #\2 '(-40 20 0) )
(send graph-3b :vertex-position #\y '(0 30 0)   )

(send graph-3b :vertex-label-position #\u '(-10 5 0))
(send graph-3b :vertex-label-position #\g '(0 -5 0))
