
;;; Copyright 1992 Jens Henrik Badsberg.
;;; License is granted to copy this program under the GNU library
;;; public licence.  This program comes with NO WARANTEE.  See file
;;; COPYRIGHT for details.


;;; This is a version of :dump-tex for the association-diagram-proto



(defmeth association-diagram-proto :dump-tex
  (&key (file-name nil) (radius 4) (unadjusted nil) (box nil) (centered nil)
	(char-width 5) (char-height 10) (extra-width 2) (extra-height 1)
	(size "(360,360)(0,0)"))
  (let ((f (if file-name (open file-name :direction :output) T))
	(label-sizes (if box 
			 (mapcar
			  #'(lambda (vertex)
			      (let ((x (split-string (send vertex :label))))
				(list (* char-width (max (mapcar #'length x)))
				      (* char-height (length x) ))))
			  (slot-value 'vertices))
		       (repeat (list '(0 0))
			       (length (send self :vertices))))))
    (unwind-protect
	(progn
	  (format f "% \\documentstyle[ ... ,pspic, ... ]{ ... }~%~%")
	  (format f "% \\vectorhead{9}{5}~%~%")
	  (format f "% \\newcommand{\\Ordinal}[1]~
                     {{\\shade{50}\\circle*{#1}\\shade{100}\\circle{#1}}}~%")
	  (format f "% \\newcommand{\\Circle}[1]~
                     {{\\shade{0}\\circle*{#1}\\shade{100}\\circle{#1}}}~%")
	  (format f "% \\newcommand{\\Dot}[1]{{\\shade{100}\\circle*{#1}}}~%")
	  (format f "% \\newcommand{\\solidlines}[0]{{}}~%")
	  (format f "% \\newcommand{\\dashedlines}[0]{{}}~%")
	  (format f "% \\def\\thinline(#1,#2)#3{\\line(#1,#2){#3}}~%")
	  (format f "~%~%")
	  (format f "\\begin{figure}~%")
	  (format f "\\begin{center}~%")
	  (format f "% Title: {~a}~%" (send self :title))
	  (format f "% \\put(~7,4f,~7,4f){\\em {~a}}~%"
		  (send self :to-x-tex -25)
		  (send self :to-y-tex  55)
		  (send self :title))
	  (format f "\\begin{picture}~a~%" size)
	  (format f "% Edges:~%")
	  (send self :tex-draw-edges f radius unadjusted box centered
		extra-width extra-height label-sizes)
	  (format f "% Blocks:~%")
	  (send self :tex-draw-blocks f unadjusted)
	  (format f "% Vertices:~%")
	  (send self :tex-draw-vertices f radius unadjusted box
		extra-width extra-height label-sizes)
	  (format f "\\end{picture}~%")
	  (format f "\\caption{~a}~%" (send self :title))
	  (format f "\\end{center}~%")
	  (format f "\\end{figure}~%"))
      (if file-name (close f))))
  )

;;;
(defmeth association-diagram-proto :x-pos-to-tex (x)
  (* 2 (+ 50 x))
  )

(defmeth association-diagram-proto :y-pos-to-tex (y)
  (* 2 (- 50 y))
  )

(defmeth association-diagram-proto :to-x-tex (x)
  (let ((a (send self  :x-pos-to-tex x)))
    (if (= 0 (mod a 1)) (round a) a))
  )

(defmeth association-diagram-proto :to-y-tex (y)
  (let ((a (send self  :y-pos-to-tex y)))
    (if (= 0 (mod a 1)) (round a) a))
  )

(defmeth association-diagram-proto :to-tex (x)
  (list (send self :to-x-tex (car x)) (send self :to-y-tex (cadr x)))
  )

(defmeth association-diagram-proto :tex-draw-color (f color)
;;;  (format f "~a " color)
  )

(defmeth association-diagram-proto :tex-line-type (f type)
  (if (equal 'solid type)
      (format f "\\solidlines")
    (if (equal 'dashed type)
	(format f "\\dashedlines")))
  )

(defmeth association-diagram-proto :tex-line-width (f w)
  (if (= 1 w)
      (format f "\\thicklines")
    (format f "\\linethickness{~7,4fcm}" (/ w 200)))
  )


(defmeth association-diagram-proto :tex-draw-vertex (f type x radius width)
  (if (or (eq type 'continuous) (eq type 0))
      (format f "\\put(~7,4f,~7,4f){\\Circle{~7,4f}}"
	      (car x) (cadr x) (* 2 radius))
    (if (or (eq type 'discrete) (eq type 1))
	(format f "\\put(~7,4f,~7,4f){\\Dot{~7,4f}}"
		(car x) (cadr x) (* 2 radius))
      (if (or (eq type 'ordinal) (eq type 2))
	  (format f "\\put(~7,4f,~7,4f){\\Ordinal{~7,4f}}"
		  (car x) (cadr x) (* 2 radius))
	(format f "\\put(~7,4f,~7,4f){\\Gryf{~7,4f,~7,4f}}"
		(car x) (cadr x) (* 2 radius) (* 2 radius))
	)))
  (format f " ")
  )

(defmeth association-diagram-proto :tex-draw-string (f s x)
  (format f "\\put(~7,4f,~7,4f){~1a}~%" (car x) (cadr x) s)
  )

;;;
(defun gcd (a b)
  (if (= b 0) a (gcd b (mod a b)))
  )

(defun int-round (a)
  (if (= 0 (mod a 1)) (round a) a)
  )

(defmeth association-diagram-proto :tex-return-length
  (x y a b &optional (d 0))
  (let* ((i (- a x))
	 (j (- b y))
	 (l (sqrt (+ (^ i 2) (^ j 2)))))
    (if (= 0 i) (int-round (- l d))
      (if (= 0 j) (int-round (- l d))
	(int-round (* (abs i) (/ (- l d) l))))))
  )

(defmeth association-diagram-proto :tex-return-direction (x y a b)
  (let* ((i (- a x))
	 (j (- b y)))
    (if (= 0 i) (list 0 (round (/ j (abs j))))
      (if (= 0 j) (list (round (/ i (abs i))) 0)
	(let ((g (if (and (= 0 (mod i 1)) (= 0 (mod j 1)))
		     (abs (if (< i j) (gcd i j) (gcd j i))) 1)))
	  (list (int-round (/ i g)) (int-round (/ j g)))))))
  )

#|

(defun select-closest-slope (a n)
  (if (= 0 (car a) (cadr a)) (list 0 0)
    (let* ((l (sqrt (+ (^ (car a) 2) (^ (cadr a) 2))))
	   (p (abs (/ (car a) l)))
	   (q (abs (/ (cadr a) l)))
	   (d (list 1 0 0)))
      (mapcar #'(lambda (u v)
		  (let* ((l (sqrt (+ (^ u 2) (^ v 2))))
			 (x (/ u l))
			 (y (/ v l))
			 (z (+ (^ (- x p) 2) (^ (- y q) 2))))
		    (if (< z (car d)) (setf d (list z u v)))))
	      (cdr (repeat (iseq n) n))
	      (cdr (repeat (iseq n) (repeat n n))))
      (* (cdr d) (list (if (< (car a) 0) -1 1) (if (< (cadr a) 0) -1 1)))))
  )

;;; More elegant, but slower than the one to follow:

(defun select-closest-slope (a n)
  (if (= 0 (car a) (cadr a)) (list 0 0)
    (let ((b (abs (/ a (sqrt (sum (^ a 2))))))
	  (d (list 1 0 0)))
      (do ((s (if (= 6 n)
		  '((0 1) (1 0) (1 1) (1 2) (1 3) (1 4) (1 5)
		    (2 1) (2 3) (2 5) (3 1) (3 2) (3 4) (3 5)
		    (4 1) (4 3) (4 5) (5 1) (5 2) (5 3) (5 4))
		'((0 1) (1 0) (1 1) (1 2) (1 3) (2 1) (2 3) (3 1) (3 2)))
	      (cdr s)))
	  ((or (not s) (= 0 (car d)))
	   (* (cadr d) (mapcar #'(lambda (x) (if (< x 0) -1 1)) a)))
	  (let ((z (sum (^ (- (/ (car s) (sqrt (sum (^ (car s) 2)))) b) 2))))
	    (if (< z (car d)) (setf d (list z (car s))))))))
  )

|#

;;;
(defun select-closest-slope (a n)
  (if (= 0 (car a) (cadr a)) (list 0 0)
    (let* ((l (sqrt (+ (^ (car a) 2) (^ (cadr a) 2))))
	   (p (abs (/ (car a) l)))
	   (q (abs (/ (cadr a) l)))
	   (d (list 1 0 0)))
      (do ((s (if (= 6 n)
		  '((0 1) (1 0) (1 1) (1 2) (1 3) (1 4) (1 5)
		    (2 1) (2 3) (2 5) (3 1) (3 2) (3 4) (3 5)
		    (4 1) (4 3) (4 5) (5 1) (5 2) (5 3) (5 4))
		'((0 1) (1 0) (1 1) (1 2) (1 3) (2 1) (2 3) (3 1) (3 2)))
	      (cdr s)))
	  ((or (not s) (= 0 (car d)))
	   (* (cadr d) (mapcar #'(lambda (x) (if (< x 0) -1 1)) a)))
	  (let* ((l (sqrt (+ (^ (caar s) 2) (^ (cadar s) 2))))
		 (z (+ (^ (- (/ (caar s) l) p) 2)
		       (^ (- (/ (cadar s) l) q) 2))))
	    (if (< z (car d)) (setf d (list z (car s))))))))
  )

(defmeth association-diagram-proto :tex-draw-vl
  (f type n x y a b radius unadjusted)
  (let ((d (send self :tex-return-direction
		 (car a) (cadr a) (car b) (cadr b))))
    (if (or unadjusted
	    (and (integerp (car d)) (integerp (cadr d)) (< (max (abs d)) n)))
	(format f "\\put(~7,4f,~7,4f){\\~a(~7,4f,~7,4f){~7,4f}}~%"
		(car x) (cadr x) type (car d) (cadr d)
		(send self :tex-return-length
		      (car x) (cadr x) (car y) (cadr y) radius))
      (let* ((c (select-closest-slope d n))
	     (l (- (sqrt (+ (^ (- (car x) (car y)) 2)
			    (^ (- (cadr x) (cadr y)) 2))) (* 2 radius)))
	     (r (/ (sqrt (+ (^ (car c) 2) (^ (cadr c) 2)))))
	     (p (/ (- (+ (car x) (car y)) (* l (car c) r)) 2))
	     (q (/ (- (+ (cadr x) (cadr y)) (* l (cadr c) r)) 2)))
	(format f "\\put(~7,4f,~7,4f){\\~a(~7,4f,~7,4f){~7,4f}}~%"
		p q type (car c) (cadr c)
		(send self :tex-return-length p q
		      (/ (+ (car x) (car y) (* l (car c) r)) 2)
		      (/ (+ (cadr x) (cadr y) (* l (cadr c) r)) 2)
		      radius)))))
  )

(defmeth association-diagram-proto :tex-draw-vector
  (f x y a b radius unadjusted)
  (send self :tex-draw-vl f "vector" 4 x y a b radius unadjusted)
  )

(defmeth association-diagram-proto :tex-draw-line (f x y a b radius unadjusted)
  (send self :tex-draw-vl f "line" 6 x y a b radius unadjusted)
  )  

(defmeth association-diagram-proto :tex-draw-dashed-line
  (f x y a b radius unadjusted)
  (send self :tex-draw-vl f "thinline" 6 x y a b radius unadjusted)
  )    

(defmeth association-diagram-proto :tex-draw-box (f x y)
  (format f "\\put(~7,4f,~7,4f){\\dashbox{3}(~7,4f,~7,4f){}}"
	  (car x) (cadr x) (- (car y) (car x)) (- (cadr y) (cadr x)))
  )

;;;
(defmeth association-diagram-proto :tex-draw-vertex-in-box
  (f type x extra-width extra-height label-size s)
  (if (eq type 0)
      (progn
	(format f "\\put(~7,4f,~7,4f){\\oval(~7,4f,~7,4f)} ~%" (car x) (cadr x)
		(+ extra-width (car label-size))
		(+ extra-height (cadr label-size)))
;;;	(format f "\\put(~7,4f,~7,4f){\\parbox{10in}{~1a}}"
;;;		(- (car x) (/ (car label-size) 2)) (cadr x) s)
	))
  (format f "\\put(~7,4f,~7,4f){\\~1a(~7,4f,~7,4f) ~% ~
                   {\\shortstack[c]{~1a}}}"
	  (- (car x) (/ (+ extra-width (car label-size)) 2))
	  (- (cadr x) (/ (+ extra-height (cadr label-size)) 2))
	  (case type
		(0 "makebox")
		(1 "dashbox{3}")
		(2 "framebox")
		(t "makebox"))
	  (+ extra-width (car label-size))
	  (+ extra-height (cadr label-size)) s)
  (format f "~%")
  )

(defmeth association-diagram-proto :tex-draw-vertex-and-label
  (f vertex use-variables radius unadjusted box
     extra-width extra-height label-size)
  (if (and use-variables (> use-variables 0))
      (let ((position (send vertex :position))
	    (label-position (send vertex :label-position))
	    (label (send vertex :label)))
	(let ((x (send self :to-tex (send self :project position)))
	      (y (send self :to-tex
		       (send self :project
			     (if (send vertex :fix-label-position)
				 label-position
			       (if (car label-position)
				   (+ position label-position)
				 (rescale-procent 1.10 position)))))))
	  (format f " % [~1a]: ~%" (send vertex :name))
	  (if (screen-has-color)
	      (send self :tex-draw-color f
		    (send self :item-color 'vertex))) ; 'blue)
	  (if box
	      (send self :tex-draw-vertex-in-box f (send vertex :type) x
		    extra-width extra-height label-size
		    (string (if (send vertex :label)
				(send vertex :label) (send vertex :name))))
	    (progn
	      (send self :tex-draw-vertex f (send vertex :type) x radius 1)
	      (if (screen-has-color)
		  (send self :tex-draw-color f
			(send self :item-color 'vertex-label))) ; 'cyan)
	      (if (send vertex :label-arrow)
		  (progn
		    (send self :tex-line-width f 1)
		    (send self :tex-line-type f 'dashed)
		    (send self :tex-draw-vector f y x y x radius unadjusted)))
	      (send self :tex-draw-string f
		    (string (if (send vertex :label)
				(send vertex :label)
			      (send vertex :name))) y))))))
  )

;;;
(defmeth association-diagram-proto :tex-draw-vertices
  (f radius unadjusted box extra-width extra-height label-sizes)
  (mapcar #'(lambda (vertex use-variables label-size)
	      (send self :tex-draw-vertex-and-label
		    f vertex use-variables radius unadjusted box
		    extra-width extra-height label-size))
	  (send self :vertices)
	  (slot-value 'use-variables)
	  (if label-sizes label-sizes
	    (repeat (list '(0 0)) (length (slot-value 'positions)))))
  )

(defmeth association-diagram-proto :tex-draw-edge-label (f edge unadjusted)
  (if (and (send edge :test) (send edge :label))
      (let* ((label (send edge :label))
	      (m (send edge :center))
	      (x (send self :to-tex
		       (send self :project
			     (+ (if (send edge :fix-label-position)
				    (list 0 0 0) m)
				(send edge :slot-value 'label-position))))))
	(if (send edge :label-arrow)
	    (let ((y (send self :to-tex (send self :project m))))
	      (send self :tex-line-width f 1)
	      (send self :tex-line-type f 'dashed)
	      (if (screen-has-color) (send self :tex-set-edge-color f edge))
	      (send self :tex-draw-line f x y x y unadjusted 0)))
	(if (screen-has-color) (send self :tex-set-edge-color f edge))
	(send self :tex-draw-string f
	      (send self :format-p-value
		    (send (send edge :test) :select-p-value)) x)))
  )

(defmeth association-diagram-proto :tex-set-edge-color (f edge)
  (send self :tex-draw-color f
	(send self :item-color
	      (if (send edge :dashed) (send edge :dashed)
		(if (send edge :width) 'fitted 'not-fitted))))
  )

(defmeth association-diagram-proto :tex-cut-edge (point box centered)
  (let ((alfa (car point))
	(beta (cadr point))
	(a (car box))
	(b (cadr box)))
    (if (= 0 alfa beta) nil
      (let ((p1 (if (not (= 0 alfa))
		    (* (if (> (* alfa a) 0) 1 -1)
		       (list a (* a (/ beta alfa))))))
	    (p2 (if (not (= 0 beta))
		    (* (if (> (* beta b) 0) 1 -1)
		       (list (* b (/ alfa beta)) b)))))
	(if p1 (if p2 (if (< (sum (* p1 (list alfa beta)))
			     (sum (* p2 (list alfa beta))))
			  (if centered (list (car p1) 0) p1)
			(if centered (list 0 (cadr p2)) p2))
		 (if centered (list  (car p1) 0) p1))
	  (if p2 (if centered (list 0 (cadr p2)) p2))))))
  )

;;;
(defmeth association-diagram-proto :tex-draw-thick-edge
  (f a b width radius unadjusted box centered dx dy)
  (let ((c (+ a (if box (send self :tex-cut-edge (- b a) dx centered)
		  (* (/ (- b a) (^ (sum (^ (- b a) 2)) .5)) radius))))
	(d (+ b (if box (send self :tex-cut-edge (- a b) dy centered)
		  (* (/ (- a b) (^ (sum (^ (- a b) 2)) .5)) radius)))))
    (send self :tex-draw-line f c d
	  (if centered c a) (if centered d b) 0 unadjusted))
  )

(defmeth association-diagram-proto :tex-draw-arrow
  (f a b width radius unadjusted box centered dx dy)
  (let ((c (+ a (if box (send self :tex-cut-edge (- b a) dx centered)
		  (* (/ (- b a) (^ (sum (^ (- b a) 2)) .5)) radius))))
	(d (+ b (if box (send self :tex-cut-edge (- a b) dy centered)
		  (* (/ (- a b) (^ (sum (^ (- a b) 2)) .5)) radius)))))
    (send self :tex-draw-vector f c d
	  (if centered c a) (if centered d b) (if box 0 0) unadjusted))
  )

(defmeth association-diagram-proto :tex-draw-edge
  (f edge radius unadjusted box centered extra-width extra-height label-sizes)
  (let ((vertices (send edge :vertices))
	(width (send edge :width)))
    (if (screen-has-color) (send self :tex-set-edge-color f edge))
    (send self :tex-line-width f (if width width 1))
    (send self :tex-line-type f (if (send edge :dashed) 'dashed 'solid))
    (format f " % [~1a,~1a]: ~%"
	    (send (car vertices) :name)
	    (send (cadr vertices) :name))
    (let ((x (send self :to-tex (send self :project (send (car vertices)
							  :position))))
	  (dx (/ (+ (nth (send (car vertices) :index) label-sizes)
		    (list extra-width extra-height)) 2))
	  (y (send self :to-tex (send self :project (send (cadr vertices)
							  :position))))
	  (dy (/ (+ (nth (send (cadr vertices) :index) label-sizes)
		    (list extra-width extra-height)) 2)))
      (if (< (send (car vertices) :stratum) (send (cadr vertices) :stratum))
	  (send self :tex-draw-arrow f x y
		(if (and width (< 1 width))
		    (/ width 16) 2) radius unadjusted box centered dx dy)
	(if (< (send (cadr vertices) :stratum) (send (car vertices) :stratum))
	    (send self :tex-draw-arrow f y x
		  (if (and width (< 1 width))
		      (/ width 16) 2) radius unadjusted box centered dy dx)
	  (send self :tex-draw-thick-edge f x y
		(if (and width (< 1 width))
		    (/ width 16) 1) radius unadjusted box centered dx dy))))
    )
  )

(defmeth association-diagram-proto :tex-draw-edges
  (f radius unadjusted box centered extra-width extra-height label-sizes)
  (dolist (edge (send self :edges) NIL)
	  (send self :tex-draw-edge f edge radius unadjusted box centered
		extra-width extra-height label-sizes))
  (format f "% Edge labels: ~%")
  (dolist (edge (send self :edges) NIL)
	  (send self :tex-draw-edge-label f edge unadjusted))
  )

;;;
(defmeth association-diagram-proto :tex-draw-rectangle-line (f a b unadjusted)
  (let ((x (send self :to-tex (send self :project a)))
	(y (send self :to-tex (send self :project b))))
    (send self :tex-draw-dashed-line f x y x y 0 unadjusted))
  )

(defmeth association-diagram-proto :tex-draw-rectangle-box (f a b)
  (let ((x (send self :to-tex (send self :project a)))
	(y (send self :to-tex (send self :project b))))
    (send self :tex-draw-box f x y))
  )

(defmeth association-diagram-proto :tex-draw-rectangle
  (f a b c x y z unadjusted)
  (send self :tex-line-width f 1)
  (send self :tex-line-type f 'dashed)
  (format f "~%")
  (if (send self :transformation)
      (progn
	(send self :tex-draw-rectangle-line f
	      (list a b c) (list a y c) unadjusted)
	(send self :tex-draw-rectangle-line f
	      (list x b c) (list x y c) unadjusted)
	(send self :tex-draw-rectangle-line f
	      (list a b c) (list x b c) unadjusted)
	(send self :tex-draw-rectangle-line f
	      (list a y c) (list x y c) unadjusted)
	(send self :tex-draw-rectangle-line f
	      (list a b z) (list a y z) unadjusted)
	(send self :tex-draw-rectangle-line f
	      (list x b z) (list x y z) unadjusted)
	(send self :tex-draw-rectangle-line f
	      (list a b z) (list x b z) unadjusted)
	(send self :tex-draw-rectangle-line f
	      (list a y z) (list x y z) unadjusted)
	(send self :tex-draw-rectangle-line f
	      (list a b c) (list a b z) unadjusted)
	(send self :tex-draw-rectangle-line f
	      (list a y c) (list a y z) unadjusted)
	(send self :tex-draw-rectangle-line f
	      (list x b c) (list x b z) unadjusted)
	(send self :tex-draw-rectangle-line f
	      (list x y c) (list x y z) unadjusted))
    (send self :tex-draw-rectangle-box f (list a b c) (list x y c)))
  )

(defmeth association-diagram-proto :tex-draw-block (f block unadjusted)
  (if (send block :visual)
      (let ((position (send block :position))
	    (label-position (send block :label-position)))
	(send self :tex-draw-rectangle f
	      (car (car position)) (cadr (car position)) (caddr (car position))
	      (car (cadr position)) (cadr (cadr position)) (caddr (cadr position))
	      unadjusted)
	(send self :tex-draw-string f (send self :format-block-label block)
	      (send self :to-tex (send self :project
				       (+ (car position)
					  (if label-position label-position 0))
				       )))))
  )

(defmeth association-diagram-proto :tex-draw-blocks (f unadjusted)
  (if (screen-has-color)
      (send self :tex-draw-color f (send self :item-color 'block))) ; 'wheat)
  (dolist (block (send self :blocks) NIL)
	  (send self :tex-draw-block f block unadjusted))
  )

;;

(provide "graphtotex")
