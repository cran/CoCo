(if (screen-has-color)
    (progn

      (make-color 'brown       0.6  0    0   ) ; dark-red
      (make-color 'dark-green  0    0.6  0   )
      (make-color 'dark-violet 0    0    0.6 ) ; dark-blue

      (make-color 'pale-red    1.00 0.80 0.80) ; light-red
      (make-color 'light-green 0.80 1.00 0.80)
      (make-color 'light-blue  0.80 0.80 1.00)

      (make-color 'orange      1.00 0.70 0   ) ; red-green
      (make-color 'green-red   0.70 1.00 0   )
      (make-color 'green-blue  0    1.00 0.70)
      (make-color 'blue-green  0    0.70 1.00)
      (make-color 'blue-red    0.70 0    1.00)
      (make-color 'light-red   1.00 0    0.70) ; red-blue

      (make-color 'pink               1.0   0.749 0.788)
      (make-color 'red3               0.8   0.0   0.0  )
      (make-color 'violet-red         0.808 0.118 0.557)
      (make-color 'violet             0.929 0.498 0.929)
      (make-color 'blue-violet        0.537 0.157 0.878)
      (make-color 'blue3              0.0   0.0   0.8  )
      (make-color 'cornflower-blue    0.388 0.576 0.918)
      (make-color 'light-sky-blue     0.518 0.8   0.976)
      (make-color 'pale-green         0.588 0.976 0.588)
      (make-color 'medium-aqua-marine 0.4   0.8   0.659)
      (make-color 'sea-green          0.176 0.537 0.337)
      (make-color 'green-yellow       0.667 1.0   0.176)
      (make-color 'pale-yellow        1.0   1.0   0.486)
      (make-color 'gold               1.0   0.839 0.0  )
      (make-color 'goldenrod          0.847 0.639 0.118)
      (make-color 'orange-yellow      1.0   0.8   0.149)
      (make-color 'light-salmon       1.0   0.62  0.467)
      (make-color 'salmon             0.976 0.498 0.439)
      (make-color 'wheat              0.957 0.867 0.698)
      (make-color 'beige              0.957 0.957 0.859)
      ))

(setf *default-colors*
      (list
       'blue        ;	'vertex
       'cyan        ;	'vertex-label
       'yellow      ;	'new-edge
       'green       ;	'not-fitted
       'dark-green  ;	'fitted
       'brown       ;	'non-decomposable
       'dark-violet ;	'not-submodel-of-base
       'light-red   ;	'coherence
       'pale-red    ;	'error-edge
       'orange      ;	'fix-edge
       'magenta     ;	'grid
       'salmon      ;   'rotate
       'wheat       ;   'block
       'violet      ;	'future-edge
       'gold        ;   'controls
       'black       ;   'error
       
       'red 'dark-violet 'pale-red 'light-green 'light-blue
       'green-red 'green-blue 'blue-green 'blue-red
       'pink 'red3 'violet-red 'violet 'blue-violet
       'blue3 'cornflower-blue 'light-sky-blue 'pale-green 'medium-aqua-marine
       'sea-green 'green-yellow 'pale-yellow 'gold 'goldenrod
       'orange-yellow 'light-salmon 'salmon 'wheat 'beige
       
       )
      )
