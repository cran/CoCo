
### TestStepwise

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  read.model(".;")
  read.model("*;")
  base()
  read.model("ACDE,ABCF.;")
  make.current(2)

  "backward" ;
  backward () ;
  backward (model = 2) ;
  backward (model = 3) ;
  backward (model = "ACDE,ABCF.;") ;
  backward(only = T, reversed = F, sorted = T, short = F,
           p.accepted = F, p.rejected = F, decomposable.mode = T,
           recursive = T, coherent = T, headlong = T, 
           follow = T, least.significant = T, separators = F, edges = T,
           model = F, coco.id = .current.coco)

  backward(only = T, reversed = F, sorted = T, short = F,
           p.accepted = F, p.rejected = F, decomposable.mode = F,
           recursive = T, coherent = T, headlong = T, 
           follow = T, least.significant = T, separators = F, edges = F)

  "fix.edges" ;
  fix.edges ("what") ;
  fix.edges ("ABC,CD") ;
  fix.edges ("what") ;
  "and.fix.edges" ;
  fix.edges ("what", and.fix.edges = TRUE) ;
  fix.edges ("AF",   and.fix.edges = TRUE) ;
  fix.edges ("what") ;
  fix.edges ("what", and.fix.edges = TRUE) ;
  fix.edges ("BF",   and.fix.edges = TRUE) ;
  fix.edges ("what") ;
  fix.edges ("what", and.fix.edges = TRUE) ;
  fix.edges ("ABC,CD") ;
  fix.edges ("what") ;
  fix.edges ("what", and.fix.edges = TRUE) ;

  "backward" ;

  fix.edges (";") ;
  fix.edges ("what") ;
  fix.edges ("what", and.fix.edges = TRUE) ;

  "forward" ;

  forward(only = T, reversed = F, sorted = T, short = F,
          p.accepted = F, p.rejected = F, decomposable.mode = T,
          recursive = T, coherent = T, headlong = T, 
          all.significant = T, separators = F, edges = T,
          model = 1, coco.id = .current.coco)

  make.current(1)
  forward () ;

  .quit()
