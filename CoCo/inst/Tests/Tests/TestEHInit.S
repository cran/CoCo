
### TestEHInit

  library(CoCo);
  library(CoCoOldData);
  ReinisCoCoObject <- Reinis()

#  Do not start with this (????):
#  read.base.model ("what") ;

  enterModel(".;")

  "search.base" ;
  enterModel("*;")
  makeBase("current")
  ehSetBase("current") ;

  showOptions("eh")
  disposeOfEh ("all") ;

  "search.base" ;
  enterModel("ACDE,ABCF.;")
  makeBase("current")
  ehSetBase("current") ;

  showOptions("eh")
  disposeOfEh ("all") ;

  showModel("all")
  "read.base.model" ;
  ehSetBase ("what") ;
#  ehSetBase("current") ;
  .eh.enter.base.model () ;
  ehSetBase ("what") ;
  ehSetBase ("*") ;
  ehSetBase ("what") ;

  showOptions("eh")
  disposeOfEh ("all") ;

  "set.main.effects" ;
  ehSetMainEffects ("what") ;
  ehSetMainEffects () ;
  ehSetMainEffects ("ABCDE") ;
  ehSetMainEffects ("what") ;
  ehSetMainEffects ("*") ;
  ehSetMainEffects ("what") ;

  showOptions("eh")
  disposeOfEh ("all") ;

  "fix.in" ;
  ehForceFix ("what", fix = "in") ;
  ehForceFix (        fix = "in") ;
  ehForceFix ("what", fix = "in") ;
  ehForceFix ("AB",   fix = "in") ;
  ehForceFix ("what", fix = "in") ;
  ehForceFix ("AC",   fix = "in") ;

  "fix.out" ;
  ehForceFix ("what", fix = "out") ;
  ehForceFix (        fix = "out") ;
  ehForceFix ("what", fix = "out") ;
  ehForceFix ("FE",   fix = "out") ;
  ehForceFix ("what", fix = "out") ;
  ehForceFix ("FD",   fix = "out") ;

  "add.fix.in" ;
  ehForceFix ("what", fix = "in", add = TRUE) ;
  ehForceFix (        fix = "in", add = TRUE) ;
  ehForceFix ("what", fix = "in", add = TRUE) ;
  ehForceFix ("AD",   fix = "in", add = TRUE) ;
  ehForceFix ("what", fix = "in", add = TRUE) ;
  ehForceFix ("AE",   fix = "in", add = TRUE) ;

  "add.fix.out" ;
  ehForceFix ("what", fix = "out", add = TRUE) ;
  ehForceFix (        fix = "out", add = TRUE) ;
  ehForceFix ("what", fix = "out", add = TRUE) ;
  ehForceFix ("FC",   fix = "out", add = TRUE) ;
  ehForceFix ("what", fix = "out", add = TRUE) ;
  ehForceFix ("FB",   fix = "out", add = TRUE) ;

  "redo.fix.in" ;
  ehForceFix (fix = "in", redo = TRUE) ;

  "redo.fix.out" ;
  ehForceFix (fix = "out", redo = TRUE) ;

  showOptions("eh")
  disposeOfEh ("all") ;

  "set.search" ;
# .set.search.DEPRECATED ("what") ;
# .set.search.DEPRECATED () ;
#  .set.search.DEPRECATED ("what") ;
  x <- c("smallest", "graphical", "alternating", "hierarchical", "rough")
  for (i in x) {
#    .set.search.DEPRECATED (i) ;
#    .set.search.DEPRECATED ("what") ;
  }

  showOptions("eh")

  "set.graphical.search" ;
#  .set.graphical.search.DEPRECATED () ;
#  .set.graphical.search.DEPRECATED ("what")
  for (i in c("off", "flop", "on", "what")) {
#    .set.graphical.search.DEPRECATED (i) ;
#    .set.graphical.search.DEPRECATED ("what")
  }

  showOptions("eh")

  "dispose.of.eh" ;
  x <- c("all", "duals", "a.duals", "r.duals", "classes", "accepted", "rejected")
  for (i in x) {
    disposeOfEh (i) ;
  }

  showOptions("eh")

  showOptions()

  rm( x)

  showOptions()

  quitCoCo()
