
### TestEHInit

  library(CoCo);
  library(CoCoOldData);
  ReinisCoCoObject <- Reinis()

#  Do not start with this (????):
#  read.base.model ("what") ;

  read.model(".;")

  "search.base" ;
  read.model("*;")
  base()
  eh.set.base () ;

  status("eh")
  dispose.of.eh ("all") ;

  "search.base" ;
  read.model("ACDE,ABCF.;")
  base()
  eh.set.base () ;

  status("eh")
  dispose.of.eh ("all") ;

  printModel("all")
  "read.base.model" ;
  eh.set.base ("what") ;
#  eh.set.base () ;
  .eh.read.base.model () ;
  eh.set.base ("what") ;
  eh.set.base ("*") ;
  eh.set.base ("what") ;

  status("eh")
  dispose.of.eh ("all") ;

  "set.main.effects" ;
  eh.set.main.effects ("what") ;
  eh.set.main.effects () ;
  eh.set.main.effects ("ABCDE") ;
  eh.set.main.effects ("what") ;
  eh.set.main.effects ("*") ;
  eh.set.main.effects ("what") ;

  status("eh")
  dispose.of.eh ("all") ;

  "fix.in" ;
  eh.fix ("what", fix = "in") ;
  eh.fix (        fix = "in") ;
  eh.fix ("what", fix = "in") ;
  eh.fix ("AB",   fix = "in") ;
  eh.fix ("what", fix = "in") ;
  eh.fix ("AC",   fix = "in") ;

  "fix.out" ;
  eh.fix ("what", fix = "out") ;
  eh.fix (        fix = "out") ;
  eh.fix ("what", fix = "out") ;
  eh.fix ("FE",   fix = "out") ;
  eh.fix ("what", fix = "out") ;
  eh.fix ("FD",   fix = "out") ;

  "add.fix.in" ;
  eh.fix ("what", fix = "in", add = TRUE) ;
  eh.fix (        fix = "in", add = TRUE) ;
  eh.fix ("what", fix = "in", add = TRUE) ;
  eh.fix ("AD",   fix = "in", add = TRUE) ;
  eh.fix ("what", fix = "in", add = TRUE) ;
  eh.fix ("AE",   fix = "in", add = TRUE) ;

  "add.fix.out" ;
  eh.fix ("what", fix = "out", add = TRUE) ;
  eh.fix (        fix = "out", add = TRUE) ;
  eh.fix ("what", fix = "out", add = TRUE) ;
  eh.fix ("FC",   fix = "out", add = TRUE) ;
  eh.fix ("what", fix = "out", add = TRUE) ;
  eh.fix ("FB",   fix = "out", add = TRUE) ;

  "redo.fix.in" ;
  eh.fix (fix = "in", redo = TRUE) ;

  "redo.fix.out" ;
  eh.fix (fix = "out", redo = TRUE) ;

  status("eh")
  dispose.of.eh ("all") ;

  "set.search" ;
# .set.search.DEPRECATED ("what") ;
# .set.search.DEPRECATED () ;
  .set.search.DEPRECATED ("what") ;
  x <- c("smallest", "graphical", "alternating", "hierarchical", "rough")
  for (i in x) {
    .set.search.DEPRECATED (i) ;
    .set.search.DEPRECATED ("what") ;
  }

  status("eh")

  "set.graphical.search" ;
  .set.graphical.search.DEPRECATED () ;
  .set.graphical.search.DEPRECATED ("what")
  for (i in c("off", "flop", "on", "what")) {
    .set.graphical.search.DEPRECATED (i) ;
    .set.graphical.search.DEPRECATED ("what")
  }

  status("eh")

  "dispose.of.eh" ;
  x <- c("all", "duals", "a.duals", "r.duals", "classes", "accepted", "rejected")
  for (i in x) {
    dispose.of.eh (i) ;
  }

  status("eh")

  status()

  rm( x)

  status()

  .quit()
