
### TestEH

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  read.model("*")

  "find.dual" ;
  eh.find.dual () ;
  dispose.of.eh("all")
  eh.find.dual (sub.class = "decomposable") ;

  x <- c("a.duas", "r.dual", "both", "both.duals")
  y <- c("decomposable", "graphical", "hierarchical")
  for (j in y) {
    for (i in x) {
      print(j)
      print(i)
      eh.find.dual (i, j) ;
      status("eh")
      dispose.of.eh("all")
    }
  }

  "eh" ;
  eh();
  dispose.of.eh("all")
  eh(strategy = "rough");
  dispose.of.eh("all")
  eh(sub.class = "decomposable");
  dispose.of.eh("all")

  x <- c("smallest", "alternating", "rough")
  y <- c("decomposable", "graphical", "hierarchical")
  for (j in y) {
    for (i in x) {
      print(j)
      print(i)
      eh (i, j) ;
      status("eh")
      dispose.of.eh("all")
    }
  }

  "fit.EH" ;
  eh.fit () ;
  dispose.of.eh("all")
  eh.fit(dual = "both");
  dispose.of.eh("all")
  eh.fit(sub.class = "decomposable");
  dispose.of.eh("all")

  x <- c("a.dual", "r.dual", "smallest.dual", "largest.dual", "both.duals")
  y <- c("decomposable", "graphical", "hierarchical")
  for (j in y) {
    for (i in x) {
      print(j)
      print(i)
      eh.fit (i, j) ;
      status("eh")
      dispose.of.eh("all")
    }
  }

  printModel("all")

  rm( x, y)

  .quit()
