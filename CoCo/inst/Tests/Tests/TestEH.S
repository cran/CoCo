
### TestEH

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  enterModel("*")

  "find.dual" ;
  ehFindDual () ;
  disposeOfEh("all")
  ehFindDual (sub.class = "decomposable") ;

  x <- c("a.duas", "r.dual", "both", "both.duals")
  y <- c("decomposable", "graphical", "hierarchical")
  for (j in y) {
    for (i in x) {
      print(j)
      print(i)
      ehFindDual (i, j) ;
      showOptions("eh")
      disposeOfEh("all")
    }
  }

  "eh" ;
  eh();
  disposeOfEh("all")
  eh(strategy = "rough");
  disposeOfEh("all")
  eh(sub.class = "decomposable");
  disposeOfEh("all")

  x <- c("smallest", "alternating", "rough")
  y <- c("decomposable", "graphical", "hierarchical")
  for (j in y) {
    for (i in x) {
      print(j)
      print(i)
      eh (i, j) ;
      showOptions("eh")
      disposeOfEh("all")
    }
  }

  "fit.EH" ;
  ehFit () ;
  disposeOfEh("all")
  ehFit(dual = "both");
  disposeOfEh("all")
  ehFit(sub.class = "decomposable");
  disposeOfEh("all")

  x <- c("a.dual", "r.dual", "smallest.dual", "largest.dual", "both.duals")
  y <- c("decomposable", "graphical", "hierarchical")
  for (j in y) {
    for (i in x) {
      print(j)
      print(i)
      ehFit (i, j) ;
      showOptions("eh")
      disposeOfEh("all")
    }
  }

  showModel("all")

  rm( x, y)

  quitCoCo()
