
### TestEHExtract

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  enterModel("*")

  "extract" ;

  ehExtract ("accepted", "decomposable") ;
  showOptions("eh")
  showModel("all")

  ehExtract ("accepted", "graphical") ;
  showOptions("eh")
  showModel("all")

  ehExtract ("accepted", "hierarchical") ;
  showOptions("eh")
  showModel("all")

  eh();

  "extract" ;

  ehExtract ("accepted", "decomposable") ;
  showOptions("eh")
  showModel("all")

  ehExtract ("accepted", "graphical") ;
  showOptions("eh")
  showModel("all")

  ehExtract ("accepted", "hierarchical") ;
  showOptions("eh")
  showModel("all")

  x <- c("accepted", "rejected", "a.duals", "r.duals")
  for (i in x) {
    print(i)
    ehExtract (i) ;
  }

  y <- c("decomposable", "graphical", "hierarchical")

  showOptions("eh")
  showModel("all")

  rm(  x, y)

  quitCoCo()
