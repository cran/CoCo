
### TestEHExtract

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  read.model("*")

  "extract" ;

  eh.extract ("accepted", "decomposable") ;
  status("eh")
  printModel("all")

  eh.extract ("accepted", "graphical") ;
  status("eh")
  printModel("all")

  eh.extract ("accepted", "hierarchical") ;
  status("eh")
  printModel("all")

  eh();

  "extract" ;

  eh.extract ("accepted", "decomposable") ;
  status("eh")
  printModel("all")

  eh.extract ("accepted", "graphical") ;
  status("eh")
  printModel("all")

  eh.extract ("accepted", "hierarchical") ;
  status("eh")
  printModel("all")

  x <- c("accepted", "rejected", "a.duals", "r.duals")
  for (i in x) {
    print(i)
    eh.extract (i) ;
  }

  y <- c("decomposable", "graphical", "hierarchical")

  status("eh")
  printModel("all")

  rm(  x, y)

  .quit()
