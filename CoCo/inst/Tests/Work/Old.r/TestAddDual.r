
### TestAddDual.S

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  "add.dual.to.class" ;
  eh.add.dual.to.class () ;
  a <- c("a.dual", "r.dual")
  b <- c("accepted", "rejected")
  d <- c("decomposable", "graphical", "hierarchical")
  for (i in d) {
    for (j in b) {
      for (k in a) {
        print(i)
        print(j)
        print(k)
        eh.add.dual.to.class (k, j, i) ;
      }
    }
  }

  printModel("all")
  status("eh")

  rm(  a, b, d)

  .quit()
