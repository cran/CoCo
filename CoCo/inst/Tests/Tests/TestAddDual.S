
### TestAddDual.S

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  "add.dual.to.class" ;
  ehForceAddDualToClass () ;
  a <- c("a.dual", "r.dual")
  b <- c("accepted", "rejected")
  d <- c("decomposable", "graphical", "hierarchical")
  for (i in d) {
    for (j in b) {
      for (k in a) {
        print(i)
        print(j)
        print(k)
        ehForceAddDualToClass (k, j, i) ;
      }
    }
  }

  showModel("all")
  showOptions("eh")

  rm(  a, b, d)

  quitCoCo()
