
### TestForceEH.S

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  enterModel(".;")
  enterModel("*;")
  makeBase("current")
  enterModel("ACDE,ABCF.;")
  editModel(action = "drop.interactions", modification = "ABCF.;")
  editModel(action = "drop.edges", modification = "AC,BF.;")
  enterModel("ACE,ADE,BC,F.;")
  enterModel(".;")

  showModel("all")

  ehForceAccept("models", "*;")
  ehForceAccept("models", "ACDE,ABCF.;")
  ehForceAccept("models", "ACE,ADE,BC,F.;")
  ehForceReject("models", ".;")
  ehForceReject("models", "AB. BC. AC. ;")
  ehFit("models", "ACE,ADE,BC,F.;")

  ehForceAccept("base")
  ehForceReject(1)
  x <- c("a.dual", "r.dual", "smallest.dual", "largest.dual", "both", "both.duals")
  for (i in x) {
    print(i)
    ehFit(i)
  }

#  models <- list("base" , "current", "last" , 4 , "previous" , 5 , "next", "all") ; "all"
  models <- list("base" , "current", "last" , 4 , "previous" , 5 , "next");
  for (i in models) {
    print(i)
#   print(.encode.model.1(i))
    ehFit(i)
  }

  ehFit("interval", 2, 3)
  ehFit("list", 2:3)
  ehFit("list", c("current", "base"))
  ehFit("list", c("base", "current"))

  ehForceAccept("list", c("base", "current"))

  "reject('list', c(1, 'last'))"
#   ehForceReject("list", c(1, "last"))

  ehFit("interval", 1, 6)
  ehForceAccept("interval", 2, 6)
  ehForceReject("interval", 1, 1)

  showModel("all")
  showOptions("eh")

  rm(  x, models)

  quitCoCo()
