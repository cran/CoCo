
### TestForceEH.S

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  read.model(".;")
  read.model("*;")
  base()
  read.model("ACDE,ABCF.;")
  editModel(action = "drop.interactions", modification = "ABCF.;")
  editModel(action = "drop.edges", modification = "AC,BF.;")
  read.model("ACE,ADE,BC,F.;")
  read.model(".;")

  printModel("all")

  eh.accept("models", "*;")
  eh.accept("models", "ACDE,ABCF.;")
  eh.accept("models", "ACE,ADE,BC,F.;")
  eh.reject("models", ".;")
  eh.reject("models", "AB. BC. AC. ;")
  eh.fit("models", "ACE,ADE,BC,F.;")

  eh.accept("base")
  eh.reject(1)
  x <- c("a.dual", "r.dual", "smallest.dual", "largest.dual", "both", "both.duals")
  for (i in x) {
    print(i)
    eh.fit(i)
  }

#  models <- list("base" , "current", "last" , 4 , "previous" , 5 , "next", "all") ; "all"
  models <- list("base" , "current", "last" , 4 , "previous" , 5 , "next");
  for (i in models) {
    print(i)
    print(.encode.model.1(i))
    eh.fit(i)
  }

  eh.fit("interval", 2, 3)
  eh.fit("list", 2:3)
  eh.fit("list", c("current", "base"))
  eh.fit("list", c("base", "current"))

  eh.accept("list", c("base", "current"))

  "reject('list', c(1, 'last'))"
#   eh.reject("list", c(1, "last"))

  eh.fit("interval", 1, 6)
  eh.accept("interval", 2, 6)
  eh.reject("interval", 1, 1)

  printModel("all")
  status("eh")

  rm(  x, models)

  .quit()
