
### TestToEH

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

  ".to.search" ;
  .to.search("accept", "base")
  .to.search("reject", 1)
  models <- list("base" , "current", "last" , 4 , "previous" , 5 , "next") ;
  for (i in models) {
    print(i)
    print(.encode.model.1(i))
    .to.search("fit", i)
  }

  ".to.search.models" ;
  .to.search ("fit", 2:3)
  .to.search ("fit", c("current", "base"))
  .to.search ("fit", c("base", "current"))
  ".to.search.models( ... accept ...) " ;
  .to.search ("accept", c("base", "current"))
  .to.search ("reject", c(1, "last"))

  ".to.search.interval" ;
  .to.search ("fit",     "interval", a = 1, b = 6)
  .to.search ("acccept", "interval", a = 2, b = 6)
  .to.search ("reject",  "interval", a = 1, b = 1)

  status("eh")

  rm( models)

  .quit()
