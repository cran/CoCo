
### TestRedefine

  library(CoCo);
  library(CoCoOldData);
  CoCoObject <- makeCoCo()
  optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
              decimals.table.expected = 1, decimals.table.residual = 1)

  .set.switch(124)
  .set.switch(130)

  "read.specification" ;
  set.specification.file("coco.nms")
  showOptions("files")
  read.names () ;

  "redefine.factor" ;
  redefine.factor ("A", 1, 1) ;

  "cutpoints" ;
  cutpoints ("A", c(1.5)) ;
  cutpoints ("A", "what") ;

  "skip.missing" ;
  skip.missing () ;

  "read.observations" ;
  set.observations.file("coco.lst")
  showOptions("files")
  read.list () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

  showOptions("specification")
  showOptions("observations")
  showTable("observed", "*")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  "read.specification" ;
  set.specification.file("coco.nms")
  showOptions("files")
  read.names () ;

  "redefine.factor" ;
  redefine.factor ("A", 1, 1) ;

  "skip.missing" ;
  skip.missing () ;

  "read.observations" ;
  set.observations.file("coco.tab")
  showOptions("files")
  read.table.coco () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

  showOptions("specification")
  showOptions("observations")
  showTable("observed", "*")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  "read.specification" ;
  set.specification.file("coco.nms")
  showOptions("files")
  read.names () ;

#  "redefine.factor" ;
#  redefine.factor ("A", 1, 1) ;

  "read.observations" ;
  set.observations.file("coco.tab")
  showOptions("files")
  read.table.coco () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

#  "exclude.missing" ;
#  excludeMissing ("on") ;

  showOptions("specification")
  showOptions("observations")
  showTable("observed", "*")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  "read.specification" ;
  set.specification.file("coco.nms")
  showOptions("files")
  read.names () ;

  "redefine.factor" ;
  redefine.factor ("A", 1, 1) ;

  "read.observations" ;
  set.observations.file("coco.tab")
  showOptions("files")
  read.table.coco () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

  showOptions("specification")
  showOptions("observations")
  showTable("observed", "*")

  "em.on" ;
  emOn () ;

  showOptions("specification")
  showOptions("observations")
  showTable("observed", "*")

# """X.substitute" ;                    X.substitute () ;
# """em.on" ;                           emOn () ;

  quitCoCo()
