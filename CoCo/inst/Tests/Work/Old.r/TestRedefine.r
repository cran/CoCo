
### TestRedefine

  library(CoCo);
  library(CoCoOldData);
  CoCoObject <- make.coco()
  optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
              decimals.table.expected = 1, decimals.table.residual = 1)

  set.switch(124)
  set.switch(130)

  "read.specification" ;
  set.specification.file("coco.nms")
  status("files")
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
  status("files")
  read.list () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

  status("specification")
  status("observations")
  printTable("observed", "*")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  "read.specification" ;
  set.specification.file("coco.nms")
  status("files")
  read.names () ;

  "redefine.factor" ;
  redefine.factor ("A", 1, 1) ;

  "skip.missing" ;
  skip.missing () ;

  "read.observations" ;
  set.observations.file("coco.tab")
  status("files")
  read.table.coco () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

  status("specification")
  status("observations")
  printTable("observed", "*")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  "read.specification" ;
  set.specification.file("coco.nms")
  status("files")
  read.names () ;

#  "redefine.factor" ;
#  redefine.factor ("A", 1, 1) ;

  "read.observations" ;
  set.observations.file("coco.tab")
  status("files")
  read.table.coco () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

#  "exclude.missing" ;
#  exclude.missing ("on") ;

  status("specification")
  status("observations")
  printTable("observed", "*")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  "read.specification" ;
  set.specification.file("coco.nms")
  status("files")
  read.names () ;

  "redefine.factor" ;
  redefine.factor ("A", 1, 1) ;

  "read.observations" ;
  set.observations.file("coco.tab")
  status("files")
  read.table.coco () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

  status("specification")
  status("observations")
  printTable("observed", "*")

  "em.on" ;
  em.on () ;

  status("specification")
  status("observations")
  printTable("observed", "*")

# """X.substitute" ;                    X.substitute () ;
# """em.on" ;                           em.on () ;

  .quit()
