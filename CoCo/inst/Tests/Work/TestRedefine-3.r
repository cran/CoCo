
### TestRedefine

  library(CoCo);
  CoCoObject <- make.coco()
  set.table.formats(c(6, 2, 1, 1))

  "read.specification" ;
  set.specification.file("coco.nms")
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
  read.list () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

  status("specification")
  status("observations")
  coco.print.table("observed", "*")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  "read.specification" ;
  set.specification.file("coco.nms")
  read.names () ;

  "redefine.factor" ;
  redefine.factor ("A", 1, 1) ;

  "skip.missing" ;
  skip.missing () ;

  "read.observations" ;
  set.observations.file("coco.tab")
  coco.read.table () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

  status("specification")
  status("observations")
  coco.print.table("observed", "*")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  "read.specification" ;
  set.specification.file("coco.nms")
  read.names () ;

#  "redefine.factor" ;
#  redefine.factor ("A", 1, 1) ;

  "read.observations" ;
  set.observations.file("coco.tab")
  coco.read.table () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

#  "exclude.missing" ;
#  exclude.missing ("on") ;

  status("specification")
  status("observations")
  coco.print.table("observed", "*")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  "read.specification" ;
  set.specification.file("coco.nms")
  read.names () ;

  "redefine.factor" ;
  redefine.factor ("A", 1, 1) ;

  "read.observations" ;
  set.observations.file("coco.tab")
  coco.read.table () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

  status("specification")
  status("observations")
  coco.print.table("observed", "*")

  "em.on" ;
  em.on () ;

  status("specification")
  status("observations")
  coco.print.table("observed", "*")

# """X.substitute" ;                    X.substitute () ;
# """em.on" ;                           em.on () ;

  .quit()
