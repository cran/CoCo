
### TestDataFiles

  library(CoCo);
  library(CoCoOldData);
  CoCoObject <- make.coco()
  optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
              decimals.table.expected = 1, decimals.table.residual = 1)

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

  "read.data" ;
  set.data.file("coco.dat")
  status("files")
  read.data () ;

  set.specification.file("what")
  set.observations.file("what"); set.data.file("what")

  status("specification"); status("observations"); status("files")
  printTable("observed", "*")

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

  "read.specification" ;
  set.specification.file("coco.spe")
  status("files")
  read.specification () ;

  "read.observations" ;
  set.observations.file("coco.obs")
  status("files")
  read.observations () ;

  set.specification.file("what")
  set.observations.file("what"); set.data.file("what")

  status("specification"); status("observations"); status("files")
  printTable("observed", "*")

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

  "read.specification: Factors" ;
  set.specification.file("coco.fac")
  status("files")
  read.factors () ;

  "read.observations: Table" ;
  set.observations.file("coco.tab")
  status("files")
  read.table.coco () ;

  set.specification.file("what")
  set.observations.file("what"); set.data.file("what")

  status("specification"); status("observations"); status("files")
  printTable("observed", "*")

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

  "read.specification: Names" ;
  set.specification.file("coco.nms")
  read.names () ;

  "read.observations: List" ;
  set.observations.file("coco.lst")
  status("files")
  read.list () ;

  set.specification.file("what")
  set.observations.file("what"); set.data.file("what")

  status("specification"); status("observations"); status("files")
  printTable("observed", "*")

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

  "set.datastructure" ;
  set.datastructure () ;
  set.datastructure ("all") ;
  set.datastructure ("necessary") ;
  set.datastructure ("file") ;
  set.datastructure ("large") ;
                     
  .quit()
