
### TestDataFiles

  library(CoCo);
  library(CoCoOldData);
  CoCoObject <- makeCoCo()
  optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
              decimals.table.expected = 1, decimals.table.residual = 1)

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

  "read.data" ;
  set.data.file("coco.dat")
  showOptions("files")
  read.data () ;

  set.specification.file("what")
  set.observations.file("what"); set.data.file("what")

  showOptions("specification"); showOptions("observations"); showOptions("files")
  showTable("observed", "*")

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

  "read.specification" ;
  set.specification.file("coco.spe")
  showOptions("files")
  read.specification () ;

  "read.observations" ;
  set.observations.file("coco.obs")
  showOptions("files")
  read.observations () ;

  set.specification.file("what")
  set.observations.file("what"); set.data.file("what")

  showOptions("specification"); showOptions("observations"); showOptions("files")
  showTable("observed", "*")

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

  "read.specification: Factors" ;
  set.specification.file("coco.fac")
  showOptions("files")
  read.factors () ;

  "read.observations: Table" ;
  set.observations.file("coco.tab")
  showOptions("files")
  read.table.coco () ;

  set.specification.file("what")
  set.observations.file("what"); set.data.file("what")

  showOptions("specification"); showOptions("observations"); showOptions("files")
  showTable("observed", "*")

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

  "read.specification: Names" ;
  set.specification.file("coco.nms")
  read.names () ;

  "read.observations: List" ;
  set.observations.file("coco.lst")
  showOptions("files")
  read.list () ;

  set.specification.file("what")
  set.observations.file("what"); set.data.file("what")

  showOptions("specification"); showOptions("observations"); showOptions("files")
  showTable("observed", "*")

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

  "set.datastructure" ;
  .set.datastructure () ;
  .set.datastructure ("all") ;
  .set.datastructure ("necessary") ;
  .set.datastructure ("file") ;
  .set.datastructure ("large") ;
                     
  quitCoCo()
