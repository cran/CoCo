
### TestDataFiles

  library(CoCo);
  CoCoObject <- make.coco()
  set.table.formats(c(6, 2, 1, 1))

  "read.data" ;
  set.data.file("coco.dat")
  read.data () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

  status("specification")
  status("observations")
  coco.print.table("observed", "*")

  "read.specification" ;
  set.specification.file("coco.spe")
  read.specification () ;

  "read.observations" ;
  set.observations.file("coco.obs")
  read.observations () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

  status("specification")
  status("observations")
  coco.print.table("observed", "*")

  "read.specification" ;
  set.specification.file("coco.fac")
  read.factors () ;

  "read.observations" ;
  set.observations.file("coco.tab")
  coco.read.table () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

  status("specification")
  status("observations")
  coco.print.table("observed", "*")

  "read.specification" ;
  set.specification.file("coco.nms")
  read.names () ;

  "read.observations" ;
  set.observations.file("coco.lst")
  read.list () ;

  set.specification.file("what")
  set.observations.file("what")
  set.data.file("what")

  status("specification")
  status("observations")
  coco.print.table("observed", "*")

  "set.datastructure" ;
  set.datastructure () ;
  set.datastructure ("all") ;
  set.datastructure ("necessary") ;
  set.datastructure ("file") ;
  set.datastructure ("large") ;
                     
  .quit()
