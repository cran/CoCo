
### TestQQtables

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
              decimals.table.expected = 1, decimals.table.residual = 1)

  read.model(".;")
  read.model("*;")
  base()

  "read.q.table" ;
###     read.q.table ("AB") ;
###   
###   
###   1 0 0 1 ;

  printTable("zero", "AB")
  "read.q.table" ;
###     read.q.table ("CDE") ;
###   
###   
###   1 0 0 1
###   0 1 1 0 ;

  printTable("zero", "CDE")
  status("data")

  "clean.data" ;
  clean.data () ;
  printTable("observed", "AB")
  printTable("observed", "CDE")
  printTable("observed", "*")
  

  "dispose.of.q.table" ;
  dispose.of.q.table ("AB") ;
  status("data")
  dispose.of.q.table ("CDE") ;
  status("data")

  "read.q.list" ;
###     read.q.list ("ABC") ;
###   
###   
###   1 1 1
###   1 1 2 ;

  printTable("zero", "ABC")
  status("data")

  "read.q.list" ;
###     read.q.list ("CD") ;
###   
###   
###   1 1
###   2 2 ;

  printTable("zero", "CD")
  status("data")

  "read.q.list" ;
###     read.q.list ("EF") ;
###   
###   
###   1 2
###   2 1 ;

  printTable("zero", "EF")
  status("data")

  "clean.data" ;
  clean.data () ;
  printTable("observed", "ABC")
  printTable("observed", "CD")
  printTable("observed", "EF")
  printTable("observed", "*")

  "dispose.of.q.table" ;
  dispose.of.q.table ("ABC") ;
  status("data")
  dispose.of.q.table ("CD") ;
  status("data")
  dispose.of.q.table ("EF") ;
  status("data")

  "dispose.of.all.q.tables" ;
  clean.up(code = "q-tables") ;
  status("data")

  "enter.q.table" ;
  enter.q.table ("AB",  c(1, 0, 0, 1)) ;
  printTable("zero", "AB")
  enter.q.table ("CDE", c(1, 0, 0, 1, 0, 1, 1, 0)) ;
  printTable("zero", "CDE")

  "clean.data" ;
  clean.data () ;
  printTable("observed", "AB")
  printTable("observed", "CDE")
  printTable("observed", "*")
  

  "enter.q.list" ;
  enter.q.list ("ABC", c(1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1, 1)) ;
  printTable("zero", "ABC")
  enter.q.list ("CD", c(1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1, 1)) ;
  printTable("zero", "EF")
  enter.q.list ("EF", c(1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1, 1)) ;
  printTable("zero", "EF")

#  "enter.q.table" ;
#  enter.q.table ("EF",  c(1, 0, 0, 1)) ;
#  printTable("zero", "EF")
#  enter.q.table ("CD",  c(1, 0, 0, 1)) ;
#  printTable("zero", "CD")
#  enter.q.table ("ABC", c(1, 0, 0, 1, 0, 1, 1, 0)) ;
#  printTable("zero", "ABC")

  "clean.data" ;
  clean.data () ;
  printTable("observed", "ABC")
  printTable("observed", "CD")
  printTable("observed", "EF")
  printTable("observed", "*")

  "dispose.of.q.table" ;
  dispose.of.q.table ("AB") ;
  status("data")
  dispose.of.q.table ("CDE") ;
  status("data")

  "dispose.of.q.table" ;
  dispose.of.q.table ("ABC") ;
  status("data")
  dispose.of.q.table ("CD") ;
  status("data")
  dispose.of.q.table ("EF") ;
  status("data")

  "dispose.of.all.q.tables" ;
  clean.up(code = "q-tables") ;
  status("data")

# "X.substitute" ;                    substitute.data () ;
# "em.on" ;                           em.on () ;

  .quit()
