
### TestQQtables

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
              decimals.table.expected = 1, decimals.table.residual = 1)

  enterModel(".;")
  enterModel("*;")
  makeBase("current")

  "read.q.table" ;
###     read.q.table ("AB") ;
###   
###   
###   1 0 0 1 ;

  showTable("zero", "AB")
  "read.q.table" ;
###     read.q.table ("CDE") ;
###   
###   
###   1 0 0 1
###   0 1 1 0 ;

  showTable("zero", "CDE")
  showOptions("data")

  "clean.data" ;
  cleanData () ;
  showTable("observed", "AB")
  showTable("observed", "CDE")
  showTable("observed", "*")
  

  "dispose.of.q.table" ;
  disposeOfQtable ("AB") ;
  showOptions("data")
  disposeOfQtable ("CDE") ;
  showOptions("data")

  "read.q.list" ;
###     read.q.list ("ABC") ;
###   
###   
###   1 1 1
###   1 1 2 ;

  showTable("zero", "ABC")
  showOptions("data")

  "read.q.list" ;
###     read.q.list ("CD") ;
###   
###   
###   1 1
###   2 2 ;

  showTable("zero", "CD")
  showOptions("data")

  "read.q.list" ;
###     read.q.list ("EF") ;
###   
###   
###   1 2
###   2 1 ;

  showTable("zero", "EF")
  showOptions("data")

  "clean.data" ;
  cleanData () ;
  showTable("observed", "ABC")
  showTable("observed", "CD")
  showTable("observed", "EF")
  showTable("observed", "*")

  "dispose.of.q.table" ;
  disposeOfQtable ("ABC") ;
  showOptions("data")
  disposeOfQtable ("CD") ;
  showOptions("data")
  disposeOfQtable ("EF") ;
  showOptions("data")

  "dispose.of.all.q.tables" ;
  .clean.up(code = "q-tables") ;
  showOptions("data")

  "enter.q.table" ;
  enterQtable ("AB",  c(1, 0, 0, 1)) ;
  showTable("zero", "AB")
  enterQtable ("CDE", c(1, 0, 0, 1, 0, 1, 1, 0)) ;
  showTable("zero", "CDE")

  "clean.data" ;
  cleanData () ;
  showTable("observed", "AB")
  showTable("observed", "CDE")
  showTable("observed", "*")
  

  "enter.q.list" ;
  enterQlist ("ABC", c(1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1, 1)) ;
  showTable("zero", "ABC")
  enterQlist ("CD", c(1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1, 1)) ;
  showTable("zero", "EF")
  enterQlist ("EF", c(1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 1, 1)) ;
  showTable("zero", "EF")

#  "enter.q.table" ;
#  enterQtable ("EF",  c(1, 0, 0, 1)) ;
#  showTable("zero", "EF")
#  enterQtable ("CD",  c(1, 0, 0, 1)) ;
#  showTable("zero", "CD")
#  enterQtable ("ABC", c(1, 0, 0, 1, 0, 1, 1, 0)) ;
#  showTable("zero", "ABC")

  "clean.data" ;
  cleanData () ;
  showTable("observed", "ABC")
  showTable("observed", "CD")
  showTable("observed", "EF")
  showTable("observed", "*")

  "dispose.of.q.table" ;
  disposeOfQtable ("AB") ;
  showOptions("data")
  disposeOfQtable ("CDE") ;
  showOptions("data")

  "dispose.of.q.table" ;
  disposeOfQtable ("ABC") ;
  showOptions("data")
  disposeOfQtable ("CD") ;
  showOptions("data")
  disposeOfQtable ("EF") ;
  showOptions("data")

  "dispose.of.all.q.tables" ;
  .clean.up(code = "q-tables") ;
  showOptions("data")

# "X.substitute" ;                    randomTableReplacement () ;
# "em.on" ;                           emOn () ;

  quitCoCo()
