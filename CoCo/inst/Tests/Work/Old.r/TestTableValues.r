
### TestTableValues

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

  make.base(2)
  make.current(6)

  printModel("all")

  "coco.print.table" ;                printTable(0) ;

x <- c("counts"        , "observed"      , "probabilities" ,
       "expected"      , "unadjusted"    , "absolute"      ,
       "f-res"         , "r-f"           , "g-res"         ,
       "r-g"           , "adjusted"      , "leverage"      ,
       "c-res"         , "m-res"         , "standardized"  ,
       "standard"      , "x-res"         , "deviance"      ,
       "-2log"         , "l-res"         , "freeman-tukey" ,
       "2n-m"          , "sqrt"          , "power"         ,
       "index"         , "zero"          , "error"         )

optionsCoCo(width = 80, height = 40,
            digits.table = 6, decimals.table.probabilities = 2, 
            decimals.table.expected = 1, decimals.table.residual = 1)

for (i in x) {
  print(i)
  print(.table.value(i))
  printTable(i, "ABC")
}

optionsCoCo(digits.table = 10, decimals.table.probabilities = 6, 
            decimals.table.expected = 2, decimals.table.residual = 2)

optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
            decimals.table.expected = 1, decimals.table.residual = 1)

printTable(0, "ABCD", random = F, log.transformed = F, complete = F, permuted = T)
printTable(0, "ABCD", random = F, log.transformed = F, complete = T, permuted = T)
printTable(0, "ABCD", random = F, log.transformed = F, complete = F, permuted = T)
printTable(0, "DCBA", random = F, log.transformed = F, complete = F, permuted = F)
printTable(1, "BCDE", random = F, log.transformed = F, complete = F, permuted = T)
printTable(1, "CDEF", random = F, log.transformed = T, complete = F, permuted = T)
printTable(0, "*",    random = T, log.transformed = F, complete = F, permuted = T)

models <- list("base"   , "current", "last"   , 4        , "previous" , 5        , "next"     ) ;
for (i in models) {
  print(i)
  print(.encode.model.1(i))
  printTable("expected", "ABCD", i)
}

  "describe.table" ;
  describe.table (0) ;
  describe.table ("counts", "ABCD") ;
  describe.table ("counts", "ABCD", "base") ;
  describe.table ("expected", "ABCD", uniform = F, rankit = F, probit = F) ;
  describe.table ("expected", "ABCD", uniform = T, rankit = F, probit = F) ;
  describe.table ("expected", "ABCD", uniform = F, rankit = T, probit = F) ;
  describe.table ("expected", "ABCD", uniform = F, rankit = F, probit = T) ;
  describe.table ("expected", "ABCD", "ACE,ADE,BC,F.;") ;

  "return.vector" ;
  return.table ("observed") ;
  return.table ("observed", "EF") ;
  return.table ("observed", random = T) ;
  return.table ("observed", random = T) ;

  "XXX.return.matrix" ;
  ### XXX.return.matrix () ;

  "print.sparse.table" ;
  printTable (output.form = "sparse.table") ;
  printTable ("*", output.form = "sparse.table") ;
  printTable ("BDE", output.form = "sparse.table") ;

  "plotCoCo" ;
  plotCoCo("observed", "expected") ;
  plotCoCo("observed", "expected", set = "*", 
           X.model = F, X.random = F, X.log.transformed = F, 
           Y.model = F, Y.random = F, Y.log.transformed = F, complete = F)
  plotCoCo("observed", "observed", set = "*", 
           X.model = F, X.random = F, X.log.transformed = F, 
           Y.model = F, Y.random = T, Y.log.transformed = F, complete = F)
  plotCoCo("observed", "observed", set = "*", 
           X.model = F, X.random = F, X.log.transformed = F, 
           Y.model = F, Y.random = T, Y.log.transformed = F, complete = F)
  plotCoCo("expected", "expected", set = "*", 
           X.model = "base",   X.random = F, X.log.transformed = F, 
           Y.model = "current", Y.random = F, Y.log.transformed = F, complete = F)
  plotCoCo("expected", "expected", set = "*", 
           X.model = F, X.random = F, X.log.transformed = T, 
           Y.model = F, Y.random = F, Y.log.transformed = F, complete = F)

  "list.values" ;
  printTable (output.form = "list.all.values") ;
  printTable ("*", output.form = "list.all.values") ;
  printTable ("BDE", output.form = "list.all.values") ;

  "case.list" ;
  printTable (output.form = "case.list") ;
  printTable ("*", output.form = "case.list") ;
  printTable ("BDE", output.form = "case.list") ;

  "X.substitute" ;
  printTable(0) ;
  substitute.data () ;
  printTable(0) ;

  rm( x, models)

  .quit()
