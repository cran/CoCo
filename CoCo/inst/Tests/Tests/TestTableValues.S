
### TestTableValues

  library(CoCo);
  ReinisCoCoObject <- Reinis()

  enterModel(".;")
  enterModel("*;")
  makeBase("current")
  enterModel("ACDE,ABCF.;")
  editModel(action = "drop.interactions", modification = "ABCF.;")
  editModel(action = "drop.edges", modification = "AC,BF.;")
  enterModel("ACE,ADE,BC,F.;")
  enterModel(".;")

  showModel("all")

  makeBase(2)
  makeCurrent(6)

  showModel("all")

  "coco.print.table" ;                showTable(0) ;

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
# print(.table.value(i))
  showTable(i, "ABC")
}

optionsCoCo(digits.table = 10, decimals.table.probabilities = 6, 
            decimals.table.expected = 2, decimals.table.residual = 2)

optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
            decimals.table.expected = 1, decimals.table.residual = 1)

showTable(0, "ABCD", random = F, log.transformed = F, complete = F, discrete.ordered = T)
showTable(0, "ABCD", random = F, log.transformed = F, complete = T, discrete.ordered = T)
showTable(0, "ABCD", random = F, log.transformed = F, complete = F, discrete.ordered = T)
showTable(0, "DCBA", random = F, log.transformed = F, complete = F, discrete.ordered = F)
showTable(1, "BCDE", random = F, log.transformed = F, complete = F, discrete.ordered = T)
showTable(1, "CDEF", random = F, log.transformed = T, complete = F, discrete.ordered = T)
showTable(0, "*",    random = T, log.transformed = F, complete = F, discrete.ordered = T)

models <- list("base"   , "current", "last"   , 4        , "previous" , 5        , "next"     ) ;
for (i in models) {
  print(i)
# print(.encode.model.1(i))
  showTable("expected", "ABCD", i)
}

  "describe.table" ;
  summaryTable (0) ;
  summaryTable ("counts", "ABCD") ;
  summaryTable ("counts", "ABCD", "base") ;
  summaryTable ("expected", "ABCD", uniform = F, rankit = F, probit = F) ;
  summaryTable ("expected", "ABCD", uniform = T, rankit = F, probit = F) ;
  summaryTable ("expected", "ABCD", uniform = F, rankit = T, probit = F) ;
  summaryTable ("expected", "ABCD", uniform = F, rankit = F, probit = T) ;
  summaryTable ("expected", "ABCD", "ACE,ADE,BC,F.;") ;

  "return.vector" ;
  returnTable ("observed") ;
  returnTable ("observed", "EF") ;
  returnTable ("observed", random = T) ;
  returnTable ("observed", random = T) ;

  "XXX.return.matrix" ;
  ### XXX.return.matrix () ;

  "print.sparse.table" ;
  showTable (output.form = "sparse.table") ;
  showTable ("*", output.form = "sparse.table") ;
  showTable ("BDE", output.form = "sparse.table") ;

  "plotCoCo" ;
  .plotCoCo("observed", "expected") ;
  .plotCoCo("observed", "expected", set = "*", 
           X.model = F, X.random = F, X.log.transformed = F, 
           Y.model = F, Y.random = F, Y.log.transformed = F, complete = F)
  .plotCoCo("observed", "observed", set = "*", 
           X.model = F, X.random = F, X.log.transformed = F, 
           Y.model = F, Y.random = T, Y.log.transformed = F, complete = F)
  .plotCoCo("observed", "observed", set = "*", 
           X.model = F, X.random = F, X.log.transformed = F, 
           Y.model = F, Y.random = T, Y.log.transformed = F, complete = F)
  .plotCoCo("expected", "expected", set = "*", 
           X.model = "base",   X.random = F, X.log.transformed = F, 
           Y.model = "current", Y.random = F, Y.log.transformed = F, complete = F)
  .plotCoCo("expected", "expected", set = "*", 
           X.model = F, X.random = F, X.log.transformed = T, 
           Y.model = F, Y.random = F, Y.log.transformed = F, complete = F)

  "list.values" ;
  showTable (output.form = "list.all.values") ;
  showTable ("*", output.form = "list.all.values") ;
  showTable ("BDE", output.form = "list.all.values") ;

  "case.list" ;
  showTable (output.form = "case.list") ;
  showTable ("*", output.form = "case.list") ;
  showTable ("BDE", output.form = "case.list") ;

  "X.substitute" ;
  showTable(0) ;
  randomTableReplacement () ;
  showTable(0) ;

  rm( x, models)

  quitCoCo()
