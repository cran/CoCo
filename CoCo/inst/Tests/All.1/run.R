library(CoCo);
library(CoCoCg);

runCg.forward <- function(data, name, 
                          size = c(dim(data)[2], h(data), dim(data)[1]),
                          do.eh = size[1] < 7, print.tables = TRUE) {
print(paste(" - - - - - - - - - - - - - - - ", name, " - - - - - - - - - - - - - - - "))
CoCo.Object <- makeCoCoCg();
result <- enterDataFrame(data, object = CoCo.Object);
sinkCoCo(paste(name, ".dia", sep = ""), type = "diary", object = CoCo.Object)
sinkCoCo(paste(name, ".rpt", sep = ""), type = "report", object = CoCo.Object)
sinkCoCo(paste(name, ".dmp", sep = ""), type = "dump", object = CoCo.Object)
sinkCoCo(paste(name, ".log", sep = ""), type = "log", object = CoCo.Object)
showOptions("specification", object = CoCo.Object);
variableDescription <- returnVariableDescription(object = CoCo.Object);
variableNames <- paste(variableDescription$names, collapse="")
enterModel(".;", object = CoCo.Object);
optionsCoCo("bic" = FALSE); optionsCoCo("ic" = FALSE);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, 
		 object = CoCo.Object);
# g <- dynamic.Graph("last", object = CoCo.Object)
if (do.eh) {
  eh(object = CoCo.Object)
  ehExtract(class = "accepted", object = CoCo.Object)
  ehExtract(class = "rejected", object = CoCo.Object)
  ehExtract(class = "a.dual",   object = CoCo.Object)
  ehExtract(class = "r.dual",   object = CoCo.Object)
  eh(sub.class = "hierarchical", object = CoCo.Object)
  ehExtract(class = "accepted", sub.class = "hierarchical", object = CoCo.Object)
  ehExtract(class = "rejected", sub.class = "hierarchical", object = CoCo.Object)
  ehExtract(class = "a.dual",   sub.class = "hierarchical", object = CoCo.Object)
  ehExtract(class = "r.dual",   sub.class = "hierarchical", object = CoCo.Object)
}
exportCoCo(paste(name, ".xpt", sep = ""), object = CoCo.Object)
showModel("all", object = CoCo.Object)
makeCurrent("last", object = CoCo.Object)
showFormula(object = CoCo.Object)
str(returnJunctionTree(object = CoCo.Object))
discrete <- variableDescription$levels != 0
discrete.outer <- length(discrete[!discrete]) > 0
if (print.tables) {
  r <- returnTable("moment",    variableNames, split = TRUE, object = CoCo.Object); print(r);
  r <- returnTable("ms",        variableNames, split = TRUE, object = CoCo.Object); print(r);
  r <- returnTable("canonical", variableNames, split = TRUE, object = CoCo.Object); print(r);
  r <- returnTable("mk",        variableNames, split = TRUE, object = CoCo.Object); print(r);
}
showOptions("data", object = CoCo.Object);
endCoCo(object = CoCo.Object);
print(warnings())
}