library(CoCo);
library(CoCoCg);

system("mkdir -p Dia")
system("mkdir -p Rpt")
system("mkdir -p Dmp")
system("mkdir -p Log")
system("mkdir -p Xpt")

runCg.forward <- function(data, name, 
                          size = c(dim(data)[2], h(data), dim(data)[1]),
                          do.eh = size[1] < 7, print.tables = TRUE,
                          to.factor = NULL, missing.values = c("."), delta = 0) {
print(paste(" - - - - - - - - - - - - - - - ", name, " - - - - - - - - - - - - - - - "))
CoCo.Object <- makeCoCoCg();
sinkCoCo(paste("Dia/", name, ".dia", sep = ""), 
         type = "diary", object = CoCo.Object)
sinkCoCo(paste("Rpt/", name, ".rpt", sep = ""), 
         type = "report", object = CoCo.Object)
sinkCoCo(paste("Dmp/", name, ".dmp", sep = ""), 
         type = "dump", object = CoCo.Object)
sinkCoCo(paste("Log/", name, ".log", sep = ""), 
         type = "log", object = CoCo.Object)
.set.switch("timer", object = CoCo.Object);
result <- enterDataFrame(data, to.factor = to.factor, 
                         missing.values = missing.values, object = CoCo.Object);
exportCoCo(paste("Xpt/", name, ".xpt", sep = ""), object = CoCo.Object)
showOptions("specification", object = CoCo.Object);
showOptions("data", object = CoCo.Object);
print(paste("Dimension: ", paste(dim(data), collapse = ", ")))
variableDescription <- returnVariableDescription(object = CoCo.Object);
variableNames <- paste(variableDescription$names, collapse = "")
print(c("All variables:", variableNames))
str(variableDescription);
discrete <- variableDescription$levels != 0
discrete.outer <- length(discrete[!discrete]) > 1
discreteNames <- paste(variableDescription$names[discrete], collapse = "")
continuousNames <- paste(variableDescription$names[!discrete], collapse = "")
print(discrete)
print(c("Discrete:   ", discreteNames))
print(c("Continuous: ", continuousNames))
enterModel(".;", object = CoCo.Object);
print(paste("N-variables: ", paste(c(length(discrete[discrete]), 
                                     length(discrete[!discrete])), collapse = ", ")))
n.cells <- prod(variableDescription$levels[discrete])
print(paste("N-cells: ", n.cells))
min.count <- 0
ok <- n.cells < 2^16
if (ok) {
  r <- returnTable("observed", discreteNames, object = CoCo.Object)
  print(r)
  if (is.array(r) && (length(dim(r)) > 2))
    print(ftable(r))
  min.count <- min(r)
  ok <- length(discrete[!discrete]) <= min.count + delta
}
print(paste("Min-count: ", min.count))
print(c(ok, discrete.outer))
if (!ok) {
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
  ehExtract(class = "accepted", sub.class = "hierarchical", 
            object = CoCo.Object)
  ehExtract(class = "rejected", sub.class = "hierarchical", 
            object = CoCo.Object)
  ehExtract(class = "a.dual",   sub.class = "hierarchical", 
            object = CoCo.Object)
  ehExtract(class = "r.dual",   sub.class = "hierarchical", 
            object = CoCo.Object)
}
showModel("all", object = CoCo.Object)
makeCurrent("last", object = CoCo.Object)
showFormula(object = CoCo.Object)
str(returnJunctionTree(object = CoCo.Object))
if (print.tables) {
  r <- returnTable("moment",    variableNames, split = TRUE, 
                   discrete.outer = discrete.outer, 
                   object = CoCo.Object); print(r);
  r <- returnTable("ms",        variableNames, split = TRUE, 
                   discrete.outer = discrete.outer, 
                   object = CoCo.Object); print(r);
  r <- returnTable("canonical", variableNames, split = TRUE, 
                   discrete.outer = discrete.outer, 
                   object = CoCo.Object); print(r);
  r <- returnTable("mk",        variableNames, split = TRUE, 
                   discrete.outer = discrete.outer, 
                   object = CoCo.Object); print(r);
}
} else print(c(name, "Already fitted!!!!"))
exportCoCo(paste("Xpt/", name, ".xpt", sep = ""), object = CoCo.Object)
showOptions("data", object = CoCo.Object);
endCoCo(object = CoCo.Object);
print(warnings())
}