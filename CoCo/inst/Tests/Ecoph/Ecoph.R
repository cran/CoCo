
data(esoph); esoph;

  # Accumulated case list:
  esoph.ca <- cbind(esoph[,c(4,1:3)], type = "Case")
  esoph.co <- cbind(esoph[,c(5,1:3)], type = "Control")
  dimnames(esoph.ca)[[2]][1] <- "n"
  dimnames(esoph.co)[[2]][1] <- "n"
  Esoph <- rbind(esoph.ca, esoph.co)
  ESOPH <- Esoph
  for (i in 2:5) ESOPH[,i] <- as.numeric(Esoph[,i])
  levels <- apply(ESOPH, 2, max)[-1]; 
  missing <- c(rep(0, 4));

names(levels) <- paste(":", names(levels), sep = "")
library(CoCo);
CoCoObject <- makeCoCo();
enterNames(names(levels), levels - missing, missing = missing, object = CoCoObject);
enterList(c(t(ESOPH)), accumulated = TRUE, object = CoCoObject);
showOptions("specification", object = CoCoObject);
returnTable("sparse.table", object = CoCoObject);
showTable("observed", "*", output.form = "sparse.table", object = CoCoObject);
delta <- 0
CoCo.Object <- CoCoObject
showOptions("specification", object = CoCo.Object);
variableDescription <- returnVariableDescription(object = CoCo.Object);
str(variableDescription);
variableNames <- paste(variableDescription$names, collapse = "")
print(variableNames)
discrete <- variableDescription$levels != 0
discrete.outer <- length(discrete[!discrete]) > 1
discreteNames <- paste(variableDescription$names[discrete], collapse = "")
print(discreteNames)
enterModel(".;", object = CoCo.Object);
n.cells <- prod(variableDescription$levels[discrete])
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
print(discrete)
print(c(dim(data), length(discrete[discrete]), length(discrete[!discrete]), 
                   n.cells, min.count, ok, discrete.outer))
if (ok) {
optionsCoCo("bic" = FALSE); optionsCoCo("ic" = FALSE);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, 
        object = CoCo.Object);
# g <- dynamic.Graph("last", object = CoCo.Object)


lastModel <- makeModel("last")
g <- dynamic.Graph(lastModel)

}
endCoCo(object = CoCoObject);
