library(CoCo);
data(Reinis);
CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);
numberVariates(object = CoCoObject);

Specification <- returnVariableDescription(object = CoCoObject);
setUseVariables(paste(Specification[[1]][c(1,3,4,6)], collapse = ""), object = CoCoObject);
enterTable(c(Reinis), object = CoCoObject);
numberVariates(object = CoCoObject);
numberVariates(full = TRUE, object = CoCoObject);

showOptions("specification", object = CoCoObject);

endCoCo(object = CoCoObject);

# - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - - #

library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);

numberVariates(object = CoCoObject);
numberVariates(full = TRUE, object = CoCoObject);

endCoCo(object = CoCoObject);

# - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - - #

library(CoCo);
data(Byssinosis38);
CoCoObject <- makeCoCo();
enterTable(Byssinosis38, object = CoCoObject);

numberVariates(object = CoCoObject);

endCoCo(object = CoCoObject);
