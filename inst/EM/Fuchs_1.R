#
#
#   This example illustrates the estimation of missing values by
#   the EM-algorithm in CoCo.  Data is read from the source file.
#   Table 2 of Fuchs (1982) is estimated.
#   Five different initial values is tried for the EM-algorithm.
#
#
#     Fuchs, C. (1982).    Maximum likelihood estimation and
#     model selection in contingencytables with missing data.
#     J. Amer. Statist Soc. 77, 270-278.
#
#
#	Variable		Levels				Name
#
#	Survival 		status	survived, deceased	D
#	Group membership	experiment, control		G
#	Sex			male, female			S
#	Age			less than 75, over 75		A
#	Physical status		poor, good			P
#	Mental status		poor, good			M
#
#


library(CoCo)
data(Fuchs82)

CoCo.Object <- makeCoCo();

optionsCoCo(diary.name="fuchs1.dia", 
            force.files = TRUE, object = CoCo.Object);
optionsCoCo(report.name="fuchs1.rep", 
            force.files = TRUE, object = CoCo.Object);
optionsCoCo(timer  = TRUE, object = CoCo.Object);
enterTable(Fuchs82, missing = c(0, 0, 0, 0, 1, 1), object = CoCo.Object);
# Sorry about this hidden command:
.set.datastructure(code = "necessary", object = CoCo.Object);
# You can ignore the warning on "File ad datastructure" under Linux/Unix.

showOptions("specification", object = CoCo.Object);
str(optionsCoCo(section="formats", object = CoCo.Object))
optionsCoCo(digits.table = 6, decimals.table.probabilities = 3, 
            object = CoCo.Object)
showTable(output.form = "sparse.table", object = CoCo.Object)
showTable("*", type = "observed", object = CoCo.Object)
ftable(returnTable("*", type = "observed", object = CoCo.Object), 
       col.vars = 1:4)

emOn(object = CoCo.Object);
optionsCoCo("em.epsilon", object = CoCo.Object);
optionsCoCo(em.epsilon = 0.0001, object = CoCo.Object);

optionsCoCo(trace  = TRUE, object = CoCo.Object);
optionsCoCo(report = TRUE, object = CoCo.Object);

# set option on 126 ?

optionsCoCo(adjusted.df = FALSE)

optionsCoCo("em.initial", object = CoCo.Object);

optionsCoCo(em.initial = "uniform", object = CoCo.Object);
enterModel("*", object = CoCo.Object);
returnDeviance(object = CoCo.Object);
ftable(returnTable("*", type = "observed", object = CoCo.Object), 
       col.vars=1:2)
showTable("*", type = "observed", object = CoCo.Object)
showTable(output.form = "case.list", object = CoCo.Object);

optionsCoCo(em.initial = "first",   object = CoCo.Object);
enterModel("*", object = CoCo.Object);
returnDeviance(object = CoCo.Object);
showTable("*", type = "observed", object = CoCo.Object)
showTable(output.form = "case.list", object = CoCo.Object);

optionsCoCo(em.initial = "last",    object = CoCo.Object);
enterModel("*", object = CoCo.Object);
returnDeviance(object = CoCo.Object);
showTable("*", type = "observed", object = CoCo.Object)
showTable(output.form = "case.list", object = CoCo.Object);

optionsCoCo(em.initial = "mean",    object = CoCo.Object);
enterModel("*", object = CoCo.Object);
returnDeviance(object = CoCo.Object);
showTable("*", type = "observed", object = CoCo.Object)
showTable(output.form = "case.list", object = CoCo.Object);

optionsCoCo(exact.seed = round(runif(1)*10000))
optionsCoCo(em.initial = "random",  object = CoCo.Object);
enterModel("*", object = CoCo.Object);
returnDeviance(object = CoCo.Object);
showTable("*", type = "observed", object = CoCo.Object)
showTable(output.form = "case.list", object = CoCo.Object);

# You will (might) get different results:

optionsCoCo(exact.seed = round(runif(1)*10000))
optionsCoCo(em.initial = "random",  object = CoCo.Object);
enterModel("*", object = CoCo.Object);
returnDeviance(object = CoCo.Object);
showTable("*", type = "observed", object = CoCo.Object)
showTable(output.form = "case.list", object = CoCo.Object);

optionsCoCo(exact.seed = round(runif(1)*10000))
optionsCoCo(em.initial = "random",  object = CoCo.Object);
enterModel("*", object = CoCo.Object);
returnDeviance(object = CoCo.Object);
showTable("*", type = "observed", object = CoCo.Object)
showTable(output.form = "case.list", object = CoCo.Object);

optionsCoCo(em.initial = "input",   object = CoCo.Object);
enterModel("*", object = CoCo.Object);
returnDeviance(object = CoCo.Object);
showTable("*", type = "observed", object = CoCo.Object)
showTable(output.form = "case.list", object = CoCo.Object);

# Print the observed table:

excludeMissing("off",  object = CoCo.Object)
showTable("*", type = "observed", object = CoCo.Object)
showTable(output.form = "case.list", object = CoCo.Object);

# Print only observations with complete information:

excludeMissing("on",  object = CoCo.Object)
showTable("*", type = "observed", object = CoCo.Object)
showTable(output.form = "case.list", object = CoCo.Object);

endCoCo(object = CoCo.Object);

quit()
