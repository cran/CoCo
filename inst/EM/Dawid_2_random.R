#
#   This example illustrates the use of latent variables in CoCo.
#   The example is time consuming.
#
#
#   The data is found and analyzed in
#
#     Dawid, A. P. and Skene, A. M. (1979). Maximum likelihood
#     estimation of observed errorrates using the EM algorithm.
#     Appl. Statist. 28, 20-28.
#
#

library(CoCo)
data(Dawid79)

CoCo.Object <- makeCoCo();

optionsCoCo(diary.name="dawid2random.dia", 
            force.files = TRUE, object = CoCo.Object);
optionsCoCo(report.name="dawid2random.rep", 
            force.files = TRUE, object = CoCo.Object);
optionsCoCo(timer  = TRUE, object = CoCo.Object);

# Exclude "b", "c", "v" and "w":

Dawid79.ax2345 <- Dawid79[,-c(2:3, 5:6)]

levels <- apply(Dawid79.ax2345, 2, max)[-1];
missing <- rep(0, ncol(Dawid79.ax2345) - 1);
names(missing) <- names(levels)
missing["x"] <- 1
result <- enterNames(names(levels), levels - missing, missing = missing,
                          object = CoCo.Object);
# Sorry about this hidden command:
.set.datastructure(code = "necessary", object = CoCo.Object);
# You can ignore the warning on "File ad datastructure" under Linux/Unix.
enterList(c(t(Dawid79.ax2345)), accumulated = TRUE, object = CoCo.Object);
showTable(output.form = "sparse.table", object = CoCo.Object);

optionsCoCo(trace  = TRUE, object = CoCo.Object);
optionsCoCo(report = TRUE, object = CoCo.Object);

# set option on 126 ?

emOn(object = CoCo.Object);
optionsCoCo("em.epsilon", object = CoCo.Object);
optionsCoCo(em.epsilon = 0.00001, object = CoCo.Object);
optionsCoCo("em.initial", object = CoCo.Object);
optionsCoCo(em.initial = "mean", object = CoCo.Object);
optionsCoCo(exact.seed = round(runif(1)*10000))
optionsCoCo(em.initial = "mean",    object = CoCo.Object);
optionsCoCo(em.initial = "uniform", object = CoCo.Object);
optionsCoCo(em.initial = "input",   object = CoCo.Object);
optionsCoCo(em.initial = "first",   object = CoCo.Object);
optionsCoCo(em.initial = "last",    object = CoCo.Object);
optionsCoCo(exact.seed = 117)
optionsCoCo(exact.seed = round(runif(1)*10000))
optionsCoCo(em.initial = "random",  object = CoCo.Object);

enterModel("ax,2x,3x,4x,5x", object = CoCo.Object);
showDeviance(object = CoCo.Object);
returnDeviance(object = CoCo.Object);
.show.log.lik(object = CoCo.Object);

showTable("probabilities", "ax", object = CoCo.Object);
showTable("probabilities", "2x", object = CoCo.Object);
showTable("probabilities", "3x", object = CoCo.Object);
showTable("probabilities", "4x", object = CoCo.Object);
showTable("probabilities", "5x", object = CoCo.Object);

showTable(output.form = "case.list", object = CoCo.Object);

endCoCo(object = CoCo.Object);

quit()
