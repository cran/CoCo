
### .char.ok <- F

library(CoCo);

CoCoObject <- makeCoCo(n = 2048, p = 1024, q = 128, title = "First CoCo Object", 
                        location = c(50, 50), manager = T, silent = F)

enterNames(":Gender:Smoking:Sex:Drinking:Drugs", rep(2, 5), c(4, 3, 2, 1))

#enterNames(":Smoking:Sex:Drinking:Drugs", rep(2, 4), c( 3, 2, 1))

###showOptions("specification")

levels <- .return.level.list () ;
missing <- .return.missing.list () ;

n <- 1000
list <- floor(runif(n*length(levels), 1, levels + 1))
case.list <- matrix(list, ncol = length(levels), byrow = T)

optionsCoCo(digits.table = 4, decimals.table.probabilities = 1, 
            decimals.table.expected = 1, decimals.table.residual = 1)

### set.page.formats(c(63, 79))

# setUseVariables("subset", ":Smoking:Sex:Drugs;")

setUseVariables(":Smoking:Sex:Drugs")

###showOptions("specification")

enterList(list, accumulated = F, ncol = F, select.case.fun = F, columns = F,
           silent = F, object = CoCoObject)

showTable("observed", "*")

rm(  CoCoObject, levels, missing, n, list, case.list)

### showOptions("observations")

### .set.switch(124, "on")
### .set.switch(124, "what")

### source("TestDefault.S")
