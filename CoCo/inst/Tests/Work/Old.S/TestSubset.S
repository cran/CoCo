
### .char.ok <- F

library(CoCo);

CoCoObject <- make.coco(n = 2048, p = 1024, q = 128, title = "First CoCo Object", 
                        location = c(50, 50), manager = T, silent = F)

enter.names(":Gender:Smoking:Sex:Drinking:Drugs", rep(2, 5), c(4, 3, 2, 1))

#enter.names(":Smoking:Sex:Drinking:Drugs", rep(2, 4), c( 3, 2, 1))

###status("specification")

levels <- .return.level.list () ;
missing <- .return.missing.list () ;

n <- 1000
list <- floor(runif(n*length(levels), 1, levels + 1))
case.list <- matrix(list, ncol = length(levels), byrow = T)

optionsCoCo(digits.table = 4, decimals.table.probabilities = 1, 
            decimals.table.expected = 1, decimals.table.residual = 1)

### set.page.formats(c(63, 79))

# set.use.variables("subset", ":Smoking:Sex:Drugs;")

set.use.variables(":Smoking:Sex:Drugs")

###status("specification")

enter.list(list, accumulated = F, ncol = F, select.case.fun = F, columns = F,
           silent = F, coco.id = CoCoObject)

printTable("observed", "*")

rm(  CoCoObject, levels, missing, n, list, case.list)

### status("observations")

### set.switch(124, "on")
### set.switch(124, "what")

### source("TestDefault.S")

.quit()
