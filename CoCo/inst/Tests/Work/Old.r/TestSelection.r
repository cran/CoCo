
### TestSelect

library(CoCo);
library(CoCoOldData);

CoCoObject <- make.coco(n = 2048, p = 1024, q = 128, title = "First CoCo Object", 
                        location = c(50, 50), manager = T, silent = F)

enter.names("ABCDEF", rep(2, 6))

"set.ordinal" ;
set.ordinal("BD") ;

"select.cases" ;
select.cases ("AB", c(1, 1)) ;

"or.select.cases" ;
or.select.cases ("AB", c(2, 2)) ;

"reject.cases" ;
reject.cases ("CD", c(1, 1)) ;

"or.reject.cases" ;
or.reject.cases ("CD", c(2, 2)) ;

status("specification")

###status("specification")

n <- c(44, 40, 112, 67, 129,145,  12, 23,    35, 12,  80, 33, 109, 67,   7,  9,
       23, 32,  70, 66,  50, 80,   7, 13,    24, 25,  73, 57,  51, 63,   7, 16,
        5,  7,  21,  9,   9, 17,   1,  4,     4,  3,  11,  8,  14, 17,   5,  2,
        7,  3,  14, 14,   9, 16,   2,  3,     4,  0,  13, 11,   5, 14,   4,  4)

enter.table(n)

optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
            decimals.table.expected = 1, decimals.table.residual = 1)

printTable("observed", "AB")
printTable("observed", "CD")

rm(n)

.quit()
