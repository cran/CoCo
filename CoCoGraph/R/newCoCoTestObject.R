"newCoCoTestObject" <-
function (test) 
{
    use.ic <- FALSE
    n.cases <- test["number.of.cases"]
    df <- test["df"]
    adj <- test["adj"]
    adj.df <- df - adj
    n.tables <- test["number.of.tables"]
    deviance <- test["deviance"]
    gamma <- test["gamma"]
    gamma.s <- test["gamma.s"]
    e.deviance <- test["e.deviance"]
    e.gamma.2 <- test["e.gamma.2"]
    if (use.ic) 
        if ((!is.numeric(use.ic)) && (is.na(deviance) || is.na(df) || 
            is.na(adj) || is.na(n.cases) || (n.cases == 0))) 
            p <- 0
        else p <- -(deviance - (adj.df * ifelse(is.numeric(use.ic), 
            use.ic, log(n.cases))))
    else {
        if ((!is.na(gamma)) && (-2 < gamma) && (gamma < 2)) {
            if ((!is.na(e.gamma.2)) && (n.tables > 0) && (e.gamma.2 > 
                -1)) 
                p <- e.gamma.2
            else if ((!is.na(gamma.s)) && (gamma.s > 0)) 
                p <- 2 * (1 - pnorm((abs(gamma)/sqrt(gamma.s))))
            else p <- 0
        }
        else {
            if ((!is.na(e.deviance)) && (n.tables > 0) && (e.deviance > 
                -1)) 
                p <- e.deviance
            else if (!(is.na(deviance) || is.na(df) || is.na(adj) || 
                (adj.df <= 0))) 
                p <- (1 - ifelse((adj.df > 0), pchisq(deviance, 
                  adj.df), 0))
            else p <- NA
        }
    }
    result <- new("CoCoTestClass", df = df, deviance = deviance, 
        p = p)
    return(result)
}
