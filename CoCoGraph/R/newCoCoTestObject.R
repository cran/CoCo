"newCoCoTestObject" <-
function (test) 
{
    result <- new("CoCoTestClass", df = test["df"], deviance = test["deviance"], 
        p = 1 - pchisq(test["deviance"], test["df"]))
    return(result)
}
