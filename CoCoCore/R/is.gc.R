"is.gc" <-
function (model = "*") 
{
    if (.encode(c("base", "current", "last", "previous", "next", 
        "all"), model, -(1:6), 1) < 0) 
        return(FALSE)
    else is.character(model)
}
