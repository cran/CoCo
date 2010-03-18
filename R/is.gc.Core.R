"is.gc" <-
function (model = "*") 
{
    if (is.character(model)) # (!(class(model) == "CoCoModelClass"))
        if (.encode(c("base", "current", "last", "previous", "next", "all"), 
             model, -(1:6), 1) < 0) 
           return(FALSE)
        else return(TRUE) # = is.character(model)
    else return(FALSE)
}
