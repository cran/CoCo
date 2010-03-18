"coco.enter.string" <-
function (code, argument = FALSE, sub.code = FALSE, length = 128,
          no.warnings = NULL, object = .currentCoCo()) 
{
    result <- call.coco.chars(code, argument, sub.code, object = object)
    if (((argument == "what") || !is.character(argument))) 
        result
    else result
}
