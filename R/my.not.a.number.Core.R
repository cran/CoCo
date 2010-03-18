# is.element(Sys.info()["sysname"] , c("Windows"))
# .Platform$OS.type == "windows"
"my.not.a.number" <-
function () if (Sys.info()["sysname"]=="Windows") -99999999 else NA
