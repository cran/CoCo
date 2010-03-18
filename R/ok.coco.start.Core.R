"ok.coco.start" <-
function (result) 
{
    if ((result$ifail == .apiVersion()) | (result$ifail == 0)) 
        NULL
    else if ((50 <= result$ifail) & (result$ifail <= 60)) 
        ok.coco(result)
    else warning("Old version of CoCo object file")
}
