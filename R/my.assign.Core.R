"my.assign" <-
function (x, value, frame) 
{
    if (frame == 0) 
        assign(x, value, pos = .GlobalEnv)
    else assign(x, value, pos = frame + 1)
}
