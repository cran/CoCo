library(CoCo)
data(Fuchs82)
read.model("mdp,da,am,dg,gs,sa", coco.id=Fuchs82)

##

# set.switch(124, "on")
# set.switch(127, "on")

# Path:
return.vertex.order(model = "current", 
                    path.order = TRUE, marked = paste("pd"),
                    u = "m", v = "g", coco.id = Fuchs82)

return.vertex.order(model = "current", invers.order = TRUE,
                    path.order = TRUE, marked = paste("pd"),
                    u = "m", v = "g", coco.id = Fuchs82)

return.vertex.order(model = "current", sub.path = TRUE,
                    path.order = TRUE, marked = paste("pd"),
                    u = "m", v = "g", coco.id = Fuchs82)

return.vertex.order(model = "current", 
                    path.order = TRUE, marked = paste("adps"),
                    u = "m", v = "g", coco.id = Fuchs82)

return.vertex.order(model = "current", sub.path = TRUE,
                    path.order = TRUE, marked = paste("adps"),
                    u = "m", v = "g", coco.id = Fuchs82)

return.vertex.order(model = "current", 
                    path.order = TRUE, marked = paste("adms"),
                    u = "p", v = "g", coco.id = Fuchs82)


return.vertex.order(model = "current", sub.path = TRUE,
                    path.order = TRUE, marked = paste("adms"),
                    u = "p", v = "g", coco.id = Fuchs82)

return.vertex.order(model = "current", 
                    path.order = TRUE, marked = paste("adms"),
                    u = "g", v = "p", coco.id = Fuchs82)


return.vertex.order(model = "current", sub.path = TRUE,
                    path.order = TRUE, marked = paste("adms"),
                    u = "g", v = "p", coco.id = Fuchs82)

# read.model()
# backward(recursive = TRUE, follow = TRUE)
# current()
# printModel("all")

# editModel("drop.edges", "md")
# current()
# printModel("all")

.quit()
