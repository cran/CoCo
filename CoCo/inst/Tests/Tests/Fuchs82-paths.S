library(CoCo)
data(Fuchs82)
enterModel("mdp,da,am,dg,gs,sa", object=Fuchs82)

##

# .set.switch(124, "on")
# .set.switch(127, "on")

# Path:
returnVertexOrder(model = "current", 
                    path.order = TRUE, marked = paste("pd"),
                    u = "m", v = "g", object = Fuchs82)

returnVertexOrder(model = "current", invers.order = TRUE,
                    path.order = TRUE, marked = paste("pd"),
                    u = "m", v = "g", object = Fuchs82)

returnVertexOrder(model = "current", sub.path = TRUE,
                    path.order = TRUE, marked = paste("pd"),
                    u = "m", v = "g", object = Fuchs82)

returnVertexOrder(model = "current", 
                    path.order = TRUE, marked = paste("adps"),
                    u = "m", v = "g", object = Fuchs82)

returnVertexOrder(model = "current", sub.path = TRUE,
                    path.order = TRUE, marked = paste("adps"),
                    u = "m", v = "g", object = Fuchs82)

returnVertexOrder(model = "current", 
                    path.order = TRUE, marked = paste("adms"),
                    u = "p", v = "g", object = Fuchs82)


returnVertexOrder(model = "current", sub.path = TRUE,
                    path.order = TRUE, marked = paste("adms"),
                    u = "p", v = "g", object = Fuchs82)

returnVertexOrder(model = "current", 
                    path.order = TRUE, marked = paste("adms"),
                    u = "g", v = "p", object = Fuchs82)


returnVertexOrder(model = "current", sub.path = TRUE,
                    path.order = TRUE, marked = paste("adms"),
                    u = "g", v = "p", object = Fuchs82)

# enterModel()
# backward(recursive = TRUE, follow = TRUE)
# makeCurrent("last")
# showModel("all")

# editModel("drop.edges", "md")
# makeCurrent("last")
# showModel("all")

quitCoCo()
