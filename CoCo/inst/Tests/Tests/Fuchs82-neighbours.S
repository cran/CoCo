library(CoCo)
data(Fuchs82)
enterModel("mdp,da,am,dg,gs,sa", object=Fuchs82)

# .set.switch(124, "on")
# .set.switch(127, "on")

returnModel(object = Fuchs82)

returnModel(split.generators = TRUE, object = Fuchs82)

returnSets(model = FALSE, type = "neighbours", set = c("d", "m"), 
		  object = Fuchs82)

returnSets(model = FALSE, type = "neighbours", set = "dm", 
		  object = Fuchs82)

returnSets(model = FALSE, type = "neighbours", set = "[dm]", 
		  object = Fuchs82)

# enterModel()
# backward(recursive = TRUE, follow = TRUE)
# makeCurrent("last")
# showModel("all")

# editModel("drop.edges", "md")
# makeCurrent("last")
# showModel("all")

quitCoCo()


