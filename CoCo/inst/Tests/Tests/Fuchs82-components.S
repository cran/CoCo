library(CoCo)
data(Fuchs82)
enterModel("mdp,da,am,dg,gs,sa", object=Fuchs82)

# .set.switch(124, "on")
.set.switch(127, "on")

returnSets(model = FALSE, type = "connected.component", set = "dm",
		  object = Fuchs82)
returnSets(model = FALSE, type = "connected.components", 
		  object = Fuchs82)
returnSets(model = FALSE, type = "prime.components", 
		  object = Fuchs82)
returnSets(model = FALSE, type = "junction.tree.components", 
		  object = Fuchs82)
returnSets(model = FALSE, type = "chain.components", 
		  object = Fuchs82)
returnSets(model = FALSE, type = "ancestral.set", 
		  object = Fuchs82)
returnSets(model = FALSE, type = "shortests.paths", u = "s", v = "p", 
		  object = Fuchs82)
returnSets(model = FALSE, type = "cut.sets", u = "s", v = "p", 
		  object = Fuchs82)
returnSets(model = FALSE, type = "cut.sets", set.a = "sg", set.b = "p",
	          object = Fuchs82)
returnSets(model = FALSE, type = "separators", 
		  object = Fuchs82)
returnSets(model = FALSE, type = "d-separators", 
		  object = Fuchs82)
returnSets(model = FALSE, type = "neighbours", set = "dm", 
		  object = Fuchs82)
returnSets(model = FALSE, type = "is.separator", set = "dm", 
		  object = Fuchs82)
returnSets(model = FALSE, type = "is.d-separator", set = "dm", 
		  object = Fuchs82)

# enterModel()
# backward(recursive = TRUE, follow = TRUE)
# makeCurrent("last")
# showModel("all")

# editModel("drop.edges", "md")
# makeCurrent("last")
# showModel("all")

quitCoCo()


