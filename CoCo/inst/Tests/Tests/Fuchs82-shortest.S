library(CoCo)
data(Fuchs82)
enterModel("mdp,da,am,dg,gs,sa", object=Fuchs82)

# .set.switch(124, "on")
.set.switch(127, "on")

returnSets(model = FALSE, type = "shortests.paths", u = "s", v = "p", 
		  object = Fuchs82)

# enterModel()
# backward(recursive = TRUE, follow = TRUE)
# makeCurrent("last")
# showModel("all")

# editModel("drop.edges", "md")
# makeCurrent("last")
# showModel("all")

quitCoCo()


