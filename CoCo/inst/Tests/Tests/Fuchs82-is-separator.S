library(CoCo)
data(Fuchs82)
enterModel("mdp,da,am,dg,gs,sa", object=Fuchs82)

# .set.switch(124, "on")
.set.switch(127, "on")

propertySet("in.one.clique", set = "d,m.", object = Fuchs82)

propertySet(query = "separator", set = "ds.", object = Fuchs82)
propertySet(query = "separator", set = "a.", object = Fuchs82)
propertySet(query = "separator", set = "ag.", object = Fuchs82)
propertySet(query = "separator", set = "adm.", object = Fuchs82)
propertySet(query = "separator", set = "am.", object = Fuchs82)
propertySet(query = "separator", set = "dm.", object = Fuchs82)

returnSets(model = FALSE, type = "is.separator", set = "dm.", 
		  object = Fuchs82)

# enterModel()
# backward(recursive = TRUE, follow = TRUE)
# makeCurrent("last")
# showModel("all")

# editModel("drop.edges", "md")
# makeCurrent("last")
# showModel("all")

quitCoCo()


