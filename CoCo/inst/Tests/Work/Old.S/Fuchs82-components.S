library(CoCo)
data(Fuchs82)
read.model("mdp,da,am,dg,gs,sa", coco.id=Fuchs82)

# set.switch(124, "on")
set.switch(127, "on")

return.components(model = FALSE, type = "connected.component", set = "dm",
		  coco.id = Fuchs82)
return.components(model = FALSE, type = "connected.components", 
		  coco.id = Fuchs82)
return.components(model = FALSE, type = "prime.components", 
		  coco.id = Fuchs82)
return.components(model = FALSE, type = "junction.tree.components", 
		  coco.id = Fuchs82)
return.components(model = FALSE, type = "chain.components", 
		  coco.id = Fuchs82)
return.components(model = FALSE, type = "ancestral.set", 
		  coco.id = Fuchs82)
return.components(model = FALSE, type = "shortests.paths", u = "s", v = "p", 
		  coco.id = Fuchs82)
return.components(model = FALSE, type = "cut.sets", u = "s", v = "p", 
		  coco.id = Fuchs82)
return.components(model = FALSE, type = "cut.sets", set.a = "sg", set.b = "p",
	          coco.id = Fuchs82)
return.components(model = FALSE, type = "separators", 
		  coco.id = Fuchs82)
return.components(model = FALSE, type = "d-separators", 
		  coco.id = Fuchs82)
return.components(model = FALSE, type = "neighbours", set = "dm", 
		  coco.id = Fuchs82)
return.components(model = FALSE, type = "is.separator", set = "dm", 
		  coco.id = Fuchs82)
return.components(model = FALSE, type = "is.d-separator", set = "dm", 
		  coco.id = Fuchs82)

# read.model()
# backward(recursive = TRUE, follow = TRUE)
# current()
# printModel("all")

# editModel("drop.edges", "md")
# current()
# printModel("all")

.quit()


