library(CoCo)
data(Fuchs82)
read.model("mdp,da,am,dg,gs,sa", coco.id=Fuchs82)

# set.switch(124, "on")
# set.switch(127, "on")

return.model(coco.id = Fuchs82)

return.model(split.generators = TRUE, coco.id = Fuchs82)

return.components(model = FALSE, type = "neighbours", set = c("d", "m"), 
		  coco.id = Fuchs82)

return.components(model = FALSE, type = "neighbours", set = "dm", 
		  coco.id = Fuchs82)

return.components(model = FALSE, type = "neighbours", set = "[dm]", 
		  coco.id = Fuchs82)

# read.model()
# backward(recursive = TRUE, follow = TRUE)
# current()
# printModel("all")

# editModel("drop.edges", "md")
# current()
# printModel("all")

.quit()


