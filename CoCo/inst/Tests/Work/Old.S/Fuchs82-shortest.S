library(CoCo)
data(Fuchs82)
read.model("mdp,da,am,dg,gs,sa", coco.id=Fuchs82)

# set.switch(124, "on")
set.switch(127, "on")

return.components(model = FALSE, type = "shortests.paths", u = "s", v = "p", 
		  coco.id = Fuchs82)

# read.model()
# backward(recursive = TRUE, follow = TRUE)
# current()
# printModel("all")

# editModel("drop.edges", "md")
# current()
# printModel("all")

.quit()


