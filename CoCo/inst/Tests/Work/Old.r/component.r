library(CoCo)
data(Fuchs82)
read.model("mdp,da,am,dg,gs,sa", coco.id=Fuchs82)

set.switch(124, "on")
set.switch(127, "on")

return.components(model = FALSE, type = "connected.component", set = ";",
		  coco.id = Fuchs82)

.quit()


