library(CoCo)
data(Fuchs82)
enterModel("mdp,da,am,dg,gs,sa", object=Fuchs82)

.set.switch(124, "on")
.set.switch(127, "on")

returnSets(model = FALSE, type = "connected.components", 
		  object = Fuchs82)

quitCoCo()


