library(CoCo)
data(Fuchs82)
read.model("mdp,da,am,dg,gs,sa", coco.id=Fuchs82)

##

# set.switch(124, "on")
# set.switch(127, "on")

editModel("collaps.model", "adm", coco.id=Fuchs82)
editModel("marginal.model", "adm", coco.id=Fuchs82)

editModel("collaps.model", "gadm", coco.id=Fuchs82)
editModel("marginal.model", "gadm", coco.id=Fuchs82)

editModel("collaps.model", "gadp", coco.id=Fuchs82)
editModel("marginal.model", "gadp", coco.id=Fuchs82)

editModel("collaps.model", "agp", coco.id=Fuchs82)
editModel("marginal.model", "agp", coco.id=Fuchs82)
printModel("all")

# read.model()
# backward(recursive = TRUE, follow = TRUE)
# current()
# printModel("all")

# editModel("drop.edges", "md")
# current()
# printModel("all")

.quit()
