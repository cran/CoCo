library(CoCo)
data(Fuchs82)
read.model("mdp,da,am,dg,gs,sa", coco.id=Fuchs82)

# set.switch(124, "on")
set.switch(127, "on")

is.set("in.one.clique", set = "d,m.", coco.id = Fuchs82)

is.set(query = "separator", set = "ds.", coco.id = Fuchs82)
is.set(query = "separator", set = "a.", coco.id = Fuchs82)
is.set(query = "separator", set = "ag.", coco.id = Fuchs82)
is.set(query = "separator", set = "adm.", coco.id = Fuchs82)
is.set(query = "separator", set = "am.", coco.id = Fuchs82)
is.set(query = "separator", set = "dm.", coco.id = Fuchs82)

return.components(model = FALSE, type = "is.separator", set = "dm.", 
		  coco.id = Fuchs82)

# read.model()
# backward(recursive = TRUE, follow = TRUE)
# current()
# printModel("all")

# editModel("drop.edges", "md")
# current()
# printModel("all")

.quit()


