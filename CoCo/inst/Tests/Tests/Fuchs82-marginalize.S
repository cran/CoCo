library(CoCo)
data(Fuchs82)
enterModel("mdp,da,am,dg,gs,sa", object=Fuchs82)

##

# .set.switch(124, "on")
# .set.switch(127, "on")

editModel("collaps.model", "adm", object=Fuchs82)
editModel("marginal.model", "adm", object=Fuchs82)

editModel("collaps.model", "gadm", object=Fuchs82)
editModel("marginal.model", "gadm", object=Fuchs82)

editModel("collaps.model", "gadp", object=Fuchs82)
editModel("marginal.model", "gadp", object=Fuchs82)

editModel("collaps.model", "agp", object=Fuchs82)
editModel("marginal.model", "agp", object=Fuchs82)
showModel("all")

# enterModel()
# backward(recursive = TRUE, follow = TRUE)
# makeCurrent("last")
# showModel("all")

# editModel("drop.edges", "md")
# makeCurrent("last")
# showModel("all")

quitCoCo()
