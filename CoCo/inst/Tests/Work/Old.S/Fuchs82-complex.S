library(CoCo)
data(Fuchs82)
read.model("mdp,da,am,dg,gs,sa", coco.id=Fuchs82)

# State-space, with generators:
return.complex(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = FALSE,
               omit.generators = FALSE, state.space = TRUE, coco.id = Fuchs82)

# State-space, without generators:
return.complex(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = FALSE,
               omit.generators = TRUE, state.space = TRUE, coco.id = Fuchs82)

printFormula()

# State-space, with generators:
return.complex(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = FALSE,
               omit.generators = FALSE, state.space = TRUE, coco.id = Fuchs82)

# State-space, without generators:
return.complex(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = FALSE,
               omit.generators = TRUE, state.space = TRUE, coco.id = Fuchs82)

# Expression:
return.complex(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = FALSE,
               omit.generators = FALSE, state.space = FALSE, coco.id = Fuchs82)

# Expression, without generators:
return.complex(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = FALSE,
               omit.generators = TRUE, state.space = FALSE, coco.id = Fuchs82)

# Prime components:
return.complex(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = TRUE,
               omit.generators = TRUE, state.space = FALSE, coco.id = Fuchs82)
return.complex(model = FALSE, type = "prime.components", coco.id = Fuchs82)

# Separators:
return.complex(model = FALSE, type = "expression", 
               omit.prime.components = TRUE, omit.separators = FALSE,
               omit.generators = TRUE, state.space = FALSE, coco.id = Fuchs82)
return.complex(model = FALSE, type = "separators", coco.id = Fuchs82)

# read.model()
# backward(recursive = TRUE, follow = TRUE)
# current()
# printModel("all")

# editModel("drop.edges", "md")
# current()
# printModel("all")

.quit()
