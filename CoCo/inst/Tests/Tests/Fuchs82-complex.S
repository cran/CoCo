library(CoCo)
data(Fuchs82)
enterModel("mdp,da,am,dg,gs,sa", object=Fuchs82)

# State-space, with generators:
returnExpression(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = FALSE,
               omit.generators = FALSE, state.space = TRUE, object = Fuchs82)

# State-space, without generators:
returnExpression(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = FALSE,
               omit.generators = TRUE, state.space = TRUE, object = Fuchs82)

showFormula()

# State-space, with generators:
returnExpression(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = FALSE,
               omit.generators = FALSE, state.space = TRUE, object = Fuchs82)

# State-space, without generators:
returnExpression(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = FALSE,
               omit.generators = TRUE, state.space = TRUE, object = Fuchs82)

# Expression:
returnExpression(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = FALSE,
               omit.generators = FALSE, state.space = FALSE, object = Fuchs82)

# Expression, without generators:
returnExpression(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = FALSE,
               omit.generators = TRUE, state.space = FALSE, object = Fuchs82)

# Prime components:
returnExpression(model = FALSE, type = "expression", 
               omit.prime.components = FALSE, omit.separators = TRUE,
               omit.generators = TRUE, state.space = FALSE, object = Fuchs82)
returnExpression(model = FALSE, type = "prime.components", object = Fuchs82)

# Separators:
returnExpression(model = FALSE, type = "expression", 
               omit.prime.components = TRUE, omit.separators = FALSE,
               omit.generators = TRUE, state.space = FALSE, object = Fuchs82)
returnExpression(model = FALSE, type = "separators", object = Fuchs82)

# enterModel()
# backward(recursive = TRUE, follow = TRUE)
# makeCurrent("last")
# showModel("all")

# editModel("drop.edges", "md")
# makeCurrent("last")
# showModel("all")

quitCoCo()
