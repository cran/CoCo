library(CoCo)
data(Fuchs82)
enterModel("mdp,da,am,dg,gs,sa", object=Fuchs82)

##

showVertexOrder()

# Default order:
VertexOrder <- returnVertexOrder(model = "current",
                                   invers.order = FALSE,
                                   object = Fuchs82)
VertexOrder

# Default order, inverse:
InverseOrder <- returnVertexOrder(model = "current",
                                    invers.order = TRUE,
                                    object = Fuchs82)
InverseOrder

# Lex order:
returnVertexOrder(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = FALSE,
                    object = Fuchs82)

# Lex order, inverse:
returnVertexOrder(model = "current", invers.order = TRUE,
                    default.order = FALSE,  max.card = FALSE,
                    object = Fuchs82)

# Max-card order:
returnVertexOrder(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = TRUE,
                    object = Fuchs82)

# Max-card order, inverse:
returnVertexOrder(model = "current", invers.order = TRUE,
                    default.order = FALSE,  max.card = TRUE,
                    object = Fuchs82)

##

# Marked order, default:
returnVertexOrder(model = "current", invers.order = FALSE,
                    marked = "g", object = Fuchs82)

# Marked order, Lex:
returnVertexOrder(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = FALSE,
                    marked = "g", object = Fuchs82)

# (Marked order), Max-card:
returnVertexOrder(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = TRUE,
                    marked = "g", object = Fuchs82)

##

# Marked order, default:
returnVertexOrder(model = "current", invers.order = FALSE,
                    marked = "d", object = Fuchs82)

# Marked order, Lex:
returnVertexOrder(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = FALSE,
                    marked = "d", object = Fuchs82)

# (Marked order), Max-card:
returnVertexOrder(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = TRUE,
                    marked = "d", object = Fuchs82)

##

# Marked order, default:
returnVertexOrder(model = "current", invers.order = FALSE,
                    marked = "p", object = Fuchs82)

# Marked order, Lex:
returnVertexOrder(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = FALSE,
                    marked = "p", object = Fuchs82)

# (Marked order), Max-card:
returnVertexOrder(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = TRUE,
                    marked = "p", object = Fuchs82)

##

# enterModel()
# backward(recursive = TRUE, follow = TRUE)
# makeCurrent("last")
# showModel("all")

# editModel("drop.edges", "md")
# makeCurrent("last")
# showModel("all")

quitCoCo()
