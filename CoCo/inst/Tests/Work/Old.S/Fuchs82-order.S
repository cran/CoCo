library(CoCo)
data(Fuchs82)
read.model("mdp,da,am,dg,gs,sa", coco.id=Fuchs82)

##

printVertexOrder()

# Default order:
VertexOrder <- return.vertex.order(model = "current",
                                   invers.order = FALSE,
                                   coco.id = Fuchs82)
VertexOrder

# Default order, inverse:
InverseOrder <- return.vertex.order(model = "current",
                                    invers.order = TRUE,
                                    coco.id = Fuchs82)
InverseOrder

# Lex order:
return.vertex.order(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = FALSE,
                    coco.id = Fuchs82)

# Lex order, inverse:
return.vertex.order(model = "current", invers.order = TRUE,
                    default.order = FALSE,  max.card = FALSE,
                    coco.id = Fuchs82)

# Max-card order:
return.vertex.order(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = TRUE,
                    coco.id = Fuchs82)

# Max-card order, inverse:
return.vertex.order(model = "current", invers.order = TRUE,
                    default.order = FALSE,  max.card = TRUE,
                    coco.id = Fuchs82)

##

# Marked order, default:
return.vertex.order(model = "current", invers.order = FALSE,
                    marked = "g", coco.id = Fuchs82)

# Marked order, Lex:
return.vertex.order(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = FALSE,
                    marked = "g", coco.id = Fuchs82)

# (Marked order), Max-card:
return.vertex.order(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = TRUE,
                    marked = "g", coco.id = Fuchs82)

##

# Marked order, default:
return.vertex.order(model = "current", invers.order = FALSE,
                    marked = "d", coco.id = Fuchs82)

# Marked order, Lex:
return.vertex.order(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = FALSE,
                    marked = "d", coco.id = Fuchs82)

# (Marked order), Max-card:
return.vertex.order(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = TRUE,
                    marked = "d", coco.id = Fuchs82)

##

# Marked order, default:
return.vertex.order(model = "current", invers.order = FALSE,
                    marked = "p", coco.id = Fuchs82)

# Marked order, Lex:
return.vertex.order(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = FALSE,
                    marked = "p", coco.id = Fuchs82)

# (Marked order), Max-card:
return.vertex.order(model = "current", invers.order = FALSE,
                    default.order = FALSE,  max.card = TRUE,
                    marked = "p", coco.id = Fuchs82)

##

# read.model()
# backward(recursive = TRUE, follow = TRUE)
# current()
# printModel("all")

# editModel("drop.edges", "md")
# current()
# printModel("all")

.quit()
