
# Attach data, here table of counts:
data(Reinis);

# Create a CoCo-object:
CoCoObject <- makeCoCo();

# Enter the table of counts into the CoCo-object:
enterTable(Reinis, object = CoCoObject);

# Enter the saturated model into the CoCo-object, and return a model object:
fullModel <- makeModel(enterModel("*", object = CoCoObject));

# Display a graph of the model:
fullGraph <- dynamic.Graph(fullModel, title = "Full", vertexColor = "blue",
                           width = 300, height = 300, returnLink = TRUE);

# Do a backward elimination of edges:
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE,
         follow = TRUE, object = CoCoObject);

# Display the result of the backward elimination:
lastModel <- makeModel("last", object = CoCoObject);
backwardGraph <- dynamic.Graph(lastModel, title = "Last",
                               dynamicGraph = fullGraph, slave = TRUE);

# Do the EH-procedure:
eh(object = CoCoObject);

# Terminate the CoCo-object with disposing temporary files:
# endCoCo(object = CoCoObject);
