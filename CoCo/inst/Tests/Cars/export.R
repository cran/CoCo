library(CoCoCg);
library(MASS)
data(Cars93);

Cars93CoCo <- makeCoCoCg();

levels(Cars93[,"Type"])
# "Compact", "Large", "Midsize", "Small", "Sporty", "Van"

Type <- ifelse(Cars93[,"Type"]== "Compact", 1, 
                    ifelse(Cars93[,"Type"]== "Small", 2,
                    ifelse(Cars93[,"Type"]== "Midsize", 3,
                    ifelse(Cars93[,"Type"]== "Sporty", 4,
                    ifelse(Cars93[,"Type"]== "Large", 5,
                    ifelse(Cars93[,"Type"]== "Van", 6, 0))))))
Cars93[,"Type"] <- Type

levels(Cars93[,"DriveTrain"])
# "4WD", "Front", "Rear"
# (Better: Exclude the 5 cars with 4WD)
DriveTrain <- ifelse(Cars93[,"DriveTrain"]== "Front", 0, 
                  ifelse(Cars93[,"DriveTrain"]== "Rear", 1, 2)) # "4WD"
Cars93[,"DriveTrain"] <- DriveTrain

levels(Cars93[,"Man.trans.avail"])
# "No", "Yes"
levels(Cars93[,"Origin"])
# "USA", "non-USA"

levels(Cars93[,"AirBags"])
# "Driver & Passenger", "Driver only", "None"
AirBags <- ifelse(Cars93[,"AirBags"]== "None", 0, 
                  ifelse(Cars93[,"AirBags"]== "Driver only", 1, 2)) # "Driver & Passenger"
Cars93[,"AirBags"] <- AirBags

levels(Cars93[,"Cylinders"])
# "3", "4", "5", "6", "8", "rotary"
Cylinders <- ifelse(Cars93[,"Cylinders"]== 3, 3, 
                    ifelse(Cars93[,"Cylinders"]== 4, 4,
                    ifelse(Cars93[,"Cylinders"]== 5, 5,
                    ifelse(Cars93[,"Cylinders"]== 6, 6,
                    ifelse(Cars93[,"Cylinders"]== 8, 8, 4)))))
Cars93[,"Cylinders"] <- Cylinders

result <- enterDataFrame(Cars93[-c(1,2,27)], object = Cars93CoCo);
showOptions("specification", object = Cars93CoCo);

enterModel(".;", object = Cars93CoCo);

# showTable("observed", ":DriveTrain:Man_tran:Origin", object = Cars93CoCo);
showTable("observed", ":Man_tran:Origin", object = Cars93CoCo);

optionsCoCo("bic" = FALSE); optionsCoCo("ic" = FALSE);
forward(recursive = TRUE, headlong = TRUE, coherent = TRUE, object = Cars93CoCo);

g <- dynamic.Graph("last")

exportCoCo("Cars93.xpt")

showModel("all")
makeCurrent()
showFormula()

endCoCo(object = Cars93CoCo);

q();
