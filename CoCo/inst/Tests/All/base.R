 library(base); # ;										
 
  data(state);                 # f("state",              "", state); 		# R		                             	 		    
  data(euro);                  f("euro",                 "", euro); 		# R		   #   0   0   1   11      "euro"                  
  data(rivers);                f("rivers",               "", rivers); 		# R		   #   0   0   1   141     "rivers"                
  data(VADeaths);              f("VADeaths",             "", VADeaths); 	# R		   #   0   0   1   20      "VADeaths"              
  data(eurodist);              f("eurodist",             "", eurodist); 	# R		   #   0   0   1   210     "eurodist"              
  data(USPersonalExpenditure); f("USPersonalExpenditure","", USPersonalExpenditure); # R	   #   0   0   1   25      "USPersonalExpenditure" 
  data(state);                 f("state.center",         "", state.center   );  # R		   #   0   0   1   2       "state.center"          
  data(state);                 f("state.x77",            "", state.x77      );  # R		   #   0   0   1   400     "state.x77"             
  data(islands);               f("islands",              "", islands); 		# R		   #   0   0   1   48      "islands"               
  data(phones); names(dimnames(phones)) <- c("Year", "Region")
                               f("phones",               "", phones, IsArray = TRUE); # R	   #   0   0   1   49      "phones"                
  data(state);                 f("state.abb",            "", state.abb      );  # R		   #   0   0   1   50      "state.abb"             
  data(state);                 f("state.area",           "", state.area     );  # R		   #   0   0   1   50      "state.area"            
  data(state);                 f("state.division",       "", state.division );  # R		   #   0   0   1   50      "state.division"        
  data(state);                 f("state.name",           "", state.name     );  # R		   #   0   0   1   50      "state.name"            
  data(state);                 f("state.region",         "", state.region   );  # R		   #   0   0   1   50      "state.region"          
  data(volcano);               # f("volcano",            "", volcano); 		# R		   #   0   0   1   5307    "volcano"               
  data(precip);                f("precip",               "", precip); 		# R		   #   0   0   1   70      "precip"                
  data(women);                 f("women",                "", women); 		# R		   #   2   0   2   15      "women"                 
  data(pressure);              f("pressure",             "", pressure); 	# R		   #   2   0   2   19      "pressure"              
  data(faithful);              f("faithful",             "", faithful); 	# R		   #   2   0   2   272     "faithful"              
  data(cars);                  f("cars",                 "", cars); 		# R		   #   2   0   2   50      "cars"                  
  data(Formaldehyde);          f("Formaldehyde",         "", Formaldehyde); 	# R		   #   2   0   2   6       "Formaldehyde"          
  data(sleep);                 f("sleep",                "", sleep); 		# R		   #   2   1   1   20      "sleep"                 
  data(PlantGrowth);           f("PlantGrowth",          "", PlantGrowth); 	# R		   #   2   1   1   30      "PlantGrowth"           
  data(chickwts);              f("chickwts",             "", chickwts); 	# R		   #   2   1   1   71      "chickwts"              
  data(InsectSprays);          f("InsectSprays",         "", InsectSprays); 	# R		   #   2   1   1   72      "InsectSprays"          
  data(morley);                f("morley",               "", morley); 		# tab		   #   3   0   3   100     "morley"                
  data(trees);                 f("trees",                "", trees); 		# R		   #   3   0   3   31      "trees"                 
  data(randu);                 f("randu",                "", randu); 		# R		   #   3   0   3   400     "randu"                 
  data(ToothGrowth);           f("ToothGrowth",          "", ToothGrowth); 	# R		   #   3   1   2   60      "ToothGrowth"           
  data(warpbreaks);            f("warpbreaks",           "", warpbreaks); 	# R		   #   3   2   1   54      "warpbreaks"            
  data(iris3);                 f("iris3",                "", iris3); 		# R		   #   3   3   0   2078.7  "iris3"                 
  data(UCBAdmissions);         f("UCBAdmissions",        "", UCBAdmissions); 	# R		   #   3   3   0   4526    "UCBAdmissions"         
  data(HairEyeColor);          f("HairEyeColor",         "", HairEyeColor); 	# R		   #   3   3   0   592     "HairEyeColor"          
  data(stackloss);             f("stackloss",            "", stackloss); 	# R		   #   4   0   4   21      "stackloss"             
  data(USArrests);             f("USArrests",            "", USArrests); 	# tab		   #   4   0   4   50      "USArrests"             
  data(OrchardSprays);         f("OrchardSprays",        "", OrchardSprays); 	# R		   #   4   1   3   64      "OrchardSprays"         
  data(Titanic);               f("Titanic",              "", Titanic); 		# R		   #   4   4   0   2201    "Titanic"               
  data(quakes);                f("quakes",               "", quakes); 		# tab		   #   5   0   5   1000    "quakes"                
  data(LifeCycleSavings);      f("LifeCycleSavings",     "", LifeCycleSavings); # R		   #   5   0   5   50      "LifeCycleSavings"      
  data(iris);                  f("iris",                 "", iris); 		# R		   #   5   1   4   150     "iris"                  
  data(attenu);                f("attenu",               "", attenu[,-3]);	# R		   #   5   1   4   182     "attenu"                
  data(esoph);                 # f("esoph",                "", esoph); 		# R		   #   5   3   2   88      "esoph"                 

											                           	 		   
  # Accumulated case list:
  esoph.ca <- cbind(esoph[,c(4,1:3)], type = "Case")
  esoph.co <- cbind(esoph[,c(5,1:3)], type = "Control")
  dimnames(esoph.ca)[[2]][1] <- "n"
  dimnames(esoph.co)[[2]][1] <- "n"
  Esoph <- rbind(esoph.ca, esoph.co)

  for (i in 1:dim(Esoph)[2]) Esoph[,i] <- as.numeric(Esoph[,i])

  ESOPH <- matrix(unlist(c(Esoph)), ncol=5)
  ESOPH <- ESOPH[ESOPH[,1]!=0, ]
  dimnames(ESOPH)[[2]] <- dimnames(Esoph)[[2]]

  data(esoph);                 f("ESOPH",                "", ESOPH, Accumulated = TRUE); # R	   #   5   3   2   88      "esoph"                 

  data(airquality);            f("airquality",           "", airquality); 	# tab		   #   6   0   6   153     "airquality"            
  data(swiss);                 f("swiss",                "", swiss); 		# tab		   #   6   0   6   47      "swiss"                 
  data(longley);               f("longley",              "", longley); 		# R		   #   7   0   7   16      "longley"               
  data(attitude);              f("attitude",             "", attitude); 	# R		   #   7   0   7   30      "attitude"              
  data(anscombe);              f("anscombe",             "", anscombe[-(1:2)]);	# R		   #   8   0   8   11      "anscombe"              
  data(infert);                f("infert",               "", infert); 		# R		   #   8   1   7   248     "infert"                
# data(mtcars);                f("mtcars",               "", mtcars,
#                                            to.factor = 8:9, delta = 2);	# R		   #   11  0   11  32      "mtcars"                
  data(USJudgeRatings);        f("USJudgeRatings",       "", USJudgeRatings); 	# R		   #   12  0   12  43      "USJudgeRatings"        
