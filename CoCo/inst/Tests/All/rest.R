

library(boot); # ;						                           	 		   

  data(cane);                  f("cane",                 "", cane);				   #   5   2   3   180     "cane"                  


library(base); # ;						                           	 		   

  data(mtcars);                f("mtcars",               "", mtcars,
                                            to.factor = 8:9, delta = 2);	# R		   #   11  0   11  32      "mtcars"                


library(MASS); # ;						                           	 		   
											                           	 		   
  data(coop);                  f("coop",                 "", coop);				   #   4   3   1   252     "coop"                  
  data(crabs);                 f("crabs",                "", crabs[,-3]);			   #   8   2   6   200     "crabs"                 
  data(fgl);
  FGL <- fgl[,c(2:9)]
  s <- apply(FGL, 1, sum)
  s <- ifelse(s < 100, s, 100)
  FGL <- FGL + (100-s)/8
  FGL[FGL == 0] <- 0.00001
  FGL <- cbind(RI = fgl[,1], log(FGL/(100-FGL)), type = fgl[,10])
  data(fgl);                   f("fgl",                  "", FGL);				   #   10  1   9   214     "fgl"                   
  data(Boston);                f("Boston",               "", Boston, to.factor = 4);		   #   14  0   14  506     "Boston"                
  data(biopsy);                f("biopsy",               "", biopsy[-1]);			   #   11  1   10  699     "biopsy"                
  data(UScereal);              f("UScereal",             "", UScereal[-1], to.factor = 8);	   #   11  2   9   65      "UScereal"              

                               		       							                           	 		   
library(cluster); # ;						                           	 		   

  data(votes.repub);           f("votes.repub",          "", votes.repub); 	# tab		   #   31  0   31  50      "votes.repub"           


library(nlme); # ;						                           	 		   

  data(Milk);                  f("Milk",                 "", Milk);				   #   4   2   2   1337    "Milk"                  
  data(Oxboys);                f("Oxboys",               "", Oxboys);				   #   4   2   2   234     "Oxboys"                
  data(Wafer);                 f("Wafer",                "", Wafer);				   #   4   2   2   400     "Wafer"                 
  data(Wheat2);                f("Wheat2",               "", Wheat2);				   #   5   2   3   224     "Wheat2"                
  data(RatPupWeight);          f("RatPupWeight",         "", RatPupWeight);			   #   5   3   2   322     "RatPupWeight"          
  data(MathAchSchool);         f("MathAchSchool",        "", MathAchSchool);			   #   7   3   4   160     "MathAchSchool"         

# data(Phenobarb);             f("Phenobarb",            "", Phenobarb);			   #   7   3   4   744     "Phenobarb"             
# data(Remifentanil);          f("Remifentanil",         "", Remifentanil);			   #   12  2   10  2107    "Remifentanil"          
# data(Quinidine);             f("Quinidine",            "", Quinidine);			   #   14  6   8   1471    "Quinidine"             
# data(bdf);                   f("bdf",                  "", bdf);				   #   28  8   20  2287    "bdf"                   


library(rpart); # ;						                           	 		   

  data(solder);                f("solder",               "", solder); 		# tab		   #   6   4   2   720     "solder"                


library(stats); # ;

  data(ChickWeight);           f("ChickWeight",          "", ChickWeight); 	# R		   #   4   2   2   578     "ChickWeight"           


library(survival); # ;						                           	 		   

  data(colon);                 f("colon",                "", colon[,-1], 
					to.factor = c(3, 5:7, 9:13, 15));			   #   16  1   15  1858    "colon"                 
