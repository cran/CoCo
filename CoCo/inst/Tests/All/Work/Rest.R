
source("init.R")

library(MASS); # ;						                           	 		   
											                           	 		   
# data(fgl);                   # f("fgl",                  "", FGL);				   #   10  1   9   214     "fgl"                   
# data(biopsy);                # f("biopsy",               "", biopsy[-1]);			   #   11  1   10  699     "biopsy"                
  data(UScereal);              f("UScereal",             "", UScereal[-1], to.factor = 8);	   #   11  2   9   65      "UScereal"              
  data(survey);                f("survey",               "", survey);				   #   12  7   5   237     "survey"                
# data(Boston);                # f("Boston",               "", Boston, to.factor = 4);		   #   14  0   14  506     "Boston"                
  data(UScrime);               f("UScrime",              "", UScrime, to.factor = 2);		   #   16  0   16  47      "UScrime"               
  data(waders);                f("waders",               "", waders);				   #   19  0   19  15      "waders"                
  data(Cars93);                f("Cars93",               "", Cars93[-27]);			   #   27  9   18  93      "Cars93"                


library(cluster); # ;						                           	 		   

  data(votes.repub);           f("votes.repub",          "", votes.repub); 	# tab		   #   31  0   31  50      "votes.repub"           


library(nlme); # ;						                           	 		   

  data(Wafer);                 f("Wafer",                "", Wafer);				   #   4   2   2   400     "Wafer"                 

# data(Phenobarb);             f("Phenobarb",            "", Phenobarb);			   #   7   3   4   744     "Phenobarb"             
# data(Remifentanil);          f("Remifentanil",         "", Remifentanil);			   #   12  2   10  2107    "Remifentanil"          
# data(Quinidine);             f("Quinidine",            "", Quinidine);			   #   14  6   8   1471    "Quinidine"             
# data(bdf);                   f("bdf",                  "", bdf);				   #   28  8   20  2287    "bdf"                   


library(rpart); # ;						                           	 		   

  data(solder);                f("solder",               "", solder); 		# tab		   #   6   4   2   720     "solder"                


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



q()
