
source("init.R")

library(boot); # ;						                           	 		   

# data(aids);                  f("aids",                 "", aids);				   #   6   0   6   570     "aids"                  

library(survival); # ;						                           	 		   

#  data(colon);                 f("colon",                "", colon[,-1], 
#					to.factor = c(3, 5:7, 9:13, 15));			   #   16  1   15  1858    "colon"                 
#  data(pbc);                   f("pbc",                  "", pbc, missing.values = -9,
#					to.factor = c(4, 7, 9, 13, 15:18));				   #   20  0   20  418     "pbc"                   

library(MASS); # ;						                           	 		   

  data(Sitka89);               f("Sitka89",              "", Sitka89);				   #   4   1   3   632     "Sitka89"               
  data(ships);                 f("ships",                "", ships);				   #   5   1   4   40      "ships"                 
  data(minn38);                f("minn38",               "", minn38);				   #   5   4   1   168     "minn38"                

# data(cabbages);              f("cabbages",             "", cabbages);				   #   4   2   2   60      "cabbages"              
# data(immer);                 f("immer",                "", immer);				   #   4   2   2   30      "immer"                 
# data(survey);                f("survey",               "", survey);				   #   12  7   5   237     "survey"                

  FGL <- fgl[,c(2:9)]
  s <- apply(FGL, 1, sum)
  s <- ifelse(s < 100, s, 100)
  FGL <- FGL + (100-s)/8
  FGL[FGL == 0] <- 0.00001
  FGL <- cbind(RI = fgl[,1], log(FGL/(100-FGL)), type = fgl[,10])

# data(fgl);                   f("fgl",                  "", FGL);				   #   10  1   9   214     "fgl"                   

# data(UScereal);              f("UScereal",             "", UScereal[-1], to.factor = 8);	   #   11  2   9   65      "UScereal"              

# data(Boston);                f("Boston",               "", Boston, to.factor = 4);		   #   14  0   14  506     "Boston"                

# data(mtcars);                f("mtcars",               "", mtcars[-1],
#                                                        to.factor = 7:8);	# R		   #   11  0   11  32      "mtcars"                

q()
