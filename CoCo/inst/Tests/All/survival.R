library(survival); # ;						                           	 		   

  data(ratetables);            # f("ratetables",           "", ratetables);			     

  data(ratetables);            f("survexp.fl",           "", survexp.fl      );			   #   3   0   3   0.20252 "survexp.fl"            
  data(ratetables);            f("survexp.mn",           "", survexp.mn      );			   #   3   0   3   0.20296 "survexp.mn"            
  data(ratetables);            f("survexp.az",           "", survexp.az      );			   #   3   0   3   0.20377 "survexp.az"            
  data(ratetables);            f("survexp.mnwhite",      "", survexp.mnwhite );			   #   3   0   3   0.34828 "survexp.mnwhite"       
  data(ratetables);            f("survexp.us",           "", survexp.us      );			   #   3   0   3   0.53392 "survexp.us"            
  data(ratetables);            f("survexp.wnc",          "", survexp.wnc     );			   #   3   0   3   0.63104 "survexp.wnc"           
  data(tobin);                 f("tobin",                "", tobin); 		# txt		   #   3   0   3   20      "tobin"                 
  data(aml);                   f("aml",                  "", aml);				   #   3   1   2   23      "aml"                   
  data(leukemia);              f("leukemia",             "", leukemia);				   #   3   1   2   23      "leukemia"              
  data(ratetables);            f("survexp.azr",          "", survexp.azr     );			   #   4   0   4   0.38125 "survexp.azr"           
  data(ratetables);            f("survexp.flr",          "", survexp.flr     );			   #   4   0   4   0.46008 "survexp.flr"           
  data(rats);                  f("rats",                 "", rats);				   #   4   0   4   150     "rats"                  
  data(ratetables);            f("survexp.usr",          "", survexp.usr     );			   #   4   0   4   1.66297 "survexp.usr"           
  data(stanford2);             f("stanford2",            "", stanford2);			   #   5   0   5   184     "stanford2"             
  data(ovarian);               f("ovarian",              "", ovarian);				   #   6   0   6   26      "ovarian"               
  data(bladder);               f("bladder",              "", bladder);				   #   7   0   7   340     "bladder"               
  data(kidney);                f("kidney",               "", kidney);				   #   7   1   6   76      "kidney"                
  data(veteran);               f("veteran",              "", veteran);                  	   #   8   1   7   137     "veteran"               
  data(heart);                 f("heart",                "", heart);				   #   8   1   7   172     "heart"                 
  data(cancer);                f("cancer",               "", cancer);				   #   10  0   10  228     "cancer"                
  data(lung);                  f("lung",                 "", lung);				   #   10  0   10  228     "lung"                  
# data(colon);                 f("colon",                "", colon[,-1], 
#					to.factor = c(3, 5:7, 9:13, 15));			   #   16  1   15  1858    "colon"                 
  data(pbc);                   f("pbc",                  "", pbc, missing.values = -9,
					to.factor = c(4, 7, 9, 13, 15:18));				   #   20  0   20  418     "pbc"                   
