
  # library(CoCo);
  # library(CoCoOldData);

  Fever <- makeCoCo(n = 2048, p = 1024, q = 128, uniq.title = TRUE, 
  title = "Spagnuolo,M., Pasternack, B. and Taranta, A  (1971) Risk of rheumatic fever recurrence after ... New Eng. J. Med. 285, 641-647.")

  set.data.file("Fever.dat")
  read.data();

  endCoCo();

  # quitCoCo()
