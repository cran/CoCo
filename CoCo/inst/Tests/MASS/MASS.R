g <- function(data) {
  if (is.data.frame(data)) {
   x <- rep(FALSE, ncol(data))
   for (i in 1:ncol(data)) x[i] <- is.factor(data[,i])
   x; 
  } else
   FALSE }
h <- function(data) {x <- g(data); (c(length(x[x]), length(x[!x]))) }
f <- function(name, x) { 
  if (is.data.frame(x)) 
    dims <- c(dim(x), h(x)) 
  else
    dims <- length(x);
  print(paste(name, paste(dims, collapse = " ")))
}

library(MASS)

data(abbey);      f("abbey",     abbey);     
data(accdeaths);  f("accdeaths", accdeaths); 
data(Aids2);      f("Aids2",     Aids2);     
data(Animals);    f("Animals",   Animals);     
data(anorexia);   f("anorexia",  anorexia);     
data(austres);    f("austres",   austres);     
data(bacteria);   f("bacteria",  bacteria);     
data(beav1);      f("beav1",     beav1);     
data(beav2);      f("beav2",     beav2);     
data(biopsy);     f("biopsy",    biopsy);     
data(birthwt);    f("birthwt",   birthwt);     
data(Boston);     f("Boston",    Boston);     
data(cabbages);   f("cabbages",  cabbages);     
data(caith);      f("caith",     caith);     
data(Cars93);     f("Cars93",    Cars93);     
data(cats);       f("cats",      cats);     
data(cement);     f("cement",    cement);     
data(chem);       f("chem",      chem);     
data(coop);       f("coop",      coop);     
data(cpus);       f("cpus",      cpus);     
data(crabs);      f("crabs",     crabs);     
data(Cushings);   f("Cushings",  Cushings);     
data(DDT);        f("DDT",       DDT);     
data(deaths);     f("deaths",    deaths);     
data(drivers);    f("drivers",   drivers);     
data(eagles);     f("eagles",    eagles);     
data(epil);       f("epil",      epil);     
data(farms);      f("farms",     farms);     
data(fdeaths);    f("fdeaths",   fdeaths);     
data(fgl);        f("fgl",       fgl);     
data(forbes);     f("forbes",    forbes);     
data(GAGurine);   f("GAGurine",  GAGurine);     
data(galaxies);   f("galaxies",  galaxies);     
data(gehan);      f("gehan",     gehan);     
data(genotype);   f("genotype",  genotype);     
data(geyser);     f("geyser",    geyser);     
data(gilgais);    f("gilgais",   gilgais);     
data(hills);      f("hills",     hills);     
data(housing);    f("housing",   housing);     
data(immer);      f("immer",     immer);     
data(Insurance);  f("Insurance", Insurance); 
data(leuk);       f("leuk",      leuk);     
data(lh);         f("lh",        lh);     
data(mammals);    f("mammals",   mammals);     
data(mcycle);     f("mcycle",    mcycle);     
data(mdeaths);    f("mdeaths",   mdeaths);     
data(Melanoma);   f("Melanoma",  Melanoma);     
data(menarche);   f("menarche",  menarche);     
data(michelson);  f("michelson", michelson); 
data(minn38);     f("minn38",    minn38);     
data(motors);     f("motors",    motors);     
data(muscle);     f("muscle",    muscle);     
data(newcomb);    f("newcomb",   newcomb);     
data(nlschools);  f("nlschools", nlschools); 
data(nottem);     f("nottem",    nottem);     
data(npk);        f("npk",       npk);     
data(npr1);       f("npr1",      npr1);     
data(oats);       f("oats",      oats);     
data(OME);        f("OME",       OME);     
data(painters);   f("painters",  painters);     
data(petrol);     f("petrol",    petrol);     
data(phones);     f("phones",    phones);     
data(Pima.te);    f("Pima.te",   Pima.te);     
data(Pima.tr2);   f("Pima.tr2",  Pima.tr2);     
data(Pima.tr);    f("Pima.tr",   Pima.tr);     
data(quine);      f("quine",     quine);     
data(Rabbit);     f("Rabbit",    Rabbit);     
data(road);       f("road",      road);     
data(rock);       f("rock",      rock);     
data(rotifer);    f("rotifer",   rotifer);     
data(Rubber);     f("Rubber",    Rubber);     
data(ships);      f("ships",     ships);     
data(shoes);      f("shoes",     shoes);     
data(shrimp);     f("shrimp",    shrimp);     
data(shuttle);    f("shuttle",   shuttle);     
data(Sitka89);    f("Sitka89",   Sitka89);     
data(Sitka);      f("Sitka",     Sitka);     
data(Skye);       f("Skye",      Skye);     
data(snails);     f("snails",    snails);     
data(SP500);      f("SP500",     SP500);     
data(steam);      f("steam",     steam);     
data(stormer);    f("stormer",   stormer);     
data(survey);     f("survey",    survey);     
data(synth.te);   f("synth.te",  synth.te);     
data(synth.tr);   f("synth.tr",  synth.tr);     
data(topo);       f("topo",      topo);     
data(Traffic);    f("Traffic",   Traffic);     
data(UScereal);   f("UScereal",  UScereal);     
data(UScrime);    f("UScrime",   UScrime);     
data(VA);         f("VA",        VA);     
data(waders);     f("waders",    waders);     
data(whiteside);  f("whiteside", whiteside); 
data(wtloss);     f("wtloss",    wtloss);

q()
