g <- function(data) {
  if (is.data.frame(data)) {
    x <- rep(FALSE, ncol(data))
    for (i in 1:ncol(data)) 
      x[i] <- is.factor(data[,i])
    return(x)
  } else
    return(FALSE)
}

h <- function(data) {
  x <- g(data)
  return(c(length(x[x]), length(x[!x])))
}

f <- function(name, lib, x) { 
  if (is.array(x) && !is.matrix(x)) {
    S <- sum(x)
    if (is.na(S)) {
      dims <- c(rep(length(dim(x)), 2), 0, -1) 
      dims <- dims[c(1, 3, 2, 4)]
    } else {
      dims <- c(rep(length(dim(x)), 2), 0, S) 
      if ( ((abs(S - round(S)) / S) > 0.001) )
        dims <- dims[c(1, 3, 2, 4)]
    }
  }
  else if (is.data.frame(x)) 
    dims <- c(dim(x)[2], h(x), dim(x)[1]) 
  else
    dims <- c(0, 0, 1, length(x));
  print(paste(paste(dims, collapse = " "), name))
}

library(base); # ;				
data(airquality);            f("airquality",           "", airquality); 	# tab
data(anscombe);              f("anscombe",             "", anscombe); 		# R
data(attenu);                f("attenu",               "", attenu); 		# R
data(attitude);              f("attitude",             "", attitude); 		# R
data(cars);                  f("cars",                 "", cars); 		# R
data(chickwts);              f("chickwts",             "", chickwts); 		# R
data(esoph);                 f("esoph",                "", esoph); 		# R
data(eurodist);              f("eurodist",             "", eurodist); 		# R
data(euro);                  f("euro",                 "", euro); 		# R
data(faithful);              f("faithful",             "", faithful); 		# R
data(Formaldehyde);          f("Formaldehyde",         "", Formaldehyde); 	# R
data(HairEyeColor);          f("HairEyeColor",         "", HairEyeColor); 	# R
data(infert);                f("infert",               "", infert); 		# R
data(InsectSprays);          f("InsectSprays",         "", InsectSprays); 	# R
data(iris3);                 f("iris3",                "", iris3); 		# R
data(iris);                  f("iris",                 "", iris); 		# R
data(islands);               f("islands",              "", islands); 		# R
data(LifeCycleSavings);      f("LifeCycleSavings",     "", LifeCycleSavings); 	# R
data(longley);               f("longley",              "", longley); 		# R
data(morley);                f("morley",               "", morley); 		# tab
data(mtcars);                f("mtcars",               "", mtcars); 		# R
data(OrchardSprays);         f("OrchardSprays",        "", OrchardSprays); 	# R
data(phones);                f("phones",               "", phones); 		# R
data(PlantGrowth);           f("PlantGrowth",          "", PlantGrowth); 	# R
data(precip);                f("precip",               "", precip); 		# R
data(pressure);              f("pressure",             "", pressure); 		# R
data(quakes);                f("quakes",               "", quakes); 		# tab
data(randu);                 f("randu",                "", randu); 		# R
data(rivers);                f("rivers",               "", rivers); 		# R
data(sleep);                 f("sleep",                "", sleep); 		# R
data(stackloss);             f("stackloss",            "", stackloss); 		# R
data(state);                 # f("state",              "", state); 		# R
data(state);                 f("state.abb",            "", state.abb      );    # R
data(state);                 f("state.area",           "", state.area     );    # R
data(state);                 f("state.center",         "", state.center   );    # R
data(state);                 f("state.division",       "", state.division );    # R
data(state);                 f("state.name",           "", state.name     );    # R
data(state);                 f("state.region",         "", state.region   );    # R
data(state);                 f("state.x77",            "", state.x77      );    # R
data(swiss);                 f("swiss",                "", swiss); 		# tab
data(Titanic);               f("Titanic",              "", Titanic); 		# R
data(ToothGrowth);           f("ToothGrowth",          "", ToothGrowth); 	# R
data(trees);                 f("trees",                "", trees); 		# R
data(UCBAdmissions);         f("UCBAdmissions",        "", UCBAdmissions); 	# R
data(USArrests);             f("USArrests",            "", USArrests); 		# tab
data(USJudgeRatings);        f("USJudgeRatings",       "", USJudgeRatings); 	# R
data(USPersonalExpenditure); f("USPersonalExpenditure","", USPersonalExpenditure); # R
data(VADeaths);              f("VADeaths",             "", VADeaths); 		# R
data(volcano);               f("volcano",              "", volcano); 		# R
data(warpbreaks);            f("warpbreaks",           "", warpbreaks); 		# R
data(women);                 f("women",                "", women); 		# R
                             		       				
                             library(boot); # ;				
data(acme);                  f("acme",                 "", acme);			
data(aids);                  f("aids",                 "", aids);			
data(aircondit7);            f("aircondit7",           "", aircondit7);		
data(aircondit);             f("aircondit",            "", aircondit);		
data(amis);                  f("amis",                 "", amis);			
data(aml);                   f("aml",                  "", aml);			
data(beaver);                f("beaver",               "", beaver);		
data(bigcity);               f("bigcity",              "", bigcity);		
data(brambles);              f("brambles",             "", brambles);		
data(breslow);               f("breslow",              "", breslow);		
data(calcium);               f("calcium",              "", calcium);		
data(cane);                  f("cane",                 "", cane);			
data(capability);            f("capability",           "", capability);		
data(catsM);                 f("catsM",                "", catsM);		
data(cav);                   f("cav",                  "", cav);			
data(cd4.nested);            f("cd4.nested",           "", cd4.nested);		
data(cd4);                   f("cd4",                  "", cd4);			
data(channing);              f("channing",             "", channing);		
data(city);                  f("city",                 "", city);			
data(claridge);              f("claridge",             "", claridge);		
data(cloth);                 f("cloth",                "", cloth);		
data(coal);                  f("coal",                 "", coal);			
data(co.transfer);           f("co.transfer",          "", co.transfer);		
data(darwin);                f("darwin",               "", darwin);		
data(dogs);                  f("dogs",                 "", dogs);			
data(downs.bc);              f("downs.bc",             "", downs.bc);		
data(ducks);                 f("ducks",                "", ducks);		
data(fir);                   f("fir",                  "", fir);			
data(frets);                 f("frets",                "", frets);		
data(gravity);               f("gravity",              "", gravity);		
data(grav);                  f("grav",                 "", grav);			
data(hirose);                f("hirose",               "", hirose);		
data(islay);                 f("islay",                "", islay);		
data(manaus);                f("manaus",               "", manaus);		
data(melanoma);              f("melanoma",             "", melanoma);		
data(motor);                 f("motor",                "", motor);		
data(neuro);                 f("neuro",                "", neuro);		
data(nitrofen);              f("nitrofen",             "", nitrofen);		
data(nodal);                 f("nodal",                "", nodal);		
data(nuclear);               f("nuclear",              "", nuclear);		
data(paulsen);               f("paulsen",              "", paulsen);		
data(poisons);               f("poisons",              "", poisons);		
data(polar);                 f("polar",                "", polar);		
data(remission);             f("remission",            "", remission);		
data(salinity);              f("salinity",             "", salinity);		
data(survival);              f("survival",             "", survival);		
data(tau);                   f("tau",                  "", tau);			
data(tuna);                  f("tuna",                 "", tuna);			
data(urine);                 f("urine",                "", urine);		
data(wool);                  f("wool",                 "", wool);			
                             		       				
                             library(cluster); # ;				
data(agriculture);           f("agriculture",          "", agriculture); 	# tab
data(animals);               f("animals",              "", animals); 		# tab
data(flower);                f("flower",               "", flower); 		# R
data(pluton);                f("pluton",               "", pluton); 		# tab
data(ruspini);               f("ruspini",              "", ruspini); 		# tab
data(votes.repub);           f("votes.repub",          "", votes.repub); 	# tab
data(xclara);                f("xclara",               "", xclara); 		# R
                             		       				
                             library(CoCoCg); # ;				
data(fev);                   f("fev",                  "", fev);			
data(Rats);                  f("Rats",                 "", Rats);			
data(Reinis);                f("Reinis",               "", Reinis);		
                             		       				
                             library(CoCo); # ;				
data(Byssinosis38);          f("Byssinosis38",         "", Byssinosis38);		
data(Dawid79);               f("Dawid79",              "", Dawid79);		
data(Fever);                 f("Fever",                "", Fever);		
data(Fuchs82);               f("Fuchs82",              "", Fuchs82);		
data(Hochberg77);            f("Hochberg77",           "", Hochberg77);		
data(Reinis);                f("Reinis",               "", Reinis);		
data(Scrotal94);             f("Scrotal94",            "", Scrotal94);		
data(UterineCervix411);      f("UterineCervix411",     "", UterineCervix411);	
                             		       				
                             library(lattice); # ;				
data(barley);                f("barley",               "", barley); 		# R
data(environmental);         f("environmental",        "", environmental); 	# R
data(ethanol);               f("ethanol",              "", ethanol); 		# R
data(melanoma);              f("melanoma",             "", melanoma); 		# R
data(singer);                f("singer",               "", singer); 		# R
data(sunspot);               f("sunspot",              "", sunspot); 		# R
                             		       				
                             library(MASS); # ;				
data(abbey);                 f("abbey",                "", abbey);		
data(accdeaths);             f("accdeaths",            "", accdeaths);		
data(Aids2);                 f("Aids2",                "", Aids2);		
data(Animals);               f("Animals",              "", Animals);		
data(anorexia);              f("anorexia",             "", anorexia);		
data(austres);               f("austres",              "", austres);		
data(bacteria);              f("bacteria",             "", bacteria);		
data(beav1);                 f("beav1",                "", beav1);		
data(beav2);                 f("beav2",                "", beav2);		
data(biopsy);                f("biopsy",               "", biopsy);		
data(birthwt);               f("birthwt",              "", birthwt);		
data(Boston);                f("Boston",               "", Boston);		
data(cabbages);              f("cabbages",             "", cabbages);		
data(caith);                 f("caith",                "", caith);		
data(Cars93);                f("Cars93",               "", Cars93);		
data(cats);                  f("cats",                 "", cats);			
data(cement);                f("cement",               "", cement);		
data(chem);                  f("chem",                 "", chem);			
data(coop);                  f("coop",                 "", coop);			
data(cpus);                  f("cpus",                 "", cpus);			
data(crabs);                 f("crabs",                "", crabs);		
data(Cushings);              f("Cushings",             "", Cushings);		
data(DDT);                   f("DDT",                  "", DDT);			
data(deaths);                f("deaths",               "", deaths);		
data(drivers);               f("drivers",              "", drivers);		
data(eagles);                f("eagles",               "", eagles);		
data(epil);                  f("epil",                 "", epil);			
data(farms);                 f("farms",                "", farms);		
data(fdeaths);               f("fdeaths",              "", fdeaths);		
data(fgl);                   f("fgl",                  "", fgl);			
data(forbes);                f("forbes",               "", forbes);		
data(GAGurine);              f("GAGurine",             "", GAGurine);		
data(galaxies);              f("galaxies",             "", galaxies);		
data(gehan);                 f("gehan",                "", gehan);		
data(genotype);              f("genotype",             "", genotype);		
data(geyser);                f("geyser",               "", geyser);		
data(gilgais);               f("gilgais",              "", gilgais);		
data(hills);                 f("hills",                "", hills);		
data(housing);               f("housing",              "", housing);		
data(immer);                 f("immer",                "", immer);		
data(Insurance);             f("Insurance",            "", Insurance);		
data(leuk);                  f("leuk",                 "", leuk);			
data(lh);                    f("lh",                   "", lh);			
data(mammals);               f("mammals",              "", mammals);		
data(mcycle);                f("mcycle",               "", mcycle);		
data(mdeaths);               f("mdeaths",              "", mdeaths);		
data(Melanoma);              f("Melanoma",             "", Melanoma);		
data(menarche);              f("menarche",             "", menarche);		
data(michelson);             f("michelson",            "", michelson);		
data(minn38);                f("minn38",               "", minn38);		
data(motors);                f("motors",               "", motors);		
data(muscle);                f("muscle",               "", muscle);		
data(newcomb);               f("newcomb",              "", newcomb);		
data(nlschools);             f("nlschools",            "", nlschools);		
data(nottem);                f("nottem",               "", nottem);		
data(npk);                   f("npk",                  "", npk);			
data(npr1);                  f("npr1",                 "", npr1);			
data(oats);                  f("oats",                 "", oats);			
data(OME);                   f("OME",                  "", OME);			
data(painters);              f("painters",             "", painters);		
data(petrol);                f("petrol",               "", petrol);		
data(phones);                f("phones",               "", phones);		
data(Pima.te);               f("Pima.te",              "", Pima.te);		
data(Pima.tr2);              f("Pima.tr2",             "", Pima.tr2);		
data(Pima.tr);               f("Pima.tr",              "", Pima.tr);		
data(quine);                 f("quine",                "", quine);		
data(Rabbit);                f("Rabbit",               "", Rabbit);		
data(road);                  f("road",                 "", road);			
data(rock);                  f("rock",                 "", rock);			
data(rotifer);               f("rotifer",              "", rotifer);		
data(Rubber);                f("Rubber",               "", Rubber);		
data(ships);                 f("ships",                "", ships);		
data(shoes);                 f("shoes",                "", shoes);		
data(shrimp);                f("shrimp",               "", shrimp);		
data(shuttle);               f("shuttle",              "", shuttle);		
data(Sitka89);               f("Sitka89",              "", Sitka89);		
data(Sitka);                 f("Sitka",                "", Sitka);		
data(Skye);                  f("Skye",                 "", Skye);			
data(snails);                f("snails",               "", snails);		
data(SP500);                 f("SP500",                "", SP500);		
data(steam);                 f("steam",                "", steam);		
data(stormer);               f("stormer",              "", stormer);		
data(survey);                f("survey",               "", survey);		
data(synth.te);              f("synth.te",             "", synth.te);		
data(synth.tr);              f("synth.tr",             "", synth.tr);		
data(topo);                  f("topo",                 "", topo);			
data(Traffic);               f("Traffic",              "", Traffic);		
data(UScereal);              f("UScereal",             "", UScereal);		
data(UScrime);               f("UScrime",              "", UScrime);		
data(VA);                    f("VA",                   "", VA);			
data(waders);                f("waders",               "", waders);		
data(whiteside);             f("whiteside",            "", whiteside);		
data(wtloss);                f("wtloss",               "", wtloss);		
                             		       				
                             library(nlme); # ;				
data(Alfalfa);               f("Alfalfa",              "", Alfalfa);		
data(Assay);                 f("Assay",                "", Assay);		
data(bdf);                   f("bdf",                  "", bdf);			
data(BodyWeight);            f("BodyWeight",           "", BodyWeight);		
data(Cefamandole);           f("Cefamandole",          "", Cefamandole);		
data(Dialyzer);              f("Dialyzer",             "", Dialyzer);		
data(Earthquake);            f("Earthquake",           "", Earthquake);		
data(ergoStool);             f("ergoStool",            "", ergoStool);		
data(Fatigue);               f("Fatigue",              "", Fatigue);		
data(Gasoline);              f("Gasoline",             "", Gasoline);		
data(Glucose2);              f("Glucose2",             "", Glucose2);		
data(Glucose);               f("Glucose",              "", Glucose);		
data(Gun);                   f("Gun",                  "", Gun);			
data(IGF);                   f("IGF",                  "", IGF);			
data(Machines);              f("Machines",             "", Machines);		
data(MathAchieve);           f("MathAchieve",          "", MathAchieve);		
data(MathAchSchool);         f("MathAchSchool",        "", MathAchSchool);	
data(Meat);                  f("Meat",                 "", Meat);			
data(Milk);                  f("Milk",                 "", Milk);			
data(Muscle);                f("Muscle",               "", Muscle);		
data(Nitrendipene);          f("Nitrendipene",         "", Nitrendipene);		
data(Oats);                  f("Oats",                 "", Oats);			
data(Orthodont);             f("Orthodont",            "", Orthodont);		
data(Ovary);                 f("Ovary",                "", Ovary);		
data(Oxboys);                f("Oxboys",               "", Oxboys);		
data(Oxide);                 f("Oxide",                "", Oxide);		
data(PBG);                   f("PBG",                  "", PBG);			
data(Phenobarb);             f("Phenobarb",            "", Phenobarb);		
data(Pixel);                 f("Pixel",                "", Pixel);		
data(Quinidine);             f("Quinidine",            "", Quinidine);		
data(Rail);                  f("Rail",                 "", Rail);			
data(RatPupWeight);          f("RatPupWeight",         "", RatPupWeight);		
data(Relaxin);               f("Relaxin",              "", Relaxin);		
data(Remifentanil);          f("Remifentanil",         "", Remifentanil);		
data(Soybean);               f("Soybean",              "", Soybean);		
data(Spruce);                f("Spruce",               "", Spruce);		
data(Tetracycline1);         f("Tetracycline1",        "", Tetracycline1);	
data(Tetracycline2);         f("Tetracycline2",        "", Tetracycline2);	
data(Wafer);                 f("Wafer",                "", Wafer);		
data(Wheat2);                f("Wheat2",               "", Wheat2);		
data(Wheat);                 f("Wheat",                "", Wheat);		
                             		       				
                             library(rpart); # ;				
data(car.test.frame);        f("car.test.frame",       "", car.test.frame); 	# csv
data(cu.summary);            f("cu.summary",           "", cu.summary);		
data(kyphosis);              f("kyphosis",             "", kyphosis); 		# tab
data(solder);                f("solder",               "", solder); 		# tab
                             		       				
                             library(stats); # ;				
data(ability.cov);           f("ability.cov",          "", ability.cov); 	# R
data(airmiles);              f("airmiles",             "", airmiles); 		# R
data(AirPassengers);         f("AirPassengers",        "", AirPassengers); 	# R
data(austres);               f("austres",              "", austres); 		# R
data(beavers);               # f("beavers",             "", beavers); 		# R
data(beavers);               f("beaver1",              "", beaver1); 		# R
data(beavers);               f("beaver2",              "", beaver2); 		# R
data(BJsales);               f("BJsales",              "", BJsales); 		# R
data(BOD);                   f("BOD",                  "", BOD); 		# R
data(ChickWeight);           f("ChickWeight",          "", ChickWeight); 	# R
data(co2);                   f("co2",                  "", co2); 		# R
data(discoveries);           f("discoveries",          "", discoveries); 	# R
data(DNase);                 f("DNase",                "", DNase); 		# R
data(EuStockMarkets);        f("EuStockMarkets",       "", EuStockMarkets); 	# R
data(freeny);                f("freeny",               "", freeny); 		# R
data(Harman23.cor);          f("Harman23.cor",         "", Harman23.cor); 	# R
data(Harman74.cor);          f("Harman74.cor",         "", Harman74.cor); 	# R
data(Indometh);              f("Indometh",             "", Indometh); 		# R
data(JohnsonJohnson);        f("JohnsonJohnson",       "", JohnsonJohnson); 	# R
data(LakeHuron);             f("LakeHuron",            "", LakeHuron); 		# R
data(lh);                    f("lh",                   "", lh); 			# R
data(Loblolly);              f("Loblolly",             "", Loblolly); 		# R
data(lynx);                  f("lynx",                 "", lynx); 		# R
data(nhtemp);                f("nhtemp",               "", nhtemp); 		# R
data(Nile);                  f("Nile",                 "", Nile); 		# R
data(nottem);                f("nottem",               "", nottem); 		# R
data(Orange);                f("Orange",               "", Orange); 		# R
data(presidents);            f("presidents",           "", presidents); 		# R
data(Puromycin);             f("Puromycin",            "", Puromycin); 		# R
data(rock);                  f("rock",                 "", rock); 		# tab
data(Seatbelts);             f("Seatbelts",            "", Seatbelts); 		# R
data(sunspot);               f("sunspot",              "", sunspot); 		# R
data(sunspots);              f("sunspots",             "", sunspots); 		# R
data(Theoph);                f("Theoph",               "", Theoph); 		# R
data(treering);              f("treering",             "", treering); 		# R
data(UKDriverDeaths);        f("UKDriverDeaths",       "", UKDriverDeaths); 	# R
data(UKgas);                 f("UKgas",                "", UKgas); 		# R
data(UKLungDeaths);          #f("UKLungDeaths",        "", UKLungDeaths); 	# R
data(UKLungDeaths);          f("fdeaths",              "", fdeaths);		# R
data(UKLungDeaths);          f("ldeaths",              "", ldeaths);		# R
data(UKLungDeaths);          f("mdeaths",              "", mdeaths);		# R
data(USAccDeaths);           f("USAccDeaths",          "", USAccDeaths); 	# R
data(uspop);                 f("uspop",                "", uspop); 		# R
data(WWWusage);              f("WWWusage",             "", WWWusage); 		# R
data(zCO2);                  f("CO2",                  "", CO2); 		# R
                             		       				
                             library(survival); # ;				
data(aml);                   f("aml",                  "", aml);			
data(bladder);               f("bladder",              "", bladder);		
data(cancer);                f("cancer",               "", cancer);		
data(colon);                 f("colon",                "", colon);		
data(heart);                 f("heart",                "", heart);		
data(kidney);                f("kidney",               "", kidney);		
data(leukemia);              f("leukemia",             "", leukemia);		
data(lung);                  f("lung",                 "", lung);			
data(ovarian);               f("ovarian",              "", ovarian);		
data(pbc);                   f("pbc",                  "", pbc);			
data(ratetables);            # f("ratetables",           "", ratetables);		
data(ratetables);            f("survexp.az",           "", survexp.az      );		
data(ratetables);            f("survexp.azr",          "", survexp.azr     );		
data(ratetables);            f("survexp.fl",           "", survexp.fl      );		
data(ratetables);            f("survexp.flr",          "", survexp.flr     );		
data(ratetables);            f("survexp.mn",           "", survexp.mn      );		
data(ratetables);            f("survexp.mnwhite",      "", survexp.mnwhite );		
data(ratetables);            f("survexp.us",           "", survexp.us      );		
data(ratetables);            f("survexp.usr",          "", survexp.usr     );		
data(ratetables);            f("survexp.wnc",          "", survexp.wnc     );		
data(rats);                  f("rats",                 "", rats);			
data(stanford2);             f("stanford2",            "", stanford2);		
data(tobin);                 f("tobin",                "", tobin); 		# txt
data(veteran);               f("veteran",              "", veteran);                  

q()
