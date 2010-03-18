
R = R

all: Objects

Fuchs82.Rout: Fuchs82.r
	time ${R} --save < Fuchs82.r > Fuchs82.Rout

Byssinosis38.Rout: Byssinosis38.r
	time ${R} --save < Byssinosis38.r > Byssinosis38.Rout

UterineCervix411.Rout: UterineCervix411.R
	time ${R} --save < UterineCervix411.R > UterineCervix411.Rout

Reinis.Rout: Reinis.R
	time ${R} --save < Reinis.R > Reinis.Rout

CoCo.Rout: CoCo.R
	time ${R} --save < CoCo.R > CoCo.Rout

Objects: UterineCervix411.Rout Fuchs82.Rout Byssinosis38.Rout Reinis.Rout CoCo.Rout 

subdir = R+S/tests/Test.R+CoCo

clean:
	@echo "cleaning in $(subdir)"
	@-rm -f *~

distclean: clean
	@-rm -rf Affald ? Work
