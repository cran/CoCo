
# R = /user/jhb/bin/R

R = R

all:
	make init.Sout
#	make FewerWarnings.Sout
	make tests

init.Sout: init.S
	time ${R} --save < init.S   > init.Sout

Fuchs: Fuchs82-complex.Sout Fuchs82-components.Sout Fuchs82-is.Sout Fuchs82-order.Sout Fuchs82-paths.Sout Fuchs82-neighbours.Sout Fuchs82-shortest.Sout Fuchs82-is-separator.Sout Fuchs82-marginalize.Sout UterineCervix411.Sout

tests: Fuchs82.Sout Byssinosis38.Sout TestFiles.Sout TestSwitch.Sout TestSet.Sout TestDataFiles.Sout TestSubset.Sout TestDefault.Sout TestReturn.Sout TestSelection.Sout TestRedefine.Sout TestQtable.Sout TestTableValues.Sout TestModel.Sout TestModelList.Sout TestPrintModel.Sout TestIs.Sout TestTest.Sout TestEdit.Sout TestDrop.Sout TestStepwise.Sout TestEHInit.Sout TestEH.Sout TestToEH.Sout TestExtractEH.Sout TestForceEH.Sout TestAddDual.Sout TestCoCoModelObject.Sout TestKrippendorfObjects.Sout TestKrippendorfRecover.Sout

TestData.Sout: TestData.S
	( echo "TestData.S              " ; \
	time ${R} --no-save < TestData.S             > TestData.Sout )

FewerWarnings.Sout: FewerWarnings.S
	make init.Sout
	time ${R} --save < FewerWarnings.S > FewerWarnings.Sout

UterineCervix411.Sout: UterineCervix411.S
	( echo "UterineCervix411.S      " ; \
	time ${R} --save < UterineCervix411.S     > UterineCervix411.Sout )

Fuchs82.Sout: Fuchs82.S
	( echo "Fuchs82.S               " ; \
	time ${R} --save < Fuchs82.S              > Fuchs82.Sout )

Fuchs82-is.Sout: Fuchs82-is.S
	( echo "Fuchs82-is.S            " ; \
	time ${R} --save < Fuchs82-is.S           > Fuchs82-is.Sout )

Fuchs82-order.Sout: Fuchs82-order.S
	( echo "Fuchs82-order.S         " ; \
	time ${R} --save < Fuchs82-order.S        > Fuchs82-order.Sout )

Fuchs82-paths.Sout: Fuchs82-paths.S
	( echo "Fuchs82-paths.S         " ; \
	time ${R} --save < Fuchs82-paths.S        > Fuchs82-paths.Sout )

Fuchs82-complex.Sout: Fuchs82-complex.S
	( echo "Fuchs82-complex.S       " ; \
	time ${R} --save < Fuchs82-complex.S      > Fuchs82-complex.Sout )

Fuchs82-components.Sout: Fuchs82-components.S
	( echo "Fuchs82-components.S    " ; \
	time ${R} --save < Fuchs82-components.S   > Fuchs82-components.Sout )

Fuchs82-neighbours.Sout: Fuchs82-neighbours.S
	( echo "Fuchs82-neighbours.S    " ; \
	time ${R} --save < Fuchs82-neighbours.S   > Fuchs82-neighbours.Sout )

Fuchs82-shortest.Sout: Fuchs82-shortest.S
	( echo "Fuchs82-shortest.S      " ; \
	time ${R} --save < Fuchs82-shortest.S     > Fuchs82-shortest.Sout )

Fuchs82-is-separator.Sout: Fuchs82-is-separator.S
	( echo "Fuchs82-is-separator.S  " ; \
	time ${R} --save < Fuchs82-is-separator.S > Fuchs82-is-separator.Sout )

Fuchs82-marginalize.Sout: Fuchs82-marginalize.S
	( echo "Fuchs82-marginalize.S  " ; \
	time ${R} --save < Fuchs82-marginalize.S > Fuchs82-marginalize.Sout )

Byssinosis38.Sout: Byssinosis38.S
	( echo "Byssinosis38.S          " ; \
	time ${R} --save < Byssinosis38.S         > Byssinosis38.Sout )

TestFiles.Sout: TestFiles.S
	( echo "TestFiles.S             " ; \
	time ${R} --no-save < TestFiles.S            > TestFiles.Sout )

TestSwitch.Sout: TestSwitch.S
	( echo "TestSwitch.S            " ; \
	time ${R} --no-save < TestSwitch.S           > TestSwitch.Sout )

TestSet.Sout: TestSet.S
	( echo "TestSet.S               " ; \
	time ${R} --no-save < TestSet.S              > TestSet.Sout )

TestDataFiles.Sout: TestDataFiles.S
	( echo "TestDataFiles.S         " ; \
	time ${R} --no-save < TestDataFiles.S        > TestDataFiles.Sout )

TestSubset.Sout: TestSubset.S
	( echo "TestSubset.S            " ; \
	time ${R} --no-save < TestSubset.S           > TestSubset.Sout )
# cp TestSubset.S TestSubset.S

TestDefault.Sout: TestDefault.S
	( echo "TestDefault.S           " ; \
	time ${R} --no-save < TestDefault.S          > TestDefault.Sout )

TestReturn.Sout: TestReturn.S
	( echo "TestReturn.S            " ; \
	time ${R} --no-save < TestReturn.S           > TestReturn.Sout )

TestSelection.Sout: TestSelection.S
	( echo "TestSelection.S         " ; \
	time ${R} --no-save < TestSelection.S        > TestSelection.Sout )

TestRedefine.Sout: TestRedefine.S
	( echo "TestRedefine.S          " ; \
	time ${R} --no-save < TestRedefine.S         > TestRedefine.Sout )

TestQtable.Sout: TestQtable.S
	( echo "TestQtable.S            " ; \
	time ${R} --no-save < TestQtable.S           > TestQtable.Sout )

TestTableValues.Sout: TestTableValues.S
	( echo "TestTableValues.S       " ; \
	time ${R} --no-save < TestTableValues.S      > TestTableValues.Sout )

TestModel.Sout: TestModel.S
	( echo "TestModel.S             " ; \
	time ${R} --no-save < TestModel.S            > TestModel.Sout )

TestModelList.Sout: TestModelList.S
	( echo "TestModelList.S         " ; \
	time ${R} --no-save < TestModelList.S        > TestModelList.Sout )

TestPrintModel.Sout: TestPrintModel.S
	( echo "TestPrintModel.S        " ; \
	time ${R} --no-save < TestPrintModel.S       > TestPrintModel.Sout )

TestIs.Sout: TestIs.S
	( echo "TestIs.S                " ; \
	time ${R} --no-save < TestIs.S               > TestIs.Sout )

TestTest.Sout: TestTest.S
	( echo "TestTest.S              " ; \
	time ${R} --no-save < TestTest.S             > TestTest.Sout )

TestEdit.Sout: TestEdit.S
	( echo "TestEdit.S              " ; \
	time ${R} --no-save < TestEdit.S             > TestEdit.Sout )

TestDrop.Sout: TestDrop.S
	( echo "TestDrop.S              " ; \
	time ${R} --no-save < TestDrop.S             > TestDrop.Sout )

TestStepwise.Sout: TestStepwise.S
	( echo "TestStepwise.S          " ; \
	time ${R} --no-save < TestStepwise.S         > TestStepwise.Sout )

TestEHInit.Sout: TestEHInit.S
	( echo "TestEHInit.S            " ; \
	time ${R} --no-save < TestEHInit.S           > TestEHInit.Sout )

TestEH.Sout: TestEH.S
	( echo "TestEH.S                " ; \
	time ${R} --no-save < TestEH.S               > TestEH.Sout )

TestToEH.Sout: TestToEH.S
	( echo "TestToEH.S              " ; \
	time ${R} --no-save < TestToEH.S             > TestToEH.Sout )

TestExtractEH.Sout: TestExtractEH.S
	( echo "TestExtractEH.S         " ; \
	time ${R} --no-save < TestExtractEH.S        > TestExtractEH.Sout )

TestForceEH.Sout: TestForceEH.S
	( echo "TestForceEH.S           " ; \
	time ${R} --no-save < TestForceEH.S          > TestForceEH.Sout )

TestAddDual.Sout: TestAddDual.S
	( echo "TestAddDual.S           " ; \
	time ${R} --no-save < TestAddDual.S          > TestAddDual.Sout )

TestCoCoModelObject.Sout: TestCoCoModelObject.S
	( echo "TestCoCoModelObject.S   " ; \
	time ${R} --no-save < TestCoCoModelObject.S   > TestCoCoModelObject.Sout ) 

TestKrippendorfObjects.Sout: TestKrippendorfObjects.S
	( echo "TestKrippendorfObjects.S           " ; \
	time ${R} --no-save < TestKrippendorfObjects.S > TestKrippendorfObjects.Sout )

TestKrippendorfRecover.Sout: TestKrippendorfRecover.S
	( echo "TestKrippendorfRecover.S           " ; \
	time ${R} --no-save < TestKrippendorfRecover.S > TestKrippendorfRecover.Sout )

subdir = R+S/tests/Test.S+CoCo

clean:
	@echo "cleaning in $(subdir)"
	@-rm -f *~

distclean: clean
	@-rm -rf Affald ? Work
