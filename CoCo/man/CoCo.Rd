\name{CoCo}
\title{The CoCo package}
\alias{CoCo}

\alias{dynamic.Graph} % From CoCoGraph

%    \alias{CoCoIdClass-class}
%    \alias{CoCoClass-class}
%    \alias{CoCoModelClass-class}
%    \alias{CoCoTestClass-class}
%    
%    \alias{label} % CoCoTestClass-method
%    \alias{testLabel} % CoCoTestClass-method
%    \alias{width} % CoCoTestClass-method
%    \alias{testWidth} % CoCoTestClass-method
%    
%    \alias{dynamic.Graph} % CoCoModelClass-method
%    \alias{dynamic.Graph.Old} % CoCoModelClass-method
%    \alias{modifyModel} % CoCoModelClass-method
%    \alias{subModifyModel} % CoCoModelClass-method
%    \alias{subModifyModel.Old} % CoCoModelClass-method
%    \alias{testEdge} % CoCoModelClass-method
%    \alias{testEdge.Old} % CoCoModelClass-method
%    
%    \alias{makeModel} % CoCoObjects
%    \alias{newCoCoTestObject} % CoCoObjects
%    \alias{recover.coco.model} % CoCoObjects
%    
     \alias{coco.init} % From CoCoCore
%    \alias{coco.resume} % From CoCoCore
%    \alias{coco.start} % From CoCoCore
%    \alias{currentCoCo} % From CoCoObjects
%    \alias{endCoCo} % From CoCoRaw
%    \alias{is.number} % From CoCoCore
%    \alias{make.cococg} % From CoCoCore
%    \alias{make.coco} % From CoCoCore
%    \alias{makeCurrentCoCo} % From CoCoObjects
%    \alias{make.model} % From CoCoCore
%    \alias{my.assign} % From CoCoCore
%    \alias{quitCoCo} % From CoCoRaw
%    
%    \alias{backward} % From CoCoRaw
%    \alias{cleanData} % From CoCoRaw
%    \alias{disposeOfEh} % From CoCoRaw
%    \alias{disposeOfModel} % From CoCoRaw
%    \alias{disposeOfQtable} % From CoCoRaw
%    \alias{editModel} % From CoCoRaw
%    \alias{eh} % From CoCoRaw
%    \alias{ehExtract} % From CoCoRaw
%    \alias{ehFindDual} % From CoCoRaw
%    \alias{ehFit} % From CoCoRaw
%    \alias{ehForceAccept} % From CoCoRaw
%    \alias{ehForceAddDualToClass} % From CoCoRaw
%    \alias{ehForceFix} % From CoCoRaw
%    \alias{ehForceReject} % From CoCoRaw
%    \alias{ehSetBase} % From CoCoRaw
%    \alias{ehSetMainEffects} % From CoCoRaw
%    \alias{emOn} % From CoCoRaw
%    \alias{enterDataFrame} % From CoCoRaw
%    \alias{enterList} % From CoCoRaw
%    \alias{enterModel} % From CoCoRaw
%    \alias{enterNames} % From CoCoRaw
%    \alias{enterQlist} % From CoCoRaw
%    \alias{enterQtable} % From CoCoRaw
%    \alias{enterTable} % From CoCoRaw
%    \alias{enterTwoLists} % From CoCoRaw
%    \alias{excludeMissing} % From CoCoRaw
%    \alias{exportCoCo} % From CoCoRaw
%    \alias{extractData} % From CoCoRaw
%    \alias{forward} % From CoCoRaw
%    \alias{importCoCo} % From CoCoRaw
%    \alias{isSubmodel} % From CoCoRaw
%    \alias{makeBase} % From CoCoRaw
%    \alias{makeCurrent} % From CoCoRaw
%    \alias{numberVariates} % From CoCoRaw
%    \alias{optionsCoCo} % From CoCoRaw
%    \alias{partialAssociations} % From CoCoRaw
%    \alias{propertyModel} % From CoCoRaw
%    \alias{propertySet} % From CoCoRaw
%    \alias{randomTableReplacement} % From CoCoRaw
%    \alias{returnDeviance} % From CoCoRaw
%    \alias{returnEdges} % From CoCoRaw
%    \alias{returnExpression} % From CoCoRaw
%    \alias{returnFix} % From CoCoRaw
%    \alias{returnJunctionTree} % From CoCoRaw
%    \alias{returnModel} % From CoCoRaw
%    \alias{returnModelNumber} % From CoCoRaw
%    \alias{returnModelVariates} % From CoCoRaw
%    \alias{returnNcells} % From CoCoRaw
%    \alias{returnSets} % From CoCoRaw
%    \alias{returnTable} % From CoCoRaw
%    \alias{returnTest} % From CoCoRaw
%    \alias{returnVariableDescription} % From CoCoRaw
%    \alias{returnVertexOrder} % From CoCoRaw
%    \alias{setOrdinal} % From CoCoRaw
%    \alias{setUseVariables} % From CoCoRaw
%    \alias{showDeviance} % From CoCoRaw
%    \alias{showFormula} % From CoCoRaw
%    \alias{showModel} % From CoCoRaw
%    \alias{showOptions} % From CoCoRaw
%    \alias{showTable} % From CoCoRaw
%    \alias{showTest} % From CoCoRaw
%    \alias{showTests} % From CoCoRaw
%    \alias{showVertexOrder} % From CoCoRaw
%    \alias{sinkCoCo} % From CoCoRaw
%    \alias{summaryTable} % From CoCoRaw

\description{The CoCo package with the interface functions to CoCo
             and CoCo for discrete data.}

% \details{}

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\note{Please quit by \code{\link[quitCoCo]{quitCoCo}}
      to remove temporary files.}

% \section{ToDo}{....}

\seealso{ \code{\link[CoCoCg]{CoCoCg}}. }

\examples{
# Attach data, here table of counts:
data(Reinis);

# Create a CoCo-object:
CoCoObject <- makeCoCo();

# Enter the table of counts into the CoCo-object:
enterTable(Reinis, object = CoCoObject);

# Enter the saturated model into the CoCo-object, and return a model object:
fullModel <- makeModel(enterModel("*", object = CoCoObject));

# Display a graph of the model:
fullGraph <- dynamic.Graph(fullModel, title = "Full", returnLink = TRUE);

# Do a backward elimination of edges:
backward(recursive = TRUE, headlong = TRUE, coherent = TRUE,
         follow = TRUE, object = CoCoObject);

# Display the result of the backward elimination:
lastModel <- makeModel("last", object = CoCoObject);
backwardGraph <- dynamic.Graph(lastModel, title = "Last", 
                               dynamicGraph = fullGraph, slave = TRUE);

# Do the EH-procedure:
eh(object = CoCoObject);

# Terminate the CoCo-object with disposing temporary files:
endCoCo(object = CoCoObject);
}

\keyword{internal}
