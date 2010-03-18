\name{CoCoClass-class}
\docType{class}
\alias{CoCoClass-class}

\title{CoCoClass Class}

\description{ 
Objects of class "CoCoClass-class".

% The functions for the CoCo objects are found mainly
% in the package \link{CoCoRaw-package}.
% A few function for CoCo objects are found in
% \link{CoCoObjects-package} and 
% \link[CoCoGraph]{CoCoGraph-package}. 

% Datasets are found in the packages \link[CoCo]{CoCo-package} and
% \link[CoCoCg]{CoCoCg-package}.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CoCoClass", ...)}.

% Objects from the class is returned by \code{\link[CoCo]{makeCoCo}}
% and \code{\link[CoCoCg]{makeCoCoCg}}.
}

\section{Slots}{
  \describe{
    \item{\code{.parameters}:}{Object of class \code{"list"}  
			       with the parameters for creating
			       the CoCo object, the arguemnts of
			       \code{\link{coco.init}}. }
    \item{\code{.invalid}:}{Object of class \code{"list"}
			    with the not recoverable values
			    of the object, the arguments of % the functions
			  % \code{\link[CoCoOldData]{set.data.file}}, 
			  % \code{\link[CoCoOldData]{set.observations.file}}, 
			  % \code{\link[CoCoOldData]{set.specification.file}}, 
			  % \code{\link[CoCoOldData]{read.data}}, 
			  % \code{\link[CoCoOldData]{read.specification}}, 
			  % \code{\link[CoCoOldData]{read.names}}, 
			  % \code{\link[CoCoOldData]{read.factors}}, 
			  % \code{\link[CoCoOldData]{reject.cases}}, 
			  % \code{\link[CoCoOldData]{select.cases}}, 
			  % \code{\link[CoCoOldData]{or.reject.cases}}, 
			  % \code{\link[CoCoOldData]{or.select.cases}}, 
			  % \code{\link[CoCoOldData]{redefine.factor}}, 
			  % \code{\link[CoCoOldData]{cutpoints}}, 
			  % \code{\link[CoCoOldData]{skip.missing}}, 
			  % \code{\link[CoCoOldData]{read.observations}}, 
			  % \code{\link[CoCoOldData]{read.table.coco}}, 
			  % \code{\link[CoCoOldData]{read.list}}, 
			  % \code{\link[CoCoOldData]{read.q.list}}, 
			  % \code{\link[CoCoOldData]{read.q.table}}, 
			    \code{\link{cleanData}}, and
			    \code{\link{randomTableReplacement}}
			    (if \code{setslot} is TRUE). }
    \item{\code{.specification}:}{Object of class \code{"list"} 
				  with the values of the specification
				  of the object, the arguments of
			          \code{\link{enterNames}} and
			          \code{\link{importCoCo}}
				  if \code{setslot} is TRUE. }
    \item{\code{.medio}:}{Object of class \code{"list"} 
			  with the values of the medio
			  of the object, the arguments of
			  \code{\link{setUseVariables}}
			  if \code{setslot} is TRUE. }
    \item{\code{.observations}:}{Object of class \code{"list"} 
				  with the values of the observations
				  of the object, the arguments of
			          \code{\link{enterTable}},
			          \code{\link{enterList}},
			        % \code{\link{enterTwoList}},
			          and \code{\link{enterDataFrame}}
				  if \code{setslot} is TRUE. }
    \item{\code{.structure}:}{Object of class \code{"list"} 
				  with the values of the structure
				  of the object, the arguments of
			          \code{\link{setOrdinal}},
			          \code{\link{excludeMissing}},
			          \code{\link{emOn}},
			          \code{\link{enterQtable}}
			          and \code{\link{enterQlist}}
				  if \code{setslot} is TRUE. }
    \item{\code{.reference}:}{Object of class \code{"numeric"}  
			      with the reference of the object:
			      a pointer to the memory. }
    \item{\code{.key}:}{Object of class \code{"character"}  
			with the key of the object. }
    \item{\code{.id.env}:}{Object of class \code{"character"},
			with an other identification of the object. }
    \item{\code{.type}:}{Object of class \code{"numeric"}  
			 with the type,
			 1 for CoCo objects,
			 2 for CoCoCg objects, of the object. }
    \item{\code{.title}:}{Object of class \code{"character"} 
			  with the title of the object. }
  }
}

\section{Extends}{
Class \code{"CoCoIdClass"}, directly.
}

\section{Methods}{
No methods defined with class "CoCoClass" in the signature.
}

\references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{  }

\seealso{
  % \code{\link{CoCoClass-class}}, 
  \code{\link{CoCoIdClass-class}}, 
  \code{\link{CoCoModelClass-class}}, 
  \code{\link{coco.init}}.
}

% \examples{ }

\keyword{classes}
