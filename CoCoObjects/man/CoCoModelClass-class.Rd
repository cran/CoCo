\name{CoCoModelClass-class}
\docType{class}
\alias{CoCoModelClass-class}

% \alias{dynamic.Graph,CoCoModelClass-method}
% \alias{dynamic.Graph.Old,CoCoModelClass-method}
% \alias{modifyModel,CoCoModelClass-method}
% \alias{subModifyModel,CoCoModelClass-method}
% \alias{subModifyModel.Old,CoCoModelClass-method}
% \alias{testEdge,CoCoModelClass-method}
% \alias{testEdge.Old,CoCoModelClass-method}

% \alias{dynamic.Graph} % CoCoModelClass-method
% \alias{dynamic.Graph.Old} % CoCoModelClass-method
% \alias{modifyModel} % CoCoModelClass-method
% \alias{subModifyModel} % CoCoModelClass-method
% \alias{subModifyModel.Old} % CoCoModelClass-method
% \alias{testEdge} % CoCoModelClass-method
% \alias{testEdge.Old} % CoCoModelClass-method

\title{CoCoModelClass Class}

\description{
The functions for the CoCo objects are found mainly
in the package \link[CoCoRaw]{CoCoRaw-package}.
A few function for CoCo objects are found in
\link[CoCoObjects]{CoCoObjects-package} and 
\link[CoCoGraph]{CoCoGraph-package}. 

Datasets are found in the packages \link[CoCo]{CoCo-package} and
\link[CoCoCg]{CoCoCg-package}.
 }

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CoCoModelClass", ...)}.
Objects from tha class is returned by \code{\link{makeModel}}.
}

\section{Slots}{
  \describe{
    \item{\code{.model}:}{Object of class \code{"character"}
			  with the generating class(es) of the model. }
    \item{\code{.model.number}:}{Object of class \code{"numeric"}
				 with the model number of the model. }
    \item{\code{.reference}:}{Object of class \code{"numeric"},
			      see \code{\link{CoCoClass-class}}. }
    \item{\code{.id.env}:}{Object of class \code{"character"},
			   see \code{\link{CoCoClass-class}}. }
    \item{\code{.key}:}{Object of class \code{"character"},
			see \code{\link{CoCoClass-class}}. }
    \item{\code{.type}:}{Object of class \code{"numeric"},
			 see \code{\link{CoCoClass-class}}. }
    \item{\code{.title}:}{Object of class \code{"character"},
			  see \code{\link{CoCoClass-class}}. }
  }
}

\section{Extends}{
Class \code{"CoCoIdClass"}, directly.
}

\section{Methods}{
No methods defined with class "CoCoModelClass" in the signature.
}

\references{ Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{  }

\seealso{
  \code{\link{CoCoClass-class}}, 
  \code{\link{CoCoIdClass-class}}, 
  % \code{\link{CoCoModelClass-class}}, 
  \code{\link[CoCoCore]{coco.init}}, 
  or \code{\link[dynamicGraph]{GraphLatticeProto-class}}.
}

% \examples{ }

\keyword{classes}
