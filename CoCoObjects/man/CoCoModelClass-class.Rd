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

\title{ Class "CoCoModelClass" }

\description{ }

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CoCoModelClass", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{.model}:}{Object of class \code{"character"}
			  with the generating class(es) of the model. }
    \item{\code{.model.number}:}{Object of class \code{"numeric"}
				 with the model number of the model. }
    \item{\code{.reference}:}{Object of class \code{"numeric"},
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
