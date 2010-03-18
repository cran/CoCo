\name{propertyModel}
\alias{propertyModel}

\title{ Ask a query for a model }

\description{ Test a query for a model. }

\usage{
propertyModel(query = "chordal", model = FALSE, 
              prior.action = NULL, modification = NULL, data = NULL, 
              object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{query}{ A character string. The argument \code{query} is
		\code{"graphical"}, 
		\code{"decomposable"}, (or \code{"chordal"}, 
		                            \code{"triangulated"}, 
                                        or \code{"rigid.circuit"}),
		\code{"tree"}, 
		\code{"connected"}, 
		\code{"undirected"} (or \code{"skeleton"}),
		\code{"moral"}, 
		\code{"acyclic"}, 
		\code{"MIM-model"}, 
		\code{"mean-linear"}, 
		\code{"d-collapsible"}, 
		\code{"q-equivalent"}, 
		\code{"homogeneous"}, 
		\code{"full-specified"}, 
		\code{"degenerated"}, 
		\code{"discrete"} (or \code{"pure-discrete"}),
		\code{"continuous"} (or \code{"pure-continuous"}),
		\code{"mixed"}, 
		or \code{"regression"} (or \code{"cg-regression"}).
		}

  \item{model}{ See \code{\link{returnModel}}. }

  \item{prior.action}{ A character string,
		       if \code{prior.action} is set, then
		       the model is modified by this action before
		       asking the query. See \code{\link{editModel}}. }
  \item{modification}{ See \code{\link{editModel}}. }

  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}


\details{
    \itemize{
	\item{ Graphical }{:} 
	\item{ Decomposable }{ (or \code{"chordal"}, \code{"triangulated"},
                                or \code{"rigid.circuit"}): }
	\item{ Tree }{:} 
	\item{ Connected }{:} 
	\item{ Undirected }{ (or \code{"skeleton"}): }
	\item{ Moral }{:} 
	\item{ Acyclic }{:} 
	\item{ MIM-model }{:} 
	\item{ Degenerated }{:} 
	\item{ Mean-linear }{:} 
	\item{ D-collapsible }{:} 
	\item{ Q-equivalent }{:} 
	\item{ Homogeneous }{:} 
	\item{ Full-specified }{:} 
	\item{ Discrete }{ (or \code{"pure-discrete"}): }
	\item{ Continuous }{ (or \code{"pure-continuous"}): }
	\item{ Mixed }{ (or \code{"regression"} or \code{"cg-regression"}): }
    }
 }

\value{ Logical. }

% \references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
%             ( http://www.jstatsoft.org/v06/i04/ )
%             and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

\seealso{ \code{\link{propertySet}}. }

\examples{
library(CoCo);
data(Fuchs82);
CoCoObject <- makeCoCo();
enterTable(Fuchs82, object = CoCoObject);

enterModel("mdp,da,am,dg,gs,sa", object = CoCoObject);

propertyModel("graphical", object = CoCoObject);
propertyModel("decomposable", object = CoCoObject);
propertyModel("connected", object = CoCoObject);
propertyModel("tree", object = CoCoObject);

enterModel("*", object = CoCoObject);
backward(recursive = TRUE, follow = TRUE, object = CoCoObject);
makeCurrent("last", object = CoCoObject);
showModel("all", object = CoCoObject);
propertyModel("graphical", object = CoCoObject);
propertyModel("decomposable", object = CoCoObject);
propertyModel("connected", object = CoCoObject);
propertyModel("tree", object = CoCoObject);

editModel("drop.edges", "md", object = CoCoObject);
makeCurrent("last", object = CoCoObject);
showModel("all", object = CoCoObject);
propertyModel("graphical", object = CoCoObject);
propertyModel("decomposable", object = CoCoObject);
propertyModel("connected", object = CoCoObject);
propertyModel("tree", object = CoCoObject);

endCoCo(CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
