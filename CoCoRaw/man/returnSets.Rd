\name{returnSets}
\alias{returnSets}

\title{ Return a specific subset of the variables  }

\description{ Return for a model
                     the connected component containing a specific set, 
		     connected components,  
		     prime components,  
		     junction tree components,  
		     chain components,  
		     ancestral set,  
		     shortest paths between two sets, 
		     cut sets, 
		     separators,  
		     D-separators,  
		     neighbours of a set, 
		     TRUE if s set is a separator, 
		     or TRUE if s set is a d-separator. }

\usage{
returnSets(model = FALSE, set = "", set.a = "", set.b = "", 
           u = "", v = "", type = "primes", 
           split.gc = FALSE, split.generators = FALSE, data = NULL, 
           object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{model}{ See \code{\link{returnModel}}. }

  \item{set}{ A character string with a variable set. }
  \item{set.a}{ A character string with a variable set. }
  \item{set.b}{ A character string with a variable set. }
  \item{u}{ A character string with a single variable name. }
  \item{v}{ A character string with a single variable name. }
  \item{type}{ A character string with the text
		\code{"connected.component"} (\code{set} used),
		\code{"connected.components"}, 
		\code{"prime.components"}, 
		\code{"chain.components"}, 
		\code{"junction.tree.components"}, 
		\code{"ancestral.set"}, 
		\code{"shortest.paths"} (\code{u} and \code{v} used),
		\code{"cut.sets"} (\code{u} and \code{v} or
                                   \code{set.a} and \code{set.a} used),
		\code{"separators"}, 
		\code{"d-separators"}, 
		\code{"neighbours"}, (\code{set} used),
		\code{"is.separator"}, (\code{set} used),
		\code{"is.d-separator"}, (\code{set} used).
		}

  \item{split.gc}{ See \code{\link{returnModel}}. }
  \item{split.generators}{ See \code{\link{returnModel}}. }

  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }

}

\details{
    \itemize{
	\item{ Connected component }{: (\code{set} used) }
	\item{ Connected components }{:} 
	\item{ Prime components }{:} 
	\item{ Junction tree components }{:} 
	\item{ Chain components }{:} 
	\item{ Ancestral set }{:} 
	\item{ Shortest paths }{: (\code{u} and \code{v} used) }
	\item{ Cut sets }{: (\code{u} and \code{v} or \code{set.a} and \code{set.a} used) }
	\item{ Separators }{:} 
	\item{ D-separators }{:} 
	\item{ Neighbours }{: (\code{set} used) }
	\item{ Is separator }{: (\code{set} used) }
	\item{ Is d-separator }{: (\code{set} used) }
    }
 }

\value{A boolean, NULL,
       a list with the component \code{string} with a generating class,
       a list with vectors of character strings of variables,
       or a vector of character strings of sets of variable. }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

\section{ToDo}{Separators not returned from mixed models. }

\seealso{ \code{\link{propertySet}}, \code{\link{propertyModel}},
          and \code{\link{returnVertexOrder}}. }

\examples{
library(CoCo);
data(Fuchs82);

CoCoObject <- makeCoCo();
enterTable(Fuchs82, object = CoCoObject);

enterModel("mdp,da,am,dg,gs,sa", object = CoCoObject);

returnSets(model = "current", type = "connected.component", set = "dm",
           object = CoCoObject);
returnSets(model = "current", type = "connected.components", 
           object = CoCoObject);

returnSets(model = "current", type = "prime.components", 
           object = CoCoObject); 
returnExpression(type = "prime.components",
           object = CoCoObject);

returnSets(model = "current", type = "junction.tree.components", 
           object = CoCoObject);
returnSets(model = "current", type = "chain.components", 
           object = CoCoObject);
returnSets(model = "current", type = "ancestral.set", 
           object = CoCoObject);
returnSets(model = "current", type = "shortests.paths", u = "s", v = "p", 
           object = CoCoObject);
returnSets(model = "current", type = "cut.sets", u = "s", v = "p", 
           object = CoCoObject);
returnSets(model = "current", type = "cut.sets", set.a = "sg", set.b = "p",
           object = CoCoObject);
returnSets(model = "current", type = "separators", 
           object = CoCoObject);
returnSets(model = "current", type = "d-separators", 
           object = CoCoObject);
returnSets(model = "current", type = "neighbours", set = "dm", 
           object = CoCoObject);
returnSets(model = "current", type = "is.separator", set = "dm", 
           object = CoCoObject);
returnSets(model = "current", type = "is.d-separator", set = "dm", 
           object = CoCoObject);

returnModel(object = CoCoObject);

returnModel(split.generators = TRUE, object = CoCoObject);

returnSets(model = FALSE, type = "neighbours", set = c("d", "m"), 
           object = CoCoObject);

returnSets(model = FALSE, type = "neighbours", set = "dm", 
           object = CoCoObject);

returnSets(model = FALSE, type = "neighbours", set = "[dm]", 
           object = CoCoObject);

returnSets(model = "current", type = "separators", 
           split.gc = TRUE, object = CoCoObject);

returnSets(model = "current", type = "separators", 
           split.generator = TRUE, split.gc = TRUE, object = CoCoObject);

returnSets(model = "current", type = "separators", 
           split.generator = TRUE, object = CoCoObject); # ?

endCoCo(object = CoCoObject);

library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();
enterDataFrame(Rats, object = CoCoObject);
enterModel("ax,xb,y.", object = CoCoObject);

returnSets(model = "current", type = "connected.component", set = "ax",
           object = CoCoObject);
returnSets(model = "current", type = "connected.components", 
           object = CoCoObject);

returnSets(model = "current", type = "prime.components", 
           object = CoCoObject); 
returnExpression(type = "prime.components",
           object = CoCoObject);

returnSets(model = "current", type = "junction.tree.components", 
           object = CoCoObject);
returnSets(model = "current", type = "chain.components", 
           object = CoCoObject);
returnSets(model = "current", type = "ancestral.set", 
           object = CoCoObject);

returnSets(model = "current", type = "shortests.paths", u = "a", v = "b", 
           object = CoCoObject);
returnSets(model = "current", type = "cut.sets", u = "a", v = "b", 
           object = CoCoObject);
returnSets(model = "current", type = "cut.sets", set.a = "a", set.b = "b",
           object = CoCoObject);

returnSets(model = "current", type = "separators", 
           object = CoCoObject); # ?
returnSets(model = "current", type = "d-separators", 
           object = CoCoObject);
returnSets(model = "current", type = "neighbours", set = "x", 
           object = CoCoObject);
returnSets(model = "current", type = "is.separator", set = "x", 
           object = CoCoObject);
returnSets(model = "current", type = "is.d-separator", set = "x", 
           object = CoCoObject);

returnModel(object = CoCoObject);

returnModel(split.generators = TRUE, object = CoCoObject);

returnSets(model = FALSE, type = "neighbours", set = c("x", "b"), 
           object = CoCoObject);

returnSets(model = FALSE, type = "neighbours", set = "x", 
           object = CoCoObject);

returnSets(model = FALSE, type = "neighbours", set = "[x]", 
           object = CoCoObject);

returnSets(model = "current", type = "separators", 
           split.gc = TRUE, object = CoCoObject);

returnSets(model = "current", type = "separators", 
           split.generator = TRUE, split.gc = TRUE, object = CoCoObject);

returnSets(model = "current", type = "separators", 
           split.generator = TRUE, object = CoCoObject); # ?

returnSets(model = "current", type = "separators", 
           split.gc = TRUE, object = CoCoObject); # ?

returnSets(model = "current", type = "separators", 
           split.generator = TRUE, split.gc = TRUE, object = CoCoObject); # ?

returnSets(model = "current", type = "separators", 
           split.generator = TRUE, object = CoCoObject); # ?

endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
