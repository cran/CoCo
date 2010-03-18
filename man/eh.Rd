\name{eh}
\alias{eh}

\title{ The Edwards and Havranek model search procedure }

\description{ After using the
	      "Fast Procedure for Model Search", the EH-procedure,
	      by Edwards and Havranek (1985, 1985) any model
	      can be labeled as accepted or as rejected.	
	      By the principles of weak acceptance, weak rejection,
	      and coherence, the search space of all hierarchical
	      graphical, or decomposable models is divided into
	      two sets of models: The class of minimal acceptable models,
	      and the class of maximal rejected models.
}

\usage{
eh(strategy = c("smallest", "alternating", "rough")[1],
   sub.class = c("decomposable", "graphical", "hierarchical")[2],
   p.accepted = FALSE,  data = NULL, 
   object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{strategy}{ A character string. 
		   The argument \code{strategy} should be
		   \code{"smallest"}, 
		   \code{"alternating"}, 
		   or \code{"rough"}.
		   See the section "Details" below.
		   }
  \item{sub.class}{ A character string.
		    The argument \code{sub.class} should be
		    \code{"decomposable"}, 
		    \code{"graphical"}, 
		    or \code{"hierarchical"}.
		    See the section "Details" below.
		    }
  \item{p.accepted}{ Numeric, models with p-value greater than
		     \code{p.accepted} are accepted, else they are rejected. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

\details{
    \itemize{
	\item{Smallest    }{: At each cycle, select the dual
			      with fewest models to fit. }   
	\item{Alternating }{: Switch between fitting the a-dual
			      and the r-dual. }   
	\item{Rough 	  }{: Give a rough estimate of the number of
			      models in each dual, and fit the dual
			      with the smallest rough size. }   
    }
    \itemize{
	\item{Decomposable}{: Restrict the search space to decomposable
    models. } 
	\item{Graphical   }{: Restrict the search space to graphical
    models. }
	\item{Hierarchical}{: Set the the search space to hierarchical
    models.}
    }
 }

\value{ TRUE }

\references{Badsberg, J.H. (2001): A guide to CoCo, JSS
            ( http://www.jstatsoft.org/v06/i04/ ).

            Badsberg, J.H. (1996): Xlisp+CoCo, Aalborg University.

            Edwards, D. and Havranek, T. (1985). A fast procedure for
	    model search in multidimensional contingency tables.
	    Biometrika 72: 339-351.

            Edwards, D. and Havranek, T. (1987). A fast model
	    selection procedure for large families of models.
	    J. Amer. Stat. Assoc. 82: 205-231.
}

\author{Jens Henrik Badsberg}

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{forward}}, \code{\link{backward}},
	  and \code{\link{disposeOfEh}}. }

\examples{ 
library(CoCo);
data(Reinis);

CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);
eh(sub.class = "decomposable", object = CoCoObject);
eh(sub.class = "graphical",    object = CoCoObject);
eh(sub.class = "hierarchical", object = CoCoObject);
endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
