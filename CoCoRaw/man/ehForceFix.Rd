\name{ehForceFix}
\alias{ehForceFix}

\title{ Fix edges or terms of the models in the EH procedure }

\description{ Force terms into or out of models in the EH procedure.
	      By \code{ehForceFix(gc, fix = "in", ...)} edges (or generators)
	      are forced into all the models in the EH-procedure.
	      By \code{ehForceFix(gc, fix = "out", ...)} all terms 
	      containing a generator in the generating class
	      \code{gc} are excluded from all models considered
	      in the EH-procedure.
}

\usage{
ehForceFix(gc = "what", fix = "out", add.fix = FALSE, redo.fix = FALSE, 
           data = NULL, object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{gc}{  A character string. 
	      The argument \code{gc} is a generating class.
	       }
  \item{fix}{ A character string.  
	      The argument \code{fix} should be
	       \code{"out"} ( ... ),
	       or \code{"in"} ( ... ).
	       }
  \item{add.fix}{ Logical. 
		  \code{ehForceFix(gc, fix = "out", ...)} will clear
		  the fixing set by previous use of
		  \code{ehForceFix(gc, fix = "out", ...)},
		  if \code{add.fix} is FALSE.
		  If \code{add.fix} is TRUE then
		  the terms are added to the fixing.
		  Similar for \code{ehForceFix(gc, fix = "in", ...)}.
		  }
  \item{redo.fix}{ Logical, if \code{redo.fix} is TRUE then
		   the fixing is repeated. 
		  \code{ehForceFix(gc, fix = "out", redo.fix = TRUE, ...)}
		  will redo the combination of the last
		  \code{ehForceFix(gc, fix = "out", ...)} and following
		  \code{ehForceFix(gc, fix = "out", add.fix = TRUE, ...)}.
		  Fixing set by \code{ehForceFix(gc, fix = "in", ...)}
		  is then modified according to the fixing out.
		  Similar for 
		  \code{ehForceFix(gc, fix = "in", redo.fix = TRUE, ...)}.
		  }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

\details{ Terms set by \code{ehForceFix(gc, fix = "out", ...)}
	  is the dual representation of the maximal models considered,
	  see pp. 340 Edwards \& Havranek (1985). To exclude edges
	  in the graphical search only generators with cardinality 2
	  (edges) should be given to \code{ehForceFix(gc, fix = "out", ...)}.

	  }

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\note{ Using the function \code{ehForceFix} will only
       set up fixing for future models visited be \code{\link{eh}}.
       Thus if the EH-procedure already has been used the duals and the 
       model classes should be disposed of be \code{\link{disposeOfEh}}.

       \code{ehForceFix(gc, fix = "out", ...)} and 
       \code{ehForceFix(gc, fix = "in", ...)} will interact:

       Terms set by \code{ehForceFix(gc, fix = "in", ...)} 
       are remove from the terms set by
       \code{ehForceFix(gc, fix = "out", ...)}:
       Terms set by \code{ehForceFix(gc, fix = "in", ...)}
       are added to the generating class achieved by transforming
       \code{ehForceFix(gc, fix = "out", ...)}
       from the dual representation to the normal representation,
       and the resulting generating class is transformed back to
       the dual representation.

       Term set by \code{ehForceFix(gc, fix = "out", ...)} 
       are remove from the terms set by
       \code{ehForceFix(gc, fix = "in", ...)}:
       The terms set by \code{ehForceFix(gc, fix = "in", ...)}
       is restricted to the generating class achieved by transforming
       \code{ehForceFix(gc, fix = "out", ...)} from the dual representation
       to the normal representation.
       }

% \section{ToDo}{....}

\seealso{ \code{\link{eh}} and \code{\link{returnFix}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
