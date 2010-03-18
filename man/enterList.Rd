\name{enterList}
\alias{enterList}
\alias{enterTwoLists}

\title{ Enter a list of cases into a CoCo object }

\description{ Enter a list of cases into a CoCo object. }

\usage{
enterList(discrete, continuous = NULL, accumulated = FALSE, setslot = TRUE, 
          % ncol = FALSE, select.case.fun = FALSE, columns = FALSE, 
          % silent = FALSE, 
          data = NULL, object = .object.of.thing(data = data, ...), ...)

enterTwoLists(discrete, continuous, accumulated = FALSE, setslot = TRUE, 
              % ncol = FALSE, select.case.fun = FALSE, columns = FALSE, 
              % silent = FALSE, 
              data = NULL, object = .object.of.thing(data = data, ...), ...)

}

\arguments{
  \item{discrete}{ A vector (for \code{\link{enterList}})
		   or matrix (for \code{enterTwoList}) of numerics
		   for % the levels of
                   the \code{discrete} variables.
		   For \code{\link{enterList}} the cases should be given
		   one by one in the vector.
		   If \code{your.data} is a \code{data.frame}
		   or a \code{matrix} with cases row by row
		   you will have to give \code{discrete = c(t(your.data))}.
		   For \code{enterTwoList} the data should be given
		   in a \code{data.frame} or a \code{matrix} with
		   cases row by row.
		   }
  \item{continuous}{ A vector or matrix of numerics for the
		     \code{continuous} variables. Similar to \code{discrete}. }
  \item{accumulated}{ Logical, if \code{accumulated} is TRUE then the
		      first integer, count, in \code{discrete} for each
		      \code{"case"} should be the count for that case. 
		      For \code{\link{enterList}} the length of 
		      \code{discrete} thus have to be number of cases times
		      number of variables plus one,
		      For \code{enterTwoList} the number of columns
		      of \code{discrete} is number of variables plus one.
		      }
  % \item{ncol}{ ~~Describe \code{ncol} here~~ }
  % \item{select.case.fun}{ ~~Describe \code{select.case.fun} here~~ }
  % \item{columns}{ ~~Describe \code{columns} here~~ }
  % \item{silent}{ ~~Describe \code{silent} here~~ }

  \item{setslot}{ See \code{\link{enterNames}}. }
  \item{data}{ ( See \code{\link{exportCoCo}}. ) }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ ( Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. ) }
}

% \details{ }

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{enterNames}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
