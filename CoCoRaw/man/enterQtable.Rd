\name{enterQtable}
\alias{enterQtable}
\alias{enterQlist}

\title{ Enter initial values for the IPS algorithm and
	declare cells to be zero by structure }

\description{ Enter a Q-table or Q-list: 
	      Initial values for the IPS-algorithm and
	      the cells of a table to be zero by structure. }

\usage{
enterQtable(set, table, setslot = TRUE, data = NULL, 
            object = .object.of.thing(data = data, ...), ...)

enterQlist(set, list, setslot = TRUE, data = NULL, 
           object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{set}{ A character string giving the \code{set} of variables
	      of the table. }
  \item{table}{ A vector of integers, one for each \code{cells}
	        of the table formed by the cross classification
	        of the variables of \code{set}. }
  \item{list}{ A vector of integers, a list of \code{cells} of the
	       table formed by the cross classification of the
	       variables of \code{set}. }
  \item{setslot}{ See \code{\link{enterNames}}. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

\details{ Cells equal to zero are by structure zero. 
	  \code{\link{enterQlist}} will enter these cells. }

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{disposeOfQtable}}. }

\examples{ 
library(CoCo);

without <- makeCoCo(title = "Without structural zero");
optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
            decimals.table.expected = 1, decimals.table.residual = 1);
enterNames("smv", rep(2, 3));
n <- c(11, 2209, 0, 111, 48, 239, 72, 2074);
enterTable(n);
A.1 <- make.model("*");
A.2 <- make.model("mv,vs");
showTest();
endCoCo();

with <- makeCoCo(title = "With structural zero");
optionsCoCo(digits.table = 6, decimals.table.probabilities = 2, 
            decimals.table.expected = 1, decimals.table.residual = 1);
enterNames("smv", rep(2, 3));
n <- c(11, 2209, -1, 111, 48, 239, 72, 2074);
enterTable(n);
returnTable("zero");
B.1 <- make.model("*");
B.2 <- make.model("mv,vs");
showTest();
endCoCo();
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
