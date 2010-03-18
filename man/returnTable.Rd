\name{returnTable}
\alias{returnTable}

\title{ Return the counts and fitted values of a marginal table }

\description{ Return the observed probabilities, counts, 
	      means, covariance matrix, canonical parameters,
	      fitted values, residuals, etc. of marginal tables. }

\usage{
returnTable(type = "observed", set = "*", model = FALSE, 
            random = FALSE, log.transformed = FALSE, complete = FALSE, 
            discrete.ordered = TRUE, dump = FALSE, label = TRUE, 
            split = TRUE, discrete.outer = TRUE, mixed = FALSE, data = NULL, 
            object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{type}{ See also \code{\link{showTable}}. 
	       For \code{set} with only discrete variables
	       the \code{type} \code{"sparse.table"} is avaliable
	       to return the cells of the table with count different
	       from zero.
	       For \code{set} with both discrete and continuous variables
	       the argument \code{type} should be
	       \code{"canonical"},
	       \code{"moment"},
	       \code{"raw"},
	       \code{"mk"},
	       or, \code{"ms"}.
	     }
  \item{set}{ See \code{\link{showTable}}. }
  \item{model}{ See \code{\link{returnModel}}. }
  \item{random}{ See \code{\link{showTable}}. }
  \item{log.transformed}{ See \code{\link{showTable}}. }
  \item{complete}{ See \code{\link{showTable}}. }
  \item{discrete.ordered}{ See \code{\link{showTable}}. }

  \item{dump}{ Logical. For \code{set} with only discrete variables.
	       If \code{dump} is TRUE and \code{dump} of
	       \code{\link{optionsCoCo}} also is TRUE
	       then the table is written to the dump file.  }

  \item{label}{ Logical, if \code{label} then labels
		are set on returned arrays. }
  \item{split}{ Logical, for the argument \code{type} 
		with both discrete and continuous variables:
		If \code{split} is TRUE then
		the returned quantities are split
		into the three parts
		discrete, linear and quadratic. }
  \item{discrete.outer}{ Logical,  for the argument \code{type} 
			 with both discrete and continuous variables:
			 If \code{discrete.outer} is FALSE then 
			 order of the discrete and the continuous
			 variables are permuted. }

  \item{mixed}{ See \code{\link{showTable}}. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

% \details{ }

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{showTable}}. }

\examples{
example.requiring.CoCoCg <- function() {
library(CoCoCg);
data(fev);

fevCoCo <- makeCoCoCg();
result <- enterDataFrame(fev, object = fevCoCo);

enterModel("*;", object = fevCoCo);

showTable("observed", ":Sex:Smoker", object = fevCoCo);

showTable("canonical", ":Sex:Smoker:Age:FEV:Height", 
		       table = TRUE, object = fevCoCo);
showTable("canonical", ":Sex:Smoker:Age:FEV:Height", 
		       matrix = TRUE, object = fevCoCo);
showTable("canonical", ":Age:FEV:Height",            
		       matrix = TRUE, object = fevCoCo);

c1 <- returnTable("canonical", ":Sex:Smoker:Age:FEV:Height", 
                  label = FALSE, split = FALSE, object = fevCoCo);
c2 <- returnTable("canonical", ":Sex:Smoker:Age:FEV:Height", 
                  label = TRUE, object = fevCoCo);
c3 <- returnTable("canonical", ":Sex:Smoker:Age:FEV:Height", 
                  label = TRUE, split = FALSE, object = fevCoCo);
c4 <- returnTable("canonical", ":Sex:Smoker:Age:FEV:Height", 
                  label = TRUE, split = FALSE, 
                  discrete.outer = TRUE, object = fevCoCo);

returnTable("canonical", ":Sex:Smoker:Age:FEV:Height", label = TRUE);

returnTable("moment", ":Sex:Smoker:Age:Height", label = TRUE)$Mean;
returnTable("moment", ":Smoker:Age:Height", label = TRUE)$Mean;
returnTable("moment", ":Sex:Age:Height", label = TRUE)$Mean;

c4[,,1]
c4[1,1,]
c5 <- returnTable("canonical", ":Sex:Smoker:Age:FEV:Height", 
                  label = TRUE, split = TRUE, discrete.outer = FALSE, 
		  object = fevCoCo);
c6 <- returnTable("canonical", ":Sex:Smoker:Age:FEV:Height", 
                  label = TRUE, split = TRUE, discrete.outer = TRUE, 
		  object = fevCoCo);

showTable("moment", ":Sex:Smoker:Age:FEV:Height", 
		    matrix = TRUE, object = fevCoCo);

m1 <- returnTable("moment", ":Sex:Smoker:Age:FEV:Height", 
                  label = FALSE, split = FALSE, object = fevCoCo);
m2 <- returnTable("moment", ":Sex:Smoker:Age:FEV:Height", 
                  label = TRUE, object = fevCoCo);
m3 <- returnTable("moment", ":Sex:Smoker:Age:FEV:Height", 
                  label = TRUE, split = FALSE, object = fevCoCo);
m4 <- returnTable("moment", ":Sex:Smoker:Age:FEV:Height", 
                  label = TRUE, split = FALSE, 
                  discrete.outer = TRUE, object = fevCoCo);
m4[,,1]
m4[1,1,]
m5 <- returnTable("moment", ":Sex:Smoker:Age:FEV:Height", 
                  label = TRUE, split = TRUE, 
                  discrete.outer = FALSE, object = fevCoCo);
m6 <- returnTable("moment", ":Sex:Smoker:Age:FEV:Height", 
                  label = TRUE, split = TRUE, 
                  discrete.outer = TRUE, object = fevCoCo);

endCoCo(object = fevCoCo);
}
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
