\name{summaryTable}
\alias{summaryTable}

\title{ Univariate summary statistics for table values }

\description{ Print on standard output univariate summary statistics, 
	      histogram, and uniform and quantile-quantile plots
	      of values in marginal tables. }

\usage{
summaryTable(type = "observed", set = "*", model = FALSE, random = FALSE, 
             log.transformed = FALSE, complete = FALSE, uniform = FALSE, 
             rankit = FALSE, probit = FALSE, data = NULL, 
             object = .object.of.model(model, data = data, ...), ...)
}

\arguments{
  \item{type}{ A character string.
	       The argument \code{type} for discrete variables:
	       \code{"counts"}, %           "0"  
	       \code{"probabilities"}, %    "1"  
	       \code{"expected"}, % 	    "2"  
	       \code{"unadjusted"}, % 	    "3"  
	       \code{"absolute"}, % 	    "3"  
	       \code{"f-res"}, % 	    "4"  
	       \code{"r-f"}, % 		    "5"  
	       \code{"g-res"}, % 	    "6"  
	       \code{"r-g"}, % 		    "7"  
	       \code{"adjusted"}, % 	    "8"  
	       \code{"c-res"}, % 	    "8"  
	       \code{"m-res"}, % 	    "9"  
	       % \code{"standardized"}, %     "9"  
	       \code{"standard"}, % 	    "9"  
	       \code{"x-res"}, % 	    "9"  
	       \code{"l-res"}, % 	    "10" 
	       \code{"freeman-tukey"}, %    "11" 
	       \code{"sqrt"}, %	       	    "12" 
	       \code{"power"}, % 	    "13" 
	       \code{"index"}, % 	    "14" 
	       \code{"zero"}, %		    "15" 
	       \code{"error"}. % 	    "63" 
	       See also \code{\link{showTable}}. 
	       }
  \item{set}{ See \code{\link{showTable}}. }
  \item{model}{ See \code{\link{returnModel}}. }
  \item{random}{ See \code{\link{showTable}}. }
  \item{log.transformed}{ See \code{\link{showTable}}. }
  \item{complete}{ See \code{\link{showTable}}. }
  \item{uniform}{ Logical, if \code{uniform} is TRUE
		  then a Q-Q-plot is plotted according to the argument. }
  \item{rankit}{ Logical, if \code{rankit} is TRUE
		  then a Q-Q-plot is plotted. }
  \item{probit}{ Logical, if \code{probit} is TRUE
		  then a Q-Q-plot is plotted. }
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

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
