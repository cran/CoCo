\name{showTable}
\alias{showTable}

\title{ The counts and fitted values of a marginal table }

\description{ Print on standard output the observed probabilities,
	      counts, means, covariance matrix, canonical parameters,
	      fitted values, residuals, etc. of marginal tables. }

\usage{
showTable(type = "observed", set = "*", model = FALSE, 
          random = FALSE, log.transformed = FALSE, complete = FALSE, 
          discrete.ordered = TRUE, table = FALSE, matrix = TRUE, 
          mixed = FALSE, output.form = "table", data = NULL, 
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
	       \code{"r-f"}, %  	    "5"  
	       \code{"g-res"}, % 	    "6"  
	       \code{"r-g"}, %		    "7"  
	       \code{"adjusted"}, % 	    "8"  
	       \code{"c-res"}, % 	    "8"  
	       \code{"m-res"}, % 	    "9"  
	       \code{"standardized"}, %     "9"  
	       \code{"standard"}, % 	    "9"  
	       \code{"x-res"}, % 	    "9"  
	       \code{"l-res"}, % 	    "10" 
	       \code{"freeman-tukey"}, %    "11" 
	       \code{"sqrt"}, % 	    "12" 
	       \code{"power"}, % 	    "13" 
	       \code{"index"}, % 	    "14" 
	       \code{"zero"}, % 	    "15" 
	       \code{"error"}. % 	    "63" 

	       The argument \code{type} for both discrete and
	       continuous variables:
	       \code{"leverage"}, % 	    "16" 
	       \code{"canonical"}, % 	    "18" 
	       \code{"gs"}, % 		    "19" 
	       \code{"hs"}, % 		    "20" 
	       \code{"ks"}, % 		    "21" 
	       \code{"moment"}, % 	    "22" 
	       \code{"means"}, % 	    "23" 
	       \code{"covariance"}, % 	    "24" 
	       \code{"raw"}, %		    "25" 
	       \code{"total"}, % 	    "26" 
	       \code{"ss"}, % 		    "27" 
	       \code{"ssds"}, % 	    "28" 
	       \code{"determinants"}, %     "29" 
	       \code{"mk"}, % 		    "30" 
	       \code{"ms"}. % 		    "31" 

 }
  \item{set}{ A character string with the \code{set} of variables. }
  \item{model}{ See \code{\link{returnModel}}. }
  \item{random}{ Logical, if \code{random} then a random table
		 with counts in the sufficient marginal tables
		 as in the observed table is generated,
		 and the values are are computed for this random table. }
  \item{log.transformed}{ Logical, if TRUE then the values are
			   \code{log.transformed} before printed. }
  \item{complete}{ Logical, if \code{complete} is TRUE then
		   no value is returned for cells to be zero
		   by structure. }
  \item{discrete.ordered}{ Logical, if \code{discrete.ordered} then
			   the variables are ordered as specified
			   by the call. }
  \item{table}{ Logical, for CoCoCg objects.
		If \code{table} is FALSE then for each configuration
		in the cross classification of the discrete variables
		the means and covariances of the continuous variables
		are printed according to the argument \code{matrix}.
		If \code{table} is TRUE then for each mean
		and for each covariance a table formed by the
		cross classification of the discrete variables
		is printed with the quantity. }
  \item{matrix}{ Logical, for CoCoCg objects.
		If \code{matrix} is TRUE then the means and covariances
		are printed in a matrix with the continuous variables
		as headings, else all the quantities for the continuous
		variables are printed on a single line for each configuration
		in the cross classification of the discrete variables. }
  \item{mixed}{ Logical, if \code{mixed} is TRUE then mixed
		quantities are printed.. }
  \item{output.form}{ A character string.
		      The argument \code{output.form} is only
		      used for only discrete variables:
		      \code{"table"} for the table of the value selected by
				     the argument \code{type},
		      \code{"sparse.table"} for a list of counts in cells
					    with count different for zero,
		      \code{"case.list"} for a case list,
		      or, \code{"list.all.values"} for a list of all the
		      values for discrete variables. }

  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

\details{
	\itemize{
	\item{Marginal model }{:}   
	\item{ Observed }{: (or \code{"counts"})} %		"0"
	\item{ Probabilities }{: } %	"1"
	\item{ Expected }{: } %		"2"
	\item{ Unadjusted }{: (or \code{"absolute"})} %	"3"
	\item{ F-res }{: } %		"4"
	\item{ R-f }{: } %		"5"
	\item{ G-res }{: } %		"6"
	\item{ R-g }{: } %		"7"
	\item{ Adjusted }{: } %		"8"
	\item{ C-res }{: } %		"8"
	\item{ Standardized }{: (or \code{"m-res"},
				    \code{"x-res"}, or
                                    \code{"standard"}) } %	"9"
	\item{ X-res }{: } %		"9"
	\item{ Deviance }{: (or \code{"l-res"} or
			        \code{"-2log"})} %		"10"
	\item{ Freeman-tukey }{: } %	"11"
	\item{ Sqrt }{: (or \code{"2n-m"})} %		"12"
	\item{ Power }{: } %		"13"
	\item{ Index }{: } %		"14"
	\item{ Zero }{: } %		"15"
	\item{ Leverage }{: } %		"16"
	\item{ Canonical }{: } %	"18"
	\item{ Gs }{: } %		"19"
	\item{ Hs }{: } %		"20"
	\item{ Ks }{: } %		"21"
	\item{ Moment }{: } %		"22"
	\item{ Means }{: } %		"23"
	\item{ Covariance }{: } %	"24"
	\item{ Raw }{: } %		"25"
	\item{ Total }{: } %		"26"
	\item{ Ss }{: } %		"27"
	\item{ Ssds }{: (or \code{"sigma"}) } %		"28"
	\item{ Determinants }{: } %	"29"
	\item{ Mk }{: } %		"30"
	\item{ Ms }{: } %		"31"
	\item{ Error }{: } %		"63"
    }
    \itemize{
	\item{ Table }{: } % 
	\item{ Sparse table }{: } %
	\item{ Case list }{: } % 
	\item{ List all values }{: } % 
    }
}

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{summaryTable}}. }

\examples{
example.requiring.CoCoCg <- function() {
library(CoCoCg);
library(MASS)
data(crabs);

crabsCoCo <- makeCoCoCg();
result <- enterDataFrame(crabs[,-3], object = crabsCoCo);

enterModel("*;", object = crabsCoCo);

showTable("observed", "[:sp:sex]", object = crabsCoCo);

showTable("canonical", "[:sp:sex:FL:RW:CL:CW:BD]", 
		       matrix = TRUE, object = crabsCoCo);

showTable("canonical", "[:sp:sex:FL:RW:CL:CW:BD]", matrix = TRUE);

showTable("moment", "[:sp:sex:FL:RW:CL:CW:BD]", 
		       matrix = TRUE, object = crabsCoCo);

returnTable("canonical", "[:sp:sex:FL:RW:CL:CW:BD]");

returnTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]");

# backward(recursive = TRUE, headlong = TRUE, coherent = TRUE, follow = TRUE, 
#          object = crabsCoCo);

m5 <- "[[:sp:sex]] / [[:sp:sex:FL][:sp:sex:RW][:sex:CL][:sp:sex:BD][:sp:sex:CW]] / [[:sex:FL:CL][:sp:sex:FL:CW][:sp:sex:FL:BD][:sp:sex:RW:CW][:sex:CL:CW][:sex:CL:BD][:sp:sex:CW:BD]]"
m4 <- "[[:sp:sex]] / [[:sp:sex:RW][:sp:sex:BD][:sp:sex:CW][:sp:sex:FL][:sex:CL]] / [[:sp:sex:CW:BD][:sex:CL:BD][:sex:CL:CW][:sp:sex:RW:BD][:sp:sex:RW:CW][:sp:sex:FL:BD][:sp:sex:FL:CW][:sex:FL:CL]]"
m3 <- "[[:sp:sex]] / [[:sp:sex:FL][:sp:sex:RW][:sex:CL][:sp:sex:BD][:sp:sex:CW]] / [[:sp:sex:FL:RW][:sex:FL:CL][:sp:sex:FL:CW][:sp:sex:FL:BD][:sp:sex:RW:CW][:sp:sex:RW:BD][:sex:CL:CW][:sex:CL:BD][:sp:sex:CW:BD]]"
m2 <- "[[:sp:sex]] / [[:sp:sex:BD][:sp:sex:CW][:sex:CL][:sp:sex:RW][:sp:sex:FL]] / [[:sp:sex:CW:BD][:sex:CL:BD][:sex:CL:CW][:sp:sex:RW:BD][:sp:sex:RW:CW][:sex:RW:CL][:sp:sex:FL:BD][:sp:sex:FL:CW][:sex:FL:CL][:sp:sex:FL:RW]]"
m1 <- "[[:sp:sex]] / [[:sp:sex:BD][:sp:sex:CW][:sp:sex:CL][:sp:sex:RW][:sp:sex:FL]] / [[:sp:sex:CW:BD][:sp:sex:CL:BD][:sp:sex:CL:CW][:sp:sex:RW:BD][:sp:sex:RW:CW][:sp:sex:RW:CL][:sp:sex:FL:BD][:sp:sex:FL:CW][:sp:sex:FL:CL][:sp:sex:FL:RW]]"

enterModel(m1, object = crabsCoCo);
enterModel(m2, object = crabsCoCo);
enterModel(m3, object = crabsCoCo);
enterModel(m4, object = crabsCoCo);
enterModel(m5, object = crabsCoCo);

makeCurrent("last", object = crabsCoCo)
returnTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]");

Rfirst <- returnTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]", model = 1);
Rlast  <- returnTable("mk", "[:sp:sex:FL:RW:CL:CW:BD]", model = 6);
Sfirst <- returnTable("ms", "[:sp:sex:FL:RW:CL:CW:BD]", model = 1);
Slast  <- returnTable("ms", "[:sp:sex:FL:RW:CL:CW:BD]", model = 6);

Rfirst$h-Rlast$h
Rfirst$K-Rlast$K
Sfirst$Mean-Slast$Mean

endCoCo(object = crabsCoCo);
}
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
