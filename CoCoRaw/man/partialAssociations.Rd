\name{partialAssociations}
\alias{partialAssociations}

\title{ Show partial associations }

\description{ Print on standard output the two dimensional tables with margins,
	      possible for each configuration in the cross classification
	      of other variables, and compute the measures of partial
	      associations on these tables. }

\usage{
partialAssociations(a = "", b = "", c = "", options, data = NULL, 
                    object = .object.of.thing(data = data, ...), ...)
}

\arguments{
  \item{a}{ A character string with the name of first variable. }
  \item{b}{ A character string with the name of second variable. }
  \item{c}{ A character string with the names of variables
	    to condition on. }
  \item{options}{ A character string with \code{options},
		  currently not used. }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
  \item{\dots}{ Additional arguments to generate the CoCo \code{object}
	 from the \code{data} argument. See \code{\link{propertyModel}}. }
}

\details{
	\itemize{

	\item{             Fisher's exact test }{:}  

	\item{                Pearson X.2 test }{:}  
	\item{       G.2 likelihood ratio test }{:}  
	\item{             Continuity-adj. X.2 }{:}  
	\item{             Yates corrected X.2 }{:}  
	\item{        McNemar test of symmetry }{:}  
	\item{                      Cramer's V }{:}  
	\item{                             Phi }{:}  
	\item{                         Max Phi }{:}  
	\item{       Contingency Coefficient C }{:}  
	\item{           Max Contingency coef. }{:}  
	\item{       Cross-product ratio alpha }{:}  
	\item{      Mantel-Haenszel chi-square }{:}  

	\item{         Pearson (product) corr. }{:}  
	\item{       Spearman rank corr. coef. }{:}  
	\item{         Ln(Cross-product ratio) }{:}  
	\item{                        Yule's Q }{:}  
	\item{                        Yule's Y }{:}  
	\item{                        Gamma, G }{:}  
	\item{                 Kendall's Tau b }{:}  
	\item{                  Stuart's Tau c }{:}  
	\item{                  Somers' D, R|C }{:}  
	\item{                  Somers' D, C|R }{:}  
	\item{      Goodman-Kruskal's Tau, R|C }{:}  
	\item{      Goodman-Kruskal's Tau, C|R }{:}  
	\item{       Optimal pred. lambda, R|C }{:}  
	\item{       Optimal pred. lambda, C|R }{:}  
	\item{       Optimal pred. lambda, sym }{:}  
	\item{       Optimal pred. lambda* R|C }{:}  
	\item{       Optimal pred. lambda* C|R }{:}  
	\item{        Uncertainty coef. U, R|C }{:}  
	\item{        Uncertainty coef. U, C|R }{:}  
	\item{        Uncertainty coef. U, sym }{:}  
	\item{                           Kappa }{:}  
 %	\item{                           Kappa }{:}  

	}
 }

\value{ TRUE }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{showTest}}. }

\examples{
library(CoCo);
data(HairEyeColor);
CoCoObject <- makeCoCo();
enterTable(HairEyeColor, object = CoCoObject);

partialAssociations(":Hair:Eye", object = CoCoObject);
partialAssociations(":Hair", ":Eye", object = CoCoObject);
partialAssociations(":Hair", ":Eye", ":Sex", object = CoCoObject);

endCoCo(object = CoCoObject);

# - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - -  - - - - - #

library(CoCo);
data(Reinis);
CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);
partialAssociations("AB", object = CoCoObject);

setOrdinal("AB", object = CoCoObject);
partialAssociations("AB", object = CoCoObject);

# Get Fisher's EXACT test when the total count of the sub table
# is less than 1000:

partialAssociations("A", "B", "F", object = CoCoObject);

endCoCo(object = CoCoObject);

}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
