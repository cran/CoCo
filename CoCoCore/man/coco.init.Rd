\name{coco.init}
\alias{coco.init}

\title{ Init a CoCo object. }

\description{ Internal CoCoCore function:
              Malloc the internal space for the CoCo object. }

\usage{
coco.init(n = 131072, p = 65536, q = 1024, 
          r = 65536, s = 65536, ss = 65536, t = 65536, 
          init = TRUE, title = NULL, type = 1, silent = FALSE, 
          location = c(700, 550), manager = TRUE, sh.lib.name = NULL)
}

\arguments{
  \item{n}{  Integer, sets the size of the \code{N}-array:
             The array for observed table and marginal tables
	     in pure discrete models. }
  \item{p}{  Integer, sets the size of the \code{P}-array:
             Estimated marginal tables in pure discrete models. }
  \item{q}{  Integer, sets the size of the \code{Q}-array:
             For Q-tables. }
  \item{r}{  Integer, sets the size of the \code{R}-array:
             For observed canonical parameters (g, h and K)
             in CoCoCg objects. }
  \item{s}{  Integer, sets the size of the \code{S}-array:
             For observed moment parameters (means and covariances)
             in CoCoCg objects. }
  \item{ss}{ Integer, sets the size of the \code{Ss}-array:
             For observed moment parameters (means and square sums)
             in CoCoCg objects.}
  \item{t}{  Integer, sets the size of the \code{T}-array:
             For estimated canonical parameters (g, h and K)
             and temporary space in CoCoCg objects. }

  \item{init}{ Logical, \code{init} should be TRUE,
	       else CoCo is "started", and CoCo commands are expected
	       from standard input. }
  \item{title}{ A character string for the \code{title} of the CoCo object. }
  \item{type}{ 1 for CoCo objects and 2 for CoCoCg objects. }
  \item{silent}{ Logical: if \code{silent} is TRUE then CoCo
		 is more silent. }
  \item{location}{ Numeric: \code{location} is currently not used in R. }
  \item{manager}{ Logical: \code{manager} is currently not used in R. }

  \item{sh.lib.name}{ See \code{\link[CoCoCore]{coco.load}}. }
}

\details{ This function is no longer to be called by the user. }

\value{ NULL }

\references{ Badsberg, J.H.: A guide to CoCo, JSS, 2001
             ( http://www.jstatsoft.org/v06/i04/ )
             and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{ Jens Henrik Badsberg }

% \note{ }

% \section{ToDo}{....}

\seealso{ \code{\link{coco.load}}. }

% \examples{ }

\keyword{ interface }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
