\name{sinkCoCo}
\alias{sinkCoCo}

\title{ Direct output to a file }

\description{ Diverts CoCo output to a file. }

\usage{
sinkCoCo(file.name = "Diary.tmp", type = "diary", object = .current.coco)
}

\arguments{
  \item{file.name}{ A character string naming the file to write to.
     To return the file name set the \code{file.name} to \code{"what"}. }
  \item{type}{ Textstring with \code{"output"}, \code{"log"}, \code{"diary"},
  \code{"report"}, or \code{"dump"}. 
 }
  \item{object}{ See \code{\link{exportCoCo}}. }
}

\details{ The output from CoCo to standard output consists of \code{"output"}
	  form the procedures and echo, \code{"log"}, from the parser.
	  Both parts can be copied to the \code{"diary"}.
	  Number of cycles in iterative algorithmes and simular
	  are reported in \code{"report"}.
	  Some output, from e.g. \code{\link{returnTable}},
	  can be written to the \code{"dump"} file.
}

\value{ The text string with the file name. }

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

% \note{  }

\seealso{ \code{\link{showOptions}} and \code{\link{optionsCoCo}}. }

% \examples{ }

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
