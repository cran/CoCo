\name{Fuchs82}
\alias{Fuchs82}
\docType{data}
\title{ Fuchs, C. (1982) }

\description{ Data used in "Maximum likelihood estimation and
         model selection in contingencytables with missing data".
}

\usage{data(Fuchs82)}

\format{
  The format is:
 num [1:2, 1:2, 1:2, 1:2, 1:3, 1:3] 0 2 5 3 0 0 2 1 1 0 ...
 - attr(*, "dimnames")=List of 6
  ..\$ g: chr [1:2] "1" "2"
  ..\$ a: chr [1:2] "1" "2"
  ..\$ s: chr [1:2] "1" "2"
  ..\$ d: chr [1:2] "1" "2"
  ..\$ p: chr [1:3] "1" "2" "3"
  ..\$ m: chr [1:3] "1" "2" "3"
}

\details{
  \itemize{
    \item{D, Survival status  	}{: survived, deceased,		}
    \item{G, Group membership	}{: experiment, control,	}
    \item{S, Sex		}{: male, female,		}
    \item{A, Age		}{: less than 75, over 75,	}
    \item{P, Physical status	}{: poor, good,			}
    \item{M, Mental status	}{: poor, good,			}
  }
}

\source{ Fuchs, C. (1982).    Maximum likelihood estimation and
         model selection in contingencytables with missing data.
         J. Amer. Statist Soc. 77, 270-278. }

% \references{ }

\examples{
data(Fuchs82);
ftable(Fuchs82, col.vars=4:6);

CoCoObject <- makeCoCo();
enterTable(Fuchs82, object = CoCoObject);
eh(object = CoCoObject);
eh(sub.class = "hierarchical", object = CoCoObject);
endCoCo(object = CoCoObject);
}

\keyword{datasets}
