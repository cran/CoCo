\name{Reinis}
\alias{Reinis}
\docType{data}

\title{ Reinis: Coronary heart disease }

\description{
  Prognostic significance of the risk profile
  in the prevention of coronary heart disease.
}

\usage{data(Reinis)}

\format{
  The format is:
 num [1:2, 1:2, 1:2, 1:2, 1:2, 1:2] 44 40 112 67 129 145 12 23 35 12 ...
 - attr(*, "dimnames")=List of 6
  ..\$ A: chr [1:2] "1" "2"
  ..\$ B: chr [1:2] "1" "2"
  ..\$ D: chr [1:2] "1" "2"
  ..\$ E: chr [1:2] "1" "2"
  ..\$ F: chr [1:2] "1" "2"
  ..\$ G: chr [1:2] "1" "2"
}

\details{
  \itemize{
    \item{A}{: Smoking, }
    \item{B}{: strenuous mental work,  }
    \item{C}{: strenuous physical work, }
    \item{D}{: systolic blood pressure, }
    \item{E}{: ratio of `alpha' to `beta' lipoproteins, }
    \item{F}{: family anamnesis of coronary heart disease. }
  }
}

\source{
         Edwards, D. and Havranek, T. (1987). A fast model
	 selection procedure for large families of models.
	 J. Amer. Stat. Assoc. 82: 205-231. 
}

\references{
         Reinis, Z., Pokorny, J., Basika, V., Tiserova, J., Gorican,
         K.,  Horakova,  D.,  Stuchlikova,  E.,  Havranek,  T.  and
         Hrabovsky, F. (1981): Prognosticky vyznam rizikoveho 
         profilu v prevenci ischemicke choroby srdce. 
         Bratis. lek. Listy. 76, 137-50. 
         (Prognostic significance of the risk profile in the
         prevention of coronary heart disease)
}

\examples{
library(CoCo);
data(Reinis);
ftable(Reinis, col.vars=4:6);

CoCoObject <- makeCoCo();
enterTable(Reinis, object = CoCoObject);
eh(object = CoCoObject);
eh(sub.class = "hierarchical", object = CoCoObject);
endCoCo(object = CoCoObject);
}

\keyword{datasets}
