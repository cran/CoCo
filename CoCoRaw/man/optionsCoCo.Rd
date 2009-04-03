\name{optionsCoCo}
\alias{optionsCoCo}

\title{ Options settings for CoCo objects }
\description{
     Allow the user to set and examine a variety of "options" for each CoCo
     object which affect the way in which CoCo and CoCoCg computes and
     displays its results. 
}

\usage{
optionsCoCo(..., section = "all", status = FALSE, force.files = FALSE,
            data = NULL, object = .object.of.thing(data = data))
}

\arguments{
  \item{\dots}{ Any options can be defined, using 'name = value',
		similar to \code{\link{options}}.
                Further, 'options('name') == options()['name']', see the
		example.
                Those from the section \code{"cg"} are only avaliable in CoCoCg.
		}
  \item{section}{ The character string \code{"formats"}, \code{"tests"}, 
                  \code{"ips"}, \code{"cg"}, \code{"em"},
                  \code{"exact"}, \code{"other"}, or \code{"files"}. }
  \item{status}{ Logical, if \code{status} is TRUE, then the options
		 values are also print on standard output.  }
  \item{force.files}{ Logical. If \code{section} is \code{"all"} (default),
		      then the options for files (names, etc.) are only
		      returned if \code{force.files} is set to TRUE.
		      ( You do not want to share file names between 
			CoCo objects. )
		      }
  \item{data}{ See \code{\link{exportCoCo}}. }
  \item{object}{ See \code{\link{exportCoCo}}. }
}

% \details{ }

\value{A list with the following items: 

  \item{width                        }{int 127}
  \item{height                       }{int 65}
  \item{... }{... }
  \item{warnings                     }{logi TRUE}

}

\section{Formats}{ 
	\itemize{
  \item{width                        }{int 127}
  \item{height                       }{int 65}
  \item{digits.table                 }{int 10}
  \item{decimals.table.probabilities }{int 6}
  \item{decimals.table.expected      }{int 2}
  \item{decimals.table.residual      }{int 4}
  \item{digits.test.statistics       }{int 9}
  \item{decimals.test.statistics     }{int 4}
  \item{digits.test.pvalues          }{int 7}
  \item{decimals.test.pvalues        }{int 5}
  \item{digits                       }{int 10}
  \item{decimals                     }{int 4}
  \item{pausing.of.output            }{logi FALSE}
  \item{n.lines                      }{int 22}
  \item{short.test.output            }{logi FALSE}
}
}

\section{Tests}{ 
	\itemize{
  \item{algorithm                    }{chr "c": "a", "b" or "c"}
  \item{partitioning                 }{logi TRUE}
  \item{adjusted.df                  }{logi TRUE}
  \item{power.lambda                 }{num 1}
  \item{ic                           }{logi FALSE}
  \item{bic                          }{logi FALSE}
  \item{ic.kappa                     }{num 3}
  \item{decomposable.mode            }{logi FALSE}
  \item{search.statistic             }{chr "deviance": "deviance", 
						       "power", or "chisq"}
  \item{acceptance.limit             }{num 0.05}
  \item{rejection.limit              }{num 0.05}
  \item{components.limit             }{num 2}
  \item{separators.limit             }{num 2}
  \item{reuse.tests                  }{logi TRUE}
}
}

\section{Ips}{ 
	\itemize{
  \item{ips.criterion                }{chr "cell": "cell" or "sum"}
  \item{ips.algorithm                }{chr "normal": "normal", "arithmetic", 
						     "geometric", or "harmonic"}
  \item{ips.number.of.iterations     }{int 100}
  \item{ips.epsilon                  }{num 1e-07}
}
}

\section{Cips}{ 
	\itemize{
  \item{cips.criterion               }{chr "cell": "cell" or "sum"}
  \item{cips.cycles                  }{int 100}
  \item{cips.epsilon                 }{num 1e-07}
}
}

\section{Cg}{ 
	\itemize{
  \item{mixed.criterion              }{chr "rell": "cell", "sum",
						   "suff", or "rell"}
  \item{mixed.cycles                 }{int 2000}
  \item{mixed.epsilon                }{num 1e-04}
  \item{mixed.init.epsilon           }{num 0.001}
  \item{mixed.log.l.round.error      }{num 1e-05}
  \item{mixed.random.noise           }{num 1e-04}
  \item{mixed.min.lambda             }{num 0.001}
  \item{cholesky.epsilon             }{num 1e-04}
}
}

\section{Em}{ 
	\itemize{
  \item{em                           }{logi FALSE}
  \item{em.number.of.iterations      }{int 100}
  \item{em.epsilon                   }{num 1e-07}
  \item{em.initial                   }{chr "uniform": "uniform", "first", 
						      "last", "mean", 
						      "random", or "input"}
}
}

\section{Exact}{ 
	\itemize{
  \item{exact.test                   }{chr "off": "off", "flop", "on", 
						  "all", or "deviance"}
  \item{exact.epsilon                }{num 1e-07}
  \item{exact.test.joined.test       }{logi TRUE}
  \item{exact.test.components        }{logi TRUE}
  \item{exact.test.unparted.test     }{logi TRUE}
  \item{exact.test.only.for.log.l    }{logi TRUE}
  \item{exact.number.of.tables       }{int 1000}
  \item{exact.list                   }{int [1:7] 20 2 1000 8 100 20 20}
  \item{exact.asymptotic             }{num 2}
  \item{fast                         }{logi TRUE}
  \item{exact.seed                   }{int 0}
}
}

\section{Other}{ 
	\itemize{
  \item{graph.mode                   }{logi FALSE}
  \item{report                       }{logi FALSE}
  \item{trace                        }{logi FALSE}
  \item{debug                        }{logi FALSE}
  \item{timer                        }{logi FALSE}
  \item{echo                         }{logi FALSE}
  \item{note                         }{logi FALSE}
  \item{sorted                       }{logi FALSE}
  \item{large                        }{logi FALSE}
  \item{huge                         }{logi TRUE}
  \item{graphical.search             }{logi TRUE}
  \item{warnings                     }{logi TRUE}
}
}

\section{Files}{ 
	\itemize{
  \item{diary                   }{logi FALSE}
  \item{diary.name              }{chr "/home/jhb/CoCo.diary.001.tmp"}
  \item{keep.diary              }{logi FALSE}
  \item{dump                    }{logi FALSE}
  \item{dump.name               }{chr ""}
  \item{keep.dump               }{logi FALSE}
  \item{report                  }{logi FALSE}
  \item{report.name             }{chr "/home/jhb/CoCo.report.003.tmp"}
  \item{keep.report             }{logi FALSE}
  \item{log                     }{logi TRUE}
  \item{log.name                }{chr "/home/jhb/CoCo.log.002.tmp"}
  \item{keep.log                }{logi FALSE}
  \item{log.data                }{logi TRUE}
}
}

%{}

\references{Badsberg, J.H.: A guide to CoCo, JSS, 2001
            ( http://www.jstatsoft.org/v06/i04/ )
            and Badsberg, J.H.: Xlisp+CoCo, Aalborg University, 1996. }

\author{Jens Henrik Badsberg}

\seealso{ \code{\link{options}} and \code{\link{showOptions}}. }

\examples{
library(CoCoCg);
data(Rats);
CoCoObject <- makeCoCoCg();

old.options <- optionsCoCo();

enterDataFrame(Rats, object = CoCoObject);

endCoCo(object = CoCoObject);
}

\keyword{ methods }% at least one, from doc/KEYWORDS
\keyword{ multivariate }% __ONLY ONE__ keyword per line
