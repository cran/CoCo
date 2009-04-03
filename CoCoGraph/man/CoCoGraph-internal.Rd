\name{CoCoGraph-internal}
\title{Internal CoCoGraph objects}

\alias{.First.lib}
\alias{.First.lib.CoCoDynamicGraph}
\alias{.onAttach}
\alias{.onLoad}
\alias{.packageName}

% \alias{CoCoModelClass-class}

% \alias{.--C--CoCoModelClass}

% \alias{.--M--dynamic.Graph:.GlobalEnv}
% \alias{.--M--label:.GlobalEnv}
% \alias{.--M--modifyModel:.GlobalEnv}
% \alias{.--M--subModifyModel:.GlobalEnv}
% \alias{.--M--testEdge:.GlobalEnv}
% \alias{.--M--width:.GlobalEnv}

\alias{dg,numeric-method}
\alias{dg,character-method}

\alias{dg,CoCoModelClass-method}
\alias{modifyModel,CoCoModelClass-method}
\alias{subModifyModel,CoCoModelClass-method}
\alias{testEdge,CoCoModelClass-method}

\alias{dg} % CoCoModelClass-method
\alias{modifyModel} % CoCoModelClass-method
\alias{subModifyModel} % CoCoModelClass-method
\alias{testEdge} % CoCoModelClass-method

% \alias{vertexEdges,CoCoModelClass-method}

\alias{graphComponents,CoCoModelClass-method}
\alias{setGraphComponents,CoCoModelClass-method}

\alias{graphEdges,CoCoModelClass-method}
\alias{setGraphEdges,CoCoModelClass-method}

% \alias{vertexEdges} % {CoCoModelClass}

% \alias{graphComponents} % {CoCoModelClass}
% \alias{setGraphComponents} % {CoCoModelClass}

% \alias{graphEdges} % {CoCoModelClass}
% \alias{setGraphEdges} % {CoCoModelClass}


\description{Internal CoCoGraph objects.}

\details{These are not to be called by the user.
         The functions for CoCo are found in the
	 package "CoCoRaw" (and "CoCoObjects"). 
         Datasets are found in the package "CoCo" and "CoCoCg".
}

\note{
For help on
\code{\link[dynamicGraph]{dg}},
\code{\link[dynamicGraph]{modifyModel}}, and
\code{\link[dynamicGraph]{testEdge}} use
\code{help(dg, package = "dynamicGraph")},
\code{help(modifyModel, package = "dynamicGraph")}, and
\code{help(testEdge, package = "dynamicGraph")}.
}

\keyword{internal}
