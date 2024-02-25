% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Method-pmin.R
\name{pmin2}
\alias{pmin2}
\alias{pmin_by_col}
\alias{pmin_by_col,BPCellsMatrix-method}
\alias{pmin_by_row}
\alias{pmin_by_row,BPCellsMatrix-method}
\alias{pmin_scalar}
\alias{pmin_scalar,BPCellsMatrix-method}
\alias{pmin2,BPCellsMatrix,vector-method}
\alias{pmin2,vector,BPCellsMatrix-method}
\alias{pmin2,BPCellsMatrix,DelayedArray-method}
\alias{pmin2,DelayedArray,BPCellsMatrix-method}
\alias{pmax2,BPCellsMatrix,vector-method}
\alias{pmax2}
\alias{pmax2,vector,BPCellsMatrix-method}
\alias{pmax2,BPCellsMatrix,DelayedArray-method}
\alias{pmax2,DelayedArray,BPCellsMatrix-method}
\title{Maxima and Minima}
\usage{
pmin_by_col(object, values)

\S4method{pmin_by_col}{BPCellsMatrix}(object, values)

pmin_by_row(object, values)

\S4method{pmin_by_row}{BPCellsMatrix}(object, values)

pmin_scalar(object, value)

\S4method{pmin_scalar}{BPCellsMatrix}(object, value)

\S4method{pmin2}{BPCellsMatrix,vector}(e1, e2)

\S4method{pmin2}{vector,BPCellsMatrix}(e1, e2)

\S4method{pmin2}{BPCellsMatrix,DelayedArray}(e1, e2)

\S4method{pmin2}{DelayedArray,BPCellsMatrix}(e1, e2)

\S4method{pmax2}{BPCellsMatrix,vector}(e1, e2)

\S4method{pmax2}{vector,BPCellsMatrix}(e1, e2)

\S4method{pmax2}{BPCellsMatrix,DelayedArray}(e1, e2)

\S4method{pmax2}{DelayedArray,BPCellsMatrix}(e1, e2)
}
\arguments{
\item{values}{A positive atomic numeric.}
}
\value{
\itemize{
\item \code{pmin_by_col}: Take the minimum with a per-col constant
}

\itemize{
\item \code{pmin_by_row}: Take the minimum with a per-row constant
}

\itemize{
\item \code{pmin_scalar}: Take minumum with a global constant
}
}
\description{
Maxima and Minima
}