% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class-BPCellsMatrix.R, R/Method-RenameDims.R,
%   R/Method-Subassign.R, R/Method-Subset.R
\docType{class}
\name{show,BPCellsArray-method}
\alias{show,BPCellsArray-method}
\alias{show,BPCellsMatrix-method}
\alias{BPCellsMatrix-class}
\alias{BPCellsMatrix-methods}
\alias{BPCellsArray}
\alias{BPCellsMatrix}
\alias{BPCellsArray-class}
\alias{matrixClass,BPCellsArray-method}
\alias{DelayedArray,IterableMatrix-method}
\alias{aperm.BPCellsMatrix}
\alias{aperm,BPCellsMatrix-method}
\alias{as.matrix.BPCellsMatrix}
\alias{as.matrix,BPCellsMatrix-method}
\alias{as.array.BPCellsMatrix}
\alias{as.array,BPCellsMatrix-method}
\alias{t,BPCellsMatrix-method}
\alias{t}
\alias{type<-,BPCellsMatrix-method}
\alias{is.na,BPCellsMatrix-method}
\alias{is.finite,BPCellsMatrix-method}
\alias{is.infinite,BPCellsMatrix-method}
\alias{is.nan,BPCellsMatrix-method}
\alias{Ops,BPCellsArray,vector-method}
\alias{Ops,vector,BPCellsArray-method}
\alias{Ops,BPCellsArray,BPCellsArray-method}
\alias{dimnames<-,BPCellsMatrix,ListOrNULL-method}
\alias{dimnames<-}
\alias{rownames<-,BPCellsMatrix-method}
\alias{rownames<-}
\alias{colnames<-,BPCellsMatrix-method}
\alias{colnames<-}
\alias{[<-,BPCellsMatrix,ANY,ANY,BPCellsMatrix-method}
\alias{[<-,BPCellsMatrix,ANY,ANY,IterableMatrix-method}
\alias{[<-,BPCellsMatrix,ANY,ANY,ANY-method}
\alias{[,BPCellsMatrix,ANY,ANY,ANY-method}
\title{DelayedArray backend of BPCells matrix}
\usage{
\S4method{show}{BPCellsArray}(object)

\S4method{show}{BPCellsMatrix}(object)

BPCellsArray(x)

BPCellsMatrix(x)

\S4method{matrixClass}{BPCellsArray}(x)

\S4method{DelayedArray}{IterableMatrix}(seed)

\method{aperm}{BPCellsMatrix}(a, perm, ...)

\S4method{aperm}{BPCellsMatrix}(a, perm, ...)

\method{as.matrix}{BPCellsMatrix}(x)

\S4method{as.matrix}{BPCellsMatrix}(x)

\method{as.array}{BPCellsMatrix}(x, drop = FALSE)

\S4method{as.array}{BPCellsMatrix}(x, drop = FALSE)

\S4method{t}{BPCellsMatrix}(x)

\S4method{type}{BPCellsMatrix}(x) <- value

\S4method{is.na}{BPCellsMatrix}(x)

\S4method{is.finite}{BPCellsMatrix}(x)

\S4method{is.infinite}{BPCellsMatrix}(x)

\S4method{is.nan}{BPCellsMatrix}(x)

\S4method{Ops}{BPCellsArray,vector}(e1, e2)

\S4method{Ops}{vector,BPCellsArray}(e1, e2)

\S4method{Ops}{BPCellsArray,BPCellsArray}(e1, e2)

\S4method{dimnames}{BPCellsMatrix,ListOrNULL}(x) <- value

\S4method{rownames}{BPCellsMatrix}(x) <- value

\S4method{colnames}{BPCellsMatrix}(x) <- value

\S4method{[}{BPCellsMatrix,ANY,ANY,BPCellsMatrix}(x, i, j, ...) <- value

\S4method{[}{BPCellsMatrix,ANY,ANY,IterableMatrix}(x, i, j, ...) <- value

\S4method{[}{BPCellsMatrix,ANY,ANY,ANY}(x, i, j) <- value

\S4method{[}{BPCellsMatrix,ANY,ANY,ANY}(x, i, j, drop = TRUE)
}
\arguments{
\item{object}{A \link[=BPCellsMatrix-class]{BPCellsMatrix} object.}

\item{x}{A \link[=BPCellsMatrix-class]{BPCellsMatrix} object. For \code{BPCellsArray} and \code{BPCellsMatrix}
function, a matrix-like object which can be coerced into \link[Matrix:dgCMatrix-class]{dgCMatrix} object would also be okay.}

\item{seed}{A \link[=BPCellsSeed-class]{IterableMatrix} object.}

\item{a}{A \link[=BPCellsMatrix-class]{BPCellsMatrix} object.}

\item{perm}{the subscript permutation vector, usually a permutation of
    the integers \code{1:n}, where \code{n} is the number of dimensions
    of \code{a}.  When \code{a} has named dimnames, it can be a
    character vector of length \code{n} giving a permutation of those
    names. The default (used whenever \code{perm} has zero length) is to
    reverse the order of the dimensions.}

\item{...}{Additional arguments passed to specific methods
\itemize{
\item \code{aperm}: not used currently.
\item \verb{[<-}: arguments passed to
\link[BPCells:transpose_storage_order]{transpose_storage_order}
}}

\item{drop}{A bool, if \code{TRUE}, any extents of length one will be removed and
return an atomic vector.}

\item{value}{\itemize{
\item \verb{type<-}: See the mode argument in \link{convert_mode}.
\item \verb{dimnames<-}: A list of dimnames or \code{NULL}.
\item \verb{[<-}: A \link[=BPCellsMatrix-class]{BPCellsMatrix} object or a matrix-like object which can be coerced into \link[Matrix:dgCMatrix-class]{dgCMatrix} object
}}

\item{e1, e2}{One of \code{e1} or \code{e2} must be a \link[=BPCellsMatrix-class]{BPCellsMatrix} object.}

\item{i, j}{Row and Column index.}
}
\value{
\itemize{
\item \code{BPCellsArray} and \code{BPCellsMatrix}: A \link[=BPCellsMatrix-class]{BPCellsMatrix} object, since \code{BPCells} can
only support 2-dim array.
}

\itemize{
\item \code{matrixClass}: A string, always be \code{"BPCellsMatrix"}.
}

\itemize{
\item \code{as.matrix}: A dense matrix.
}

\itemize{
\item \code{as.array}: A dense matrix or an atomic vector.
}

\itemize{
\item \code{t}: A \link[=BPCellsMatrix-class]{BPCellsMatrix} object
}

\itemize{
\item \verb{type<-}: A \link[=BPCellsMatrix-class]{BPCellsMatrix} object with storage mode converted into the specified.
}

\itemize{
\item \verb{dimnames<-}: A \code{BPCellsMatrix} object.
}

\itemize{
\item \verb{rownames<-}: A \code{BPCellsMatrix} object.
}

\itemize{
\item \verb{colnames<-}: A \code{BPCellsMatrix} object.
}

\itemize{
\item \verb{[<-}: A \code{BPCellsMatrix} object.
}

\itemize{
\item \code{[}: A \code{BPCellsMatrix} object or an atomic vector.
}
}
\description{
The \code{BPCellsMatrix} class just inherits from the
\link[DelayedArray:DelayedArray-class]{DelayedMatrix} class.
}
\seealso{
\itemize{
\item \link[=BPCells-bind]{bind}: Combine two Objects by Columns or Rows.
\item \link[=BPCells-Multiplication]{\%*\%}: Matrix Multiplication.
\item \link[=BPCells-crossprod]{crossprod}: Matrix Crossproduct.
\item \link[=BPCells-Summarization]{summarization}: row/col summarization.
\item \link[=BPCells-Arithmetic]{Arith}: Binary Arithmetic operators.
\item \link[=BPCells-Math]{Math}: Math operators.
\item \link[=BPCells-Compare]{Compare}: Compare matrix.
\item \link[=pmin2]{pmin2/pmax2}: Maxima and Minima.
\item \link{DelayedArray-utils}: Common operations on DelayedArray objects
}
}
