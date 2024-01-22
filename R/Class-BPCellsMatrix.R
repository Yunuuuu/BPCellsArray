#' Base Class for Delayed BPCells matrix
#'
#' The `BPCellsMatrix` class just inherits from the
#' [DelayedMatrix][DelayedArray::DelayedMatrix] object. The purpose for
#' `BPCellsMatrix` object is to provide the common methods for all Delayed
#' BPCells matrix.
#'
#' @slot seed A [BPCellsSeed][BPCellsSeed-class] object.
#' @param value
#'  - `dimnames<-`: A list of dimnames or `NULL`.
#'  - `[<-`: A matrix which can be coerced into
#'     [dgCMatrix][Matrix::dgCMatrix-class].
#'  - `pmin_scalar`: Single positive numeric value.
#' @param ... Not used currently.
#' @seealso
#' - [bind][BPCells-bind]: Combine two Objects by Columns or Rows.
#' - [%*%][BPCells-Multiplication]: Matrix Multiplication.
#' - [crossprod][BPCells-crossprod]: Matrix Crossproduct.
#' - [Summarize][BPCells-Summarization]: row/col summarization.
#' - [binarize][BPCells-binarize]: Convert matrix elements to zeros and ones.
#' @importClassesFrom DelayedArray DelayedMatrix
#' @export
#' @include Class-BPCellsSeed.R
methods::setClass("BPCellsMatrix",
    contains = "DelayedMatrix",
    slots = list(seed = "BPCellsSeed")
)

#' @param x,object A `BPCellsMatrix` object.
#' @importMethodsFrom methods show
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("show", "BPCellsMatrix", function(object) {
    show_bpcells(object@seed, "DelayedMatrix", class(object))
})

#' @return
#' - `t`: A [BPCellsMatrix] object.
#' @importMethodsFrom DelayedArray t
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("t", "BPCellsMatrix", function(x) {
    DelayedArray(t(x@seed))
})

#' @param i,j Row and Column index.
#' @param drop Ignored, always be `FALSE`.
#' @return
#' - `[`: A [BPCellsMatrix] object.
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "[", c("BPCellsMatrix", "ANY", "ANY"),
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[i, j, ...])
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "missing", "ANY"),
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[, j, ...])
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "ANY", "missing"),
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[i, , ...])
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "missing", "missing"),
    function(x, i, j, ..., drop = FALSE) x
)

#' @return
#' - `[<-`: A [BPCellsMatrix] object.
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "[<-", "BPCellsMatrix",
    function(x, i, j, ..., value) {
        seed <- x@seed
        seed[i, j, ...] <- value
        DelayedArray(seed)
    }
)

methods::setClassUnion("ListOrNULL", c("list", "NULL"))

# for `dim`, `dimnames`, `extract_array` and `is_sparse` just use the methods
# from DelayedArray
#' @return
#' - `dimnames<-`: A [BPCellsRenameDimsMatrix][BPCellsRenameDims] object.
#' @importMethodsFrom DelayedArray dimnames<-
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "dimnames<-",
    c(x = "BPCellsMatrix", value = "ListOrNULL"), function(x, value) {
        seed <- x@seed
        dimnames(seed) <- value
        DelayedArray(seed)
    }
)

#################### Matrix Statistics ########################
#' BPCellsMatrix row/col summarization
#'
#' @importMethodsFrom DelayedArray rowSums
#' @param x A [BPCellsMatrix][BPCellsMatrix-class] object.
#' @return
#' - `rowSums()`: vector of row sums
#' @aliases rowSums
#' @name BPCells-Summarization
NULL

#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowSums", c(x = "BPCellsMatrix"), function(x) {
    rowSums(x@seed)
})

#' @importMethodsFrom DelayedArray colSums
#' @return
#' - `colSums()`: vector of col sums
#' @aliases colSums
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colSums", c(x = "BPCellsMatrix"), function(x) {
    colSums(x@seed)
})

#' @importMethodsFrom DelayedArray rowMeans
#' @return
#' - `rowMeans()`: vector of row means
#' @aliases rowMeans
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowMeans", c(x = "BPCellsMatrix"), function(x) {
    rowMeans(x@seed)
})

#' @importMethodsFrom DelayedArray colMeans
#' @return
#' - `colMeans()`: vector of col means
#' @aliases colMeans
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colMeans", c(x = "BPCellsMatrix"), function(x) {
    colMeans(x@seed)
})

#' @importMethodsFrom DelayedArray rowVars
#' @return
#' - `rowVars()`: vector of row vars
#' @aliases rowVars
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("rowVars", c(x = "BPCellsMatrix"), function(x) {
    stats <- BPCells::matrix_stats(x@seed,
        row_stats = "variance", col_stats = "none"
    )
    stats$row_stats["variance", , drop = TRUE]
})

#' @importMethodsFrom DelayedArray colVars
#' @return
#' - `colVars()`: vector of col vars
#' @aliases colVars
#' @export
#' @rdname BPCells-Summarization
methods::setMethod("colVars", c(x = "BPCellsMatrix"), function(x) {
    stats <- BPCells::matrix_stats(x@seed,
        row_stats = "none", col_stats = "variance"
    )
    stats$col_stats["variance", , drop = TRUE]
})
