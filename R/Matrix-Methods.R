#' Basic operations for `BPCellsMatrix` object
#'
#' @param x,object A `BPCellsMatrix` object.
#' @inheritParams BPCellsMatrix-methods
#' @inherit BPCellsSeed-methods seealso
#' @name BPCellsMatrix-methods
NULL

#' @importFrom methods show
#' @export
#' @order 1
#' @rdname BPCellsMatrix-methods
methods::setMethod("show", "BPCellsMatrix", function(object) {
    show_bpcells(object@seed, "DelayedMatrix", class(object))
})

#' @importMethodsFrom DelayedArray drop
#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod("drop", "BPCellsMatrix", drop_internal)

#' @export
methods::setAs("BPCellsMatrix", "dgCMatrix", function(from) {
    methods::as(from@seed, "dgCMatrix")
})

# S3/S4 combo for as.array.BPCellsMatrix
#' @exportS3Method base::as.array
#' @rdname BPCellsMatrix-methods
as.array.BPCellsMatrix <- function(x, drop = FALSE) {
    as.array(x@seed, drop = drop)
}

#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod("as.array", "BPCellsMatrix", as.array.BPCellsMatrix)

#' @exportS3Method base::as.matrix
#' @rdname BPCellsMatrix-methods
as.matrix.BPCellsMatrix <- function(x) {
    as.matrix(x@seed)
}

#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod("as.matrix", "BPCellsMatrix", as.matrix.BPCellsMatrix)

##########################################################
#' @return
#' - `t`: A [BPCellsMatrix] object.
#' @importMethodsFrom BPCells t
#' @export
#' @aliases t
#' @rdname BPCellsMatrix-methods
methods::setMethod("t", "BPCellsMatrix", function(x) {
    DelayedArray(t(x@seed))
})

# https://github.com/Bioconductor/DelayedArray/blob/devel/R/DelayedOp-class.R
# for `dim`, `dimnames`, `extract_array` and `is_sparse` just use the methods
# from `DelayedArray`, All BPCells objects have been regarded a seed of
# `BPCellsMatrix`.
#' For BPCellsMatrix object
#' @importMethodsFrom DelayedArray dim
#' @importMethodsFrom DelayedArray dimnames
#' @importMethodsFrom DelayedArray extract_array
#' @importMethodsFrom DelayedArray is_sparse
#' @importMethodsFrom DelayedArray OLD_extract_sparse_array
#' @noRd
NULL
