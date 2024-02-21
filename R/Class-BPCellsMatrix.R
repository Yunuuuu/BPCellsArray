#' DelayedArray backend of BPCells matrix
#'
#' The `BPCellsMatrix` class just inherits from the
#' [DelayedMatrix][DelayedArray::DelayedMatrix] class.
#'
#' @slot seed A [BPCellsSeed][BPCellsSeed-class] object.
#' @aliases BPCellsMatrix-methods
#' @inherit BPCellsSeed-class seealso
#' @name BPCellsMatrix-class
NULL

#' @param x
#'  - `BPCellsArray` and `BPCellsMatrix`: Details see [BPCellsSeed] for
#'    supported object.
#'  - `matrixClass`: A string.
#' @export
#' @rdname BPCellsMatrix-class
BPCellsArray <- function(x) {
    DelayedArray(BPCellsSeed(x))
}

#' @export
#' @rdname BPCellsMatrix-class
BPCellsMatrix <- BPCellsArray

#' @importClassesFrom DelayedArray DelayedMatrix
#' @export
#' @include Class-BPCellsSeed.R
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsMatrix",
    contains = "DelayedMatrix",
    slots = list(seed = "BPCellsSeed")
)

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsArray",
    contains = "DelayedArray", slots = c(seed = "BPCellsSeed")
)

#' @param seed A `BPCellsSeed` object.
#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "DelayedArray", "BPCellsSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsArray")
)

#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("matrixClass", "BPCellsArray", function(x) {
    "BPCellsMatrix"
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "BPCellsArray", function(x) {
    x <- x@seed
    methods::callGeneric()
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "BPCellsMatrix", function(x) {
    x <- x@seed
    methods::callGeneric()
})

###################################################################
#' @importFrom methods show
#' @export
#' @order 1
#' @rdname BPCellsMatrix-class
methods::setMethod("show", "BPCellsMatrix", function(object) {
    show_bpcells(object@seed, "DelayedMatrix", class(object))
})

#' @importMethodsFrom DelayedArray drop
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("drop", "BPCellsMatrix", drop_internal)

#' @export
methods::setAs("BPCellsMatrix", "dgCMatrix", function(from) {
    methods::as(from@seed, "dgCMatrix")
})

# S3/S4 combo for as.array.BPCellsMatrix
#' @exportS3Method base::as.array
#' @rdname BPCellsMatrix-class
as.array.BPCellsMatrix <- function(x, drop = FALSE) {
    as.array(x@seed, drop = drop)
}

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("as.array", "BPCellsMatrix", as.array.BPCellsMatrix)

#' @exportS3Method base::as.matrix
#' @rdname BPCellsMatrix-class
as.matrix.BPCellsMatrix <- function(x) {
    as.matrix(x@seed)
}

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("as.matrix", "BPCellsMatrix", as.matrix.BPCellsMatrix)

##########################################################
#' @return
#' - `t`: A [BPCellsMatrix] object.
#' @importMethodsFrom BPCells t
#' @export
#' @aliases t
#' @rdname BPCellsMatrix-class
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
