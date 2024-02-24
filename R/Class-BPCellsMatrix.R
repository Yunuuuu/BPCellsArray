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
BPCellsArray <- function(x) DelayedArray(x)
BPCellsMatrix <- BPCellsArray

#' @export
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsArray", contains = "DelayedArray")

#' @export
#' @include Class-BPCellsSeed.R
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsMatrix", contains = "DelayedMatrix")

#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("matrixClass", "BPCellsArray", function(x) {
    "BPCellsMatrix"
})

methods::setValidity("BPCellsArray", validate_BPCellsDelayed)
methods::setValidity("BPCellsMatrix", validate_BPCellsDelayed)

#' Since BPCells only support 2-dim matrix, `new_BPCellsArray` will always
#' return a `BPCellsMatrix` object.
#' @importFrom DelayedArray new_DelayedArray
#' @noRd
new_BPCellsArray <- function(seed) {
    new_DelayedArray(seed, Class = "BPCellsArray")
}

#' @param seed A `IterableMatrix` or `BPCellsDelayedOp` object.
#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("DelayedArray", "IterableMatrix", new_BPCellsArray)
methods::setMethod("DelayedArray", "BPCellsDelayedOp", new_BPCellsArray)

########################################################
# hepler function to set method for `BPCellsArray`
set_BPCellsArray_method <- function(..., method = NULL, before = NULL, after = NULL, Arrays = "object") {
    body <- lapply(rlang::syms(Arrays), function(Array) {
        substitute(nm <- to_BPCells(nm@seed), list(nm = Array))
    })
    new_method(rlang::pairlist2(...),
        body = body, method = method,
        before = before, after = after
    )
}

###################################################################
#' @importFrom methods show
#' @export
#' @order 1
#' @rdname BPCellsMatrix-class
methods::setMethod("show", "BPCellsArray", function(object) {
    show_bpcells(object@seed, "DelayedMatrix", class(object))
})

###########################################################
#' @export
methods::setAs("BPCellsMatrix", "dgCMatrix", set_BPCellsArray_method(from = ))

# Default drop use `as.array` and `aperm` methods
#' @importMethodsFrom DelayedArray drop
#' @noRd
NULL

# we prevent aperm to degrade into `DelayedArray` Class
### S3/S4 combo for aperm.BPCellsMatrix
aperm.BPCellsMatrix <- call_DelayedArray_method(
    a = , perm = , ... = , type = "S3"
)

#' @importFrom BiocGenerics aperm
methods::setMethod(
    "aperm", "BPCellsMatrix",
    call_DelayedArray_method(a = , perm = , ... = )
)

# S3/S4 combo for as.array.BPCellsMatrix
#' @exportS3Method base::as.array
#' @rdname BPCellsMatrix-class
as.array.BPCellsMatrix <- function(x, drop = FALSE) {
    assert_bool(drop)
    mat <- as.matrix(x)
    if (drop) drop(mat) else mat
}

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("as.array", "BPCellsMatrix", as.array.BPCellsMatrix)

#' @exportS3Method base::as.matrix
#' @rdname BPCellsMatrix-class
as.matrix.BPCellsMatrix <- function(x) {
    as_matrix_IterableMatrix(to_BPCells(x@seed))
}

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("as.matrix", "BPCellsMatrix", as.matrix.BPCellsMatrix)

##########################################################
### Seed contract
###
# https://github.com/Bioconductor/DelayedArray/blob/devel/R/DelayedOp-class.R
# for `dim`, `dimnames`, `extract_array` and `is_sparse` just use the methods
# from `DelayedArray`
#' For BPCellsMatrix object
#' @importMethodsFrom DelayedArray dim
#' @importMethodsFrom DelayedArray dimnames
#' @importMethodsFrom DelayedArray extract_array
#' @importMethodsFrom DelayedArray is_sparse
#' @importMethodsFrom DelayedArray OLD_extract_sparse_array
#' @noRd
NULL

#######################################################
#' @return
#' - `t`: A [BPCellsMatrix] object.
#' @importMethodsFrom BPCells t
#' @export
#' @aliases t
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "t", "BPCellsMatrix",
    set_BPCellsArray_method(
        x = , after = expression(DelayedArray(object)),
        Arrays = "x"
    )
)

methods::setMethod(
    "Ops", c("BPCellsArray", "vector"),
    call_DelayedArray_method(e1 = , e2 = )
)

methods::setMethod(
    "Ops", c("vector", "BPCellsArray"),
    call_DelayedArray_method(e1 = , e2 = )
)

methods::setMethod(
    "Ops", c("BPCellsArray", "BPCellsArray"),
    call_DelayedArray_method(e1 = , e2 = )
)
