#' Build seed object for `BPCellsMatrix`
#'
#' @param x A `IterableMatrix` object from `BPCells` or a matrix-like object
#' which can be coerced into dgCMatrix object.
#' @return A `IterableMatrix` object.
#' @name BPCellsSeed
#' @seealso [BPCellsSeed][BPCellsSeed-class] contract
NULL

############################################################
#' @export
#' @rdname BPCellsSeed
methods::setGeneric("BPCellsSeed", function(x) {
    standardGeneric("BPCellsSeed")
})

############################################################
#' @include Class-BPCellsSeed.R
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "IterableMatrix", function(x) x)

#' @include Class-BPCellsMatrix.R
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "BPCellsArray", function(x) {
    to_BPCells(x@seed)
})

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "BPCellsMatrix", function(x) {
    to_BPCells(x@seed)
})

#' @include Class-Delayed.R
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "BPCellsDelayedOp", function(x) {
    to_BPCells(x)
})

##############################################################
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "matrix", function(x) {
    mode <- storage_mode(x)
    x <- methods::as(x, "dgCMatrix")
    seed <- methods::callGeneric()
    BPCells::convert_matrix_type(matrix = seed, type = mode)
})

############################################################
# Iterable_dgCMatrix_wrapper
summary.Iterable_dgCMatrix_wrapper <- function(object) {
    "Load dgCMatrix from memory"
}

methods::setMethod(
    "summary", "Iterable_dgCMatrix_wrapper",
    summary.Iterable_dgCMatrix_wrapper
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "dgCMatrix", function(x) {
    methods::as(x, "IterableMatrix")
})

#############################################################
#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "ANY", function(x) {
    x <- coerce_dgCMatrix(x)
    methods::callGeneric()
})

# helper function to coerce object into `dgCMatrix` object, which can be used by
# `BPCells`
coerce_dgCMatrix <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    tryCatch(
        methods::as(x, "dgCMatrix"),
        error = function(cnd) {
            cli::cli_abort(
                "{.arg {arg}} must be a matrix-like object which can be coerced into {.cls dgCMatrix}",
                call = call
            )
        }
    )
}

#############################################################
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# to_DelayedArray
# Function used to translate `BPCells` class into `BPCellsDelayed` class
#' @keywords internal
#' @noRd
methods::setGeneric("to_DelayedArray", signature = "object", function(object) {
    standardGeneric("to_DelayedArray")
})

# only used by on-disk and on-memory BPCells Matrix
methods::setMethod("to_DelayedArray", "IterableMatrix", function(object) object)

# used by c(
#    "BPCellsConvert", "BPCellsRankTransform",
#    "BPCellsRenameDims", "BPCellsSubset", "BPCellsTransformed"
# )
to_DelayedUnaryOp <- function(object, Class) {
    object <- migrate_slots(
        Object = object,
        rename = c(matrix = "seed"), Class = Class
    )
    object@seed <- to_DelayedArray(object@seed)
    object
}
