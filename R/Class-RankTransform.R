#' Delayed BPCells MatrixRankTransform
#'
#' The `BPCellsRankTransformArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `MatrixRankTransform`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [rank_transform] function to create a `BPCellsRankTransformMatrix`.
#'
#' @param x For Specific functions:
#' - `BPCellsMatrixRankTransformArray`: A `MatrixRankTransform` object.
#' - `matrixClass`: A `BPCellsMatrixRankTransformArray` object.
#' @seealso
#' - [BPCellsSeed]
#' - [convert_matrix_type][BPCells::convert_matrix_type]
#' @name BPCellsRankTransform
NULL
methods::setClass("BPCellsRankTransformSeed",
    contains = c("BPCellsSeed", get_class("MatrixRankTransform"))
)

#' @param x A `MatrixRankTransform` object.
#' @rdname BPCellsRankTransform
#' @noRd
BPCellsRankTransformSeed <- function(x) {
    assert_s4_class(x, "MatrixRankTransform")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsRankTransformSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsRankTransform
methods::setClass("BPCellsRankTransformArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsRankTransformSeed")
)

#' @param seed A `BPCellsRankTransformSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsRankTransform
methods::setMethod(
    "DelayedArray", "BPCellsRankTransformSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsRankTransformArray")
)

#' @export
#' @rdname BPCellsRankTransform
BPCellsRankTransformArray <- function(x) {
    DelayedArray(BPCellsRankTransformSeed(x))
}

#' @importClassesFrom DelayedArray DelayedMatrix
#' @export
#' @rdname BPCellsRankTransform
methods::setClass("BPCellsRankTransformMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsRankTransformSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsRankTransform
methods::setMethod("matrixClass", "BPCellsRankTransformArray", function(x) {
    "BPCellsRankTransformMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################

#' @param object A `BPCellsRankTransformSeed` object.
#' @importMethodsFrom DelayedArray path
#' @rdname BPCellsRankTransform
#' @noRd
methods::setMethod("path", "BPCellsRankTransformSeed", function(object) {
    path(object@matrix)
})

#' @param ... Ignored, Not used curretly.
#' @inheritParams BPCellsMatrix-Class
#' @importMethodsFrom BPCells [
#' @rdname internal-methods
methods::setMethod(
    "[", "BPCellsRankTransformSeed",
    function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#####################   BPCellsRankTransformMatrix   #######################
#' Rank-transform a BPCells IterableMatrix matrix
#'
#' @param object A `BPCellsSeed` or `BPCellsMatrix` object.
#' @inheritDotParams BPCells::transpose_storage_order -matrix
#' @export
#' @name rank_transform
methods::setGeneric(
    "rank_transform",
    function(object, ...) standardGeneric("rank_transform")
)

#' @param axis Axis to rank values within. "col" to rank values within each
#'     column, and "row" to rank values within each row. If `NULL`, will use the
#'     storage order of `object` (see [storage_order][BPCells::storage_order]).
#'     If axis value is different from the storage order of `object`,
#'     [transpose_storage_order][BPCells::transpose_storage_order] will be used
#'     to transpose the underlying storage order.
#' @return A [BPCellsConverSeed][BPCellsConvert] object or
#' [BPCellsConvertMatrix][BPCellsConvert] object.
#' @seealso [rank_transform][BPCells::rank_transform]
#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname rank_transform
methods::setMethod(
    "rank_transform", "BPCellsSeed",
    function(object, axis = NULL, ...) {
        matrix_order <- BPCells::storage_order(object)
        if (is.null(axis)) {
            axis <- matrix_order
            cli::cli_inform("Doing {.val {axis}} rank transformation")
        } else {
            axis <- match.arg(axis, c("row", "col"))
            if (axis != matrix_order) {
                cli::cli_warn(
                    "{.arg axis} is different from the storage order ({.val {matrix_order}}), transposing storage order for {.arg object}" # nolint
                )
                object <- BPCells::transpose_storage_order(matrix = object, ...)
            }
        }
        BPCells:::rank_transform(mat = object, axis = axis)
    }
)

#' @export
#' @rdname rank_transform
methods::setMethod(
    "rank_transform", "BPCellsMatrix",
    function(object, axis = NULL, ...) {
        DelayedArray(rank_transform(object@seed, axis = axis, ...))
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod("rank_transform", "ANY", function(object, axis) {
    cli::cli_abort(
        "{.arg object} must be a {.cls BPCellsSeed} or {.cls BPCellsMatrix} object"
    )
})
