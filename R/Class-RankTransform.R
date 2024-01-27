#' Delayed BPCells MatrixRankTransform
#'
#' The `BPCellsRankTransformArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `MatrixRankTransform`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [rank_transform] function to create a `BPCellsRankTransformMatrix`.
#'
#' @param x A `MatrixRankTransform` object.
#' @seealso
#' - [BPCellsSeed]
#' - [convert_matrix_type][BPCells::convert_matrix_type]
#' @name BPCellsRankTransform
#' @noRd
NULL

methods::setClass("BPCellsRankTransformSeed",
    contains = c(
        "BPCellsUnaryOpsSeed",
        get_class("MatrixRankTransform")
    )
)

#' @param x A `MatrixRankTransform` object.
#' @rdname BPCellsRankTransform
#' @noRd
BPCellsRankTransformSeed <- function(x) {
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsRankTransformSeed")
}

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixRankTransform", function(x) {
    BPCellsRankTransformSeed(x = x)
})

###################################################################
###########################  Methods  #############################
###################################################################

#####################   BPCellsRankTransformMatrix   #######################
#' Rank-transform a BPCells IterableMatrix matrix
#'
#' @param x,object A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object.
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
#'     If `axis` value specified is different from the storage order of
#'     `object`, [transpose_storage_order][BPCells::transpose_storage_order]
#'     will be used to transpose the underlying storage order.
#' @return
#'  - `rank_transform`: A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object depends on the class of `object`.
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
        BPCellsSeed(BPCells:::rank_transform(mat = object, axis = axis))
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

#' @inheritParams rank_transform
#' @export
#' @rdname internal-methods
methods::setMethod("rank_transform", "ANY", function(object, axis) {
    cli::cli_abort(
        "{.arg object} must be a {.cls BPCellsSeed} or {.cls BPCellsMatrix} object"
    )
})

#######################################################################
# Rank
#' @importFrom MatrixGenerics rowRanks
#' @return
#' - `rowRanks()`: vector of row ranks.
#' @aliases rowRanks
#' @export
#' @rdname rank_transform
methods::setMethod("rowRanks", c(x = "BPCellsSeed"), function(x) {
    rank_transform(x, axis = "row")
})

#' @importFrom MatrixGenerics colRanks
#' @return
#' - `colRanks()`: vector of column ranks.
#' @aliases colRanks
#' @export
#' @rdname rank_transform
methods::setMethod("colRanks", c(x = "BPCellsSeed"), function(x) {
    rank_transform(x, axis = "col")
})

#' @importFrom MatrixGenerics rowRanks
#' @export
#' @rdname rank_transform
methods::setMethod("rowRanks", c(x = "BPCellsMatrix"), function(x) {
    rank_transform(x, axis = "row")
})

#' @importFrom MatrixGenerics colRanks
#' @export
#' @rdname rank_transform
methods::setMethod("colRanks", c(x = "BPCellsMatrix"), function(x) {
    rank_transform(x, axis = "col")
})
