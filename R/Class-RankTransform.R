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
        BPCells_class("MatrixRankTransform")
    )
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixRankTransform", function(x) {
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsRankTransformSeed")
})

methods::setMethod("summary", "BPCellsRankTransformSeed", function(object) {
    sprintf("Rank transform matrix by %s", storage_axis(object))
})

###################################################################
###########################  Methods  #############################
###################################################################

#####################   BPCellsRankTransformMatrix   #######################
#' Rank-transform a BPCells IterableMatrix matrix
#' 
#' Rank values are default offset such that the rank of a 0 value is 0. If you
#' want to get the same result of `rowRanks(object, ties.method =
#' "average")`/`colRanks(object, ties.method = "average")`, set `offset=FALSE`
#'
#' @param x,object A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object.
#' @inheritDotParams BPCells::transpose_storage_order -matrix
#' @note Ties are always handled by averaging ranks.
#' @export
#' @name rank_transform
methods::setGeneric(
    "rank_transform",
    function(object, ...) standardGeneric("rank_transform")
)

#' @param axis Axis to rank values within. "col" to rank values within each
#'     column, and "row" to rank values within each row. If `NULL`, will use the
#'     storage axis of `object` (see [storage_axis]).  If `axis` specified is
#'     different from the storage axis of `object`, [transpose_axis] will be
#'     used to transpose the underlying storage order.
#' @param offset A bool, whether or not to add offset such that the rank of a 0
#' value is 0. Default: `TRUE`.
#' @return
#'  - `rank_transform`: A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object depends on the class of `object`.
#' @seealso [rank_transform][BPCells::rank_transform]
#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname rank_transform
methods::setMethod(
    "rank_transform", "BPCellsSeed",
    function(object, axis = NULL, offset = TRUE, ...) {
        assert_bool(offset)
        main_axis <- storage_axis(object)
        if (is.null(axis)) {
            axis <- main_axis
            cli::cli_inform("Doing {.val {axis}} rank transformation")
        } else {
            axis <- match.arg(axis, c("row", "col"))
            if (axis != main_axis) {
                cli::cli_warn(c(
                    "transposing the storage order for {.arg object}",
                    i = "{.arg axis} specified is different from the the storage axis of {.arg object} ({.val {main_axis}})" # nolint
                ))
                object <- transpose_axis(object = object, ...)
            }
        }
        seed <- BPCellsSeed(BPCells:::rank_transform(mat = object, axis = axis))
        if (!offset) {
            nonzero <- switch(axis,
                row = BPCells::matrix_stats(object,
                    row_stats = "nonzero"
                )$row_stats["nonzero", , drop = TRUE],
                col = BPCells::matrix_stats(object,
                    col_stats = "nonzero"
                )$col_stats["nonzero", , drop = TRUE]
            )
            zeros <- nrow(object) - nonzero
            negatives <- nrow(object) - colSums(object > 0) - zeros
            rank_offsets <- ifelse(zeros != 0, negatives + (1 + zeros) / 2, 0)
            seed <- BPCells::add_cols(mat = seed, rank_offsets)
        }
        seed
    }
)

#' @export
#' @rdname rank_transform
methods::setMethod(
    "rank_transform", "BPCellsMatrix",
    function(object, axis = NULL, offset = TRUE, ...) {
        object <- object@seed
        DelayedArray(methods::callGeneric())
    }
)

#' @inheritParams rank_transform
#' @export
#' @rdname internal-methods
methods::setMethod("rank_transform", "ANY", function(object, axis = NULL, offset = TRUE, ...) {
    cli::cli_abort(
        "{.arg object} must be a {.cls BPCellsSeed} or {.cls BPCellsMatrix}"
    )
})

#######################################################################
# Rank
#' @param rows,cols,ties.method,useNames Ignored currently.
#' @importFrom MatrixGenerics rowRanks
#' @return
#' - `rowRanks()`: vector of row ranks.
#' @aliases rowRanks
#' @export
#' @rdname rank_transform
methods::setMethod("rowRanks", c(x = "BPCellsSeed"), function(x, ...) {
    rank_transform(object = x, axis = "row", ...)
})

#' @importFrom MatrixGenerics colRanks
#' @return
#' - `colRanks()`: vector of column ranks.
#' @aliases colRanks
#' @export
#' @rdname rank_transform
methods::setMethod("colRanks", c(x = "BPCellsSeed"), function(x, ...) {
    rank_transform(object = x, axis = "col", ...)
})

#' @importFrom MatrixGenerics rowRanks
#' @export
#' @rdname rank_transform
methods::setMethod("rowRanks", c(x = "BPCellsMatrix"), function(x, ...) {
    rank_transform(object = x, axis = "row", ...)
})

#' @importFrom MatrixGenerics colRanks
#' @export
#' @rdname rank_transform
methods::setMethod("colRanks", c(x = "BPCellsMatrix"), function(x, ...) {
    rank_transform(object = x, axis = "col", ...)
})
