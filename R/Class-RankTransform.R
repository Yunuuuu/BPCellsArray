mould_BPCells("BPCellsDelayedRankTransform",
    "MatrixRankTransform",
    remove = "matrix",
    # BPCellsDelayedUnaryIsoOp: `seed` slot
    contains = "BPCellsDelayedUnaryIsoOp"
)


#################################################################
methods::setMethod("to_DelayedArray", "MatrixRankTransform", function(object) {
    to_DelayedUnaryOp(object, Class = "BPCellsDelayedRankTransform")
})

methods::setMethod(
    "to_BPCells", "BPCellsDelayedRankTransform",
    function(object) {
        to_BPCellsUnaryOp(object = object, Class = "MatrixRankTransform")
    }
)

#############################################################
summary.BPCellsDelayedRankTransform <- function(object) {
    sprintf("Rank transform matrix by %s", storage_axis(object))
}
methods::setMethod(
    "summary", "BPCellsDelayedRankTransform",
    summary.BPCellsDelayedRankTransform
)

summary.MatrixRankTransform <- summary.BPCellsDelayedRankTransform
methods::setMethod(
    "summary", "MatrixRankTransform",
    summary.BPCellsDelayedRankTransform
)

###################################################################
###########################  Methods  #############################
###################################################################

#####################   BPCellsRankTransformMatrix   #######################
#' Rank-transform a BPCells IterableMatrix matrix
#'
#' Rank values are default offset such that the rank of a 0 value is 0. If you
#' want to get the same result of regular matrix `rowRanks(matrix, ties.method =
#' "average")`/`colRanks(matrix, ties.method = "average")`, set `offset=FALSE`.
#'
#' @param x,object A [BPCellsMatrix][BPCellsMatrix-class] object or any objects
#' can be converted into [BPCellsSeed] object.
#' @inheritDotParams BPCells::transpose_storage_order -matrix
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
#' @param ties.method Always be "average", cannot be changed.
#' @param useNames Always be `TRUE`, cannot be changed.
#' @param preserveShape Always be `TRUE`, cannot be changed.
#' @inherit BPCellsDir-IO return
#' @seealso [rank_transform][BPCells::rank_transform]
#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname rank_transform
methods::setMethod(
    "rank_transform", "BPCellsMatrix",
    function(object, axis = NULL, offset = TRUE, ...) {
        assert_bool(offset)
        seed_form <- object@SeedForm
        object <- to_BPCells(object@seed)
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
                object <- BPCells::transpose_storage_order(matrix = object, ...)
            }
        }
        seed <- BPCells:::rank_transform(mat = object, axis = axis)
        if (!offset) {
            if (axis == "row") {
                nonzero <- BPCells::matrix_stats(object,
                    row_stats = "nonzero"
                )$row_stats["nonzero", , drop = TRUE]
                zeros <- ncol(object) - nonzero
                negatives <- ncol(object) - rowSums(object > 0) - zeros
                rank_offsets <- ifelse(zeros != 0,
                    negatives + (1 + zeros) / 2, 0
                )
                seed <- seed + rank_offsets
            } else {
                nonzero <- BPCells::matrix_stats(object,
                    col_stats = "nonzero"
                )$col_stats["nonzero", , drop = TRUE]
                zeros <- nrow(object) - nonzero
                negatives <- nrow(object) - colSums(object > 0) - zeros
                rank_offsets <- ifelse(zeros != 0,
                    negatives + (1 + zeros) / 2, 0
                )
                seed <- t(t(seed) + rank_offsets)
            }
        }
        with_seed_form(seed_form, DelayedArray(seed))
    }
)

#' @inheritParams rank_transform
#' @export
#' @rdname internal-methods
methods::setMethod("rank_transform", "ANY", function(object, axis = NULL, offset = TRUE, ...) {
    cli::cli_abort("{.arg object} must be a {.cls BPCellsMatrix}")
})

#######################################################################
# Rank
#' @param rows,cols Ignored currently.
#' @importFrom MatrixGenerics rowRanks
#' @aliases rowRanks
#' @export
#' @rdname rank_transform
methods::setMethod("rowRanks", c(x = "BPCellsMatrix"), function(
    x, rows = NULL, cols = NULL, ties.method = "average",
    ..., useNames = TRUE) {
    rank_transform(object = x, axis = "row", ...)
})

#' @importFrom MatrixGenerics colRanks
#' @aliases colRanks
#' @export
#' @rdname rank_transform
methods::setMethod("colRanks", c(x = "BPCellsMatrix"), function(
    x, rows = NULL, cols = NULL, ties.method = "average",
    preserveShape = TRUE, ..., useNames = TRUE) {
    rank_transform(object = x, axis = "col", ...)
})
