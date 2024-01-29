#' Transpose the storage order for a `BPCellsSeed` or `BPCellsMatrix` object
#'
#' @param object A [BPCellsSeed] or [BPCellsMatrix] object.
#' @inheritDotParams BPCells::transpose_storage_order -matrix
#' @return A [BPCellsSeed] or [BPCellsMatrix] object.
#' @export
#' @name transpose_axis
#' @include Class-BPCellsMatrix.R
methods::setGeneric("transpose_axis", function(object, ...) {
    standardGeneric("transpose_axis")
})

#' @export
#' @rdname transpose_axis
methods::setMethod(
    "transpose_axis", "BPCellsSeed", function(object, ...) {
        BPCellsSeed(BPCells::transpose_storage_order(matrix = object, ...))
    }
)

#' @export
#' @rdname transpose_axis
methods::setMethod(
    "transpose_axis", "BPCellsMatrix", function(object, ...) {
        DelayedArray(transpose_axis(object = object@seed, ...))
    }
)
