#' Transpose the storage order for a `BPCellsSeed` or `BPCellsMatrix` object
#'
#' @param object A [BPCellsSeed] or [BPCellsMatrix] object.
#' @inheritDotParams BPCells::transpose_storage_order -matrix
#' @return A [BPCellsSeed] or [BPCellsMatrix] object.
#' @export
#' @name transpose_storage
#' @include Class-BPCellsMatrix.R
methods::setGeneric("transpose_storage", function(object, ...) {
    standardGeneric("transpose_storage")
})

#' @export
#' @rdname transpose_storage
methods::setMethod(
    "transpose_storage", "BPCellsSeed", function(object, ...) {
        BPCellsSeed(BPCells::transpose_storage_order(matrix = object, ...))
    }
)

#' @export
#' @rdname transpose_storage
methods::setMethod(
    "transpose_storage", "BPCellsMatrix", function(object, ...) {
        DelayedArray(transpose_storage(object = object@seed, ...))
    }
)
