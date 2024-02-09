#' Transpose the storage axis for a `BPCellsSeed` or `BPCellsMatrix` object
#'
#' @inherit BPCells::transpose_storage_order details 
#' @param object A [BPCellsSeed] or [BPCellsMatrix] object.
#' @inheritDotParams BPCells::transpose_storage_order -matrix
#' @return 
#'  - `transpose_axis`: A [BPCellsSeed] or [BPCellsMatrix] object with storage
#'    axis flipped.
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
        object <- object@seed
        DelayedArray(methods::callGeneric())
    }
)

#' @return 
#'  - `storage_axis`: A string indicates the storage axis, "row" or "col".
#' @export
#' @rdname transpose_axis
methods::setGeneric(
    "storage_axis", function(object) standardGeneric("storage_axis")
)

#' @export
#' @rdname transpose_axis
methods::setMethod("storage_axis", "BPCellsSeed", function(object) {
    if (object@transpose) "row" else "col"
})

#' @export
#' @rdname transpose_axis
methods::setMethod("storage_axis", "BPCellsMatrix", function(object) {
    object <- object@seed
    methods::callGeneric()
})
