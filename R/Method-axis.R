#' Transpose the storage axis for a `BPCellsSeed` or `BPCellsMatrix` object
#'
#' @inherit BPCells::transpose_storage_order details
#' @param object A [BPCellsMatrix] object.
#' @inheritDotParams BPCells::transpose_storage_order -matrix
#' @return
#'  - `transpose_axis`: A [BPCellsMatrix] object with storage axis flipped.
#' @export
#' @name transpose_axis
methods::setGeneric("transpose_axis", function(object, ...) {
    standardGeneric("transpose_axis")
})

#' @export
#' @rdname transpose_axis
methods::setMethod(
    "transpose_axis", "BPCellsMatrix",
    # `transpose_storage_order` always return a `MatrixDir` object
    # It's not necessary to use `to_DelayedArray` to convert it
    set_BPCellsArray_method(
        object = , ... = ,
        method = quote(BPCells::transpose_storage_order(matrix = object, ...)),
        after = expression(DelayedArray(object))
    )
)

#' @inheritParams transpose_axis
#' @export
#' @rdname internal-methods
methods::setMethod("transpose_axis", "ANY", function(object, mode) {
    cli::cli_abort("{.arg object} must be a {.cls BPCellsMatrix} object")
})

###########################################################
#' @return
#'  - `storage_axis`: A string indicates the storage axis, "row" or "col".
#' @export
#' @rdname transpose_axis
methods::setGeneric("storage_axis", function(object) {
    standardGeneric("storage_axis")
})

#' @export
#' @rdname transpose_axis
methods::setMethod(
    "storage_axis", "BPCellsMatrix",
    set_BPCellsArray_method(object = )
)

#' @export
#' @rdname transpose_axis
methods::setMethod(
    "storage_axis", "BPCellsDelayedOp",
    call_BPCells_method(object = )
)

#' @export
#' @rdname transpose_axis
methods::setMethod("storage_axis", "IterableMatrix", function(object) {
    BPCells::storage_order(object)
})
