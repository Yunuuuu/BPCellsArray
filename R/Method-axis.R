#' Transpose the storage axis for a `BPCellsSeed` or `BPCellsMatrix` object
#'
#' @inherit BPCells::transpose_storage_order details
#' @inheritParams convert_mode
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
    array_call_BPCells_method(
        object = , ... = ,
        method = quote(BPCells::transpose_storage_order(matrix = object, ...))
    )
)

#' @inheritParams transpose_axis
#' @export
#' @rdname internal-methods
methods::setMethod("transpose_axis", "ANY", function(object, ...) {
    cli::cli_abort("{.arg object} must be a {.cls BPCellsMatrix} object")
})

INCOMPATIBLE_STORAGE_AXIS_INFO <- c(
    i = "you can check the storage axis with {.fn storage_axis}",
    i = c_msg(
        "you can transpose the storage axis manually with",
        "{.fn transpose_axis} to specify the new storage path"
    )
)

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
    array_call_BPCells_method(object = , convert = FALSE)
)

#' @export
#' @rdname transpose_axis
methods::setMethod(
    "storage_axis", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(object = )
)

#' @export
#' @rdname transpose_axis
methods::setMethod("storage_axis", "IterableMatrix", function(object) {
    BPCells::storage_order(object)
})
