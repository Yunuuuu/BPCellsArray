#' Transpose the storage axis for a `BPCellsSeed` or `BPCellsMatrix` object
#'
#' @inherit BPCells::transpose_storage_order details
#' @inheritParams convert_mode
#' @inheritDotParams BPCells::transpose_storage_order -matrix
#' @return
#'  - `transpose_axis`: A [BPCellsMatrix] object with storage axis flipped.
#'    Note: `identical(as.matrix(transpose_axis(object)), as.matrix(object))` is
#'    `TRUE`.
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
    function(object, ...) {
        object <- to_BPCells(object@seed)
        ans <- BPCells::transpose_storage_order(matrix = object, ...)
        DelayedArray(ans)
    }
)

#' @inheritParams transpose_axis
#' @export
#' @rdname internal-methods
methods::setMethod("transpose_axis", "ANY", function(object, ...) {
    cli::cli_abort("{.arg object} must be a {.cls BPCellsMatrix} object")
})

INCOMPATIBLE_STORAGE_AXIS_MSG <- c(
    i = "you can check the storage axis with {.fn storage_axis}",
    i = paste(
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
methods::setMethod("storage_axis", "BPCellsMatrix", function(object) {
    object <- to_BPCells(object@seed)
    methods::callGeneric()
})

#' @export
#' @rdname transpose_axis
methods::setMethod("storage_axis", "BPCellsDelayedOp", function(object) {
    object <- to_BPCells(object)
    methods::callGeneric()
})

#' @export
#' @rdname transpose_axis
methods::setMethod("storage_axis", "IterableMatrix", function(object) {
    BPCells::storage_order(object)
})
