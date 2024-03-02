###########################################################
mould_BPCells("BPCellsDelayedConvert", "ConvertMatrixType",
    # BPCellsDelayedUnaryOp: `seed` slot
    remove = "matrix", contains = "BPCellsDelayedUnaryOp"
)

#################################################################
methods::setMethod("to_DelayedArray", "ConvertMatrixType", function(object) {
    to_DelayedUnaryOp(object, Class = "BPCellsDelayedConvert")
})

methods::setMethod("to_BPCells", "BPCellsDelayedConvert", function(object) {
    to_BPCellsUnaryOp(object = object, Class = "ConvertMatrixType")
})

summary.BPCellsDelayedConvert <- function(object) {
    sprintf(
        "Convert type from %s to %s",
        storage_mode(object@seed),
        storage_mode(object)
    )
}

methods::setMethod(
    "summary", "BPCellsDelayedConvert",
    summary.BPCellsDelayedConvert
)

summary.ConvertMatrixType <- function(object) {
    sprintf(
        "Convert type from %s to %s",
        storage_mode(object@matrix),
        storage_mode(object)
    )
}

methods::setMethod("summary", "ConvertMatrixType", summary.ConvertMatrixType)


###################################################################
###########################  Methods  #############################
###################################################################

#####################   BPCellsConvertMatrix   #######################
#' Convert the storage mode of a BPCellsArray object
#'
#' @param object A [BPCellsMatrix][BPCellsMatrix-class] object.
#' @param ... Additional parameters passed into specific methods.
#' @name convert_mode
NULL

#' @return
#'  - `convert_mode`: A [BPCellsMatrix][BPCellsMatrix-class] object with storage
#' mode converted into the specified.
#' @export
#' @rdname convert_mode
methods::setGeneric(
    "convert_mode",
    function(object, ...) standardGeneric("convert_mode")
)

#' @param mode Storage mode of BPCells matrix, one of `uint32_t` (unsigned
#' 32-bit integer), `float` (32-bit real number), or `double` (64-bit real
#' number). R cannot differentiate 32-bit and 64-bit real number, so
#' [type][BPCellsSeed-class] method always return "double" for both `float`
#' and `double` mode.
#' @seealso [convert_matrix_type][BPCells::convert_matrix_type]
#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname convert_mode
methods::setMethod(
    "convert_mode", "BPCellsMatrix",
    array_call_BPCells_method(
        object = , mode = ,
        method = quote(BPCells::convert_matrix_type(
            matrix = object, type = mode
        )),
        before = expression(mode <- match.arg(mode, BPCells_MODE))
    )
)

#' @inheritParams convert_mode
#' @export
#' @rdname internal-methods
methods::setMethod("convert_mode", "ANY", function(object, mode) {
    cli::cli_abort("{.arg object} must be a {.cls BPCellsMatrix} object")
})

##############################################################
#' @return
#'  - `storage_mode`: A string indicates the storage mode.
#' @export
#' @rdname convert_mode
methods::setGeneric(
    "storage_mode", function(object) standardGeneric("storage_mode")
)

#' @export
#' @rdname convert_mode
methods::setMethod(
    "storage_mode", "BPCellsMatrix",
    array_call_BPCells_method(object = , convert = FALSE)
)

#' @export
#' @rdname convert_mode
methods::setMethod(
    "storage_mode", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(object = )
)

#' @export
#' @rdname convert_mode
methods::setMethod("storage_mode", "IterableMatrix", function(object) {
    BPCells_get("matrix_type")(object)
})

#' @export
#' @rdname convert_mode
methods::setMethod("storage_mode", "matrix", function(object) {
    x <- storage.mode(object)
    switch(x,
        integer = "uint32_t",
        double = ,
        numeric = "double",
        cli::cli_abort("{.pkg BPCells} cannot support {.field {x}} mode")
    )
})

INCOMPATIBLE_STORAGE_MODE_INFO <- function(x, y, x_mode) {
    c(
        "!" = c_msg(
            "Incompatible storage mode between",
            style_arg(x), "and", style_arg(y)
        ),
        i = c_msg(
            "Convert", style_arg(y), "into",
            style_field(x_mode), "mode"
        )
    )
}
