methods::setClass("BPCellsConvertSeed",
    contains = c(
        "BPCellsUnaryOpsSeed",
        get_class("ConvertMatrixType")
    ),
    slots = list(matrix = "BPCellsSeed")
)

#' @rdname BPCellsConvert
#' @noRd
BPCellsConvertSeed <- function(x) {
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsConvertSeed")
}

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "ConvertMatrixType", function(x) {
    BPCellsConvertSeed(x = x)
})

###################################################################
###########################  Methods  #############################
###################################################################

#####################   BPCellsConvertMatrix   #######################
#' Convert the storage type of a BPCellsArray object
#'
#' @param object A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object.
#' @param ... Additional parameters passed into specific methods.
#' @name convert_mode
NULL

#' @export
#' @rdname convert_mode
methods::setGeneric(
    "convert_mode",
    function(object, ...) standardGeneric("convert_mode")
)

#' @param mode Storage mode of BPCells matrix, one of `uint32_t` (`integer`)
#' (unsigned 32-bit integer), `float` (`numeric` or `32bit_numeric`) (32-bit
#' real number), or `double` (`64bit_numeric`) (64-bit real number). R cannot
#' differentiate 32-bit and 64-bit real number, here, we use "double" to indicte
#' 64-bit real number and "numeric" to indicate 32-bit real number.
#' @return A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object.
#' @seealso [convert_matrix_type][BPCells::convert_matrix_type]
#' @export
#' @rdname convert_mode
methods::setMethod("convert_mode", "BPCellsSeed", function(object, mode) {
    obj <- BPCells::convert_matrix_type(object, type = mode_to_bpcells(mode))
    BPCellsSeed(obj)
})

#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname convert_mode
methods::setMethod("convert_mode", "BPCellsMatrix", function(object, mode) {
    object <- object@seed
    DelayedArray(methods::callGeneric())
})

#' @inheritParams convert_mode
#' @export
#' @rdname internal-methods
methods::setMethod("convert_mode", "ANY", function(object, mode) {
    cli::cli_abort(
        "{.arg object} must be a {.cls BPCellsSeed} or {.cls BPCellsMatrix} object"
    )
})
