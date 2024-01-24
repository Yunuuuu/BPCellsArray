methods::setClass("BPCellsConvertSeed",
    contains = c("BPCellsSeed", get_class("ConvertMatrixType")),
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
#' @name convert_type
NULL

#' @export
#' @rdname convert_type
methods::setGeneric(
    "convert_type",
    function(object, ...) standardGeneric("convert_type")
)

#' @param type Storage mode of BPCells matrix, one of `uint32_t` (`integer`)
#' (unsigned 32-bit integer), `float` (`numeric` or `32bit_numeric`) (32-bit
#' real number), or `double` (`64bit_numeric`) (64-bit real number). R cannot
#' differentiate 32-bit and 64-bit real number, here, we use "double" to indicte
#' 64-bit real number and "numeric" to indicate 32-bit real number.
#' @return A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object.
#' @seealso [convert_matrix_type][BPCells::convert_matrix_type]
#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname convert_type
methods::setMethod("convert_type", "BPCellsSeed", function(object, type) {
    type <- match.arg(
        type, c(
            "integer", "uint32_t", "double", "numeric",
            "32bit_numeric", "64bit_numeric"
        )
    )
    type <- switch(type,
        integer = ,
        uint32_t = "uint32_t",
        float = ,
        numeric = ,
        `32bit_numeric` = "float",
        double = ,
        `64bit_numeric` = "double",
    )
    obj <- BPCells::convert_matrix_type(object, type = type)
    BPCellsSeed(obj)
})

#' @export
#' @rdname convert_type
methods::setMethod("convert_type", "BPCellsMatrix", function(object, type) {
    DelayedArray(convert_type(object@seed, type = type))
})

#' @inheritParams convert_type
#' @export
#' @rdname internal-methods
methods::setMethod("convert_type", "ANY", function(object, type) {
    cli::cli_abort(
        "{.arg object} must be a {.cls BPCellsSeed} or {.cls BPCellsMatrix} object"
    )
})
