#' Delayed BPCells ConvertMatrixType
#'
#' The `BPCellsConvertArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `ConvertMatrixType`
#' object in BPCells.
#'
#' @note
#' Usually, you shouldn't use this class directly, instead, you should use
#' [convert_type] to create a `BPCellsConvert` object.
#'
#' @export
#' @name BPCellsConvert
methods::setClass("BPCellsConvertSeed",
    contains = c("BPCellsSeed", get_class("ConvertMatrixType")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `ConvertMatrixType` object.
#' @export
#' @rdname BPCellsConvert
BPCellsConvertSeed <- function(x) {
    assert_s4_class(x, "ConvertMatrixType")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsConvertSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsConvert
methods::setClass("BPCellsConvertArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsConvertSeed")
)

#' @param seed A `BPCellsConvertSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsConvert
methods::setMethod(
    "DelayedArray", "BPCellsConvertSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsConvertArray")
)

#' @export
#' @rdname BPCellsConvert
BPCellsConvertArray <- function(x) {
    DelayedArray(BPCellsConvertSeed(x))
}

#' @export
#' @rdname BPCellsConvert
methods::setClass("BPCellsConvertMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsConvertSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsConvert
methods::setMethod("matrixClass", "BPCellsConvertArray", function(x) {
    "BPCellsConvertMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################

#' @param object A `BPCellsConvertSeed` object.
#' @importMethodsFrom DelayedArray path
#' @export
#' @rdname BPCellsConvert
methods::setMethod("path", "BPCellsConvertSeed", function(object) {
    path(object@matrix)
})

#' @inheritParams BPCellsMatrix
#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsConvert
methods::setMethod(
    "[", "BPCellsConvertSeed",
    function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#####################   BPCellsConvertMatrix   #######################
#' Convert the type of a BPCells IterableMatrix matrix
#'
#' @param object A `BPCellsSeed` or `BPCellsMatrix` object.
#' @param ... Additional parameters passed into specific methods.
#' @export
#' @name convert_type
methods::setGeneric(
    "convert_type",
    function(object, ...) standardGeneric("convert_type")
)

#' @param type Storage mode of BPCells matrix, one of `uint32_t` (`integer`)
#' (unsigned 32-bit integer), `float` (`numeric` or `32bit_numeric`) (32-bit
#' real number), or `double` (`64bit_numeric`) (64-bit real number). R cannot
#' differentiate 32-bit and 64-bit real number, here, we use "double" to indicte
#' 64-bit real number and "numeric" to indicate 32-bit real number.
#' @return A [BPCellsConverSeed][BPCellsConvert] object or
#' [BPCellsConvertMatrix][BPCellsConvert] object.
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

#' @export
methods::setMethod("convert_type", "ANY", function(object, type) {
    cli::cli_abort(
        "{.arg object} must be a {.cls BPCellsSeed} or {.cls BPCellsMatrix} object"
    )
})
