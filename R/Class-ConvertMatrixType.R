#' Delayed BPCells ConvertMatrixType
#'
#' The `BPCellsConvertMatrixTypeArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `ConvertMatrixType`
#' object in BPCells. Usually, you shouldn't use this class directly, instead,
#' you should use [convert_type].
#'
#' @importClassesFrom BPCells ConvertMatrixType
#' @export
#' @name BPCellsConvertMatrixType
methods::setClass("BPCellsConvertMatrixTypeSeed",
    contains = "ConvertMatrixType"
)

#' @param x A `ConvertMatrixType` or `BPCellsConvertMatrixTypeSeed` object.
#' @export
#' @rdname BPCellsConvertMatrixType
BPCellsConvertMatrixTypeSeed <- function(x) {
    assert_s4_class(x, "ConvertMatrixType")
    methods::as(x, "BPCellsConvertMatrixTypeSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsConvertMatrixType
methods::setClass("BPCellsConvertMatrixTypeArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsConvertMatrixTypeSeed")
)

#' @param seed A `BPCellsConvertMatrixTypeSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname BPCellsConvertMatrixType
methods::setMethod(
    "DelayedArray", "BPCellsConvertMatrixTypeSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsConvertMatrixTypeArray")
    }
)

#' @export
#' @rdname BPCellsConvertMatrixType
BPCellsConvertMatrixTypeArray <- function(x) {
    DelayedArray(BPCellsConvertMatrixTypeSeed(x))
}

#' @export
#' @importClassesFrom DelayedArray DelayedMatrix
#' @rdname BPCellsConvertMatrixType
methods::setClass("BPCellsConvertMatrixTypeMatrix",
    contains = "DelayedMatrix",
    slots = c(seed = "BPCellsConvertMatrixTypeSeed")
)

#' @export
#' @importMethodsFrom DelayedArray matrixClass
#' @rdname BPCellsConvertMatrixType
methods::setMethod("matrixClass", "BPCellsConvertMatrixTypeArray", function(x) {
    "BPCellsConvertMatrixTypeMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################

#' @param object A `BPCellsConvertMatrixTypeSeed` object.
#' @export
#' @importMethodsFrom methods show
#' @rdname BPCellsConvertMatrixType
methods::setMethod("show", "BPCellsConvertMatrixTypeSeed", function(object) {
    cat(sprintf(
        "%i x %i BPCellsConvertMatrixTypeSeed object\n",
        nrow(object), ncol(object)
    ))
})

#' @export
#' @importMethodsFrom DelayedArray type
#' @rdname BPCellsConvertMatrixType
#' @include utils.R
methods::setMethod("type", "BPCellsConvertMatrixTypeSeed", bpcells_to_r_type)

#' @export
#' @importMethodsFrom DelayedArray is_sparse
#' @rdname BPCellsConvertMatrixType
methods::setMethod(
    "is_sparse", "BPCellsConvertMatrixTypeSeed",
    function(x) TRUE
)

#' @param object A `BPCellsConvertMatrixTypeSeed` object.
#' @export
#' @importMethodsFrom DelayedArray path
#' @rdname BPCellsConvertMatrixType
methods::setMethod("path", "BPCellsConvertMatrixTypeSeed", function(object) {
    path(object@matrix)
})

#' @inheritParams S4Arrays::extract_array
#' @export
#' @importMethodsFrom DelayedArray extract_array
#' @rdname BPCellsConvertMatrixType
methods::setMethod(
    "extract_array", "BPCellsConvertMatrixTypeSeed",
    function(x, index) {
        out <- as.matrix(extract_bpcells_array(x, index))
        storage.mode(out) <- type(x)
        out
    }
)

#' @export
#' @importMethodsFrom DelayedArray extract_sparse_array
#' @rdname BPCellsConvertMatrixType
methods::setMethod(
    "extract_sparse_array", "BPCellsConvertMatrixTypeSeed",
    function(x, index) {
        methods::as(extract_bpcells_array(x, index), "SparseArraySeed")
    }
)

#' Convert the type of a BPCells matrix
#'
#' @param object A `IterableMatrix` or [dgCMatrix][Matrix::dgCMatrix-class]
#' object.
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
#' @return A [BPCellsConvertMatrixTypeMatrix][BPCellsConvertMatrixType] object.
#' @seealso [convert_matrix_type][BPCells::convert_matrix_type]
#' @importFrom DelayedArray DelayedArray
#' @importClassesFrom BPCells IterableMatrix
#' @export
#' @rdname convert_type
methods::setMethod("convert_type", "IterableMatrix", function(object, type) {
    type <- match.arg(
        type, c(
            "integer", "uint32_t", "double", "numeric",
            "32bit_numeric", "64bit_numeric"
        )
    )
    if (any(type == c("integer", "uint32_t"))) {
        type <- "uint32_t"
    } else if (any(type == c("float", "numeric", "32bit_numeric"))) {
        type <- "float"
    } else if (any(type == c("double", "64bit_numeric"))) {
        type <- "double"
    }
    DelayedArray(BPCells::convert_matrix_type(object, type = type))
})

#' @export
#' @importClassesFrom Matrix dgCMatrix
#' @rdname convert_type
methods::setMethod("convert_type", "dgCMatrix", function(object, type) {
    convert_type(methods::as(object, "IterableMatrix"), type = type)
})
