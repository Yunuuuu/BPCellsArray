#' Delayed BPCells TransformBinarize
#'
#' The `BPCellsTransformBinarizeArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformBinarize`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformBinarizeArray`: A `TransformBinarize` object.
#' - `matrixClass`: A `BPCellsTransformBinarizeArray` object.
#' @seealso [BPCellsSeed]
#' @name BPCellsTransformBinarize
#' @noRd
NULL

methods::setClass("BPCellsTransformBinarizeSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformBinarize")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformBinarize` or `BPCellsTransformBinarizeSeed` object.
#' @rdname BPCellsTransformBinarize
#' @noRd
BPCellsTransformBinarizeSeed <- function(x) {
    assert_s4_class(x, "TransformBinarize")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformBinarizeSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @rdname BPCellsTransformBinarize
#' @noRd
methods::setClass("BPCellsTransformBinarizeArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformBinarizeSeed")
)

#' @param seed A `BPCellsTransformBinarizeSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname internal-methods
methods::setMethod(
    "DelayedArray", "BPCellsTransformBinarizeSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformBinarizeArray")
    }
)

#' @rdname BPCellsTransformBinarize
#' @noRd
BPCellsTransformBinarizeArray <- function(x) {
    DelayedArray(BPCellsTransformBinarizeSeed(x))
}

#' @rdname BPCellsTransformBinarize
#' @noRd
methods::setClass("BPCellsTransformBinarize",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformBinarizeSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @rdname internal-methods
methods::setMethod("matrixClass", "BPCellsTransformBinarizeArray", function(x) {
    "BPCellsTransformBinarize"
})

###################################################################
###########################  Methods  #############################
###################################################################
methods::setGeneric("binarize", function(object, ...) {
    makeStandardGeneric("binarize")
})

#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod(
    "binarize", "BPCellsSeed",
    function(object, threshold = 0, strict_inequality = TRUE) {
        obj <- BPCells::binarize(
            mat = object, threshold = threshold,
            strict_inequality = strict_inequality
        )
        BPCellsSeed(obj)
    }
)

#' @export
#' @rdname BPCellsMatrix
methods::setMethod("binarize", "BPCellsMatrix", function(object, ...) {
    DelayedArray(binarize(object = object@seed, ...))
})
