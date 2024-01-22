#' Delayed BPCells TransformBinarize
#'
#' The `BPCellsTransformBinarizeArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformBinarize`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [binarize][BPCells-binarize] to create `BPCellsTransformBinarizeMatrix`
#' object. 
#'
#' @param x For Specific functions:
#' - `BPCellsTransformBinarizeArray`: A `TransformBinarize` object.
#' - `matrixClass`: A `BPCellsTransformBinarizeArray` object.
#' @return A `BPCellsTransformBinarizeMatrix` object.
#' @seealso [BPCellsSeed]
#' @name BPCellsTransformBinarize
NULL

methods::setClass("BPCellsTransformBinarizeSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformBinarize")),
    slots = list(matrix = "BPCellsSeed")
)

#' @rdname BPCellsTransformBinarize
#' @noRd
BPCellsTransformBinarizeSeed <- function(x) {
    assert_s4_class(x, "TransformBinarize")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformBinarizeSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsTransformBinarize
methods::setClass("BPCellsTransformBinarizeArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformBinarizeSeed")
)

#' @param seed A `BPCellsTransformBinarizeSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsTransformBinarize
methods::setMethod(
    "DelayedArray", "BPCellsTransformBinarizeSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformBinarizeArray")
    }
)

#' @export
#' @rdname BPCellsTransformBinarize
BPCellsTransformBinarizeArray <- function(x) {
    DelayedArray(BPCellsTransformBinarizeSeed(x))
}

#' @export
#' @rdname BPCellsTransformBinarize
methods::setClass("BPCellsTransformBinarizeMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformBinarizeSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsTransformBinarize
methods::setMethod("matrixClass", "BPCellsTransformBinarizeArray", function(x) {
    "BPCellsTransformBinarizeMatrix"
})
