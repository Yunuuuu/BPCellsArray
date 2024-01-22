#' Delayed BPCells TransformPow
#'
#' The `BPCellsTransformPowArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformPow`
#' object in BPCells.
#'
#' @param x For Specific functions:
#' - `BPCellsTransformPowArray`: A `TransformPow` object.
#' - `matrixClass`: A `BPCellsTransformPowArray` object.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [^][BPCellsMatrix-class] to create `BPCellsTransformPowMatrix` object.
#' @return A `BPCellsTransformPowMatrix` object.
#' 
#' @name BPCellsTransformPow
NULL

methods::setClass("BPCellsTransformPowSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformPow")),
    slots = list(matrix = "BPCellsSeed")
)

#' @param x A `TransformPow` or `BPCellsTransformPowSeed` object.
#' @noRd
BPCellsTransformPowSeed <- function(x) {
    assert_s4_class(x, "TransformPow")
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsTransformPowSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export 
#' @rdname BPCellsTransformPow
methods::setClass("BPCellsTransformPowArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformPowSeed")
)

#' @param seed A `BPCellsTransformPowSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export 
#' @rdname BPCellsTransformPow
methods::setMethod(
    "DelayedArray", "BPCellsTransformPowSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformPowArray")
    }
)

#' @export 
#' @rdname BPCellsTransformPow
BPCellsTransformPowArray <- function(x) {
    DelayedArray(BPCellsTransformPowSeed(x))
}

#' @export 
#' @rdname BPCellsTransformPow
methods::setClass("BPCellsTransformPowMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformPowSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @export 
#' @rdname BPCellsTransformPow
methods::setMethod("matrixClass", "BPCellsTransformPowArray", function(x) {
    "BPCellsTransformPowMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################
#' @importMethodsFrom BPCells ^
#' @export
#' @rdname seed-methods
methods::setMethod("^", "BPCellsSeed", function(e1, e2) {
    BPCellsSeed(methods::callNextMethod())
})

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("^", "BPCellsMatrix", function(e1, e2) {
    e1 <- e1@seed
    DelayedArray(e1^e2)
})
