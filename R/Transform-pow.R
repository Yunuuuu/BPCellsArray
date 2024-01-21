#' Delayed BPCells TransformPow
#'
#' The `BPCellsTransformPowArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `TransformPow`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' `[` (extract methods) of other BPCellsArray objects.
#'
#' @noRd
#' @name BPCellsTransformPow
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
#' @noRd
methods::setClass("BPCellsTransformPowArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsTransformPowSeed")
)

#' @param seed A `BPCellsTransformPowSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @noRd
methods::setMethod(
    "DelayedArray", "BPCellsTransformPowSeed",
    function(seed) {
        new_DelayedArray(seed, Class = "BPCellsTransformPowArray")
    }
)

#' @noRd
BPCellsTransformPowArray <- function(x) {
    DelayedArray(BPCellsTransformPowSeed(x))
}

#' @noRd
methods::setClass("BPCellsTransformPow",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsTransformPowSeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @noRd
methods::setMethod("matrixClass", "BPCellsTransformPowArray", function(x) {
    "BPCellsTransformPow"
})

###################################################################
###########################  Methods  #############################
###################################################################
#' @importMethodsFrom BPCells ^
#' @export
#' @rdname BPCellsSeed-Class
methods::setMethod("^", "BPCellsSeed", function(e1, e2) {
    BPCellsSeed(methods::callNextMethod())
})

#' @export
#' @rdname BPCellsMatrix
methods::setMethod("^", "BPCellsMatrix", function(e1, e2) {
    e1 <- e1@seed
    DelayedArray(e1^e2)
})
