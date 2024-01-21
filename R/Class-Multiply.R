#' Delayed BPCells MatrixMultiply
#'
#' The `BPCellsMultiplyArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `MatrixMultiply`
#' object in BPCells.
#'
#' @note
#' Usually, you shouldn't use this class directly, instead, you should use `%*%`
#' or `crossprod` methods of other [BPCellsMatrix] objects.
#'
#' @param x For Specific functions:
#' - `BPCellsMatrixMultiplyArray`: A `MatrixMultiply` object.
#' - `matrixClass`: A `BPCellsMatrixMultiplyArray` object.
#' @seealso [BPCellsSeed]
#' @name BPCellsMultiply
NULL
methods::setClass("BPCellsMultiplySeed",
    contains = c("BPCellsSeed", get_class("MatrixMultiply")),
    slots = list(left = "BPCellsSeed", right = "BPCellsSeed")
)

#' @param x A `MatrixMultiply` object.
#' @rdname BPCellsMultiply
#' @noRd
BPCellsMultiplySeed <- function(x) {
    assert_s4_class(x, "MatrixMultiply")
    x@left <- BPCellsSeed(x@left)
    x@right <- BPCellsSeed(x@right)
    methods::as(x, "BPCellsMultiplySeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsMultiply
methods::setClass("BPCellsMultiplyArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsMultiplySeed")
)

#' @param seed A `BPCellsMultiplySeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
#' @rdname BPCellsMultiply
methods::setMethod(
    "DelayedArray", "BPCellsMultiplySeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsMultiplyArray")
)

#' @export
#' @rdname BPCellsMultiply
BPCellsMultiplyArray <- function(x) {
    DelayedArray(BPCellsMultiplySeed(x))
}

#' @export
#' @rdname BPCellsMultiply
methods::setClass("BPCellsMultiplyMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsMultiplySeed")
)

#' @importMethodsFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsMultiply
methods::setMethod("matrixClass", "BPCellsMultiplyArray", function(x) {
    "BPCellsMultiplyMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################

#' @param object A BPCellsMultiplySeed object.
#' @importMethodsFrom DelayedArray path
#' @rdname BPCellsMultiply
#' @noRd
methods::setMethod("path", "BPCellsMultiplySeed", function(object) {
    c(path(object@left), path(object@right))
})

# it's not necessary to re-dispatch the "[" method for `BPCellsMultiplySeed`
# class since the `MatrixMultiply` method will use `[` method of `@left` and
# `@right`. Here, we just re-dispatch it to keep consistent.
#' @param ... Ignored, Not used curretly.
#' @inheritParams BPCellsMatrix-Class
#' @importMethodsFrom BPCells [
#' @rdname internal-methods
methods::setMethod(
    "[", "BPCellsMultiplySeed",
    function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)
