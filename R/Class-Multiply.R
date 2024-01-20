#' Delayed BPCells MatrixMultiply
#'
#' The `BPCellsMultiplyArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `MatrixMultiply`
#' object in BPCells.
#'
#' Usually, you shouldn't use this class directly, instead, you should use `%*%`
#' or `crossprod` methods of other BPCellsMatrix objects.
#'
#' @importClassesFrom BPCells MatrixMultiply
#' @export
#' @name BPCellsMultiply
methods::setClass("BPCellsMultiplySeed",
    contains = c("BPCellsSeed", "MatrixMultiply"),
    slots = list(left = "BPCellsSeed", right = "BPCellsSeed")
)

#' @param x A `MatrixMultiply` object.
#' @export
#' @rdname BPCellsMultiply
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

#' @export
#' @importMethodsFrom DelayedArray path
#' @rdname BPCellsMultiply
methods::setMethod("path", "BPCellsMultiplySeed", function(object) {
    c(path(object@left), path(object@right))
})

#' @param i,j Row and Column index.
#' @param drop Not used, always be `FALSE`.
#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsDir
methods::setMethod(
    "[", "BPCellsDirSeed",
    function(x, i, j, ..., drop = FALSE) {
        BPCellsSeed(methods::callNextMethod())
    }
)
