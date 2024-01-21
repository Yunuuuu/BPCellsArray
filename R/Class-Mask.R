#' Delayed BPCells MatrixMask
#'
#' The `BPCellsMaskArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `MatrixMask`
#' object in BPCells.
#'
#' Usually, you shouldn't use this class directly, instead, you should use `%*%`
#' or `crossprod` methods of other BPCellsMatrix objects.
#'
#' @importClassesFrom BPCells MatrixMask
#' @export
#' @name BPCellsMask
methods::setClass("BPCellsMaskSeed", contains = c("MatrixMask", "BPCellsSeed"))

#' @param x A `MatrixMask` object.
#' @export
#' @rdname BPCellsMask
BPCellsMaskSeed <- function(x) {
    assert_s4_class(x, "MatrixMask")
    methods::as(x, "BPCellsMaskSeed")
}

#' @importClassesFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsMask
methods::setClass("BPCellsMaskArray",
    contains = "DelayedArray",
    slots = c(seed = "BPCellsMaskSeed")
)

#' @param seed A `BPCellsMaskSeed` object.
#' @importMethodsFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @rdname BPCellsMask
methods::setMethod(
    "DelayedArray", "BPCellsMaskSeed",
    function(seed) new_DelayedArray(seed, Class = "BPCellsMaskArray")
)

#' @export
#' @rdname BPCellsMask
BPCellsMaskArray <- function(x) {
    DelayedArray(BPCellsMaskSeed(x))
}

#' @export
#' @importClassesFrom DelayedArray DelayedMatrix
#' @rdname BPCellsMask
methods::setClass("BPCellsMaskMatrix",
    contains = "DelayedMatrix",
    slots = c(seed = "BPCellsMaskSeed")
)

#' @export
#' @importMethodsFrom DelayedArray matrixClass
#' @rdname BPCellsMask
methods::setMethod("matrixClass", "BPCellsMaskArray", function(x) {
    "BPCellsMaskMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################

#' @param i,j Row and Column index.
#' @param drop Not used, always be `FALSE`.
#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsMask
methods::setMethod(
    "[", "BPCellsMaskSeed",
    function(x, i, j, ..., drop = FALSE) {
        BPCellsSubsetSeed(methods::callNextMethod())
    }
)

#' @export
#' @importMethodsFrom DelayedArray path
#' @rdname BPCellsMask
methods::setMethod("path", "BPCellsMaskSeed", function(object) {
    c(path(object@matrix), path(object@mask))
})

#####################   BPCellsMaskMatrix   #######################
#' Convert the type of a BPCells matrix
#'
#' @param object A `BPCellsSeed` object.
#' @param ... Additional parameters passed into specific methods.
#' @export
#' @name mask_matrix
methods::setGeneric(
    "mask_matrix",
    function(object, mask, ...) standardGeneric("mask_matrix")
)

#' @param object A [BPCellsSeed] or [BPCellsMatrix] object.
#' @param mask Mask matrix (A [BPCellsSeed] or [BPCellsMatrix] object).
#' @return A [BPCellsMaskMatrix][BPCellsMaskMatrix] object.
#' @seealso [mask_matrix][BPCells::mask_matrix]
#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix",
    c(object = "BPCellsSeed", mask = "BPCellsSeedOrdgCMatrix"),
    function(object, mask, invert = FALSE) {
        seed <- BPCells::mask_matrix(object, mask = mask, invert = invert)
        BPCellsMaskSeed(seed)
    }
)

#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix",
    c(object = "BPCellsSeed", mask = "BPCellsMatrix"),
    function(object, mask, invert = FALSE) {
        mask_matrix(object, mask = mask@seed, invert = invert)
    }
)

#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix",
    c(object = "BPCellsMatrix", mask = "BPCellsSeedOrdgCMatrix"),
    function(object, mask, invert = FALSE) {
        BPCellsMaskArray(mask_matrix(object@seed, mask = mask, invert = invert))
    }
)

#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix",
    c(object = "BPCellsMatrix", mask = "BPCellsMatrix"),
    function(object, mask, invert = FALSE) {
        seed <- mask_matrix(object@seed, mask = mask@seed, invert = invert)
        BPCellsMaskArray(seed)
    }
)

#' @export
methods::setMethod(
    "mask_matrix",
    c(object = "BPCellsMatrix", mask = "ANY"),
    function(object, mask, invert = FALSE) {
        cli::cli_abort(
            "{.arg mask} must be a {.cls BPCellsSeed} object, {.cls BPCellsMatrix} object or a {.cls dgCMatrix} object"
        )
    }
)

#' @export
methods::setMethod(
    "mask_matrix",
    c(object = "BPCellsSeed", mask = "ANY"),
    function(object, mask, invert = FALSE) {
        cli::cli_abort(
            "{.arg mask} must be a {.cls BPCellsSeed} object, {.cls BPCellsMatrix} object or a {.cls dgCMatrix} object"
        )
    }
)
