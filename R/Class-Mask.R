#' Delayed BPCells MatrixMask
#'
#' The `BPCellsMaskArray` class provides a
#' [DelayedArray][DelayedArray::DelayedArray] backend for `MatrixMask`
#' object in BPCells.
#'
#' @note Usually, you shouldn't use this class directly, instead, you should use
#' [mask_matrix] of other BPCellsMatrix objects to create a `BPCellsMaskMatrix`.
#'
#' @param x For Specific functions:
#' - `BPCellsMatrixMaskArray`: A `MatrixMask` object.
#' - `matrixClass`: A `BPCellsMatrixMaskArray` object.
#' @seealso [BPCellsSeed]
#' @name BPCellsMask
NULL

methods::setClass("BPCellsMaskSeed",
    contains = c("BPCellsSeed", get_class("MatrixMask"))
)

#' @param x A `MatrixMask` object.
#' @rdname BPCellsMask
#' @noRd
BPCellsMaskSeed <- function(x) {
    assert_s4_class(x, "MatrixMask")
    x@matrix <- BPCellsSeed(x@matrix)
    x@mask <- BPCellsSeed(x@mask)
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
#' @importFrom DelayedArray DelayedArray
#' @importFrom DelayedArray new_DelayedArray
#' @export
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

#' @importClassesFrom DelayedArray DelayedMatrix
#' @export
#' @rdname BPCellsMask
methods::setClass("BPCellsMaskMatrix",
    contains = "BPCellsMatrix",
    slots = c(seed = "BPCellsMaskSeed")
)

#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsMask
methods::setMethod("matrixClass", "BPCellsMaskArray", function(x) {
    "BPCellsMaskMatrix"
})

###################################################################
###########################  Methods  #############################
###################################################################

#####################   BPCellsMaskMatrix   #######################
#' Mask matrix entries to zero
#'
#' Set matrix entries to zero given a mask matrix of the same dimensions.
#' Normally, non-zero values in the mask will set the matrix entry to zero. If
#' inverted, zero values in the mask matrix will set the matrix entry to zero.
#' @param ... Additional parameters passed into specific methods.
#' @name mask_matrix
NULL

#' @export
#' @rdname mask_matrix
methods::setGeneric(
    "mask_matrix",
    function(object, mask, ...) standardGeneric("mask_matrix")
)

#' @param object A [BPCellsSeed] or [BPCellsMatrix] object.
#' @param mask Mask matrix, A [BPCellsSeed] or [BPCellsMatrix] object.
#' Additionally, a matrix-like object which can be coerced into
#' [dgCMatrix][Matrix::dgCMatrix-class].
#' @param invert A bool, indicates whether revert the mask.
#' @return A [BPCellsMaskSeed][BPCellsMask] or [BPCellsMatrix][BPCellsMask]
#' object.
#' @seealso [mask_matrix][BPCells::mask_matrix]
#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix", c(object = "BPCellsSeed", mask = "BPCellsSeed"),
    function(object, mask, invert = FALSE) {
        seed <- BPCells:::mask_matrix(object, mask = mask, invert = invert)
        BPCellsSeed(seed)
    }
)

#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix", c(object = "BPCellsSeed", mask = "dgCMatrix"),
    function(object, mask, invert = FALSE) {
        seed <- BPCells:::mask_matrix(object, mask = mask, invert = invert)
        BPCellsSeed(seed)
    }
)

#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix", c(object = "BPCellsSeed", mask = "BPCellsMatrix"),
    function(object, mask, invert = FALSE) {
        mask_matrix(object, mask = mask@seed, invert = invert)
    }
)

#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix", c(object = "BPCellsSeed", mask = "ANY"),
    function(object, mask, invert = FALSE) {
        mask_matrix(object, mask = coerce_dgCMatrix(mask), invert = invert)
    }
)

#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix", c(object = "BPCellsMatrix", mask = "BPCellsSeed"),
    function(object, mask, invert = FALSE) {
        DelayedArray(mask_matrix(object@seed, mask = mask, invert = invert))
    }
)

#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix", c(object = "BPCellsMatrix", mask = "dgCMatrix"),
    function(object, mask, invert = FALSE) {
        DelayedArray(mask_matrix(object@seed, mask = mask, invert = invert))
    }
)

#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix", c(object = "BPCellsMatrix", mask = "BPCellsMatrix"),
    function(object, mask, invert = FALSE) {
        mask_matrix(object, mask = mask@seed, invert = invert)
    }
)

#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix", c(object = "BPCellsMatrix", mask = "ANY"),
    function(object, mask, invert = FALSE) {
        mask_matrix(object, mask = coerce_dgCMatrix(mask), invert = invert)
    }
)

#' @inheritParams mask_matrix
#' @export
#' @rdname internal-methods
methods::setMethod(
    "mask_matrix", c(object = "ANY", mask = "ANY"),
    function(object, mask, invert = FALSE) {
        cli::cli_abort(
            "{.arg object} must be a {.cls BPCellsSeed} or {.cls BPCellsMatrix} object"
        )
    }
)
