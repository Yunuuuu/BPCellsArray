methods::setClass("BPCellsMaskSeed",
    contains = c("BPCellsSeed", get_class("MatrixMask"))
)

#' @rdname BPCellsMask
#' @noRd
BPCellsMaskSeed <- function(x) {
    x@matrix <- BPCellsSeed(x@matrix)
    x@mask <- BPCellsSeed(x@mask)
    methods::as(x, "BPCellsMaskSeed")
}


#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixMask", function(x) {
    BPCellsMaskSeed(x = x)
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
#' @return A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object depends on the class of `object`.
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
