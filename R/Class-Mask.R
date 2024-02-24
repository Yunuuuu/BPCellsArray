methods::setClass("BPCellsDelayedMask", contains = "VIRTUAL")

mould_BPCells("BPCellsDelayedMaskNaryIsoOp", "MatrixMask",
    delete = c("matrix", "mask"),
    contains = c("BPCellsDelayedMask", "BPCellsDelayedNaryIsoOp")
)

mould_BPCells("BPCellsDelayedMaskUnaryIsoOp", "MatrixMask",
    delete = "matrix",
    contains = c("BPCellsDelayedMask", "BPCellsDelayedUnaryIsoOp")
)

methods::setMethod("to_DelayedArray", "MatrixMask", function(object) {
    slots <- setdiff(methods::slotNames(object), c("matrix", "mask"))
    names(slots) <- slots
    slots <- lapply(slots, methods::slot, object = object)
    matrix <- to_DelayedArray(object@matrix)
    mask <- to_DelayedArray(object@mask)
    if (methods::is(DelayedArray::seed(mask), "Iterable_dgCMatrix_wrapper")) {
        slots$seed <- matrix
        slots$mask <- object@mask
        Class <- "BPCellsDelayedMaskUnaryIsoOp"
    } else {
        slots$seeds <- list(matrix = matrix, mask = mask)
        Class <- "BPCellsDelayedMaskNaryIsoOp"
    }
    rlang::inject(S4Vectors::new2(Class = Class, !!!slots, check = FALSE))
})

methods::setMethod("to_BPCells", "BPCellsDelayedMaskNaryIsoOp", function(object) {
    slots <- setdiff(methods::slotNames(object), c("seeds", "OP", "Rargs"))
    names(slots) <- slots
    slots <- lapply(slots, methods::slot, object = object)
    seeds <- object@seeds
    slots$matrix <- to_BPCells(seeds$matrix)
    slots$mask <- to_BPCells(seeds$mask)
    rlang::inject(
        S4Vectors::new2(Class = "MatrixMask", !!!slots, check = FALSE)
    )
})

methods::setMethod("to_BPCells", "BPCellsDelayedMaskUnaryIsoOp", function(object) {
    methods::callNextMethod(object = object, Class = "MatrixMask")
})

summary.BPCellsDelayedMask <- function(object) {
    out <- "Mask entries"
    if (object@invert) out <- paste(out, "(inverted)")
    out
}
methods::setMethod("summary", "BPCellsDelayedMask", summary.BPCellsDelayedMask)

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
    "mask_matrix", c(object = "BPCellsMatrix", mask = "BPCellsMatrix"),
    function(object, mask, invert = FALSE) {
        object <- BPCells:::mask_matrix(
            mat = to_BPCells(object@seed),
            mask = to_BPCells(mask@seed),
            invert = invert
        )
        DelayedArray(to_DelayedArray(object))
    }
)

#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix", c(object = "BPCellsMatrix", mask = "dgCMatrix"),
    function(object, mask, invert = FALSE) {
        object <- BPCells:::mask_matrix(
            mat = to_BPCells(object@seed),
            mask = BPCellsSeed(mask@seed),
            invert = invert
        )
        DelayedArray(to_DelayedArray(object))
    }
)

#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix", c(object = "BPCellsMatrix", mask = "ANY"),
    function(object, mask, invert = FALSE) {
        mask <- coerce_dgCMatrix(mask)
        methods::callGeneric()
    }
)

#' @inheritParams mask_matrix
#' @export
#' @rdname internal-methods
methods::setMethod(
    "mask_matrix", c(object = "ANY", mask = "ANY"),
    function(object, mask, invert = FALSE) {
        cli::cli_abort("{.arg object} must be a {.cls BPCellsMatrix} object")
    }
)
