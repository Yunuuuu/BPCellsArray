#' @importClassesFrom DelayedArray DelayedNaryIsoOp
mould_BPCells("BPCellsDelayedMaskNaryIsoOp", "MatrixMask",
    remove = c("matrix", "mask"),
    # DelayedNaryIsoOp: `seeds` slot
    contains = c("BPCellsDelayedOp", "DelayedNaryIsoOp")
)

#' @importClassesFrom DelayedArray DelayedUnaryIsoOp
mould_BPCells("BPCellsDelayedMaskUnaryIsoOp", "MatrixMask",
    remove = "matrix",
    # DelayedUnaryIsoOp: `seed` slot
    contains = c("BPCellsDelayedOp", "DelayedUnaryIsoOp")
)

###################################################################
methods::setMethod("to_DelayedArray", "MatrixMask", function(object) {
    slots <- setdiff(methods::slotNames(object), c("matrix", "mask"))
    names(slots) <- slots
    slots <- lapply(slots, methods::slot, object = object)
    matrix <- to_DelayedArray(object@matrix)
    mask <- object@mask
    if (is_BPCellsInDisk(mask)) {
        slots$seeds <- list(
            matrix = matrix,
            mask = to_DelayedArray(mask)
        )
        Class <- "BPCellsDelayedMaskNaryIsoOp"
    } else {
        slots$seed <- matrix
        slots$mask <- mask
        Class <- "BPCellsDelayedMaskUnaryIsoOp"
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
    to_BPCellsUnaryOp(object = object, Class = "MatrixMask")
})

summary.MatrixMask <- function(object) {
    out <- "Mask entries"
    if (object@invert) out <- paste(out, "(inverted)")
    out
}
methods::setMethod("summary", "MatrixMask", summary.MatrixMask)

summary.BPCellsDelayedMaskNaryIsoOp <- summary.MatrixMask
methods::setMethod("summary", "BPCellsDelayedMaskNaryIsoOp", summary.MatrixMask)

summary.BPCellsDelayedMaskUnaryIsoOp <- summary.MatrixMask
methods::setMethod(
    "summary", "BPCellsDelayedMaskUnaryIsoOp",
    summary.MatrixMask
)

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

#' @inheritParams convert_mode
#' @param mask A `r rd_matrix()` or a `r rd_seed()`
#' @param invert A bool, indicates whether revert the mask.
#' @inherit BPCellsDir-IO return
#' @seealso [mask_matrix][BPCells::mask_matrix]
#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix", c(object = "BPCellsMatrix", mask = "BPCellsMatrix"),
    array_call_BPCells_method(
        object = , mask = , invert = FALSE,
        method = quote(
            BPCells:::mask_matrix(
                mat = object,
                mask = mask,
                invert = invert
            )
        ),
        Arrays = c("object", "mask")
    )
)

#' @export
#' @rdname mask_matrix
methods::setMethod(
    "mask_matrix", c(object = "BPCellsMatrix", mask = "ANY"),
    array_call_BPCells_method(
        object = , mask = , invert = FALSE,
        before2 = expression(mask <- BPCellsSeed(mask)),
        method = quote(
            BPCells:::mask_matrix(
                mat = object,
                mask = mask,
                invert = invert
            )
        )
    )
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
