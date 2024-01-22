#' @inherit BPCells::binarize
#' @aliases binarize
#' @name BPCells-binarize
NULL

#' @export
#' @rdname BPCells-binarize
methods::setGeneric("binarize", function(object, ...) {
    makeStandardGeneric("binarize")
})

#' @param object A [BPCellsSeed] or [BPCellsMatrix] object.
#' @inheritParams BPCells::binarize
#' @export
#' @rdname BPCells-binarize
methods::setMethod(
    "binarize", "BPCellsSeed",
    function(object, threshold = 0, strict_inequality = TRUE) {
        obj <- BPCells::binarize(
            mat = object, threshold = threshold,
            strict_inequality = strict_inequality
        )
        BPCellsSeed(obj)
    }
)

#' @param ... Additional parameters passed to `BPCellsSeed` method.
#' @export
#' @rdname BPCells-binarize
methods::setMethod("binarize", "BPCellsMatrix", function(object, ...) {
    DelayedArray(binarize(object = object@seed, ...))
})
