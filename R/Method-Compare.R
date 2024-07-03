####################################################################
# TransformBinarize
methods::setClass("BPCellsDelayedTransformBinarize",
    contains = "BPCellsDelayedTransformed"
)

#########################################################################
#' @inherit BPCellsDir-IO return
#' @inherit BPCells::binarize
#' @name BPCells-Compare
NULL

#' @inheritParams convert_mode
#' @export
#' @rdname BPCells-Compare
#' @aliases binarize
methods::setGeneric("binarize", function(object, ...) {
    standardGeneric("binarize")
})

#' @inheritDotParams BPCells::binarize -mat
#' @export
#' @rdname BPCells-Compare
methods::setMethod("binarize", "BPCellsMatrix", function(object, ...) {
    object <- to_BPCells(object@seed)
    ans <- BPCells::binarize(mat = object, ...)
    DelayedArray(ans)
})

#' @export
#' @rdname internal-methods
methods::setMethod("binarize", "ANY", function(object, ...) {
    cli::cli_abort("{.arg object} must be a {.cls BPCellsMatrix} object")
})

#########################################################################
# binary operations ---------------------------------------------
#' @note Methods listed here are supported by `BPCells`, other
#' [Compare][methods::Compare] operators will use the methods defined in
#' [DelayedArray][DelayedArray-utils].
#' @inheritParams BPCells-Arithmetic
#' @export
#' @rdname BPCells-Compare
methods::setMethod(
    "<", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        binarize(e2, threshold = e1, strict_inequality = TRUE)
    }
)

#' @export
#' @rdname BPCells-Compare
methods::setMethod(
    ">", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        binarize(e1, threshold = e2, strict_inequality = TRUE)
    }
)

#' @export
#' @rdname BPCells-Compare
methods::setMethod(
    "<=", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        binarize(e2, threshold = e1, strict_inequality = FALSE)
    }
)

#' @export
#' @rdname BPCells-Compare
methods::setMethod(
    ">=", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        binarize(e1, threshold = e2, strict_inequality = FALSE)
    }
)

##################################################################
#' @importFrom methods Compare
#' @export
#' @rdname internal-methods
methods::setMethod(
    "Compare", c(e1 = "BPCellsMatrix", e2 = "ANY"),
    function(e1, e2) {
        object <- methods::callNextMethod()
        return_BPCellsMatrix(object, .Generic) # nolint
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "Compare", c(e1 = "ANY", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        object <- methods::callNextMethod()
        return_BPCellsMatrix(object, .Generic) # nolint
    }
)
