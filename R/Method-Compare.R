####################################################################
# TransformBinarize
methods::setClass("BPCellsTransformBinarize",
    contains = "BPCellsDelayedTransformed"
)

methods::setMethod(
    "DelayedTransformedClass", "TransformBinarize",
    function(x) "BPCellsTransformBinarize"
)

methods::setMethod(
    "BPCellsTransformedClass", "BPCellsTransformBinarize",
    function(x) "TransformBinarize"
)

#########################################################################
#' @inherit BPCells::binarize
#' @aliases binarize
#' @return A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object depends on the class of
#' `object` or `e1` (`e2`).
#' @name BPCells-binarize
NULL

#' @param object A [BPCellsMatrix] object.
#' @export
#' @rdname BPCells-binarize
methods::setGeneric("binarize", function(object, ...) {
    standardGeneric("binarize")
})

#' @inheritDotParams BPCells::binarize -mat
#' @export
#' @rdname BPCells-binarize
methods::setMethod(
    "binarize", "BPCellsMatrix",
    set_BPCellsArray_method(
        object = , ... = ,
        method = quote(BPCells::binarize(mat = object, ...)),
        after = expression(DelayedArray(to_DelayedArray(object)))
    )
)

#' @export
#' @rdname internal-methods
methods::setMethod("binarize", "ANY", function(object, ...) {
    cli::cli_abort("{.arg object} must be a {.cls BPCellsMatrix} object")
})

#########################################################################
# binary operations ---------------------------------------------
#' @inheritParams BPCells-binarize
#' @export
#' @rdname BPCells-binarize
methods::setMethod(
    "<", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        binarize(e2, threshold = e1, strict_inequality = TRUE)
    }
)

#' @export
#' @rdname BPCells-binarize
methods::setMethod(
    ">", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        binarize(e1, threshold = e2, strict_inequality = TRUE)
    }
)

#' @export
#' @rdname BPCells-binarize
methods::setMethod(
    "<=", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        binarize(e2, threshold = e1, strict_inequality = FALSE)
    }
)

#' @export
#' @rdname BPCells-binarize
methods::setMethod(
    ">=", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        binarize(e1, threshold = e2, strict_inequality = FALSE)
    }
)

##################################################################
#' @export
#' @rdname internal-methods
methods::setMethod(
    "<", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        cli::cli_abort(
            "Cannot compare {.cls BPCellsMatrix} smaller than a number"
        )
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    ">", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        cli::cli_abort(
            "Cannot compare {.cls BPCellsMatrix} smaller than a number"
        )
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "<=", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        cli::cli_abort(
            "Cannot compare {.cls BPCellsMatrix} smaller than a number"
        )
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    ">=", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        cli::cli_abort(
            "Cannot compare {.cls BPCellsMatrix} smaller than a number"
        )
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "==", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        cli::cli_abort(
            "Cannot compare {.cls BPCellsMatrix} equal with a number"
        )
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "==", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        cli::cli_abort(
            "Cannot compare {.cls BPCellsMatrix} equal with a number"
        )
    }
)
