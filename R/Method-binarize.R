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
    makeStandardGeneric("binarize")
})

#########################################################################
#' @inheritDotParams BPCells::binarize -mat
#' @export
#' @rdname BPCells-binarize
methods::setMethod("binarize", "BPCellsMatrix", function(object, ...) {
    DelayedArray(binarize(object = object@seed, ...))
})

#' @param e1,e2 One of `e1` or `e2` must be [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object, and the another must be a
#' number.
#' @export
#' @rdname BPCells-binarize
methods::setMethod(
    "<", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        binarize(e2, threshold = e1, strict_inequality = TRUE)
    }
)

#' @inheritParams BPCells-binarize
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
#' @rdname BPCells-binarize
methods::setMethod(
    ">", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        binarize(e1, threshold = e2, strict_inequality = TRUE)
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
#' @rdname BPCells-binarize
methods::setMethod(
    "<=", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        binarize(e2, threshold = e1, strict_inequality = FALSE)
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
#' @rdname BPCells-binarize
methods::setMethod(
    ">=", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        binarize(e1, threshold = e2, strict_inequality = FALSE)
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


########################################################################
#' @inheritParams BPCells::binarize
#' @export
#' @rdname BPCells-binarize
methods::setMethod(
    "binarize", "BPCellsSeed",
    function(object, ...) {
        obj <- BPCells::binarize(mat = object, ...)
        BPCellsSeed(obj)
    }
)

# binary operations ---------------------------------------------
#' @inheritParams BPCells-binarize
#' @export
#' @rdname BPCells-binarize
methods::setMethod(
    "<", c(e1 = "numeric", e2 = "BPCellsSeed"),
    function(e1, e2) {
        binarize(e2, threshold = e1, strict_inequality = TRUE)
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "<", c(e1 = "BPCellsSeed", e2 = "numeric"),
    function(e1, e2) {
        cli::cli_abort(
            "Cannot compare {.cls BPCellsSeed} smaller than a number"
        )
    }
)

#' @export
#' @rdname BPCells-binarize
methods::setMethod(
    ">", c(e1 = "BPCellsSeed", e2 = "numeric"),
    function(e1, e2) {
        binarize(e1, threshold = e2, strict_inequality = TRUE)
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    ">", c(e1 = "numeric", e2 = "BPCellsSeed"),
    function(e1, e2) {
        cli::cli_abort(
            "Cannot compare {.cls BPCellsSeed} smaller than a number"
        )
    }
)

#' @export
#' @rdname BPCells-binarize
methods::setMethod(
    "<=", c(e1 = "numeric", e2 = "BPCellsSeed"),
    function(e1, e2) {
        binarize(e2, threshold = e1, strict_inequality = FALSE)
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "<=", c(e1 = "BPCellsSeed", e2 = "numeric"),
    function(e1, e2) {
        cli::cli_abort(
            "Cannot compare {.cls BPCellsSeed} smaller than a number"
        )
    }
)

#' @export
#' @rdname BPCells-binarize
methods::setMethod(
    ">=", c(e1 = "BPCellsSeed", e2 = "numeric"),
    function(e1, e2) {
        binarize(e1, threshold = e2, strict_inequality = FALSE)
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    ">=", c(e1 = "numeric", e2 = "BPCellsSeed"),
    function(e1, e2) {
        cli::cli_abort(
            "Cannot compare {.cls BPCellsSeed} smaller than a number"
        )
    }
)
