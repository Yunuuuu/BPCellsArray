####################################################################
# TransformScaleShift
mould_BPCells("BPCellsDelayedTransformScaleShift",
    "TransformScaleShift",
    remove = "matrix",
    # BPCellsDelayedTransformed: `seed` slot
    contains = "BPCellsDelayedTransformed"
)

####################################################################
# TransformPow
methods::setClass("BPCellsDelayedTransformPow",
    contains = "BPCellsDelayedTransformed"
)

####################################################################
# TransformSquare
methods::setClass("BPCellsDelayedTransformSquare",
    contains = "BPCellsDelayedTransformed"
)

###################################################################
###########################  Methods  #############################
###################################################################
# Arith
#' Arithmetic operators for BPCellsMatrix
#' @param e1,e2 One of `e1` or `e2` must be a `r rd_matrix()`, and the another
#' can be a `r rd_matrix()` or a `r rd_seed()`.
#' @inherit BPCellsDir-IO return
#' @section Arithmetic operators:
#' * `BPCells`: `+`, `-`, `*`, `/`, `^`
#' * `DelayedArray`: `%%`, - `%/%`
#' @seealso [BPCellsMatrix]
#' @name BPCells-Arithmetic
#' @aliases BPCells-Arith
NULL

# ######################################################################
#' @importFrom methods Arith
#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "Arith", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        e1 <- to_BPCells(e1@seed)
        ans <- methods::callGeneric()
        DelayedArray(ans)
    }
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "Arith", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        e2 <- to_BPCells(e2@seed)
        ans <- methods::callGeneric()
        DelayedArray(ans)
    }
)

#' @inheritParams BPCells-Arithmetic
#' @export
#' @rdname internal-methods
methods::setMethod(
    "/", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        cli::cli_abort(paste(
            "Cannot dicided by a Sparse {.cls BPCellsMatrix},",
            "since too many zeros"
        ))
    }
)

# "%%", "%/%"
#' @export
#' @rdname internal-methods
methods::setMethod(
    "%%", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        ans <- methods::callNextMethod()
        return_BPCellsMatrix(ans, .Generic) # nolint
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "%%", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        ans <- methods::callNextMethod()
        return_BPCellsMatrix(ans, .Generic) # nolint
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "%/%", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        ans <- methods::callNextMethod()
        return_BPCellsMatrix(ans, .Generic) # nolint
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "%/%", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        ans <- methods::callNextMethod()
        return_BPCellsMatrix(ans, .Generic) # nolint
    }
)
