####################################################################
# TransformScaleShift
#' @include Class-Transformed.R
#' @noRd
methods::setClass("BPCellsTransformScaleShiftSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformScaleShift"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformScaleShift",
    function(x) "TransformScaleShift"
)

###################################################################
###########################  Methods  #############################
###################################################################

#' Arithmetic operators for BPCellsMatrix
#' @inheritParams BPCells-binarize
#' @return A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object depends on the class of `e1` /
#' `e2`.
#' @name BPCells-Arithmetic
#' @aliases + - * /
NULL

#' @rdname BPCells-Arithmetic
#' @export
methods::setMethod(
    "*", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        DelayedArray(e1@seed * e2)
    }
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "*", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        DelayedArray(e1 * e2@seed)
    }
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "+", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        DelayedArray(e1@seed + e2)
    }
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "+", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        DelayedArray(e1 + e2@seed)
    }
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "/", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        DelayedArray(e1@seed / e2)
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "/", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        DelayedArray(e1 / e2@seed)
    }
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "-", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    function(e1, e2) {
        DelayedArray(e1@seed - e2)
    }
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "-", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        DelayedArray(e1 - e2@seed)
    }
)

# ######################################################################
#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "*", c(e1 = "BPCellsSeed", e2 = "numeric"),
    function(e1, e2) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "*", c(e1 = "numeric", e2 = "BPCellsSeed"),
    function(e1, e2) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "+", c(e1 = "BPCellsSeed", e2 = "numeric"),
    function(e1, e2) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "+", c(e1 = "numeric", e2 = "BPCellsSeed"),
    function(e1, e2) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "/", c(e1 = "BPCellsSeed", e2 = "numeric"),
    function(e1, e2) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "/", c(e1 = "numeric", e2 = "BPCellsSeed"),
    function(e1, e2) {
        cli::cli_abort("Cannot dicided by a {.pkg BPCells} sparce martix, since too many zeros")
    }
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "-", c(e1 = "BPCellsSeed", e2 = "numeric"),
    function(e1, e2) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "-", c(e1 = "numeric", e2 = "BPCellsSeed"),
    function(e1, e2) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#####################################################################
#' @export
#' @rdname internal-methods
methods::setMethod(
    "*", c(e1 = "BPCellsTransformScaleShiftSeed", e2 = "numeric"),
    function(e1, e2) {
        # in case of using e1 = "BPCellsSeed" and e2 = "numeric" method
        fn <- methods::getMethod(
            "*", c("TransformScaleShift", "numeric"),
            where = "BPCells"
        )
        BPCellsSeed(fn(e1, e2))
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "+", c(e1 = "BPCellsTransformScaleShiftSeed", e2 = "numeric"),
    function(e1, e2) {
        # in case of using e1 = "BPCellsSeed" and e2 = "numeric" method
        fn <- methods::getMethod(
            "+", c("TransformScaleShift", "numeric"),
            where = "BPCells"
        )
        BPCellsSeed(fn(e1, e2))
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "*", c(e1 = "numeric", e2 = "BPCellsTransformScaleShiftSeed"),
    function(e1, e2) {
        # in case of using e1 = "numeric" and e2 = "BPCellsSeed" method
        fn <- methods::getMethod(
            "*", c("numeric", "TransformScaleShift"),
            where = "BPCells"
        )
        BPCellsSeed(fn(e1, e2))
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "+", c(e1 = "numeric", e2 = "BPCellsTransformScaleShiftSeed"),
    function(e1, e2) {
        # in case of using e1 = "numeric" and e2 = "BPCellsSeed" method
        fn <- methods::getMethod(
            "+", c("numeric", "TransformScaleShift"),
            where = "BPCells"
        )
        BPCellsSeed(fn(e1, e2))
    }
)
