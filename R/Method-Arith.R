####################################################################
# TransformScaleShift
mould_BPCells("BPCellsDelayedTransformeScaleShift",
    "TransformScaleShift",
    c(matrix = "seed"),
    contains = "BPCellsDelayedTransformed"
)

methods::setMethod(
    "DelayedTransformedClass", "TransformScaleShift",
    function(x) "BPCellsDelayedTransformeScaleShift"
)

methods::setMethod(
    "BPCellsTransformedClass", "BPCellsDelayedTransformeScaleShift",
    function(x) "TransformScaleShift"
)

####################################################################
# TransformPow
methods::setClass("BPCellsTransformPow",
    contains = "BPCellsDelayedTransformed"
)

methods::setMethod(
    "DelayedTransformedClass", "TransformPow",
    function(x) "BPCellsTransformPow"
)

methods::setMethod(
    "BPCellsTransformedClass", "BPCellsTransformPow",
    function(x) "TransformPow"
)

####################################################################
# TransformSquare
methods::setClass("BPCellsTransformSquare",
    contains = "BPCellsDelayedTransformed"
)

methods::setMethod(
    "DelayedTransformedClass", "TransformSquare",
    function(x) "BPCellsTransformSquare"
)

methods::setMethod(
    "BPCellsTransformedClass", "BPCellsTransformSquare",
    function(x) "TransformSquare"
)

###################################################################
###########################  Methods  #############################
###################################################################
# Arith
#' Arithmetic operators for BPCellsMatrix
#' @inheritParams BPCells-binarize
#' @return A [BPCellsSeed][BPCellsSeed-class] or
#' [BPCellsMatrix][BPCellsMatrix-class] object depends on the class of `e1` /
#' `e2`.
#' @name BPCells-Arithmetic
#' @aliases BPCells-Arith
NULL

# ######################################################################
#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "Arith", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    set_BPCellsArray_method(
        e1 = , e2 = ,
        after = expression(DelayedArray(to_DelayedArray(object))),
        Arrays = "e1"
    )
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "Arith", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    set_BPCellsArray_method(
        e1 = , e2 = ,
        after = expression(DelayedArray(to_DelayedArray(object))),
        Arrays = "e2"
    )
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "/", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    function(e1, e2) {
        cli::cli_abort("Cannot dicided by a sparce {.cls BPCellsMatrix}, since too many zeros")
    }
)

# "%%", "%/%"
#' @export
#' @rdname internal-methods
methods::setMethod(
    "%%", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "%%", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "%/%", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "%/%", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    call_DelayedArray_method(e1 = , e2 = )
)

#######################################################################
# TransformPowSlow
methods::setClass("BPCellsTransformPowSlow",
    contains = "BPCellsDelayedTransformed"
)
methods::setMethod(
    "DelayedTransformedClass", "TransformPowSlow",
    function(x) "BPCellsTransformPowSlow"
)

methods::setMethod(
    "BPCellsTransformedClass", "BPCellsTransformPowSlow",
    function(x) "TransformPowSlow"
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setGeneric("pow_slow", function(e1, e2) {
    standardGeneric("pow_slow")
})

#' @inheritParams BPCells-binarize
#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "pow_slow", "BPCellsMatrix",
    set_BPCellsArray_method(
        e1 = , e2 = ,
        method = quote(BPCells::pow_slow(x = e1, exponent = e2)),
        after = expression(DelayedArray(to_DelayedArray(object))),
        Arrays = "e1"
    )
)
