# Math methods
#' Math operators for BPCellsMatrix
#' @param x A [BPCellsMatrix][BPCellsMatrix-class] object
#' @inherit BPCellsDir-IO return
#' @note Methods listed here are supported by `BPCells`, other
#' [Math][methods::Math] or [Math2][methods::Math2] operators will use the
#' methods defined in [DelayedArray][DelayedArray-utils].
#' @name BPCells-Math
NULL

####################################################################
# TransformExpm1Slow
methods::setClass("BPCellsTransformExpm1Slow",
    contains = "BPCellsDelayedTransformed"
)
methods::setMethod(
    "DelayedTransformedClass", "TransformExpm1Slow",
    function(x) "BPCellsTransformExpm1Slow"
)

methods::setMethod(
    "BPCellsTransformedClass", "BPCellsTransformExpm1Slow",
    function(x) "TransformExpm1Slow"
)

#' @export
#' @rdname BPCells-Math
methods::setGeneric("expm1_slow", function(x) {
    standardGeneric("expm1_slow")
})

#' @export
#' @rdname BPCells-Math
methods::setMethod(
    "expm1_slow", "BPCellsMatrix",
    set_BPCellsArray_method(
        x = , method = quote(BPCells::expm1_slow(x)),
        before = expression(delayed <- x@delayed),
        after = expression(with_delayed(delayed, DelayedArray(object))),
        Arrays = "x"
    )
)

#' @export
#' @rdname internal-methods
methods::setMethod("expm1_slow", "ANY", function(x) {
    cli::cli_abort("{.arg object} must be a {.cls BPCellsMatrix} object")
})

####################################################################
# TransformExpm1
methods::setClass("BPCellsTransformExpm1",
    contains = "BPCellsDelayedTransformed"
)
methods::setMethod(
    "DelayedTransformedClass", "TransformExpm1",
    function(x) "BPCellsTransformExpm1"
)

methods::setMethod(
    "BPCellsTransformedClass", "BPCellsTransformExpm1",
    function(x) "TransformExpm1"
)

#' @return
#'  - `expm1` and `expm1_slow`: compute `exp(x)-1` of matrix.
#' @export
#' @aliases expm1
#' @rdname BPCells-Math
methods::setMethod(
    "expm1", "BPCellsMatrix",
    set_BPCellsArray_method(
        x = ,
        before = expression(delayed <- x@delayed),
        after = expression(with_delayed(delayed, DelayedArray(object))),
        Arrays = "x"
    )
)

####################################################################
# TransformLog1pSlow
methods::setClass("BPCellsTransformLog1p",
    contains = "BPCellsDelayedTransformed"
)
methods::setMethod(
    "DelayedTransformedClass", "TransformLog1pSlow",
    function(x) "BPCellsTransformLog1p"
)

methods::setMethod(
    "BPCellsTransformedClass", "BPCellsTransformLog1p",
    function(x) "TransformLog1pSlow"
)

#' @return
#'  - `log1p` and `log1p_single`: compute `log(1+x)` of matrix. `log1p_single`
#'    use single-precision math in intermediate stages, which is 2x faster than
#'    `log1p`.
#' @export
#' @aliases log1p
#' @rdname BPCells-Math
methods::setMethod(
    "log1p", "BPCellsMatrix",
    set_BPCellsArray_method(
        x = , method = quote(BPCells::log1p_slow(x)),
        before = expression(delayed <- x@delayed),
        after = expression(with_delayed(delayed, DelayedArray(object))),
        Arrays = "x"
    )
)

####################################################################
# TransformLog1p
methods::setClass("BPCellsTransformLog1pSingle",
    contains = "BPCellsDelayedTransformed"
)
methods::setMethod(
    "DelayedTransformedClass", "TransformLog1p",
    function(x) "BPCellsTransformLog1pSingle"
)

methods::setMethod(
    "BPCellsTransformedClass", "BPCellsTransformLog1pSingle",
    function(x) "TransformLog1p"
)

#' @export
#' @aliases log1p_single
#' @rdname BPCells-Math
methods::setGeneric("log1p_single", function(x) {
    standardGeneric("log1p_single")
})

#' @export
#' @rdname BPCells-Math
methods::setMethod(
    "log1p_single", "BPCellsMatrix",
    set_BPCellsArray_method(
        x = , method = quote(log1p(x)),
        before = expression(delayed <- x@delayed),
        after = expression(with_delayed(delayed, DelayedArray(object))),
        Arrays = "x"
    )
)

####################################################################
# TransformRound
methods::setClass("BPCellsTransformRound",
    contains = "BPCellsDelayedTransformed"
)
methods::setMethod(
    "DelayedTransformedClass", "TransformRound",
    function(x) "BPCellsTransformRound"
)

methods::setMethod(
    "BPCellsTransformedClass", "BPCellsTransformRound",
    function(x) "TransformRound"
)

#' @param digits Integer indicating the number of decimal places
#' @return
#'  - `round`: Rounding of matrix Numbers.
#' @export
#' @aliases round
#' @rdname BPCells-Math
methods::setMethod(
    "round", "BPCellsMatrix",
    set_BPCellsArray_method(
        x = , digits = 0,
        before = expression(delayed <- x@delayed),
        after = expression(with_delayed(delayed, DelayedArray(object))),
        Arrays = "x"
    )
)

#' @importFrom methods Math
#' @export
#' @rdname internal-methods
methods::setMethod(
    "Math", "BPCellsArray",
    call_DelayedArray_method(x = )
)

#' @importFrom methods Math2
#' @export
#' @rdname internal-methods
methods::setMethod(
    "Math2", "BPCellsArray",
    call_DelayedArray_method(x = )
)
