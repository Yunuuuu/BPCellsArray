# Math methods
#' Math operators for BPCellsMatrix
#' @param x A `r rd_matrix()`.
#' @inherit BPCellsDir-IO return
#' @note Methods listed here are supported by `BPCells`, other
#' [Math][methods::Math] or [Math2][methods::Math2] operators will use the
#' methods defined in [DelayedArray][DelayedArray-utils].
#' @name BPCells-Math
NULL

####################################################################
# TransformExpm1Slow
methods::setClass("BPCellsDelayedTransformExpm1Slow",
    contains = "BPCellsDelayedTransformed"
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
    array_call_BPCells_method(x = , method = quote(BPCells::expm1_slow(x)))
)

#' @export
#' @rdname internal-methods
methods::setMethod("expm1_slow", "ANY", function(x) {
    cli::cli_abort("{.arg object} must be a {.cls BPCellsMatrix} object")
})

####################################################################
# TransformExpm1
methods::setClass("BPCellsDelayedTransformExpm1",
    contains = "BPCellsDelayedTransformed"
)

#' @return
#'  - `expm1` and `expm1_slow`: compute `exp(x)-1` of matrix.
#' @export
#' @aliases expm1
#' @rdname BPCells-Math
methods::setMethod(
    "expm1", "BPCellsMatrix",
    array_call_BPCells_method(x = )
)

####################################################################
# TransformLog1pSlow
methods::setClass("BPCellsDelayedTransformLog1pSlow",
    contains = "BPCellsDelayedTransformed"
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
    array_call_BPCells_method(x = , method = quote(BPCells::log1p_slow(x)))
)

####################################################################
# TransformLog1p
methods::setClass("BPCellsDelayedTransformLog1p",
    contains = "BPCellsDelayedTransformed"
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
    array_call_BPCells_method(x = , method = quote(log1p(x)))
)

####################################################################
# TransformRound
methods::setClass("BPCellsDelayedTransformRound",
    contains = "BPCellsDelayedTransformed"
)

#' @param digits Integer indicating the number of decimal places
#' @return
#'  - `round`: Rounding of matrix Numbers.
#' @export
#' @aliases round
#' @rdname BPCells-Math
methods::setMethod(
    "round", "BPCellsMatrix",
    array_call_BPCells_method(x = , digits = 0)
)

# override methods of DelayedArray
#' @export
#' @rdname internal-methods
methods::setMethod("log", "BPCellsMatrix", array_call_DelayedArray_method(x = ))

#' @importFrom methods Math
#' @export
#' @rdname internal-methods
methods::setMethod(
    "Math", "BPCellsArray",
    array_call_DelayedArray_method(x = )
)

#' @importFrom methods Math2
#' @export
#' @rdname internal-methods
methods::setMethod(
    "Math2", "BPCellsArray",
    array_call_DelayedArray_method(x = )
)
