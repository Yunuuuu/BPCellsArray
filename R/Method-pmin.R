#' Maxima and Minima
#' @name pmin2
NULL

####################################################################
# TransformMinByCol
methods::setClass("BPCellsTransformMinByCol",
    contains = "BPCellsDelayedTransformed"
)
methods::setMethod(
    "DelayedTransformedClass", "TransformMinByCol",
    function(x) "BPCellsTransformMinByCol"
)

methods::setMethod(
    "BPCellsTransformedClass", "BPCellsTransformMinByCol",
    function(x) "TransformMinByCol"
)

#' @inheritParams convert_mode
#' @param values An atomic positive numeric.
#' @export
#' @rdname pmin2
methods::setGeneric("pmin_by_col", function(object, values) {
    standardGeneric("pmin_by_col")
})

#' @return
#' - `pmin_by_col`: Take the minimum with a per-col constant
#' @export
#' @rdname pmin2
methods::setMethod(
    "pmin_by_col", "BPCellsMatrix",
    set_BPCellsArray_method(
        object = , values = ,
        method = quote(BPCells::min_by_col(mat = object, vals = values)),
        before = expression(delayed <- object@delayed),
        after = expression(with_delayed(delayed, DelayedArray(object)))
    )
)

####################################################################
# TransformMinByRow
methods::setClass("BPCellsTransformMinByRow",
    contains = "BPCellsDelayedTransformed"
)
methods::setMethod(
    "DelayedTransformedClass", "TransformMinByRow",
    function(x) "BPCellsTransformMinByRow"
)

methods::setMethod(
    "BPCellsTransformedClass", "BPCellsTransformMinByRow",
    function(x) "TransformMinByRow"
)

#' @export
#' @rdname pmin2
methods::setGeneric("pmin_by_row", function(object, values) {
    standardGeneric("pmin_by_row")
})

#' @return
#' - `pmin_by_row`: Take the minimum with a per-row constant
#' @export
#' @rdname pmin2
methods::setMethod(
    "pmin_by_row", "BPCellsMatrix",
    set_BPCellsArray_method(
        object = , values = ,
        method = quote(BPCells::min_by_row(mat = object, vals = values)),
        before = expression(delayed <- object@delayed),
        after = expression(with_delayed(delayed, DelayedArray(object)))
    )
)

####################################################################
# TransformMin
methods::setClass("BPCellsTransformMin",
    contains = "BPCellsDelayedTransformed"
)
methods::setMethod(
    "DelayedTransformedClass", "TransformMin",
    function(x) "BPCellsTransformMin"
)

methods::setMethod(
    "BPCellsTransformedClass", "BPCellsTransformMin",
    function(x) "TransformMin"
)

#' @export
#' @rdname pmin2
methods::setGeneric("pmin_scalar", function(object, value) {
    standardGeneric("pmin_scalar")
})

#' @param value A single positive numeric value
#' @return
#' - `pmin_scalar`: Take minumum with a global constant
#' @export
#' @rdname pmin2
methods::setMethod(
    "pmin_scalar", "BPCellsMatrix",
    set_BPCellsArray_method(
        object = , value = ,
        method = quote(BPCells::min_scalar(mat = object, val = value)),
        before = expression(delayed <- object@delayed),
        after = expression(with_delayed(delayed, DelayedArray(object)))
    )
)

.pmin2_internal <- function(
    e1, e2,
    arg1 = rlang::caller_arg(e1),
    arg2 = rlang::caller_arg(e2)) {
    l <- length(e2)
    if (l == 1L) {
        pmin_scalar(e1, e2)
    } else if (l == nrow(e1)) {
        pmin_by_row(e1, e2)
    } else {
        cli::cli_abort(
            "{.arg {arg2}} must be a scalar or of length {.code nrow({arg1})}"
        )
    }
}

#' @param e1,e2 One of `x` or `y` must be [BPCellsMatrix][BPCellsMatrix-class]
#' object, and the another must be a scalar or of length `nrow(e1)/nrow(e2)`.
#' @importFrom DelayedArray pmin2
#' @note For `pmin2` Methods listed here are supported by `BPCells`, other
#' `pmin2` methods and `pmax2` function will use the methods defined in
#' [DelayedArray][DelayedArray-utils].
#' @export
#' @aliases pmax2
#' @rdname pmin2
methods::setMethod("pmin2", c("BPCellsMatrix", "numeric"), function(e1, e2) {
    .pmin2_internal(e1, e2)
})

#' @export
#' @rdname pmin2
methods::setMethod("pmin2", c("numeric", "BPCellsMatrix"), function(e1, e2) {
    .pmin2_internal(e2, e1)
})

#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmin2", c("BPCellsMatrix", "vector"),
    call_DelayedArray_method(e1 = , e2 = , Array = "e1")
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmin2", c("vector", "BPCellsMatrix"),
    call_DelayedArray_method(e1 = , e2 = , Array = "e2")
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmin2", c("BPCellsMatrix", "DelayedArray"),
    call_DelayedArray_method(e1 = , e2 = , Array = "e1")
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmin2", c("DelayedArray", "BPCellsMatrix"),
    call_DelayedArray_method(e1 = , e2 = , Array = "e2")
)

######################################################################
#' @importFrom DelayedArray pmax2
#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmax2", c("BPCellsMatrix", "vector"),
    call_DelayedArray_method(e1 = , e2 = , Array = "e1")
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmax2", c("vector", "BPCellsMatrix"),
    call_DelayedArray_method(e1 = , e2 = , Array = "e2")
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmax2", c("BPCellsMatrix", "DelayedArray"),
    call_DelayedArray_method(e1 = , e2 = , Array = "e1")
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmax2", c("DelayedArray", "BPCellsMatrix"),
    call_DelayedArray_method(e1 = , e2 = , Array = "e2")
)
