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

#' @export
#' @rdname pmin2
methods::setGeneric("pmin_by_col", function(object, values) {
    standardGeneric("pmin_by_col")
})

#' @param values A positive atomic numeric.
#' @inheritParams pmin2
#' @return
#' - `pmin_by_col`: Take the minimum with a per-col constant
#' @export
#' @rdname pmin2
methods::setMethod(
    "pmin_by_col", "BPCellsMatrix",
    set_BPCellsArray_method(
        object = , values = ,
        method = quote(BPCells::min_by_col(mat = object, vals = values)),
        after = expression(DelayedArray(to_DelayedArray(object)))
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
        after = expression(DelayedArray(to_DelayedArray(object)))
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

#' @return
#' - `pmin_scalar`: Take minumum with a global constant
#' @export
#' @rdname pmin2
methods::setMethod(
    "pmin_scalar", "BPCellsMatrix",
    set_BPCellsArray_method(
        object = , value = ,
        method = quote(BPCells::min_scalar(mat = object, val = value)),
        after = expression(DelayedArray(to_DelayedArray(object)))
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

#' @importFrom DelayedArray pmin2
#' @export
#' @rdname internal-methods
methods::setMethod("pmin2", c("BPCellsMatrix", "numeric"), function(e1, e2) {
    .pmin2_internal(e1, e2)
})

#' @export
#' @rdname internal-methods
methods::setMethod("pmin2", c("numeric", "BPCellsMatrix"), function(e1, e2) {
    .pmin2_internal(e2, e1)
})

#' @export
#' @rdname pmin2
methods::setMethod(
    "pmin2", c("BPCellsMatrix", "vector"),
    call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname pmin2
methods::setMethod(
    "pmin2", c("vector", "BPCellsMatrix"),
    call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname pmin2
methods::setMethod(
    "pmin2", c("BPCellsMatrix", "DelayedArray"),
    call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname pmin2
methods::setMethod(
    "pmin2", c("DelayedArray", "BPCellsMatrix"),
    call_DelayedArray_method(e1 = , e2 = )
)

######################################################################
#' @importFrom DelayedArray pmax2
#' @export
#' @rdname pmin2
#' @aliases pmax2
methods::setMethod(
    "pmax2", c("BPCellsMatrix", "vector"),
    call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname pmin2
methods::setMethod(
    "pmax2", c("vector", "BPCellsMatrix"),
    call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname pmin2
methods::setMethod(
    "pmax2", c("BPCellsMatrix", "DelayedArray"),
    call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname pmin2
methods::setMethod(
    "pmax2", c("DelayedArray", "BPCellsMatrix"),
    call_DelayedArray_method(e1 = , e2 = )
)
