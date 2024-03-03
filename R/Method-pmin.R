#' Maxima and Minima
#' @name pmin2
NULL

####################################################################
# TransformMinByCol
methods::setClass("BPCellsDelayedTransformMinByCol",
    contains = "BPCellsDelayedTransformed"
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
    array_call_BPCells_method(
        object = , values = ,
        method = quote(BPCells::min_by_col(mat = object, vals = values))
    )
)

####################################################################
# TransformMinByRow
methods::setClass("BPCellsDelayedTransformMinByRow",
    contains = "BPCellsDelayedTransformed"
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
    array_call_BPCells_method(
        object = , values = ,
        method = quote(BPCells::min_by_row(mat = object, vals = values))
    )
)

####################################################################
# TransformMin
methods::setClass("BPCellsDelayedTransformMin",
    contains = "BPCellsDelayedTransformed"
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
    array_call_BPCells_method(
        object = , value = ,
        method = quote(BPCells::min_scalar(mat = object, val = value))
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

#' @param e1,e2 One of `x` or `y` must be a `r rd_matrix()`, and the another
#' must be a scalar or of length `nrow(e1)/nrow(e2)`.
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
    array_call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmin2", c("vector", "BPCellsMatrix"),
    array_call_DelayedArray_method(e1 = , e2 = , Array = "e2")
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmin2", c("BPCellsMatrix", "DelayedArray"),
    array_call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmin2", c("DelayedArray", "BPCellsMatrix"),
    array_call_DelayedArray_method(e1 = , e2 = , Array = "e2")
)

######################################################################
#' @importFrom DelayedArray pmax2
#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmax2", c("BPCellsMatrix", "vector"),
    array_call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmax2", c("vector", "BPCellsMatrix"),
    array_call_DelayedArray_method(e1 = , e2 = , Array = "e2")
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmax2", c("BPCellsMatrix", "DelayedArray"),
    array_call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "pmax2", c("DelayedArray", "BPCellsMatrix"),
    array_call_DelayedArray_method(e1 = , e2 = , Array = "e2")
)
