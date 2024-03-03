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
#' @param e1,e2 One of `x` or `y` must be a `r rd_matrix()`, and the another can
#' be a `r rd_matrix()` or a `r rd_seed()`.
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
    array_call_BPCells_method(e1 = , e2 = )
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "Arith", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    array_call_BPCells_method(e1 = , e2 = , Arrays = "e2")
)

#' @inheritParams BPCells-Arithmetic
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
    array_call_DelayedArray_method(e1 = , e2 = , Array = "e2")
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "%%", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    array_call_DelayedArray_method(e1 = , e2 = )
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "%/%", c(e1 = "numeric", e2 = "BPCellsMatrix"),
    array_call_DelayedArray_method(e1 = , e2 = , Array = "e2")
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "%/%", c(e1 = "BPCellsMatrix", e2 = "numeric"),
    array_call_DelayedArray_method(e1 = , e2 = )
)

#######################################################################
# TransformPowSlow
methods::setClass("BPCellsDelayedTransformPowSlow",
    contains = "BPCellsDelayedTransformed"
)

#' @export
#' @rdname BPCells-Arithmetic
methods::setGeneric("pow_slow", function(e1, e2) {
    standardGeneric("pow_slow")
})

#' @export
#' @rdname BPCells-Arithmetic
methods::setMethod(
    "pow_slow", "BPCellsMatrix",
    array_call_BPCells_method(
        e1 = , e2 = ,
        method = quote(BPCells::pow_slow(x = e1, exponent = e2))
    )
)
