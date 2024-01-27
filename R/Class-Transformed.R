#' @include Class-BPCellsSeed.R Seed-Methods.R Matrix-Methods.R
#' @noRd
methods::setClass("BPCellsTransformedSeed",
    contains = c(
        "BPCellsUnaryOpsSeed",
        get_class("TransformedMatrix")
    ),
    slots = list(matrix = "BPCellsSeed")
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "TransformedMatrix", function(x) {
    BPCellsTransformedSeed(x = x)
})

#' @noRd
methods::setGeneric("BPCellsTransformedSeed", function(x) {
    x@matrix <- BPCellsSeed(x@matrix)
    class <- standardGeneric("BPCellsTransformedSeed")
    methods::as(x, Class = sprintf("BPCells%sSeed", class))
})

####################################################################
#' @noRd
methods::setMethod("BPCellsTransformedSeed", "TransformedMatrix", function(x) {
    "Transformed"
})

####################################################################
# TransformBinarize
methods::setClass("BPCellsTransformBinarizeSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformBinarize"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformBinarize",
    function(x) "TransformBinarize"
)

####################################################################
# TransformExpm1Slow
methods::setClass("BPCellsTransformExpm1SlowSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformExpm1Slow"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformExpm1Slow",
    function(x) "TransformExpm1Slow"
)

#' @export
#' @rdname BPCellsMatrix-methods
methods::setGeneric("expm1_slow", function(x) {
    makeStandardGeneric("expm1_slow")
})

#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("expm1_slow", "BPCellsSeed", function(x) {
    BPCellsSeed(BPCells::expm1_slow(x))
})

#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod("expm1_slow", "BPCellsMatrix", function(x) {
    DelayedArray(expm1_slow(x@seed))
})

####################################################################
# TransformLog1pSlow
methods::setClass("BPCellsTransformLog1pSlowSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformLog1pSlow"))
)
#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformLog1pSlow",
    function(x) "TransformLog1pSlow"
)

#' @export
#' @rdname BPCellsMatrix-methods
methods::setGeneric("log1p_slow", function(x) {
    makeStandardGeneric("log1p_slow")
})

#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("log1p_slow", "BPCellsSeed", function(x) {
    BPCellsSeed(BPCells::log1p_slow(x))
})

#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod("log1p_slow", "BPCellsMatrix", function(x) {
    DelayedArray(log1p_slow(x@seed))
})

####################################################################
# TransformExpm1
methods::setClass("BPCellsTransformExpm1Seed",
    contains = c("BPCellsTransformedSeed", get_class("TransformExpm1"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformExpm1",
    function(x) "TransformExpm1"
)

#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("expm1", "BPCellsSeed", function(x) {
    BPCellsSeed(methods::callNextMethod())
})

#' @return
#'  - `expm1` and `expm1_slow`: compute `exp(x)-1` of matrix.
#' @export
#' @aliases expm1
#' @rdname BPCellsMatrix-methods
methods::setMethod("expm1", "BPCellsMatrix", function(x) {
    DelayedArray(expm1(x@seed))
})

####################################################################
# TransformLog1p
methods::setClass("BPCellsTransformLog1pSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformLog1p"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformLog1p",
    function(x) "TransformLog1p"
)

#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("log1p", "BPCellsSeed", function(x) {
    BPCellsSeed(methods::callNextMethod())
})

#' @return
#'  - `log1p` and `log1p_slow`: compute `log(1+x)` of matrix.
#' @export
#' @aliases log1p
#' @rdname BPCellsMatrix-methods
methods::setMethod("log1p", "BPCellsMatrix", function(x) {
    DelayedArray(log1p(x@seed))
})

####################################################################
# TransformMinByCol
methods::setClass("BPCellsTransformMinByColSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformMinByCol"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformMinByCol",
    function(x) "TransformMinByCol"
)

#' @export
#' @rdname BPCellsMatrix-methods
methods::setGeneric("pmin_by_col", function(object, values) {
    makeStandardGeneric("pmin_by_col")
})

#' @param values A positive atomic numeric.
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod(
    "pmin_by_col", "BPCellsSeed", function(object, values) {
        BPCellsSeed(BPCells::min_by_col(mat = object, vals = values))
    }
)

#' @inheritParams BPCellsSeed-methods
#' @return
#' - `pmin_by_col`: Take the minimum with a per-col constant
#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod(
    "pmin_by_col", "BPCellsMatrix", function(object, values) {
        DelayedArray(pmin_by_col(object = object, values = values))
    }
)

####################################################################
# TransformMinByRow
methods::setClass("BPCellsTransformMinByRowSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformMinByRow"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformMinByRow",
    function(x) "TransformMinByRow"
)

#' @export
#' @rdname BPCellsMatrix-methods
methods::setGeneric("pmin_by_row", function(object, values) {
    makeStandardGeneric("pmin_by_row")
})

#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod(
    "pmin_by_row", "BPCellsSeed", function(object, values) {
        BPCellsSeed(BPCells::min_by_row(mat = object, vals = values))
    }
)

#' @return
#' - `pmin_by_row`: Take the minimum with a per-row constant
#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod(
    "pmin_by_row", "BPCellsMatrix", function(object, values) {
        DelayedArray(pmin_by_row(object = object, values = values))
    }
)

####################################################################
# TransformMin

methods::setClass("BPCellsTransformMinSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformMin"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformMin", function(x) "TransformMin"
)

#' @export
#' @rdname BPCellsMatrix-methods
methods::setGeneric("pmin_scalar", function(object, value) {
    makeStandardGeneric("pmin_scalar")
})

#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod(
    "pmin_scalar", "BPCellsSeed", function(object, value) {
        BPCellsSeed(BPCells::min_scalar(mat = object, val = value))
    }
)

#' @return
#' - `pmin_scalar`: Take minumum with a global constant
#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod(
    "pmin_scalar", "BPCellsMatrix", function(object, value) {
        DelayedArray(pmin_scalar(object = object, value = value))
    }
)

####################################################################
# TransformPowSlow
methods::setClass("BPCellsTransformPowSlowSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformPowSlow"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformPowSlow", function(x) "TransformPowSlow"
)

#' @export
#' @rdname BPCellsMatrix-methods
methods::setGeneric("pow_slow", function(e1, e2) {
    makeStandardGeneric("pow_slow")
})

#' @inheritParams BPCells-binarize
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("pow_slow", "BPCellsSeed", function(e1, e2) {
    BPCellsSeed(BPCells::pow_slow(x = e1, exponent = e2))
})

#' @inheritParams BPCells-binarize
#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod("pow_slow", "BPCellsMatrix", function(e1, e2) {
    DelayedArray(pow_slow(e1@seed, e2))
})

####################################################################
# TransformPow
methods::setClass("BPCellsTransformPowSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformPow"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformPow", function(x) "TransformPow"
)

#' @importMethodsFrom BPCells ^
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("^", "BPCellsSeed", function(e1, e2) {
    BPCellsSeed(methods::callNextMethod())
})

#' @export
#' @aliases ^
#' @rdname BPCellsMatrix-methods
methods::setMethod("^", "BPCellsMatrix", function(e1, e2) {
    e1 <- e1@seed
    DelayedArray(e1^e2)
})

####################################################################
# TransformSquare
methods::setClass("BPCellsTransformSquareSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformSquare"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformSquare", function(x) "TransformSquare"
)

####################################################################
# TransformRound
methods::setClass("BPCellsTransformRoundSeed",
    contains = c("BPCellsTransformedSeed", get_class("TransformRound"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformRound", function(x) "TransformRound"
)

#' @param digits Integer indicating the number of decimal places
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod(
    "round", "BPCellsSeed", function(x, digits = 0) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @inheritParams BPCellsSeed-methods
#' @return
#'  - `round`: Rounding of matrix Numbers.
#' @export
#' @aliases round
#' @rdname BPCellsMatrix-methods
methods::setMethod(
    "round", "BPCellsMatrix", function(x, digits = 0) {
        DelayedArray(round(x = x, digits = digits))
    }
)
