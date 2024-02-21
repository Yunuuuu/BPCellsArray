#' @include Class-BPCellsSeed.R Class-BPCellsMatrix.R
#' @noRd
methods::setClass("BPCellsTransformedSeed",
    contains = c(
        "BPCellsUnaryOpsSeed",
        BPCells_class("TransformedMatrix")
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
summary.BPCellsTransformedSeed <- function(object) {
    cls <- gsub("^BPCellsTransform|Seed$", "", class(object)[1L])
    sprintf(
        "Transform by %s",
        switch(cls,
            ScaleShift = "scale and (or) shift",
            Expm1Slow = "expm1_slow",
            Min = "pmin_scalar",
            MinByCol = ,
            MinByRow = paste0("p", snakeize(cls)),
            snakeize(cls)
        )
    )
}
methods::setMethod(
    "summary", "BPCellsTransformedSeed",
    summary.BPCellsTransformedSeed
)

####################################################################
#' @noRd
methods::setMethod("BPCellsTransformedSeed", "TransformedMatrix", function(x) {
    "Transformed"
})

####################################################################
# TransformBinarize
methods::setClass("BPCellsTransformBinarizeSeed",
    contains = c("BPCellsTransformedSeed", BPCells_class("TransformBinarize"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformBinarize",
    function(x) "TransformBinarize"
)

####################################################################
# TransformExpm1Slow
methods::setClass("BPCellsTransformExpm1SlowSeed",
    contains = c("BPCellsTransformedSeed", BPCells_class("TransformExpm1Slow"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformExpm1Slow",
    function(x) "TransformExpm1Slow"
)

#' @export
#' @rdname BPCellsMatrix-class
methods::setGeneric("expm1_slow", function(x) {
    standardGeneric("expm1_slow")
})

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("expm1_slow", "BPCellsSeed", function(x) {
    BPCellsSeed(BPCells::expm1_slow(x))
})

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("expm1_slow", "BPCellsMatrix", function(x) {
    x <- x@seed
    DelayedArray(methods::callGeneric())
})

####################################################################
# TransformLog1pSlow
methods::setClass("BPCellsTransformLog1pSeed",
    contains = c("BPCellsTransformedSeed", BPCells_class("TransformLog1pSlow"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformLog1pSlow",
    function(x) "TransformLog1p"
)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("log1p", "BPCellsSeed", function(x) {
    # https://github.com/bnprks/BPCells/issues/73#issuecomment-1936642287
    # log1p_slow use double-precision math
    BPCellsSeed(BPCells::log1p_slow(x))
})

#' @export
#' @aliases log1p
#' @rdname BPCellsMatrix-class
methods::setMethod("log1p", "BPCellsMatrix", function(x) {
    x <- x@seed
    DelayedArray(methods::callGeneric())
})

####################################################################
# TransformExpm1
methods::setClass("BPCellsTransformExpm1Seed",
    contains = c("BPCellsTransformedSeed", BPCells_class("TransformExpm1"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformExpm1",
    function(x) "TransformExpm1"
)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("expm1", "BPCellsSeed", function(x) {
    BPCellsSeed(methods::callNextMethod())
})

#' @return
#'  - `expm1` and `expm1_slow`: compute `exp(x)-1` of matrix.
#' @export
#' @aliases expm1
#' @rdname BPCellsMatrix-class
methods::setMethod("expm1", "BPCellsMatrix", function(x) {
    x <- x@seed
    DelayedArray(methods::callGeneric())
})

####################################################################
# TransformLog1p
methods::setClass("BPCellsTransformLog1pSingleSeed",
    contains = c("BPCellsTransformedSeed", BPCells_class("TransformLog1p"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformLog1p",
    function(x) "TransformLog1pSingle"
)

#' @export
#' @rdname BPCellsMatrix-class
methods::setGeneric("log1p_single", function(x) {
    standardGeneric("log1p_single")
})

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("log1p_single", "BPCellsSeed", function(x) {
    fn <- methods::getMethod("log1p", "IterableMatrix", where = "BPCells")
    BPCellsSeed(fn(x))
})

#' @return
#'  - `log1p` and `log1p_single`: compute `log(1+x)` of matrix. `log1p_single`
#'    use single-precision math in intermediate stages, which is 2x faster than
#'    `log1p`.
#' @export
#' @aliases log1p_single
#' @rdname BPCellsMatrix-class
methods::setMethod("log1p_single", "BPCellsMatrix", function(x) {
    x <- x@seed
    DelayedArray(methods::callGeneric())
})

####################################################################
# TransformMinByCol
methods::setClass("BPCellsTransformMinByColSeed",
    contains = c("BPCellsTransformedSeed", BPCells_class("TransformMinByCol"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformMinByCol",
    function(x) "TransformMinByCol"
)

#' @export
#' @rdname BPCellsMatrix-class
methods::setGeneric("pmin_by_col", function(object, values) {
    standardGeneric("pmin_by_col")
})

#' @param values A positive atomic numeric.
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "pmin_by_col", "BPCellsSeed", function(object, values) {
        BPCellsSeed(BPCells::min_by_col(mat = object, vals = values))
    }
)

#' @inheritParams BPCellsSeed-class
#' @return
#' - `pmin_by_col`: Take the minimum with a per-col constant
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("pmin_by_col", "BPCellsMatrix", function(object, values) {
    object <- object@seed
    DelayedArray(methods::callGeneric())
})

####################################################################
# TransformMinByRow
methods::setClass("BPCellsTransformMinByRowSeed",
    contains = c("BPCellsTransformedSeed", BPCells_class("TransformMinByRow"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformMinByRow",
    function(x) "TransformMinByRow"
)

#' @export
#' @rdname BPCellsMatrix-class
methods::setGeneric("pmin_by_row", function(object, values) {
    standardGeneric("pmin_by_row")
})

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("pmin_by_row", "BPCellsSeed", function(object, values) {
    BPCellsSeed(BPCells::min_by_row(mat = object, vals = values))
})

#' @return
#' - `pmin_by_row`: Take the minimum with a per-row constant
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("pmin_by_row", "BPCellsMatrix", function(object, values) {
    object <- object@seed
    DelayedArray(methods::callGeneric())
})

####################################################################
# TransformMin

methods::setClass("BPCellsTransformMinSeed",
    contains = c("BPCellsTransformedSeed", BPCells_class("TransformMin"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformMin", function(x) "TransformMin"
)

#' @export
#' @rdname BPCellsMatrix-class
methods::setGeneric("pmin_scalar", function(object, value) {
    standardGeneric("pmin_scalar")
})

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("pmin_scalar", "BPCellsSeed", function(object, value) {
    BPCellsSeed(BPCells::min_scalar(mat = object, val = value))
})

#' @return
#' - `pmin_scalar`: Take minumum with a global constant
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("pmin_scalar", "BPCellsMatrix", function(object, value) {
    object <- object@seed
    DelayedArray(methods::callGeneric())
})

####################################################################
# TransformPowSlow
methods::setClass("BPCellsTransformPowSlowSeed",
    contains = c("BPCellsTransformedSeed", BPCells_class("TransformPowSlow"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformPowSlow", function(x) "TransformPowSlow"
)

#' @export
#' @rdname BPCellsMatrix-class
methods::setGeneric("pow_slow", function(e1, e2) {
    standardGeneric("pow_slow")
})

#' @inheritParams BPCells-binarize
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("pow_slow", "BPCellsSeed", function(e1, e2) {
    BPCellsSeed(BPCells::pow_slow(x = e1, exponent = e2))
})

#' @inheritParams BPCells-binarize
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("pow_slow", "BPCellsMatrix", function(e1, e2) {
    e1 <- e1@seed
    DelayedArray(methods::callGeneric())
})

####################################################################
# TransformPow
methods::setClass("BPCellsTransformPowSeed",
    contains = c("BPCellsTransformedSeed", BPCells_class("TransformPow"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformPow", function(x) "TransformPow"
)

#' @importMethodsFrom BPCells ^
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("^", c("BPCellsSeed", "numeric"), function(e1, e2) {
    BPCellsSeed(methods::callNextMethod())
})

#' @export
#' @aliases ^
#' @rdname BPCellsMatrix-class
methods::setMethod("^", "BPCellsMatrix", function(e1, e2) {
    e1 <- e1@seed
    DelayedArray(methods::callGeneric())
})

####################################################################
# TransformSquare
methods::setClass("BPCellsTransformSquareSeed",
    contains = c("BPCellsTransformedSeed", BPCells_class("TransformSquare"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformSquare", function(x) "TransformSquare"
)

####################################################################
# TransformRound
methods::setClass("BPCellsTransformRoundSeed",
    contains = c("BPCellsTransformedSeed", BPCells_class("TransformRound"))
)

#' @noRd
methods::setMethod(
    "BPCellsTransformedSeed", "TransformRound", function(x) "TransformRound"
)

#' @param digits Integer indicating the number of decimal places
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("round", "BPCellsSeed", function(x, digits = 0) {
    BPCellsSeed(methods::callNextMethod())
})

#' @inheritParams BPCellsSeed-class
#' @return
#'  - `round`: Rounding of matrix Numbers.
#' @export
#' @aliases round
#' @rdname BPCellsMatrix-class
methods::setMethod("round", "BPCellsMatrix", function(x, digits = 0) {
    x <- x@seed
    DelayedArray(methods::callGeneric())
})
