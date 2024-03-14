# Note that delayed operations like setting dimnames on an ADSArray object (with
# dimnames(A) <- new_dimnames) or transposing an ADSMatrix object (with M2 <-
# t(M)) will degrade the object to a DelayedArray or DelayedMatrix instance,
# causing max(A) and max(M2) to use the far less efficient block-processed max()
# method defined for DelayedArray objects. There is clearly room for improvement
# here and work will be done in the near future to make the max() method (and
# other block-processed methods) for DelayedArray objects try to take advantage
# of the backend-specific methods whenever it can.

# Here, we difine a new `DelayedOp` class (`BPCellsDelayedOp`) to provide
# parallel class with `DelayedArray`, so that `BPCells` delayed operations can
# transform into a similar parallel `DelayedOp` class.
# DelayedArray for `BPCellsDelayedOp` object always return a `BPCellsMatrix`

###################################################################
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# the `@<-` function will check slot class. For `IterableMatrix` object,
# we `@matrix` must be a `IterableMatrix` object, but for `DelayedOp` object
# `@seed` is a signature "ANY"
# So when define `to_BPCells`, we must run `to_BPCells` firstly with `@seed`
# before rename "seed" slot into "matrix" slot, when define `to_DelayedArray`,
# we must firstly rename "matrix" slot into "seed" slot, then using
# `to_DelayedArray` to transform it

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# to_BPCells
# Function used to translate `BPCellsDelayed` class into BPCells operations
# Always return a `IterableMatrix` object
#' @keywords internal
#' @noRd
methods::setGeneric("to_BPCells", function(object) {
    standardGeneric("to_BPCells")
})

# used by on-disk and on-memory IterableMatrix
methods::setMethod("to_BPCells", "IterableMatrix", function(object) object)

#' @importClassesFrom DelayedArray DelayedOp
methods::setMethod("to_BPCells", "DelayedOp", function(object) {
    cli::cli_abort(
        "You cannot mix {.pkg BPCells} method with {.pkg DelayedArray} method"
    )
})
to_BPCellsUnaryOp <- function(object, Class) {
    object@seed <- to_BPCells(object@seed)
    migrate_slots(
        Object = object,
        rename = c(seed = "matrix"), Class = Class
    )
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# to_DelayedArray
# Function used to translate `BPCells` class into `BPCellsDelayed` class
#' @keywords internal
#' @noRd
methods::setGeneric("to_DelayedArray", signature = "object", function(object) {
    standardGeneric("to_DelayedArray")
})

# only used by on-disk and on-memory IterableMatrix
methods::setMethod("to_DelayedArray", "IterableMatrix", function(object) object)

# used by c(
#    "BPCellsDelayedConvert", "BPCellsDelayedRankTransform",
#    "BPCellsDelayedRenameDims", "BPCellsDelayedSubset",
#    "BPCellsDelayedTransformed"
# )
to_DelayedUnaryOp <- function(object, Class) {
    object <- migrate_slots(
        Object = object,
        rename = c(matrix = "seed"), Class = Class
    )
    object@seed <- to_DelayedArray(object@seed)
    object
}

#######################################################################
# helper function to re-dispath `BPCells` method
# should used for `BPCellsDelayedOp` object
# This will not convert the final object into `DelayedArray` object so should be
# used for function return another class, usually the seed contract methods
#' @include utils.R utils-BPCells.R
delayedop_call_BPCells_method <- function(..., before = NULL, after = NULL, Array = NULL) {
    args <- rlang::pairlist2(...)
    Array <- rlang::sym(Array %||% names(args)[[1L]])
    before <- c(before, list(
        substitute(Array <- to_BPCells(Array), list(Array = Array))
    ))
    new_method(args,
        before = before,
        method = quote(methods::callGeneric()),
        after = after
    )
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' BPCellsDelayedOp objects
#'
#' Provide a parallel [DelayedOp][DelayedArray::DelayedOp] object
#' @note Just like [DelayedOp][DelayedArray::DelayedOp] object, this is not
#' intented used by users directly.
#' @importClassesFrom DelayedArray DelayedOp
methods::setClass("BPCellsDelayedOp", contains = "VIRTUAL")

# S3/S4 combo for as.matrix.BPCellsDelayedOp
#' @param x A `BPCellsDelayedOp` object.
#' @inheritParams BPCellsSeed-class
#' @inherit BPCellsSeed-class return
#' @exportS3Method base::as.matrix
#' @rdname BPCellsDelayedOp-class
as.matrix.BPCellsDelayedOp <- function(x) {
    as_matrix_IterableMatrix(to_BPCells(x))
}

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod("as.matrix", "BPCellsDelayedOp", as.matrix.BPCellsDelayedOp)

# S3/S4 combo for as.array.BPCellsDelayedOp
#' @exportS3Method base::as.array
#' @rdname BPCellsDelayedOp-class
as.array.BPCellsDelayedOp <- function(x, drop = FALSE) {
    assert_bool(drop)
    mat <- as.matrix(x)
    if (drop) drop(mat) else mat
}

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod("as.array", "BPCellsDelayedOp", as.array.BPCellsDelayedOp)

#' @export
methods::setAs("BPCellsDelayedOp", "dgCMatrix", function(from) {
    methods::as(to_BPCells(from), "dgCMatrix")
})

### list_methods("DelayedOp")
### Seed contract Methods

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "type", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = )
)

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod("is_sparse", "BPCellsDelayedOp", function(x) TRUE)

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "extract_array", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = , index = )
)

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "OLD_extract_sparse_array", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = , index = )
)

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "extract_sparse_array", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = , index = )
)

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "dim", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = )
)

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "dimnames", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = )
)

#' @importMethodsFrom BPCells t
#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "t", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(
        x = , after = expression(to_DelayedArray(object))
    )
)

#' @importFrom DelayedArray chunkdim
#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "chunkdim", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = )
)
