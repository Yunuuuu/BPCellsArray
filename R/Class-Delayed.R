# Note that delayed operations like setting dimnames on an ADSArray object (with
# dimnames(A) <- new_dimnames) or transposing an ADSMatrix object (with M2 <-
# t(M)) will degrade the object to a DelayedArray or DelayedMatrix instance,
# causing max(A) and max(M2) to use the far less efficient block-processed max()
# method defined for DelayedArray objects. There is clearly room for improvement
# here and work will be done in the near future to make the max() method (and
# other block-processed methods) for DelayedArray objects try to take advantage
# of the backend-specific methods whenever it can.

# Here, we difine a new `DelayedOp` class to provide parallel class with
# `DelayedArray`, so that `BPCells` delayed operations can transform into
# parallel `DelayedOp` class easily.

###################################################################
### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# the `@<-` method for `IterableMatrix` object will check slot class
# So we must change with `@seed` or `@seeds` slots when running `to_BPCells` or
# `to_DelayedArray`
# to_BPCells
# Function used to translate `BPCellsDelayed` class into BPCells operations
# Always return a `IterableMatrix` object
#' @keywords internal
#' @noRd
methods::setGeneric("to_BPCells", function(object, ...) {
    standardGeneric("to_BPCells")
})

methods::setMethod("to_BPCells", "IterableMatrix", function(object) object)

#' @importClassesFrom DelayedArray DelayedOp
methods::setMethod("to_BPCells", "DelayedOp", function(object) {
    cli::cli_abort(
        "You cannot mix {.pkg BPCells} method with {.pkg DelayedArray} method"
    )
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# to_DelayedArray
# Function used to translate `BPCells` class into `BPCellsDelayed` class
#' @keywords internal
#' @noRd
methods::setGeneric("to_DelayedArray", signature = "object", function(object) {
    standardGeneric("to_DelayedArray")
})

# only used by on-disk and on-memory BPCells Matrix
methods::setMethod("to_DelayedArray", "IterableMatrix", function(object) object)

# used by c(
#    "BPCellsConvert", "BPCellsRankTransform",
#    "BPCellsRenameDims", "BPCellsSubset", "BPCellsTransformed"
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
# This will not convert the final object into `BPCellsMatrix`
#' @include utils-BPCells.R
delayedop_call_BPCells_method <- function(..., before = NULL, after = NULL, Array = "object") {
    Array <- rlang::sym(Array)
    before <- c(before, list(
        substitute(Array <- to_BPCells(Array), list(Array = Array))
    ))
    new_method(rlang::pairlist2(...),
        before = before,
        method = quote(methods::callGeneric()),
        after = after
    )
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importClassesFrom DelayedArray DelayedOp
methods::setClass("BPCellsDelayedOp", contains = c("DelayedOp", "VIRTUAL"))

### list_methods("DelayedOp")
### Seed contract
### here: we override the `DelayedOp` methods
# methods::setMethod("path", "BPCellsDelayedOp", function(x) TRUE)
# methods::setReplaceMethod("path", "BPCellsDelayedOp")
# methods::setMethod("seed", "BPCellsDelayedOp", function(x) TRUE)
# methods::setReplaceMethod("seed", "BPCellsDelayedOp")

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "type", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = , Array = "x")
)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("is_sparse", "BPCellsDelayedOp", function(x) TRUE)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "extract_array", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = , index = , Array = "x")
)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "OLD_extract_sparse_array", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = , index = , Array = "x")
)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "extract_sparse_array", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = , index = , Array = "x")
)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "dim", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = , Array = "x")
)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "dimnames", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = , Array = "x")
)

#' @importMethodsFrom BPCells t
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "t", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(
        x = ,
        after = expression(to_DelayedArray(object)),
        Array = "x"
    )
)

#' @importFrom DelayedArray chunkdim
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "chunkdim", "BPCellsDelayedOp",
    delayedop_call_BPCells_method(x = , Array = "x")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importClassesFrom DelayedArray DelayedUnaryOp
methods::setClass("BPCellsDelayedUnaryOp",
    contains = c("DelayedUnaryOp", "BPCellsDelayedOp", "VIRTUAL")
)
methods::setMethod(
    "to_BPCells", "BPCellsDelayedUnaryOp",
    function(object, Class) {
        object@seed <- to_BPCells(object@seed)
        migrate_slots(
            Object = object,
            rename = c(seed = "matrix"), Class = Class
        )
    }
)

### list_methods("DelayedUnaryOp")
### Seed contract
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "chunkdim", "BPCellsDelayedUnaryOp",
    delayedop_call_BPCells_method(x = , Array = "x")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importClassesFrom DelayedArray DelayedUnaryIsoOp
methods::setClass("BPCellsDelayedUnaryIsoOp",
    contains = c("DelayedUnaryIsoOp", "BPCellsDelayedUnaryOp", "VIRTUAL")
)

### list_methods("DelayedUnaryIsoOp")
### Seed contract
### here: we override the `DelayedNaryIsoOp` methods
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "dim", "BPCellsDelayedUnaryIsoOp",
    delayedop_call_BPCells_method(x = , Array = "x")
)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "dimnames", "BPCellsDelayedUnaryIsoOp",
    delayedop_call_BPCells_method(x = , Array = "x")
)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("is_sparse", "BPCellsDelayedUnaryIsoOp", function(x) TRUE)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "extract_array", "BPCellsDelayedUnaryIsoOp",
    delayedop_call_BPCells_method(x = , index = , Array = "x")
)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "OLD_extract_sparse_array", "BPCellsDelayedUnaryIsoOp",
    delayedop_call_BPCells_method(x = , index = , Array = "x")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importClassesFrom DelayedArray DelayedNaryOp
methods::setClass("BPCellsDelayedNaryOp",
    contains = c("DelayedNaryOp", "BPCellsDelayedOp", "VIRTUAL")
)
### list_methods("DelayedNaryOp")
### Seed contract

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importClassesFrom DelayedArray DelayedNaryIsoOp
methods::setClass("BPCellsDelayedNaryIsoOp",
    contains = c("DelayedNaryIsoOp", "BPCellsDelayedNaryOp", "VIRTUAL")
)

### list_methods("DelayedNaryIsoOp")
### Seed contract
### here: we override the `DelayedNaryIsoOp` methods
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "dim", "BPCellsDelayedNaryIsoOp",
    delayedop_call_BPCells_method(x = , Array = "x")
)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "dimnames", "BPCellsDelayedNaryIsoOp",
    delayedop_call_BPCells_method(x = , Array = "x")
)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("is_sparse", "BPCellsDelayedNaryIsoOp", function(x) TRUE)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "extract_array", "BPCellsDelayedNaryIsoOp",
    delayedop_call_BPCells_method(x = , index = , Array = "x")
)

#' @export
#' @rdname BPCellsSeed-class
methods::setMethod(
    "OLD_extract_sparse_array", "BPCellsDelayedNaryIsoOp",
    delayedop_call_BPCells_method(x = , index = , Array = "x")
)
