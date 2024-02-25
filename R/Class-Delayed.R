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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @include utils-BPCells.R
#' @importClassesFrom DelayedArray DelayedOp
methods::setClass("BPCellsDelayedOp", contains = c("DelayedOp", "VIRTUAL"))

### list_methods("DelayedOp")
### Seed contract
### here: we override the `DelayedOp` methods
# methods::setMethod("path", "BPCellsDelayedOp", function(x) TRUE)
# methods::setReplaceMethod("path", "BPCellsDelayedOp")
# methods::setMethod("seed", "BPCellsDelayedOp", function(x) TRUE)
# methods::setReplaceMethod("seed", "BPCellsDelayedOp")
methods::setMethod("type", "BPCellsDelayedOp", function(x) {
    switch(storage_mode(x),
        uint32_t = "integer",
        float = ,
        double = "double"
    )
})
methods::setMethod("is_sparse", "BPCellsDelayedOp", function(x) TRUE)
methods::setMethod(
    "extract_array", "BPCellsDelayedOp",
    call_BPCells_method(x = , index = , Op = "x")
)
methods::setMethod(
    "OLD_extract_sparse_array", "BPCellsDelayedOp",
    call_BPCells_method(x = , index = , Op = "x")
)

methods::setMethod(
    "extract_sparse_array", "BPCellsDelayedOp",
    call_BPCells_method(x = , index = , Op = "x")
)
methods::setMethod(
    "dim", "BPCellsDelayedOp",
    call_BPCells_method(x = , Op = "x")
)
methods::setMethod(
    "dimnames", "BPCellsDelayedOp",
    call_BPCells_method(x = , Op = "x")
)

#' @importMethodsFrom BPCells t
methods::setMethod(
    "t", "BPCellsDelayedOp",
    call_BPCells_method(
        x = , after = expression(to_DelayedArray(object)), Op = "x"
    )
)
methods::setMethod(
    "chunkdim", "BPCellsDelayedOp",
    call_BPCells_method(x = , Op = "x")
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
methods::setMethod(
    "chunkdim", "BPCellsDelayedUnaryOp",
    call_BPCells_method(x = , Op = "x")
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importClassesFrom DelayedArray DelayedUnaryIsoOp
methods::setClass("BPCellsDelayedUnaryIsoOp",
    contains = c("DelayedUnaryIsoOp", "BPCellsDelayedUnaryOp", "VIRTUAL")
)

### list_methods("DelayedUnaryIsoOp")
### Seed contract
### here: we override the `DelayedNaryIsoOp` methods
methods::setMethod(
    "dim", "BPCellsDelayedUnaryIsoOp",
    call_BPCells_method(x = , Op = "x")
)

methods::setMethod(
    "dimnames", "BPCellsDelayedUnaryIsoOp",
    call_BPCells_method(x = , Op = "x")
)

methods::setMethod("is_sparse", "BPCellsDelayedUnaryIsoOp", function(x) TRUE)
methods::setMethod(
    "extract_array", "BPCellsDelayedUnaryIsoOp",
    call_BPCells_method(x = , index = , Op = "x")
)

methods::setMethod(
    "OLD_extract_sparse_array", "BPCellsDelayedUnaryIsoOp",
    call_BPCells_method(x = , index = , Op = "x")
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

methods::setValidity("BPCellsDelayedNaryOp", function(object) {
    BPCellsSeeds <- vapply(object@seeds,
        is_BPCellsSeed, logical(1L),
        USE.NAMES = FALSE
    )
    if (!all(BPCellsSeeds)) {
        cli::cli_abort(
            "all `@seeds` must be a {.cls BPCellsDelayedOp} or {.cls IterableMatrix} object"
        )
    }
    return(TRUE)
})

### list_methods("DelayedNaryIsoOp")
### Seed contract
### here: we override the `DelayedNaryIsoOp` methods
methods::setMethod(
    "dim", "BPCellsDelayedNaryIsoOp",
    call_BPCells_method(x = , Op = "x")
)

methods::setMethod(
    "dimnames", "BPCellsDelayedNaryIsoOp",
    call_BPCells_method(x = , Op = "x")
)

methods::setMethod("is_sparse", "BPCellsDelayedNaryIsoOp", function(x) TRUE)
methods::setMethod(
    "extract_array", "BPCellsDelayedNaryIsoOp",
    call_BPCells_method(x = , index = , Op = "x")
)

methods::setMethod(
    "OLD_extract_sparse_array", "BPCellsDelayedNaryIsoOp",
    call_BPCells_method(x = , index = , Op = "x")
)
