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
# the `@<-` function will check slot class.
# For `IterableMatrix` object, `@matrix` must be a `IterableMatrix` object, but
# for `DelayedOp` object `@seed` is a signature "ANY".
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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# https://developer.r-project.org/howMethodsWork.pdf
# The current implementation (as of version 2.4.0 of R) works as follows. The
# individual distances, as explained in section 3, are the number of steps in
# the path of superclasses between the target and the defined class, with the
# distance to "ANY" larger than any other distance. If several arguments are
# involved, the distances for each argument are added and the total distance is
# used to compare candidate methods. In the case of ties, the first of the
# candidates is chosen. Because the candidate signatures are effectively an
# outer product of the individual superclass lists, and because of the way those
# lists are created, the effect is to select the first possible method in an
# ordering by the order of superclasses in the "contains" slot of the class
# definition, ordering first according to the first argument in the signature,
# then the second, etc
# So we should always put BPCellsDelayedOp Class in the first super-class of
# other extended DelayedOp object.

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

#' @export
methods::setAs("BPCellsDelayedOp", "IterableMatrix", function(from) {
    to_BPCells(from)
})

### list_methods("DelayedOp")
### Seed contract Methods

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod("type", "BPCellsDelayedOp", function(x) {
    x <- to_BPCells(x)
    methods::callGeneric()
})

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod("is_sparse", "BPCellsDelayedOp", function(x) TRUE)

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod("extract_array", "BPCellsDelayedOp", function(x, index) {
    x <- to_BPCells(x)
    methods::callGeneric()
})

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "OLD_extract_sparse_array", "BPCellsDelayedOp",
    function(x, index) {
        x <- to_BPCells(x)
        methods::callGeneric()
    }
)

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod(
    "extract_sparse_array", "BPCellsDelayedOp",
    function(x, index) {
        x <- to_BPCells(x)
        methods::callGeneric()
    }
)

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod("dim", "BPCellsDelayedOp", function(x) {
    x <- to_BPCells(x)
    methods::callGeneric()
})

#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod("dimnames", "BPCellsDelayedOp", function(x) {
    x <- to_BPCells(x)
    methods::callGeneric()
})

#' @importMethodsFrom BPCells t
#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod("t", "BPCellsDelayedOp", function(x) {
    x <- to_BPCells(x)
    ans <- methods::callGeneric()
    to_DelayedArray(ans)
})

#' @importFrom DelayedArray chunkdim
#' @export
#' @rdname BPCellsDelayedOp-class
methods::setMethod("chunkdim", "BPCellsDelayedOp", function(x) {
    x <- to_BPCells(x)
    methods::callGeneric()
})
