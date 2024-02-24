# Note that delayed operations like setting dimnames on an ADSArray object (with
# dimnames(A) <- new_dimnames) or transposing an ADSMatrix object (with M2 <-
# t(M)) will degrade the object to a DelayedArray or DelayedMatrix instance,
# causing max(A) and max(M2) to use the far less efficient block-processed max()
# method defined for DelayedArray objects. There is clearly room for improvement
# here and work will be done in the near future to make the max() method (and
# other block-processed methods) for DelayedArray objects try to take advantage
# of the backend-specific methods whenever it can.

# Here, we difine a new `DelayedOp` class to ensure every delayed operations
# return a `BPCellsMatrix` object instead of a `DelayedMatrix` object.

#' @importClassesFrom DelayedArray DelayedOp
methods::setClass("BPCellsDelayedOp", contains = c("DelayedOp", "VIRTUAL"))

# For child-class with a seed slot
validate_BPCellsDelayed_seed <- function(object) {
    seed <- object@seed
    if (is_BPCellsSeed(seed)) {
        cli::cli_abort("{.code @seed} must be a {.cls BPCellsDelayedOp} or {.cls IterableMatrix} object")
    }
    TRUE
}

#' @importClassesFrom DelayedArray DelayedUnaryOp
methods::setClass("BPCellsDelayedUnaryOp",
    contains = c("DelayedUnaryOp", "BPCellsDelayedOp", "VIRTUAL")
)

methods::setValidity("BPCellsDelayedUnaryOp", validate_BPCellsDelayed_seed)

#' @importClassesFrom DelayedArray DelayedUnaryIsoOp
methods::setClass("BPCellsDelayedUnaryIsoOp",
    contains = c("DelayedUnaryIsoOp", "BPCellsDelayedUnaryOp", "VIRTUAL")
)

#' @importClassesFrom DelayedArray DelayedNaryOp
methods::setClass("BPCellsDelayedNaryOp",
    contains = c("DelayedNaryOp", "BPCellsDelayedOp", "VIRTUAL")
)

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

##################################################################
# helper function to create a `BPCellsDelayedOp` class from BPCells Class.
mould_BPCells <- function(myClass, mould, ..., rename = NULL, delete = NULL, add = NULL) {
    slots <- methods::getSlots(BPCells_class(mould))
    if (!is.null(delete)) slots <- slots[!names(slots) %in% delete]
    if (!is.null(add)) slots <- c(slots, add)
    if (!is.null(rename)) slots <- rename(slots, rename)
    methods::setClass(myClass, ..., slots = slots)
}

rename_slot <- function(Object, ..., Class) {
    slots <- methods::slotNames(Object)
    names(slots) <- recode(slots, c(...))
    slots <- lapply(slots, methods::slot, object = Object)
    rlang::inject(S4Vectors::new2(Class = Class, !!!slots, check = FALSE))
}

#######################################################################
# Function used to translate `BPCellsDelayed` class into BPCells operations
# Always return a `IterableMatrix` object
#' @keywords internal
#' @noRd
methods::setGeneric("to_BPCells", function(object, ...) {
    standardGeneric("to_BPCells")
})

methods::setMethod("to_BPCells", "IterableMatrix", function(object) object)
methods::setMethod("to_BPCells", "DelayedOp", function(object) {
    cli::cli_abort(
        "You cannot mix {.pkg BPCells} method with {.pkg DelayedArray} method"
    )
})
methods::setMethod(
    "to_BPCells", "BPCellsDelayedUnaryOp",
    function(object, Class) {
        object <- rename_slot(Object = object, seed = "matrix", Class = Class)
        object@matrix <- to_BPCells(object@matrix)
        object
    }
)

#######################################################################
# Function used to translate `BPCells` class into `BPCellsDelayed` class
#' @keywords internal
#' @noRd
methods::setGeneric("to_DelayedArray", function(object) {
    standardGeneric("to_DelayedArray")
})

# only used by `MatrixDir`, `MatrixH5`, `PackedMatrixMemBase`,
# `UnpackedMatrixMemBase`, and `Iterable_dgCMatrix_wrapper`
methods::setMethod("to_DelayedArray", "IterableMatrix", function(object) object)

# used by c(
#    "BPCellsConvertSeed", "BPCellsRankTransformSeed",
#    "BPCellsRenameDimsSeed", "BPCellsSubsetSeed", "BPCellsTransformedSeed"
# )
to_DelayedUnaryOp <- function(object, Class) {
    object <- rename_slot(Object = object, matrix = "seed", Class = Class)
    object@seed <- to_DelayedArray(object@seed)
    object
}

##############################################################
# helper function to re-dispath `DelayedArray` method
call_DelayedArray_method <- function(..., type = "S4") {
    next_method <- switch(type,
        S4 = quote(methods::callNextMethod()),
        S3 = quote(NextMethod())
    )
    body <- rlang::expr({
        cli::cli_inform("Using {.pkg DelayedArray} method")
        object <- !!next_method
        new_BPCellsArray(object@seed)
    })
    rlang::new_function(rlang::pairlist2(...), body = body)
}

# helper function to re-dispath `BPCells` method
call_BPCells_method <- function(..., before = NULL, after = NULL, Op = "object") {
    Op <- rlang::sym(Op)
    body <- list(
        quote(cli::cli_inform("Using {.pkg BPCells} method")),
        rlang::expr(!!Op <- to_BPCells(!!Op))
    )
    new_method(rlang::pairlist2(...),
        body = body, method = quote(methods::callGeneric()),
        before = before, after = after
    )
}

#' @include utils.R
#' @noRd
new_method <- function(args, body, method = NULL, before = NULL, after = NULL) {
    method <- method %||% quote(methods::callGeneric())
    if (is.null(after)) {
        after <- list(method)
    } else {
        after <- c(list(rlang::expr(object <- !!method)), after)
    }
    new_function(args, body = body, before = before, after = after)
}

new_function <- function(args, body, before = NULL, after = NULL) {
    if (!is.null(before)) body <- c(before, body)
    if (!is.null(after)) body <- c(body, after)
    body <- as.call(c(as.name("{"), body))
    rlang::new_function(args, body = body)
}

#########################################################
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

methods::setMethod(
    "t", "BPCellsDelayedOp",
    call_BPCells_method(
        x = , after = expression(to_DelayedArray(object)), Op = "x"
    )
)

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

methods::setMethod(
    "extract_sparse_array", "BPCellsDelayedNaryIsoOp",
    call_BPCells_method(x = , index = , Op = "x")
)
