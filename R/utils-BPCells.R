##########################################################
BPCells_class <- function(name) {
    BPCells_get(paste0(".__C__", name))
}

BPCells_get <- local({
    BPCellsNamespace <- NULL
    function(nm) {
        if (is.null(BPCellsNamespace)) {
            BPCellsNamespace <<- asNamespace("BPCells")
        }
        if (exists(nm, envir = BPCellsNamespace, inherits = FALSE)) {
            get(nm, envir = BPCellsNamespace, inherits = FALSE)
        } else {
            cli::cli_abort("Cannot find {.val {nm}} in {.pkg BPCells}")
        }
    }
})

BPCells_MODE <- c("uint32_t", "float", "double")
BPCells_Transform_classes <- c(
    TransformLog1p = "log1p",
    TransformLog1pSlow = "log1p_slow",
    TransformExpm1 = "expm1",
    TransformExpm1Slow = "expm1_slow",
    TransformSquare = NULL,
    TransformPow = "^",
    TransformPowSlow = "pow_slow",
    TransformMin = "min_scalar",
    TransformMinByRow = "min_by_row",
    TransformMinByCol = "min_by_col",
    TransformBinarize = "binarize",
    TransformRound = "round",
    SCTransformPearson = NULL,
    SCTransformPearsonTranspose = NULL,
    SCTransformPearsonSlow = NULL,
    SCTransformPearsonTransposeSlow = "sctransform_pearson",
    TransformScaleShift = NULL
)

#########################################################
# we regard `BPCellsDelayedOp` or `IterableMatrix` as seed for BPCellsMatrix
is_BPCellsSeed <- function(seed) {
    methods::is(seed, "BPCellsDelayedOp") ||
        methods::is(seed, "IterableMatrix")
}
validate_seed <- function(object) {
    seed <- object@seed
    if (!is_BPCellsSeed(seed)) {
        cli::cli_abort("{.code @seed} must be a {.cls BPCellsDelayedOp} or {.cls IterableMatrix} object")
    }
    TRUE
}

##########################################################
# helper function to coerce object into `dgCMatrix` object, which can be used by
# `BPCells`
coerce_dgCMatrix <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    tryCatch(
        methods::as(x, "dgCMatrix"),
        error = function(cnd) {
            cli::cli_abort(
                "{.arg {arg}} must be a matrix-like object which can be coerced into {.cls dgCMatrix}",
                call = call
            )
        }
    )
}

compatible_storage_mode <- function(list) {
    actual_modes <- vapply(
        list, storage_mode, character(1L),
        USE.NAMES = FALSE
    )
    BPCells_MODE[max(match(actual_modes, BPCells_MODE))]
}

##################################################################
# helper function to create a `BPCellsDelayedOp` class from BPCells Class.
#' @param mould Tha Class name from `BPCells`
#' @param remove A character specifying the slot names to remove.
#' @param new New slots to use, a list.
#' @param rename Named characters to rename the slots in BPCells class.
#' @noRd
mould_BPCells <- function(myClass, mould, ..., remove = NULL, new = NULL, rename = NULL) {
    slots <- methods::getSlots(BPCells_class(mould))
    if (!is.null(remove)) slots <- slots[!names(slots) %in% remove]
    if (!is.null(new)) slots <- c(slots, new)
    if (!is.null(rename)) slots <- rename(slots, rename)
    methods::setClass(myClass, ..., slots = slots)
}

migrate_slots <- function(Object, ..., remove = NULL, new = NULL, rename = NULL, Class) {
    slots <- methods::slotNames(Object)
    if (!is.null(remove)) slots <- setdiff(slots, remove)
    names(slots) <- slots
    slots <- lapply(slots, methods::slot, object = Object)
    if (!is.null(rename)) slots <- rename(slots, rename)
    if (!is.null(new)) slots <- c(slots, new)
    rlang::inject(S4Vectors::new2(Class = Class, !!!slots, check = FALSE))
}

#######################################################################
# the `@<-` method for `IterableMatrix` object will check slot class
# So we must run `to_BPCells` or `to_DelayedArray` with `@seed` or `@seeds`
# slots.

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# to_BPCells
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

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# to_DelayedArray
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
    object <- migrate_slots(
        Object = object,
        rename = c(matrix = "seed"), Class = Class
    )
    object@seed <- to_DelayedArray(object@seed)
    object
}

##############################################################
# helper function to re-dispath `DelayedArray` method
# should just used for `BPCellsMatrix` method
call_DelayedArray_method <- function(..., type = "S4") {
    next_method <- switch(type,
        S4 = quote(methods::callNextMethod()),
        S3 = quote(NextMethod())
    )
    body <- rlang::expr({
        object <- !!next_method
        seed <- object@seed
        if (is_BPCellsSeed(seed)) {
            # if `@seed` is a BPCellsSeed, always return a BPCellsMatrix object
            new_BPCellsArray(seed)
        } else {
            cli::cli_warn(c(
                "{.fn {.Generic}} method return a {.cls {obj_type_friendly(object)}} object",
                i = "Subsequent operation won't use {.pkg BPCells} methods"
            ))
            object
        }
    })
    rlang::new_function(rlang::pairlist2(...), body = body)
}

# helper function to re-dispath `BPCells` method
# should just used for `BPCellsDelayedOp` method
call_BPCells_method <- function(..., before = NULL, after = NULL, Op = "object", inform = FALSE) {
    Op <- rlang::sym(Op)
    body <- list(rlang::expr(!!Op <- to_BPCells(!!Op))) # nolint
    if (inform) {
        body <- c(
            list(quote(cli::cli_inform("Using {.pkg BPCells} method"))), body
        )
    }
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
