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
    TransformScaleShift = NULL,
    # Following Class were not implemented currently
    SCTransformPearson = NULL,
    SCTransformPearsonTranspose = NULL,
    SCTransformPearsonSlow = NULL,
    SCTransformPearsonTransposeSlow = "sctransform_pearson"
)

#########################################################
SUPPORTED_BPCELLS_MATRIX <- c(
    # On-memory matrix
    "Iterable_dgCMatrix_wrapper",
    "PackedMatrixMemBase", "UnpackedMatrixMemBase",
    # On-disk matrix
    "MatrixDir", "MatrixH5",
    # Unary operations
    "ConvertMatrixType",
    "MatrixRankTransform",
    "RenameDims",
    "MatrixSubset",
    "TransformedMatrix",
    # Nary operations
    "MatrixMask",
    "MatrixMultiply",
    "RowBindMatrices", "ColBindMatrices"
)

# we regard `BPCellsDelayedOp` or `IterableMatrix` as seed for BPCellsMatrix
.validate_seed <- function(seed, arg = rlang::caller_arg(seed)) {
    if (methods::is(seed, "BPCellsDelayedOp")) {
        return(TRUE)
    } else if (methods::is(seed, "IterableMatrix")) {
        for (ii in SUPPORTED_BPCELLS_MATRIX) {
            if (methods::is(seed, ii)) return(TRUE) # styler: off
        }
        cli::cli_abort(
            "{.cls {obj_s4_friendly(seed)}} is not supported at the moment"
        )
    } else {
        cli::cli_abort(
            "{.arg {arg}} must be a {.cls BPCellsDelayedOp} or {.cls IterableMatrix} object"
        )
    }
    TRUE
}

is_BPCellsMemory <- function(seed) {
    methods::is(seed, "PackedMatrixMemBase") ||
        methods::is(seed, "UnpackedMatrixMemBase") ||
        methods::is(seed, "Iterable_dgCMatrix_wrapper")
}

is_BPCellsDisk <- function(seed) {
    methods::is(seed, "MatrixDir") || methods::is(seed, "MatrixH5")
}

is_BPCellsUnary <- function(seed) {
    methods::is(seed, "ConvertMatrixType") ||
        methods::is(seed, "MatrixRankTransform") ||
        methods::is(seed, "RenameDims") ||
        methods::is(seed, "MatrixSubset") ||
        methods::is(seed, "TransformedMatrix")
}

is_BPCellsNary <- function(seed) {
    methods::is(seed, "MatrixMask") ||
        methods::is(seed, "MatrixMultiply") ||
        methods::is(seed, "RowBindMatrices") ||
        methods::is(seed, "ColBindMatrices")
}

is_BPCellsInDisk <- function(seed) {
    if (is_BPCellsDisk(seed)) {
        return(TRUE)
    } else if (is_BPCellsMemory(seed)) {
        return(FALSE)
    } else if (is_BPCellsUnary(seed)) {
        Recall(seed@matrix)
    } else if (methods::is(seed, "MatrixMask")) {
        Recall(seed@mask) || Recall(seed@matrix)
    } else if (methods::is(seed, "MatrixMultiply")) {
        # MatrixMultiply
        Recall(seed@left) || Recall(seed@right)
    } else {
        # RowBindMatrices or ColBindMatrices
        seeds <- seed@matrix_list
        for (seed in seeds) if (Recall(seed)) return(TRUE) # styler: off
        return(FALSE)
    }
}

is_BPCellsInMemory <- function(seed) !is_BPCellsInDisk(seed)

# not used currently
is_BPCellsMatrixMaskUnary <- function(seed) {
    methods::is(seed, "MatrixMask") && is_BPCellsInDisk(seed@mask)
}

# not used currently
is_BPCellsMatrixMultiplyUnary <- function(seed) {
    methods::is(seed, "MatrixMultiply") &&
        sum(vapply(list(seed@left, seed@right), is_BPCellsInDisk, logical(1L),
            USE.NAMES = FALSE
        )) < 2L
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
# This function only used for `BPCells` object, so it's safe to regard `object`
# in generic function as a `IterableMatrix` object
#' @keywords internal
#' @noRd
methods::setGeneric("to_DelayedArray",
    signature = c("object"), function(object, ..., delayed = NULL) {
        delayed <- delayed %||% GlobalOptions$DelayedBPCells
        if (delayed) {
            with_delayed(TRUE, standardGeneric("to_DelayedArray"))
        } else {
            to_DelayedArray_return_IterableMatrix(object)
        }
    }
)

to_DelayedArray_return_IterableMatrix <- function(object) {
    for (ii in SUPPORTED_BPCELLS_MATRIX) {
        if (methods::is(object, ii)) return(object) # styler: off
    }
    cli::cli_abort(
        "{.cls {obj_s4_friendly(object)}} is not supported at the moment"
    )
}

# only used by on-disk and on-memory BPCells Matrix
methods::setMethod(
    "to_DelayedArray", "IterableMatrix",
    to_DelayedArray_return_IterableMatrix
)

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

##############################################################
# helper function to re-dispath `DelayedArray` method
# should just used for `BPCellsMatrix` method
call_DelayedArray_method <- function(..., type = "S4", Array = "object") {
    method <- switch(type,
        S4 = quote(methods::callNextMethod()),
        S3 = quote(NextMethod())
    )
    Array <- rlang::sym(Array)
    body <- list(substitute(delayed <- Array@delayed, list(Array = Array)))
    new_method(rlang::pairlist2(...),
        method = method, body = body,
        after = expression(
            # for some method, it will return DelayedArray directly although
            # @seed is compatible with `BPCellsMatrix`.
            # here we just re-creating a BPCellsMatrix object when it could be.
            object <- with_delayed(delayed, DelayedArray(object@seed)),
            if (!(methods::is(object, "BPCellsMatrix") ||
                methods::is(object, "BPCellsArray"))) {
                cli::cli_warn(c(
                    sprintf("{.fn %s} method return a {.cls {obj_s4_friendly(object)}} object", .Generic), # nolint
                    i = "Subsequent operation won't use {.pkg BPCells} methods"
                ))
            },
            object
        )
    )
}

# helper function to re-dispath `BPCells` method
# should just used for `BPCellsDelayedOp` method
call_BPCells_method <- function(..., before = NULL, after = NULL, Op = "object") {
    Op <- rlang::sym(Op)
    body <- list(substitute(Op <- to_BPCells(Op), list(Op = Op)))
    new_method(rlang::pairlist2(...),
        body = body, method = quote(methods::callGeneric()),
        before = before, after = after
    )
}

# hepler function to set method for `BPCellsArray`
set_BPCellsArray_method <- function(..., method = NULL, before = NULL, after = NULL, Arrays = "object") {
    method <- method %||% quote(methods::callGeneric())
    body <- lapply(rlang::syms(Arrays), function(Array) {
        substitute(
            BPCells_mat <- to_BPCells(BPCells_mat@seed), # nolint
            list(BPCells_mat = Array)
        )
    })
    new_method(rlang::pairlist2(...),
        body = body, method = method,
        before = before, after = after
    )
}

########################################################
#' @include utils.R
#' @noRd
new_method <- function(args, body, method, before = NULL, after = NULL) {
    if (!is.null(after)) {
        method <- substitute(object <- method, list(method = method)) # nolint
    }
    body <- c(body, list(method))
    if (!is.null(before)) body <- c(before, body)
    if (!is.null(after)) body <- c(body, after)
    body <- as.call(c(as.name("{"), body))
    rlang::new_function(args, body = body)
}
