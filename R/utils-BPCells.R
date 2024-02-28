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
    # Unary operations - BPCellsDelayedUnaryOp
    "ConvertMatrixType",
    "MatrixRankTransform",
    "RenameDims",
    "MatrixSubset",
    "TransformedMatrix",
    # Nary operations - BPCellsDelayedNaryOp
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
            "{.arg {arg}} must be a {.cls BPCellsDelayedOp} or a {.cls IterableMatrix} object"
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
    # actually, we regard `MatrixMask` as NaryOp only if `@mask` slot is
    # in Disk (is_BPCellsInDisk return `TRUE`)
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
        Recall(seed@matrix) || Recall(seed@mask)
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
    if (!is.null(rename)) slots <- rename(slots, rename)
    if (!is.null(new)) slots <- c(slots, new)
    methods::setClass(myClass, ..., slots = slots)
}

migrate_slots <- function(Object, ..., remove = NULL, new = NULL, rename = NULL, Class) {
    slot_nms <- methods::slotNames(Object)
    if (!is.null(remove)) slot_nms <- setdiff(slot_nms, remove)
    names(slot_nms) <- slot_nms
    slots <- lapply(slot_nms, methods::slot, object = Object)
    if (!is.null(rename)) slots <- rename(slots, rename)
    if (!is.null(new)) slots <- c(slots, new)
    rlang::inject(S4Vectors::new2(Class = Class, !!!slots, check = FALSE))
}
