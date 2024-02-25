###########################################################
### - - - - - - - - - - - - - - - - - - - -
# On-disk Matrix
#' @inheritParams convert_mode
#' @importFrom DelayedArray path
methods::setMethod("path", "MatrixDir", function(object, ...) object@dir)

#' @importFrom DelayedArray path<-
methods::setReplaceMethod("path", "MatrixDir", function(object, ..., value) {
    object@dir <- value
    object
})

methods::setMethod("path", "MatrixH5", function(object, ...) object@path)
methods::setReplaceMethod("path", "MatrixH5", function(object, ..., value) {
    object@path <- value
    object
})

### - - - - - - - - - - - - - - - - - - - -
# On-memory Matrix don't have `path` method

### - - - - - - - - - - - - - - - - - - - -
# Unary Matrix
.path_unary <- function(object, ...) {
    object <- object@matrix
    methods::callGeneric()
}

.path_replace_unary <- function(object, ..., value) {
    path(x@matrix) <- value
    x
}

methods::setMethod("path", "ConvertMatrixType", .path_unary)
methods::setMethod("path", "MatrixRankTransform", .path_unary)
methods::setMethod("path", "RenameDims", .path_unary)
methods::setMethod("path", "RenameDims", .path_unary)
methods::setMethod("path", "TransformedMatrix", .path_unary)

methods::setReplaceMethod("path", "ConvertMatrixType", .path_replace_unary)
methods::setReplaceMethod("path", "MatrixRankTransform", .path_replace_unary)
methods::setReplaceMethod("path", "RenameDims", .path_replace_unary)
methods::setReplaceMethod("path", "RenameDims", .path_replace_unary)
methods::setReplaceMethod("path", "TransformedMatrix", .path_replace_unary)

### - - - - - - - - - - - - - - - - - - - -
# Nnary Matrix
methods::setMethod("path", "MatrixMask", function(object) {
    if (is_BPCellsInDisk(object@mask)) {
        # regard `MatrixMask` as Nary
        abort_nary_path()
    } else {
        path(object@matrix)
    }
})
methods::setReplaceMethod("path", "MatrixMask", function(object, ..., value) {
    if (is_BPCellsInDisk(object@mask)) {
        # regard `MatrixMask` as Nary
        abort_nary_path()
    } else {
        .path_replace_unary(object, ..., value = value)
    }
})

methods::setMethod("path", "MatrixMultiply", function(object) {
    # always regard `MatrixMultiply` as Nary
    abort_nary_path()
})

methods::setMethod("path", "ColBindMatrices", function(object) {
    abort_nary_path()
})

methods::setMethod("path", "RowBindMatrices", function(object) {
    abort_nary_path()
})

methods::setReplaceMethod(
    "path", "MatrixMultiply",
    function(object, ..., value) {
        # always regard `MatrixMultiply` as Nary
        abort_nary_path()
    }
)

methods::setReplaceMethod(
    "path", "ColBindMatrices",
    function(object, ..., value) {
        abort_nary_path()
    }
)

methods::setReplaceMethod(
    "path", "RowBindMatrices",
    function(object, ..., value) {
        abort_nary_path()
    }
)

############################################################
### - - - - - - - - - - - - - - - - - - - -
# On-disk and on-Memory Matrix or Unary Matrix
#' @importFrom DelayedArray seed
methods::setMethod("seed", "IterableMatrix", function(x) {
    if (is_BPCellsUnary(x)) seed(x@matrix) else x
})

#' @importFrom DelayedArray seed<-
methods::setReplaceMethod("seed", "IterableMatrix", function(x, value) {
    if (is_BPCellsUnary(x)) {
        seed(x@matrix) <- value
    } else {
        seed(x) <- value
    }
    x
})

### - - - - - - - - - - - - - - - - - - - -
# Nnary Matrix
methods::setMethod("seed", "MatrixMask", function(x) {
    if (is_BPCellsInDisk(x@mask)) {
        # regard `MatrixMask` as Nary
        abort_nary_seed()
    } else {
        seed(x@matrix)
    }
})

methods::setReplaceMethod("seed", "MatrixMask", function(x, value) {
    if (is_BPCellsInDisk(x@mask)) {
        # regard `MatrixMask` as Nary
        abort_nary_seed()
    } else {
        seed(x@matrix) <- value
    }
    x
})

methods::setMethod("seed", "MatrixMultiply", function(x) {
    # always regard `MatrixMultiply` as Nary
    abort_nary_seed()
})

methods::setMethod("seed", "ColBindMatrices", function(x) {
    abort_nary_seed()
})

methods::setMethod("seed", "RowBindMatrices", function(x) {
    abort_nary_seed()
})

methods::setReplaceMethod("seed", "MatrixMultiply", function(x, value) {
    # always regard `MatrixMultiply` as Nary
    abort_nary_seed()
})

methods::setReplaceMethod("seed", "ColBindMatrices", function(x, value) {
    abort_nary_seed()
})

methods::setReplaceMethod("seed", "RowBindMatrices", function(x, value) {
    abort_nary_seed()
})

#####################################################
### - - - - - - - - - - - - - - - - - - - -
# On-disk and on-Memory Matrix or Unary Matrix
#' @importFrom DelayedArray nseed
methods::setMethod("nseed", "IterableMatrix", function(x) {
    if (is_BPCellsUnary(x)) nseed(x@matrix) else 1L
})

# Nnary Operations
methods::setMethod("nseed", "MatrixMask", function(x) {
    mask <- x@mask
    if (is_BPCellsInDisk(mask)) {
        # regard `MatrixMask` as Nary
        sum(vapply(list(x@matrix, mask), nseed, integer(1L), USE.NAMES = FALSE))
    } else {
        nseed(x@matrix)
    }
})

methods::setMethod("nseed", "MatrixMultiply", function(x) {
    sum(vapply(list(x@left, x@right), nseed, integer(1L),
        USE.NAMES = FALSE
    ))
})

methods::setMethod("nseed", "ColBindMatrices", function(x) {
    sum(vapply(x@matrix_list, nseed, integer(1L), USE.NAMES = FALSE))
})

methods::setMethod("nseed", "RowBindMatrices", function(x) {
    sum(vapply(x@matrix_list, nseed, integer(1L), USE.NAMES = FALSE))
})

IS_NOT_SUPOORTED_IF_MULTIPLE_SEEDS <- c_msg(
    "is not supported on a DelayedArray object with multiple seeds at the",
    "moment. Note that you can check the number of seeds with nseed()."
)

abort_nary_seed <- function(call = rlang::caller_env()) {
    cli::cli_abort(c_msg("{.fn seed}", IS_NOT_SUPOORTED_IF_MULTIPLE_SEEDS),
        call = call
    )
}

abort_nary_path <- function(call = rlang::caller_env()) {
    cli::cli_abort(c_msg("{.fn path}", IS_NOT_SUPOORTED_IF_MULTIPLE_SEEDS),
        call = call
    )
}
