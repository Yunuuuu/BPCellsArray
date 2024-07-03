###########################################################
#' @inheritParams convert_mode
#' @importFrom DelayedArray path
methods::setMethod("path", "BPCellsMatrix", function(object, ...) {
    object <- object@seed
    methods::callGeneric()
})

#' @importFrom DelayedArray path<-
methods::setReplaceMethod("path", "BPCellsMatrix", function(object, ..., value) {
    seed <- object@seed
    path(object = seed, ...) <- value
    object@seed <- seed
    object
})

### - - - - - - - - - - - - - - - - - - - -
# On-disk Matrix
methods::setMethod("path", "MatrixDir", function(object, ...) object@dir)

methods::setReplaceMethod("path", "MatrixDir", function(object, ..., value) {
    object@dir <- value
    object
})

methods::setMethod("path", "MatrixH5", function(object, ...) object@path)
methods::setReplaceMethod("path", "MatrixH5", function(object, ..., value) {
    object@path <- value
    object
})

methods::setMethod("path", "10xMatrixH5", function(object, ...) object@path)
methods::setReplaceMethod("path", "10xMatrixH5", function(object, ..., value) {
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
    path(object = object@matrix, ...) <- value
    object
}

methods::setMethod("path", "ConvertMatrixType", .path_unary)
methods::setMethod("path", "MatrixRankTransform", .path_unary)
methods::setMethod("path", "RenameDims", .path_unary)
methods::setMethod("path", "MatrixSubset", .path_unary)
methods::setMethod("path", "TransformedMatrix", .path_unary)

methods::setReplaceMethod("path", "ConvertMatrixType", .path_replace_unary)
methods::setReplaceMethod("path", "MatrixRankTransform", .path_replace_unary)
methods::setReplaceMethod("path", "RenameDims", .path_replace_unary)
methods::setReplaceMethod("path", "MatrixSubset", .path_replace_unary)
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
#' @importFrom DelayedArray seed
methods::setMethod("seed", "BPCellsMatrix", function(x) seed(x@seed))

#' @importFrom DelayedArray seed<-
methods::setReplaceMethod("seed", "BPCellsMatrix", function(x, value) {
    x@seed <- methods::callGeneric(x = x@seed, value = value)
    x
})

### - - - - - - - - - - - - - - - - - - - -
# On-disk and on-Memory Matrix or Unary Matrix
methods::setMethod("seed", "IterableMatrix", function(x) {
    if (is_BPCellsUnary(x)) seed(x@matrix) else x
})

#' @importFrom DelayedArray seed<-
methods::setReplaceMethod("seed", "IterableMatrix", function(x, value) {
    if (is_BPCellsMemory(x) || is_BPCellsDisk(x)) {
        DelayedArray:::.normalize_seed_replacement_value(value, x)
    } else {
        x@matrix <- methods::callGeneric(x = x@matrix, value = value)
        x
    }
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
#' @importFrom DelayedArray nseed
methods::setMethod("nseed", "BPCellsMatrix", function(x) nseed(x@seed))

### - - - - - - - - - - - - - - - - - - - -
# On-disk and on-Memory Matrix or Unary Matrix
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

IS_NOT_SUPOORTED_IF_MULTIPLE_SEEDS <- paste(
    "is not supported on a DelayedArray object with multiple seeds at the",
    "moment. Note that you can check the number of seeds with nseed()."
)

abort_nary_seed <- function(call = rlang::caller_env()) {
    cli::cli_abort(paste("{.fn seed}", IS_NOT_SUPOORTED_IF_MULTIPLE_SEEDS),
        call = call
    )
}

abort_nary_path <- function(call = rlang::caller_env()) {
    cli::cli_abort(paste("{.fn path}", IS_NOT_SUPOORTED_IF_MULTIPLE_SEEDS),
        call = call
    )
}

###########################################################
### Avoid use of non-ASCII characters in R source code. There must be a
### better way to do this.
.VBAR <- rawToChar(as.raw(c(0xe2, 0x94, 0x82)))
.TEE <- rawToChar(as.raw(c(0xe2, 0x94, 0x9c)))
.ELBOW <- rawToChar(as.raw(c(0xe2, 0x94, 0x94)))
.HBAR <- rawToChar(as.raw(c(0xe2, 0x94, 0x80)))

.node_as_one_line_summary <- function(x, show.node.dim = TRUE) {
    if (is_BPCellsMemory(x) || is_BPCellsDisk(x)) {
        ans <- sprintf("[seed] %s", summary(x))
    } else {
        ans <- summary(x)
    }
    if (show.node.dim) {
        dim_in1string <- paste0(dim(x), collapse = "x")
        sparse <- if (is_sparse(x)) ", sparse" else ""
        ans <- sprintf("%s %s%s: %s", dim_in1string, type(x), sparse, ans)
    }
    ans
}

.rec_showtree <- function(x, indent = "", last.child = NA, prefix = "", show.node.dim = TRUE) {
    if (is.list(x) && !is_array(x)) {
        nchildren <- length(x)
        nms <- names(x)
        if (is.null(nms)) {
            nms <- rep_len("", nchildren)
        } else {
            nms <- sprintf("%s: ", nms)
        }
        for (i in seq_len(nchildren)) {
            Recall(
                x = x[[i]], indent = indent,
                last.child = (i == nchildren),
                prefix = nms[[i]]
            )
        }
    } else {
        DelayedArray:::.rec_showtree(
            x = x, indent = indent,
            last.child = last.child, prefix = prefix,
            show.node.dim = show.node.dim
        )
    }
}

### 'last.child' can be NA, TRUE, or FALSE. NA means 'x' is the root of the
### tree.
#' @param x A `IterableMatrix` or `BPCellsMatrix` object.
#' @noRd
.rec_show_BPCells <- function(x, indent = "", last.child = NA, prefix = "", show.node.dim = TRUE) {
    ## Display summary line.
    if (is.na(last.child)) {
        ## No Tprefix.
        Tprefix <- ""
    } else {
        ## 3-char Tprefix
        Tprefix <- paste0(if (last.child) .ELBOW else .TEE, .HBAR, " ")
    }
    x_as1line <- .node_as_one_line_summary(x, show.node.dim = show.node.dim)
    cat(indent, Tprefix, prefix, x_as1line, "\n", sep = "")
    # if no children, return
    if (is_BPCellsMemory(x) || is_BPCellsDisk(x)) return(NULL) # styler: off
    ## Display children.
    if (!is.na(last.child)) {
        ## Increase indent by 3 chars.
        indent <- paste0(
            indent,
            if (last.child) " " else .VBAR,
            strrep(" ", 2 + nchar(prefix))
        )
    }
    if (is_BPCellsArray(x)) {
        Recall(
            x = x@seed, indent = indent, last.child = TRUE,
            prefix = prefix, show.node.dim = show.node.dim
        )
    } else if (is_BPCellsUnary(x)) {
        Recall(
            x = x@matrix, indent = indent, last.child = TRUE,
            prefix = prefix, show.node.dim = show.node.dim
        )
    } else if (is_BPCellsNary(x)) {
        if (methods::is(x, "MatrixMask")) {
            if (is_BPCellsInDisk(x@mask)) {
                # regard `MatrixMask` as Nary
                seeds <- list(matrix = x@matrix, mask = x@mask)
            } else {
                return(Recall(x@matrix,
                    indent = indent, last.child = TRUE,
                    prefix = prefix, show.node.dim = show.node.dim
                ))
            }
        } else if (methods::is(x, "MatrixMultiply")) {
            seeds <- list(left = x@left, right = x@right)
        } else {
            seeds <- x@matrix_list
        }
        # A list of seeds
        .rec_showtree(
            x = seeds, indent = indent, last.child = FALSE,
            prefix = prefix, show.node.dim = show.node.dim
        )
    } else if (methods::is(x, "IterableMatrix")) {
        cli::cli_abort(
            "{.cls {fclass(x)}} is not supported at the moment"
        )
    }
}

#' Visualize and access the leaves of a tree of delayed operations
#'
#' `showtree` can be used to visualize the tree of delayed operations carried by
#' a `DelayedArray` object.
#'
#' Use `seedApply` to apply a function to the seeds of a `DelayedArray` object.
#'
#' @param x Typically a [DelayedArray][DelayedArray::DelayedArray] object but
#' can also be a [DelayedOp][DelayedArray::DelayedOp-class] object or a list
#' where each element is a [DelayedArray][DelayedArray::DelayedArray] or
#' [DelayedOp][DelayedArray::DelayedOp-class] object.
#' @inheritParams DelayedArray::showtree
#' @return
#'  - `showtree`: return the input invisiblely
#' @export
showtree <- function(x, show.node.dim = TRUE) {
    assert_bool(show.node.dim)
    .rec_showtree(x = x, show.node.dim = show.node.dim)
    invisible(x)
}


#' @inheritParams DelayedArray::seedApply
#' @return
#'  - `seedApply`: A list of length `nseed(x)` for `seedApply`.
#' @rdname showtree
#' @export
seedApply <- function(x, FUN, ...) {
    if (is_BPCellsMemory(x) || is_BPCellsDisk(x)) {
        return(list(FUN(x, ...)))
    } else if (is.list(x) && !is_array(x)) {
        ans <- lapply(x, FUN = seedApply, FUN, ...)
        return(unlist(ans, recursive = FALSE, use.names = FALSE))
    } else if (is_BPCellsArray(x)) {
        x <- x@seed
    } else if (is_BPCellsUnary(x)) {
        x <- x@matrix
    } else if (is_BPCellsNary(x)) {
        # For Nary operations
        if (methods::is(x, "MatrixMask")) {
            if (is_BPCellsInDisk(x@mask)) {
                # regard `MatrixMask` as Nary
                x <- list(matrix = x@matrix, mask = x@mask)
            } else {
                x <- x@matrix
            }
        } else if (methods::is(x, "MatrixMultiply")) {
            x <- list(left = x@left, right = x@right)
        } else {
            x <- x@matrix_list
        }
    } else if (methods::is(x, "IterableMatrix")) {
        cli::cli_abort(
            "{.cls {fclass(x)}} is not supported at the moment"
        )
    } else {
        return(DelayedArray::seedApply(x = x, FUN = FUN, ...))
    }
    Recall(x = x, FUN = FUN, ...)
}
