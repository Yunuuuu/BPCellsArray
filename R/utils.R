`%||%` <- function(x, y) if (is.null(x)) y else x

get_class <- function(name) {
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

show_bpcells <- function(object, baseClass, class) {
    cat(sprintf(
        "%d x %d %s object with class %s\n",
        nrow(object), ncol(object), baseClass, class
    ))

    cat("\n")
    cat(sprintf(
        "Row names: %s\n",
        BPCells:::pretty_print_vector(rownames(object), empty = "unknown names")
    ))
    cat(sprintf(
        "Col names: %s\n",
        BPCells:::pretty_print_vector(colnames(object), empty = "unknown names")
    ))

    cat("\n")
    cat(sprintf("Storage Data type: %s\n", storage_mode(object)))
    cat(sprintf("Storage axis: %s major\n", storage_axis(object)))

    cat("\n")
    showtree(object)
}


imap <- function(.x, .f, ...) {
    .mapply(.f, list(.x, names(.x) %||% seq_along(.x)), list(...))
}

c_msg <- function(..., sep = " ") {
    paste0(..., collapse = sep)
}

rebind <- function(sym, value, ns) {
    if (rlang::is_string(ns)) {
        Recall(sym, value, getNamespace(ns))
        pkg <- paste0("package:", ns)
        if (pkg %in% search()) {
            Recall(sym, value, as.environment(pkg))
        }
    } else if (is.environment(ns)) {
        if (bindingIsLocked(sym, ns)) {
            unlockBinding(sym, ns)
            on.exit(lockBinding(sym, ns))
        }
        assign(sym, value, ns)
    } else {
        stop("ns must be a string or environment")
    }
}

compatible_storage_mode <- function(list) {
    actual_modes <- vapply(
        list, storage_mode, character(1L),
        USE.NAMES = FALSE
    )
    BPCells_MODE[max(match(actual_modes, BPCells_MODE))]
}

swap_axis <- function(.fn, object, column, row, ...) {
    if (object@transpose) {
        .fn(object, row, ...)
    } else {
        .fn(object, column, ...)
    }
}

wrapMatrix <- function(class, m, ...) {
    dimnames <- dimnames(m)
    methods::new(class,
        matrix = m,
        transpose = m@transpose, dim = m@dim,
        dimnames = dimnames, ...
    )
}

drop_internal <- function(x) {
    perm <- which(dim(x) != 1L)
    if (length(perm) >= 2L) {
        return(x)
    }
    ## In-memory realization.
    ## We want to propagate the names so we use 'as.array(x, drop=TRUE)'
    ## rather than 'as.vector(x)' (both are equivalent on an Array
    ## derivative with less than 2 effective dimensions except that
    ## the former propagates the names and the latter doesn't).
    as.array(x, drop = TRUE)
}

# Use chartr() for safety since toupper() fails to convert i to I in Turkish
# locale
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

snake_class <- function(x) {
    snakeize(class(x)[1])
}

snakeize <- function(x) {
    x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
    x <- gsub(".", "_", x, fixed = TRUE)
    x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
    to_lower_ascii(x)
}
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)

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
