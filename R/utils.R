`%||%` <- function(x, y) if (is.null(x)) y else x

get_class <- function(name) {
    asNamespace("BPCells")[[paste0(".__C__", name)]]
}

extract_bpcells_array <- function(x, index) {
    if (length(index) > 2L) {
        cli::cli_abort(c(
            "{.arg index} must be a list with length <= 2",
            i = "BPCells only support matrix operations"
        ))
    }
    i <- index[[1L]]
    j <- index[[2L]]
    if (is.null(i) && is.null(j)) {
        out <- x
    } else if (!is.null(i) && !is.null(j)) {
        out <- x[i, j]
    } else if (!is.null(i)) {
        out <- x[i, ]
    } else {
        out <- x[, j]
    }
    methods::as(out, "dgCMatrix")
}

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

mock_matrix <- function(ngenes, ncells) {
    cell.means <- 2^stats::runif(ngenes, 2, 10)
    cell.disp <- 100 / cell.means + 0.5
    cell.data <- matrix(stats::rnbinom(ngenes * ncells,
        mu = cell.means,
        size = 1 / cell.disp
    ), ncol = ncells)
    rownames(cell.data) <- sprintf("Gene_%s", formatC(seq_len(ngenes),
        width = 4, flag = 0
    ))
    colnames(cell.data) <- sprintf("Cell_%s", formatC(seq_len(ncells),
        width = 3, flag = 0
    ))
    cell.data
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
    cat(sprintf("Data type: %s\n", BPCells:::matrix_type(object)))
    cat(sprintf(
        "Storage order: %s major\n",
        ifelse(object@transpose, "row", "column")
    ))

    cat("\n")
    description <- BPCells:::short_description(object)
    if (length(description) > 0) cat("Queued Operations:\n")
    for (i in seq_along(description)) {
        cat(sprintf("%d. %s\n", i, description[i]))
    }
}

imap <- function(.x, .f, ...) {
    .mapply(.f, list(.x, names(.x) %||% seq_along(.x)), list(...))
}

#' House of internal methods
#'
#' Following methods are used by package internal, for messages, or other
#' internal operations
#'
#' @name internal-methods
NULL

BPCElls_Transform_classes <- c(
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
