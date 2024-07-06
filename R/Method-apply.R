#' Apply Functions Over matrix Margins
#'
#' @param X A `r rd_matrix()`.
#' @param MARGIN A single number giving the subscripts which the function will
#' be applied over, `1` indicates rows, `2` indicates columns.
#' @param FUN Function to be applied. `FUN` is found by a call to [match.fun]
#' and typically is either a function or a symbol (e.g., a backquoted name) or a
#' character string specifying a function to be searched for from the
#' environment of the call to apply.
#' @inheritParams base::apply
#' @return
#' If each call to `FUN` returns a vector of length `n`, and simplify is `TRUE`,
#' then `apply` returns an array of dimension `c(n, dim(X)[MARGIN])` if `n > 1`.
#' If `n` equals `1`, `apply` returns a vector if `MARGIN` has length 1 and an
#' array of dimension `dim(X)[MARGIN]` otherwise. If `n` is `0`, the result has
#' length 0 but not necessarily the ‘correct’ dimension.
#'
#' If the calls to `FUN` return vectors of different lengths, or if `simplify`
#' is `FALSE`, `apply` returns a list of length `dim(X)[MARGIN]`.
#' @importFrom DelayedArray apply
#' @aliases apply
#' @export
methods::setMethod(
    "apply", "BPCellsMatrix",
    function(X, MARGIN, FUN, ..., simplify = TRUE) {
        assert_number(MARGIN)
        FUN <- match.fun(FUN)
        X_dim <- dim(X)
        MARGIN <- as.integer(MARGIN)
        if (MARGIN < 1L || MARGIN > length(X_dim)) {
            cli::cli_abort("{.arg MARGIN} must be >= 1 and <= length(dim(X))")
        }
        if (X_dim[[MARGIN]] == 0L) {
            ## base::apply seems to be doing something like that!
            ans <- FUN(as.matrix(X), ...)
            return(as.vector(ans[0L]))
        }
        seed <- to_BPCells(X@seed)
        values <- integer(dim(seed)[3L - MARGIN]) # nolint
        names(values) <- switch(MARGIN,
            colnames(seed),
            rownames(seed)
        )
        .fun <- switch(MARGIN,
            function(.value, .row_index, .col_index, ...) {
                # restore zero values
                values[.col_index] <- .value
                FUN(values, ...)
            },
            function(.value, .row_index, .col_index, ...) {
                # restore zero values
                values[.row_index] <- .value
                FUN(values, ...)
            }
        )
        ans <- switch(MARGIN,
            {
                if (BPCells::storage_order(seed) == "col") {
                    seed <- BPCells::transpose_storage_order(seed)
                }
                BPCells::apply_by_row(mat = seed, fun = .fun, ...)
            },
            {
                if (BPCells::storage_order(seed) == "row") {
                    seed <- BPCells::transpose_storage_order(seed)
                }
                BPCells::apply_by_col(mat = seed, fun = .fun, ...)
            }
        )
        ans_nms <- switch(MARGIN,
            rownames(seed),
            colnames(seed)
        )
        if (simplify) {
            lens <- lengths(ans)
            if (all(lens == .subset(lens, 1L))) {
                if (.subset(lens, 1L) == 1L) {
                    ans <- unlist(ans, recursive = FALSE, use.names = FALSE)
                    names(ans) <- ans_nms
                } else {
                    ans <- do.call(cbind, ans)
                    colnames(ans) <- ans_nms
                }
                return(ans)
            }
        }
        names(ans) <- ans_nms
        ans
    }
)
