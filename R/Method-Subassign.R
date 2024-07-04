################    BPCellsMatrix Methods    ##################
#' @return
#' - `[<-`: A `BPCellsMatrix` object.
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "BPCellsMatrix"),
    function(x, i, j, ..., value) {
        # coerce `value` into `IterableMatrix`
        value <- to_BPCells(value@seed)
        # Recall `x = BPCellsMatrix` and `value = IterableMatrix` method
        methods::callGeneric()
    }
)

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "IterableMatrix"),
    function(x, i, j, ..., value) {
        x <- to_BPCells(x@seed)
        x_mode <- storage_mode(x)
        value_mode <- storage_mode(value)
        if (x_mode != value_mode) {
            cli::cli_warn(incompatible_mode_msg("x", "value", x_mode))
            value <- BPCells::convert_matrix_type(
                matrix = value,
                type = x_mode
            )
        }
        if (x@transpose != value@transpose) {
            cli::cli_warn(c(`!` = paste(
                "Incompatible storage axis between",
                "{.arg x} and {.arg value}"
            ), i = "transposing the storage axis of {.arg value}"))
            value <- BPCells::transpose_storage_order(
                matrix = value,
                ...
            )
        }
        ans <- methods::callGeneric()
        DelayedArray(ans)
    }
)

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "ANY"),
    function(x, i, j, value) {
        # coerce `value` into `dgCMatrix`
        value <- coerce_into_dgCMatrix(value)
        # Recall `x = BPCellsMatrix` and `value = dgCMatrix` method
        methods::callGeneric()
    }
)

#' @inheritParams BPCellsMatrix-class
#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "dgCMatrix"),
    function(x, i, j, value) {
        x <- to_BPCells(x@seed)
        x_mode <- storage_mode(x)
        if (x@transpose) {
            value <- t(value)
            value <- t(BPCellsSeed(value))
        } else {
            value <- BPCellsSeed(value)
        }
        if (x_mode != "double") {
            cli::cli_warn(incompatible_mode_msg("x", "value", x_mode))
            value <- BPCells::convert_matrix_type(
                matrix = value,
                type = x_mode
            )
        }
        ans <- methods::callGeneric()
        DelayedArray(ans)
    }
)

# respect matrix storage.mode
#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "matrix"),
    function(x, i, j, value) {
        x <- to_BPCells(x@seed)
        x_mode <- storage_mode(x)
        value_mode <- storage_mode(value)
        value <- methods::as(value, "dgCMatrix")
        if (x@transpose) {
            value <- t(value)
            value <- t(BPCellsSeed(value))
        } else {
            value <- BPCellsSeed(value)
        }
        if (x_mode == "uint32_t" && value_mode != "uint32_t") {
            cli::cli_warn(incompatible_mode_msg("x", "value", x_mode))
            value <- BPCells::convert_matrix_type(
                matrix = value,
                type = "uint32_t"
            )
        } else if (x_mode != "uint32_t" && value_mode == "uint32_t") {
            cli::cli_warn(incompatible_mode_msg("x", "value", x_mode))
            value <- BPCells::convert_matrix_type(
                matrix = value,
                type = x_mode
            )
        } else {
            value <- BPCells::convert_matrix_type(
                matrix = value,
                type = x_mode
            )
        }
        ans <- methods::callGeneric()
        DelayedArray(ans)
    }
)
