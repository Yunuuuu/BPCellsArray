################    BPCellsMatrix Methods    ##################
#' @return
#' - `[<-`: A `BPCellsMatrix` object.
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "ANY"),
    function(x, i, j, ..., value) {
        value <- BPCellsSeed(value)
        methods::callGeneric()
    }
)

#' @inheritParams BPCellsMatrix-class
#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "BPCellsMatrix"),
    array_call_BPCells_method(
        x = , i = , j = , ... = , value = ,
        Arrays = c("x", "value")
    )
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "IterableMatrix"),
    array_call_BPCells_method(x = , i = , j = , ... = , value = )
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "matrix"),
    function(x, i, j, ..., value) {
        seedform <- x@SeedForm
        x <- to_BPCells(x@seed)
        x_mode <- storage_mode(x)
        value_mode <- storage_mode(value)
        value <- methods::as(value, "dgCMatrix")
        if (x@transpose) {
            value <- t(BPCellsSeed(t(value)))
        } else {
            value <- BPCellsSeed(value)
        }
        if (x_mode == "uint32_t" && value_mode != "uint32_t") {
            cli::cli_warn("Convert {.arg value} into {.field uint32_t} mode")
            value <- BPCells::convert_matrix_type(
                matrix = value, type = "uint32_t"
            )
        } else if (x_mode != "uint32_t" && value_mode == "uint32_t") {
            cli::cli_warn("Convert {.arg value} into {.field {x_mode}} mode")
            value <- BPCells::convert_matrix_type(matrix = value, type = x_mode)
        } else {
            value <- BPCells::convert_matrix_type(matrix = value, type = x_mode)
        }
        with_seedform(seedform, DelayedArray(methods::callGeneric()))
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "dgCMatrix"),
    function(x, i, j, ..., value) {
        seedform <- x@SeedForm
        x <- to_BPCells(x@seed)
        if (x@transpose) {
            value <- t(BPCellsSeed(t(value)))
        } else {
            value <- BPCellsSeed(value)
        }
        with_seedform(seedform, DelayedArray(methods::callGeneric()))
    }
)
