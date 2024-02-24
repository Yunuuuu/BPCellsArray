################    BPCellsMatrix Methods    ##################
#' @return
#' - `[<-`: A [BPCellsMatrix] object.
#' @export
#' @order 3
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "ANY"),
    function(x, i, j, ..., value) {
        value <- coerce_dgCMatrix(value)
        methods::callGeneric()
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "matrix"),
    function(x, i, j, ..., value) {
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
        methods::callGeneric()
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "dgCMatrix"),
    function(x, i, j, ..., value) {
        if (x@transpose) {
            value <- t(BPCellsSeed(t(value)))
        } else {
            value <- BPCellsSeed(value)
        }
        methods::callGeneric()
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "IterableMatrix"),
    set_BPCellsArray_method(
        x = , i = , j = , ... = , value = ,
        after = expression(DelayedArray(to_DelayedArray(object))),
        Arrays = "x"
    )
)
