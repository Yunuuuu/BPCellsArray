############################################################
# RenameDims
#' @importClassesFrom DelayedArray DelayedSetDimnames
mould_BPCells("BPCellsDelayedRenameDims", "RenameDims",
    remove = "matrix",
    # BPCellsDelayedUnaryIsoOp: `seed` slot
    # both class provide `dimnames` slot
    contains = c("DelayedSetDimnames", "BPCellsDelayedUnaryIsoOp")
)

### list_methods("DelayedSetDimnames")
### Seed contract
### here: we override the `DelayedSetDimnames` methods
#' @importFrom DelayedArray is_noop
methods::setMethod("is_noop", "BPCellsDelayedRenameDims", function(x) FALSE)

methods::setMethod(
    "dimnames", "BPCellsDelayedRenameDims",
    call_BPCells_method(x = , Op = "x")
)

############################################################
methods::setMethod("to_DelayedArray", "RenameDims", function(object) {
    to_DelayedUnaryOp(object, Class = "BPCellsDelayedRenameDims")
})

methods::setMethod("to_BPCells", "BPCellsDelayedRenameDims", function(object) {
    methods::callNextMethod(object = object, Class = "RenameDims")
})

#############################################################
summary.BPCellsDelayedRenameDims <- function(object) {
    "Rename dimnames"
}

methods::setMethod(
    "summary", "BPCellsDelayedRenameDims",
    summary.BPCellsDelayedRenameDims
)
summary.RenameDims <- summary.BPCellsDelayedRenameDims
methods::setMethod("summary", "RenameDims", summary.BPCellsDelayedRenameDims)

################    BPCellsMatrix Methods    ##################
methods::setClassUnion("ListOrNULL", c("list", "NULL"))

#' @param value
#'  - `dimnames<-`: A list of dimnames or `NULL`.
#'  - `[<-`: A [BPCellsMatrix][BPCellsMatrix-class] object or any objects can be
#'    converted into [BPCellsSeed] object.
#' @return
#' - `dimnames<-`: A `BPCellsMatrix` object.
#' @importMethodsFrom BPCells dimnames<-
#' @export
#' @aliases dimnames<-
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "dimnames<-", c(x = "BPCellsMatrix", value = "ListOrNULL"),
    set_BPCellsArray_method(
        x = , value = ,
        before = expression(delayed <- x@delayed),
        after = expression(with_delayed(delayed, DelayedArray(object))),
        Arrays = "x"
    )
)

#' @return
#' - `rownames<-`: A `BPCellsMatrix` object.
#' @importFrom BiocGenerics rownames<-
#' @export
#' @aliases rownames<-
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "rownames<-", c(x = "BPCellsMatrix", value = "ANY"),
    function(x, value) {
        set_dimnames(x, 1L, value)
    }
)

#' @return
#' - `colnames<-`: A `BPCellsMatrix` object.
#' @importFrom BiocGenerics colnames<-
#' @export
#' @aliases colnames<-
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "colnames<-", c(x = "BPCellsMatrix", value = "ANY"),
    function(x, value) {
        set_dimnames(x, 2L, value)
    }
)

set_dimnames <- function(x, axis, value, arg = rlang::caller_arg(value), call = rlang::caller_env()) {
    dnms <- dimnames(x)
    if (axis == 1L) {
        axis_nm <- "'rownames'" # nolint
        axis_len <- "nrow(x)"
    } else if (axis == 2L) {
        axis_nm <- "'colnames'"
        axis_len <- "ncol(x)"
    } else {
        axis_nm <- sprintf("names of '%d-dimension'", axis)
        axis_len <- sprintf("dim(x)[%d]", axis)
    }
    nd <- length(dnms)
    if (nd < 2L) {
        cli::cli_abort(
            "attempt to set {axis_nm} on an object with less than two dimensions"
        )
    } else if (nd < axis) {
        cli::cli_abort(
            "attempt to set {axis_nm} on an object with only {nd} dimensions"
        )
    }
    dnms <- dnms %||% vector("list", length = nd)
    if (is.null(value)) {
        dnms[axis] <- list(NULL)
    } else {
        if (!is.atomic(value)) {
            cli::cli_abort("{.arg value} must be a character or {.code NULL}")
        }
        n <- dim(x)[axis]
        if (length(value) != n) {
            cli::cli_abort(sprintf(
                "{.arg value} must have the length of %s",
                style_code(axis_len)
            ))
        }
        dnms[[axis]] <- as.character(value)
    }
    dimnames(x) <- dnms
    x
}
