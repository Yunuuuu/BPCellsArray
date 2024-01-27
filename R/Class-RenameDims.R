############################################################
# RenameDims
methods::setClass("BPCellsRenameDimsSeed",
    contains = c("BPCellsUnaryOpsSeed", get_class("RenameDims")),
    slots = list(matrix = "BPCellsSeed")
)

#' @noRd
BPCellsRenameDimsSeed <- function(x) {
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsRenameDimsSeed")
}

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "RenameDims", function(x) {
    BPCellsRenameDimsSeed(x = x)
})

################    BPCellsMatrix Methods    ##################
methods::setClassUnion("ListOrNULL", c("list", "NULL"))
#' @return
#' - `dimnames<-`: A [BPCellsMatrix][BPCellsMatrix] object.
#' @importMethodsFrom BPCells dimnames<-
#' @export
#' @aliases dimnames<-
#' @rdname BPCellsMatrix-methods
methods::setMethod(
    "dimnames<-", c(x = "BPCellsMatrix", value = "ListOrNULL"),
    function(x, value) {
        x <- x@seed
        DelayedArray(methods::callGeneric())
    }
)

#' @return
#' - `rownames<-`: A [BPCellsMatrix][BPCellsMatrix] object.
#' @importFrom BiocGenerics rownames<-
#' @export
#' @aliases rownames<-
#' @rdname BPCellsMatrix-methods
methods::setMethod(
    "rownames<-", c(x = "BPCellsMatrix", value = "atomic"),
    function(x, value) {
        x <- x@seed
        DelayedArray(methods::callGeneric())
    }
)

#' @return
#' - `colnames<-`: A [BPCellsMatrix][BPCellsMatrix] object.
#' @importFrom BiocGenerics colnames<-
#' @export
#' @aliases colnames<-
#' @rdname BPCellsMatrix-methods
methods::setMethod(
    "colnames<-", c(x = "BPCellsMatrix", value = "atomic"),
    function(x, value) {
        x <- x@seed
        DelayedArray(methods::callGeneric())
    }
)

################    BPCellsSeed Methods    ########################
# All delayed operations should be wrapped into a `BPCellsSeed` object
# In BPCells, `dimnames<-` was only defined for `IterableMatrix`.
# `dimnames<-` return another `IterableMatrix` object.
# we wrap it into a `BPCellsSeed` object.
#' @return
#' - `dimnames<-`: A [BPCellsSeed] object, usually a `BPCellsRenameDimsSeed`
#'   object.
#' @importMethodsFrom BPCells dimnames<-
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod(
    "dimnames<-", c(x = "BPCellsSeed", value = "list"), function(x, value) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importMethodsFrom BPCells dimnames<-
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod(
    "dimnames<-", c(x = "BPCellsSeed", value = "NULL"), function(x, value) {
        BPCellsSeed(methods::callNextMethod())
    }
)

#' @importFrom BiocGenerics rownames<-
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod(
    "rownames<-", c(x = "BPCellsSeed", value = "atomic"),
    function(x, value) {
        if (!is.null(value)) {
            if (!is.atomic(value)) {
                cli::cli_abort(
                    "{.arg value} must be a character or {.code NULL}"
                )
            }
            if (length(value) != nrow(x)) {
                cli::cli_abort(
                    "{.arg value} must have the length of {.code nrow(value)}"
                )
            }
        }
        dimnames(x) <- list(value, colnames(x))
        BPCellsSeed(x)
    }
)

#' @importFrom BiocGenerics colnames<-
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod(
    "colnames<-", c(x = "BPCellsSeed", value = "atomic"),
    function(x, value) {
        if (!is.null(value)) {
            if (!is.atomic(value)) {
                cli::cli_abort(
                    "{.arg value} must be a character or {.code NULL}"
                )
            }
            if (length(value) != ncol(x)) {
                cli::cli_abort(
                    "{.arg value} must have the length of {.code ncol(value)}"
                )
            }
        }
        dimnames(x) <- list(rownames(x), value)
        BPCellsSeed(x)
    }
)
