############################################################
# MatrixSubset
methods::setClass("BPCellsSubsetSeed",
    contains = c(
        "BPCellsUnaryOpsSeed",
        get_class("MatrixSubset")
    ),
    slots = list(matrix = "BPCellsSeed")
)

#' @noRd
BPCellsSubsetSeed <- function(x) {
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsSubsetSeed")
}

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixSubset", function(x) {
    BPCellsSubsetSeed(x = x)
})

################    BPCellsMatrix Methods    ##################
#' @inheritParams BPCellsSeed-methods
#' @return
#' - `[`: A [BPCellsMatrix] object.
#' @order 2
#' @export
#' @rdname BPCellsMatrix-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "ANY", "ANY"),
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[i, j, ..., drop = drop])
    }
)

#' @inheritParams BPCellsSeed-methods
#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "missing", "ANY"),
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[, j, ..., drop = drop])
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "ANY", "missing"),
    function(x, i, j, ..., drop = FALSE) {
        DelayedArray(x@seed[i, , ..., drop = drop])
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "missing", "missing"),
    function(x, i, j, ..., drop = FALSE) {
        return(x)
    }
)

#' @inheritParams convert_mode
#' @return
#' - `[<-`: A [BPCellsMatrix] object.
#' @export
#' @order 3
#' @rdname BPCellsMatrix-methods
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "ANY"),
    function(x, i, j, ..., mode = NULL, value) {
        x <- x@seed
        DelayedArray(methods::callGeneric())
    }
)

################    BPCellsSeed Methods    ########################
# Don't use BPCellsSeed to implement `[` method since it will dispatch
# IterableMatrix method but some classes of BPCells do have their own `[`
# method, so we re-dispatch method for every seed class.
BPCellsSubset_internal <- function(x, i, j, ..., drop = FALSE) {
    BPCellsSeed(methods::callNextMethod())
}

#' @param i,j Row and Column index.
#' @param drop Ignored, always be `FALSE`.
#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsdgCMatrixSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsDirSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsHDF5Seed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsMemSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsColBindMatrixSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsRowBindMatrixSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsConvertSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsMaskSeed", BPCellsSubset_internal)

# it's not necessary to re-dispatch the "[" method for `BPCellsMultiplySeed`
# class since the `MatrixMultiply` method will use `[` method of `@left` and
# `@right`. Here, we just re-dispatch it to keep consistent.
#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsMultiplySeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsRankTransformSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsRenameDimsSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsSubsetSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod("[", "BPCellsTransformedSeed", BPCellsSubset_internal)

##################################################################
# Following code shold be fixed by upstream BPCells package
#' @importMethodsFrom BPCells [<-
#' @export
#' @rdname BPCellsSeed-methods
methods::setMethod(
    "[<-", c("BPCellsSeed", "ANY", "ANY", "ANY"),
    function(x, i, j, ..., mode = NULL, value) {
        value <- coerce_dgCMatrix(value)
        methods::callGeneric()
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsSeed", "ANY", "ANY", "dgCMatrix"),
    function(x, i, j, ..., mode = NULL, value) {
        if (x@transpose) {
            value <- t(methods::as(t(value), "BPCellsdgCMatrixSeed"))
        } else {
            value <- methods::as(value, "BPCellsdgCMatrixSeed")
        }
        methods::callGeneric()
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsSeed", "ANY", "ANY", "BPCellsSeed"),
    function(x, i, j, ..., mode = NULL, value) {
        i <- BPCells:::selection_index(i, nrow(x), rownames(x))
        ni <- if (length(i) > 0) seq_len(nrow(x))[-i] else seq_len(nrow(x))
        x_i <- x[i, ]
        x_ni <- x[ni, ]
        # dispatch the "BPCellsSeed", "missing", "ANY", "BPCellsSeed" method
        x_i[, j, ..., mode = mode] <- value
        rbind2(x_i, x_ni, mode = mode)[order(c(i, ni)), ]
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsSeed", "ANY", "missing", "BPCellsSeed"),
    function(x, i, j, ..., mode = NULL, value) {
        i <- BPCells:::selection_index(i, nrow(x), rownames(x))
        ni <- if (length(i) > 0) seq_len(nrow(x))[-i] else seq_len(nrow(x))
        x_i <- x[i, ]
        x_ni <- x[ni, ]
        if (any(dim(x_i) != dim(value))) {
            cli::cli_abort("Mismatched dimensions in assignment to subset")
        }
        rownames(value) <- rownames(x_i)
        colnames(value) <- colnames(x_i)
        rbind2(value, x_ni, mode = mode)[order(c(i, ni)), ]
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsSeed", "missing", "ANY", "BPCellsSeed"),
    function(x, i, j, ..., mode = NULL, value) {
        j <- BPCells:::selection_index(j, ncol(x), colnames(x))
        nj <- if (length(j) > 0) seq_len(ncol(x))[-j] else seq_len(ncol(x))
        x_j <- x[, j]
        x_nj <- x[, nj]
        if (any(dim(x_j) != dim(value))) {
            cli::cli_abort("Mismatched dimensions in assignment to subset")
        }
        rownames(value) <- rownames(x_j)
        colnames(value) <- colnames(x_j)
        cbind2(value, x_nj, mode = mode)[, order(c(j, nj))]
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsSeed", "missing", "missing", "BPCellsSeed"),
    function(x, i, j, ..., value) {
        if (any(dim(x) != dim(value))) {
            cli::cli_abort("Mismatched dimensions in assignment to subset")
        }
        rownames(value) <- rownames(x)
        colnames(value) <- colnames(x)
        value
    }
)
