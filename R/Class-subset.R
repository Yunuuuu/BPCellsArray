############################################################
# MatrixSubset
methods::setClass("BPCellsSubsetSeed",
    contains = c(
        "BPCellsUnaryOpsSeed",
        BPCells_class("MatrixSubset")
    ),
    slots = list(matrix = "BPCellsSeed")
)

#' @export
#' @rdname BPCellsSeed
methods::setMethod("BPCellsSeed", "MatrixSubset", function(x) {
    x@matrix <- BPCellsSeed(x@matrix)
    methods::as(x, "BPCellsSubsetSeed")
})

methods::setMethod("summary", "BPCellsSubsetSeed", function(object) {
    "Subset matrix"
})

################    BPCellsMatrix Methods    ##################
#' @inheritParams BPCellsSeed-class
#' @return
#' - `[`: A [BPCellsMatrix] object or an atomic vector.
#' @order 2
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "[", c("BPCellsMatrix", "ANY", "ANY"),
    function(x, i, j, ..., drop = FALSE) {
        assert_bool(drop)
        array <- DelayedArray(x@seed[i, j, ..., drop = FALSE])
        if (drop) drop(array) else array
    }
)

#' @inheritParams BPCellsSeed-class
#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "missing", "ANY"),
    function(x, i, j, ..., drop = FALSE) {
        assert_bool(drop)
        array <- DelayedArray(x@seed[, j, ..., drop = FALSE])
        if (drop) drop(array) else array
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "ANY", "missing"),
    function(x, i, j, ..., drop = FALSE) {
        assert_bool(drop)
        array <- DelayedArray(x@seed[i, , ..., drop = FALSE])
        if (drop) drop(array) else array
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[", c("BPCellsMatrix", "missing", "missing"),
    function(x, i, j, ..., drop = FALSE) {
        assert_bool(drop)
        if (drop) drop(x) else x
    }
)

#' @inheritParams convert_mode
#' @return
#' - `[<-`: A [BPCellsMatrix] object.
#' @export
#' @order 3
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "[<-", c("BPCellsMatrix", "ANY", "ANY", "ANY"),
    function(x, i, j, ..., value) {
        x <- x@seed
        DelayedArray(methods::callGeneric())
    }
)

################    BPCellsSeed Methods    ########################
# Don't use BPCellsSeed to implement `[` method since it will dispatch
# IterableMatrix method but some classes of BPCells do have their own `[`
# method, so we re-dispatch method for every seed class.
BPCellsSubset_internal <- function(x, i, j, ..., drop = FALSE) {
    assert_bool(drop)
    seed <- BPCellsSeed(methods::callNextMethod())
    if (drop) drop(seed) else seed
}

#' @param i,j Row and Column index.
#' @param drop A bool, if `TRUE`, any extents of length one will be removed and
#' return an atomic vector.
#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-class
#' @include Class-dgCMatrix.R
methods::setMethod("[", "BPCellsdgCMatrixSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("[", "BPCellsDirSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("[", "BPCellsHDF5Seed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("[", "BPCellsMemSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("[", "BPCellsColBindMatrixSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("[", "BPCellsRowBindMatrixSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("[", "BPCellsConvertSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("[", "BPCellsMaskSeed", BPCellsSubset_internal)

# it's not necessary to re-dispatch the "[" method for `BPCellsMultiplySeed`
# class since the `MatrixMultiply` method will use `[` method of `@left` and
# `@right`. Here, we just re-dispatch it to keep consistent.
#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("[", "BPCellsMultiplySeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("[", "BPCellsRankTransformSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("[", "BPCellsRenameDimsSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("[", "BPCellsSubsetSeed", BPCellsSubset_internal)

#' @importMethodsFrom BPCells [
#' @export
#' @rdname BPCellsSeed-class
methods::setMethod("[", "BPCellsTransformedSeed", BPCellsSubset_internal)

##################################################################
# Following code shold be fixed by upstream BPCells package
# In BPCells, `[<-` was only defined for `IterableMatrix`
#' @inheritParams convert_mode
#' @return
#' - `[<-`: A [BPCellsSeed] object, the `mode` of `value` will be converted
#'   to be consistent with `x` using [convert_mode].
#' @importMethodsFrom BPCells [<-
#' @export
#' @order 3
#' @rdname BPCellsSeed-class
methods::setMethod(
    "[<-", c("BPCellsSeed", "ANY", "ANY", "ANY"),
    function(x, i, j, ..., value) {
        value <- coerce_dgCMatrix(value)
        methods::callGeneric()
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsSeed", "ANY", "ANY", "matrix"),
    function(x, i, j, ..., value) {
        x_mode <- storage_mode(x)
        value_mode <- type_to_mode(storage.mode(value))
        value <- methods::as(value, "dgCMatrix")
        if (x@transpose) {
            value <- t(BPCellsSeed(t(value)))
        } else {
            value <- BPCellsSeed(value)
        }
        if (x_mode == "uint32_t" && value_mode != "uint32_t") {
            cli::cli_warn("Convert {.arg value} into {.field uint32_t} mode")
            value <- convert_mode(value, "uint32_t")
        } else if (x_mode != "uint32_t" && value_mode == "uint32_t") {
            cli::cli_warn("Convert {.arg value} into {.field {x_mode}} mode")
            value <- convert_mode(value, x_mode)
        } else {
            value <- convert_mode(value, x_mode)
        }
        methods::callGeneric()
    }
)

#' @export
#' @rdname internal-methods
methods::setMethod(
    "[<-", c("BPCellsSeed", "ANY", "ANY", "dgCMatrix"),
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
    "[<-", c("BPCellsSeed", "ANY", "ANY", "BPCellsSeed"),
    function(x, i, j, ..., value) {
        fn <- methods::getMethod("[<-", "IterableMatrix", "BPCells")
        fn(
            x = x,
            i = rlang::maybe_missing(i),
            j = rlang::maybe_missing(j),
            ...,
            value = value
        )
    }
)
