#' DelayedArray backend of BPCells matrix
#'
#' The `BPCellsMatrix` class just inherits from the
#' [DelayedMatrix][DelayedArray::DelayedMatrix] class.
#'
#' @seealso
#' - [set_seed_form]: Manage the seed form.
#' - [bind][BPCells-bind]: Combine two Objects by Columns or Rows.
#' - [%*%][BPCells-Multiplication]: Matrix Multiplication.
#' - [crossprod][BPCells-crossprod]: Matrix Crossproduct.
#' - [summarization][BPCells-Summarization]: row/col summarization.
#' - [Arith][BPCells-Arithmetic]: Binary Arithmetic operators.
#' - [Math][BPCells-Math]: Math operators.
#' - [Compare][BPCells-Compare]: Compare matrix.
#' - [pmin2/pmax2][pmin2]: Maxima and Minima.
#' - [DelayedArray-utils]: Common operations on DelayedArray objects
#' @aliases BPCellsMatrix-methods
#' @inherit BPCellsSeed-class seealso
#' @name BPCellsMatrix-class
NULL


#' @inheritParams set_seed_form
#' @return
#'  - `BPCellsArray` and `BPCellsMatrix`: A `BPCellsMatrix` object, since
#'    `BPCells` can only support 2-dim array.
#' @param x,object A [BPCellsMatrix][BPCellsMatrix-class] object
#' @export
#' @rdname BPCellsMatrix-class
BPCellsArray <- function(x, seed_form = NULL) {
    seed_form <- match_seed_form(seed_form)
    with_seed_form(seed_form, DelayedArray(BPCellsSeed(x)))
}

#' @export
#' @rdname BPCellsMatrix-class
BPCellsMatrix <- BPCellsArray

#' @include Seed-management.R
#' @export
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsArray",
    contains = "DelayedArray",
    slots = list(SeedForm = "character"),
    prototype = list(SeedForm = GlobalOptions$SeedForm)
)

#' @export
#' @rdname BPCellsMatrix-class
methods::setClass("BPCellsMatrix",
    contains = "DelayedMatrix",
    slots = list(SeedForm = "character"),
    prototype = list(SeedForm = GlobalOptions$SeedForm)
)

#' @return
#'  - `matrixClass`: A string, always be `"BPCellsMatrix"`.
#' @importFrom DelayedArray matrixClass
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("matrixClass", "BPCellsArray", function(x) {
    "BPCellsMatrix"
})

.validate_BPCellsArray <- function(object) {
    .validate_seed(object@seed, arg = "@seed")
    .validate_seed_form(object@SeedForm, arg = "@SeedForm")
}

methods::setValidity("BPCellsArray", .validate_BPCellsArray)
methods::setValidity("BPCellsMatrix", .validate_BPCellsArray)

# Since BPCells only support 2-dim matrix, `DelayedArray` will always
# return a `BPCellsMatrix` object.
#' @param seed A [IterableMatrix][BPCellsSeed-class] or
#' [BPCellsDelayedOp][BPCellsSeed-class] object.
#' @importFrom DelayedArray DelayedArray
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("DelayedArray", "IterableMatrix", function(seed) {
    # DelayedArray can only accept one argument,
    # so we always use `with_seed_form` with `DelayedArray` function and just
    # assign the `seed_form` value into the `BPCellsArray` object after creating
    # `BPCellsArray` object
    seed_form <- GlobalOptions$SeedForm
    if (seed_form == "DelayedArray") seed <- to_DelayedArray(seed) # styler: off
    object <- DelayedArray::new_DelayedArray(seed, Class = "BPCellsArray")
    object@SeedForm <- seed_form
    object
})

#' @include Class-Delayed.R
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("DelayedArray", "BPCellsDelayedOp", function(seed) {
    object <- DelayedArray::new_DelayedArray(seed, Class = "BPCellsArray")
    object@SeedForm <- "DelayedArray"
    object
})

##############################################################
#' @param seed_form A string, `"BPCells"` or `"DelayedArray"`, if `NULL`, will
#' use the default value (use `set_seed_form()` with missing argument to check).
#' @export
#' @rdname set_seed_form
methods::setMethod("set_seed_form", "BPCellsMatrix", function(x, seed_form = NULL) {
    seed_form <- match_seed_form(seed_form)
    if (x@SeedForm == seed_form) {
        msg <- "{.arg x@seed} is already in {.pkg {seed_form}} format"
        cli::cli_inform(c_msg(msg, "nothing to do", sep = ", "))
        return(x)
    }
    with_seed_form(seed_form, DelayedArray(to_BPCells(x@seed)))
})

###################################################################
.show_internal <- function(object) {
    methods::callNextMethod()
    cat("\n")
    cat(sprintf("Seed form: %s\n", object@SeedForm))
    cat(sprintf("Storage Data type: %s\n", storage_mode(object)))
    cat(sprintf("Storage axis: %s major\n", storage_axis(object)))

    cat("\n")
    cat("Queued Operations:\n")
    showtree(object@seed)
    invisible(object)
}

#' @importFrom methods show
#' @export
#' @order 1
#' @rdname BPCellsMatrix-class
methods::setMethod("show", "BPCellsArray", .show_internal)

#' @export
#' @order 2
#' @rdname BPCellsMatrix-class
methods::setMethod("show", "BPCellsMatrix", .show_internal)

##############################################################
# helper function to re-dispath `DelayedArray` method
# should just used for `BPCellsMatrix` method
array_call_DelayedArray_method <- function(..., Array = NULL, type = "S4") {
    method <- switch(type,
        S4 = quote(methods::callNextMethod()),
        S3 = quote(NextMethod())
    )
    args <- rlang::pairlist2(...)
    Array <- rlang::sym(Array %||% names(args)[[1L]])
    # extract whether should be delayed
    seed_form <- list(substitute(
        seed_form <- Array@SeedForm,
        list(Array = Array)
    ))

    # for some method, it will return DelayedArray directly although
    # @seed is compatible with `BPCellsMatrix`.
    # here we just re-creating a BPCellsMatrix object when it could be.
    after <- expression(
        object <- with_seed_form(seed_form, DelayedArray(object))
    )
    after <- c(after, expression(
        # we check if object is a `BPCellsMatrix` object, if not, we warn it
        if (!(methods::is(object, "BPCellsMatrix") ||
            methods::is(object, "BPCellsArray"))) {
            cli::cli_warn(c(
                sprintf("{.fn %s} method return a {.cls {obj_s4_friendly(object)}} object", .Generic), # nolint
                i = "Subsequent operation won't use {.pkg BPCells} methods"
            ))
        },
        object
    ))
    new_method(args,
        before = seed_form,
        method = method, after = after
    )
}

# hepler function to call BPCells method for `BPCellsArray`
# running order
# 1. before - extract seed_form - to_BPCells
# 2. method
# 3. body - DelayedArray - after
#' @include utils.R
array_call_BPCells_method <- function(..., before = NULL, method = NULL, body = NULL, after = NULL, Arrays = NULL) {
    method <- method %||% quote(methods::callGeneric())
    args <- rlang::pairlist2(...)
    Arrays <- rlang::syms(Arrays %||% names(args)[[1L]])
    # extract seed_form, always respect the first Array
    seed_form <- substitute(
        seed_form <- Array@SeedForm,
        list(Array = Arrays[[1L]])
    )
    before <- c(
        before, list(seed_form),
        # transform all Arrays into BPCells object
        lapply(Arrays, function(Array) {
            substitute(Array <- to_BPCells(Array@seed), list(Array = Array))
        })
    )
    # then transform IterableMatrix into BPCellsMatrix
    back <- quote(with_seed_form(seed_form, DelayedArray(object))) # nolint
    if (!is.null(after)) {
        after <- c(list(substitute(object <- back, list(back = back))), after)
    } else {
        after <- back
    }
    after <- c(body, after)
    new_method(args,
        before = before,
        method = method, after = after
    )
}

###########################################################
#' @export
methods::setAs("BPCellsMatrix", "dgCMatrix", function(from) {
    methods::as(to_BPCells(from@seed), "dgCMatrix")
})

# Default drop use `as.array` and `aperm` methods
### S3/S4 combo for aperm.BPCellsMatrix
# list_methods("DelayedAperm")
aperm.BPCellsMatrix <- array_call_DelayedArray_method(
    a = , perm = , ... = , type = "S3"
)

#' @importFrom BiocGenerics aperm
methods::setMethod(
    "aperm", "BPCellsMatrix",
    array_call_DelayedArray_method(a = , perm = , ... = )
)

#' @return
#'  - `as.matrix`: A dense matrix.
#' @exportS3Method base::as.matrix
#' @rdname BPCellsMatrix-class
as.matrix.BPCellsMatrix <- function(x) {
    as_matrix_IterableMatrix(to_BPCells(x@seed))
}

#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("as.matrix", "BPCellsMatrix", as.matrix.BPCellsMatrix)

# S3/S4 combo for as.array.BPCellsMatrix
#' @inheritParams BPCellsSeed-class
#' @exportS3Method base::as.array
#' @rdname BPCellsMatrix-class
as.array.BPCellsMatrix <- as.array.BPCellsDelayedOp

#' @return
#'  - `as.array`: A dense matrix or an atomic vector.
#' @export
#' @rdname BPCellsMatrix-class
methods::setMethod("as.array", "BPCellsMatrix", as.array.BPCellsMatrix)

##########################################################
### Seed contract
###
# https://github.com/Bioconductor/DelayedArray/blob/devel/R/DelayedOp-class.R
# for `dim`, `dimnames`, `extract_array` and `is_sparse` just use the methods
# from `DelayedArray`
#' For BPCellsMatrix object
#' @importMethodsFrom DelayedArray dim
#' @importMethodsFrom DelayedArray dimnames
#' @importMethodsFrom DelayedArray extract_array
#' @importMethodsFrom DelayedArray is_sparse
#' @importMethodsFrom DelayedArray OLD_extract_sparse_array
#' @noRd
NULL

#######################################################
#' @return
#' - `t`: A `BPCellsMatrix` object.
#' @importMethodsFrom BPCells t
#' @export
#' @aliases t
#' @rdname BPCellsMatrix-class
methods::setMethod(
    "t", "BPCellsMatrix",
    array_call_BPCells_method(x = )
)

#' @importFrom methods Ops
methods::setMethod(
    "Ops", c("BPCellsArray", "vector"),
    array_call_DelayedArray_method(e1 = , e2 = )
)

methods::setMethod(
    "Ops", c("vector", "BPCellsArray"),
    array_call_DelayedArray_method(e1 = , e2 = , Array = "e2")
)

methods::setMethod(
    "Ops", c("BPCellsArray", "BPCellsArray"),
    array_call_DelayedArray_method(e1 = , e2 = )
)
