`%||%` <- function(x, y) if (is.null(x)) y else x

bpcells_helper_classes <- c("ConvertMatrixType", "MatrixSubset")

extract_object <- function(x) {
    if (methods::is(x, "ConvertMatrixType") || methods::is(x, "MatrixSubset")) {
        x@matrix
    } else {
        x
    }
}

extract_bpcells_array <- function(x, index) {
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

bpcells_to_r_type <- function(x) {
    switch(x@type,
        uint32_t = "integer",
        float = "double",
        double = "double"
    )
}
