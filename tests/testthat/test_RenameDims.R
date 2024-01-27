mat <- mock_matrix(2000, 200)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
dimnames(obj) <- list(
    paste0("G", seq_len(2000L)),
    paste0("C", seq_len(200))
)


testthat::test_that("`BPCellsRenameDimsSeed()` works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    obj <- DelayedArray(seed)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_identical(path(seed), path)
    testthat::expect_identical(path(obj), path)
})

testthat::test_that("subset `BPCellsRenameDimsSeed` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed[1:10, ], "BPCellsRenameDimsSeed")
    testthat::expect_s4_class(seed[, 1:10], "BPCellsRenameDimsSeed")
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsRenameDimsSeed")
})

testthat::test_that("subset `BPCellsMatrix` object works as expected", {
    obj <- BPCellsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_identical(path(obj), path)
    testthat::expect_s4_class(obj[1:10, ], "BPCellsMatrix")
    testthat::expect_s4_class(obj[, 1:10], "BPCellsMatrix")
    testthat::expect_s4_class(obj[1:10, 1:10], "BPCellsMatrix")
})

testthat::test_that("`convert_type` for `BPCellsRenameDimsSeed` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    float_seed <- convert_type(seed, "numeric")
    testthat::expect_s4_class(float_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(float_seed), "double")
    integer_seed <- convert_type(float_seed, "integer")
    testthat::expect_s4_class(integer_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(integer_seed), "integer")
})

testthat::test_that("`convert_type` for `BPCellsMatrix` object works as expected", {
    obj <- BPCellsArray(obj)
    float_obj <- convert_type(obj, "numeric")
    testthat::expect_s4_class(float_obj, "BPCellsMatrix")
    testthat::expect_identical(type(float_obj), "double")
    integer_obj <- convert_type(float_obj, "integer")
    testthat::expect_s4_class(integer_obj, "BPCellsMatrix")
    testthat::expect_identical(type(integer_obj), "integer")
})

testthat::test_that("`t()` for `BPCellsRenameDims` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_s4_class(t(seed), "BPCellsRenameDimsSeed")
    obj <- BPCellsArray(obj)
    testthat::expect_s4_class(t(obj), "BPCellsMatrix")
})

testthat::test_that("`dimnames<-` for `BPCellsRenameDims` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    dimnames(seed) <- list(
        paste0("G", seq_len(nrow(seed))),
        paste0("C", seq_len(ncol(seed)))
    )
    testthat::expect_s4_class(seed, "BPCellsSeed")
    obj <- BPCellsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    dimnames(obj) <- list(
        paste0("G", seq_len(nrow(obj))),
        paste0("C", seq_len(ncol(obj)))
    )
    testthat::expect_s4_class(obj, "BPCellsMatrix")
})

testthat::test_that("`rownames<-` for `BPCellsRenameDims` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_error(rownames(seed) <- list())
    testthat::expect_error(rownames(seed) <- 1:10)
    rownames(seed) <- paste0("G", seq_len(nrow(seed)))
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_identical(rownames(seed), paste0("G", seq_len(nrow(seed))))
    obj <- BPCellsMatrix(obj)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_error(rownames(obj) <- list())
    testthat::expect_error(rownames(obj) <- 1:10)
    rownames(obj) <- paste0("G", seq_len(nrow(obj)))
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_identical(rownames(obj), paste0("G", seq_len(nrow(obj))))
    rownames(seed) <- NULL
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_null(rownames(seed))
    rownames(obj) <- NULL
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_null(rownames(obj))
})

testthat::test_that("`colnames<-` for `BPCellsRenameDims` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_error(colnames(seed) <- list())
    testthat::expect_error(colnames(seed) <- 1:10)
    colnames(seed) <- paste0("G", seq_len(ncol(seed)))
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_identical(colnames(seed), paste0("G", seq_len(ncol(seed))))
    obj <- BPCellsMatrix(obj)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_error(colnames(obj) <- list())
    testthat::expect_error(colnames(obj) <- 1:10)
    colnames(obj) <- paste0("G", seq_len(ncol(obj)))
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_identical(colnames(obj), paste0("G", seq_len(ncol(obj))))
    colnames(seed) <- NULL
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_null(colnames(seed))
    colnames(obj) <- NULL
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_null(colnames(obj))
})

testthat::test_that("`%*%` for `BPCellsRenameDims` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_warning(temp <- seed %*% t(seed))
    testthat::expect_s4_class(temp, "BPCellsMultiplySeed")
    testthat::expect_true(is.matrix(seed %*% as.matrix(t(seed))))
    testthat::expect_true(is.matrix(seed %*% seq_len(ncol(seed))))
    testthat::expect_true(is.matrix(seq_len(nrow(seed)) %*% seed))
    obj <- BPCellsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_warning(temp <- obj %*% t(obj))
    testthat::expect_s4_class(temp, "BPCellsMatrix")
    testthat::expect_true(is.matrix(obj %*% as.matrix(t(obj))))
    testthat::expect_true(is.matrix(obj %*% seq_len(ncol(obj))))
    testthat::expect_true(is.matrix(seq_len(nrow(obj)) %*% obj))
})

testthat::test_that("`rbind` for `BPCellsRenameDims` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_s4_class(rbind2(seed, seed), "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(rbind(seed, seed), "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(arbind(seed, seed), "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(
        bindROWS(seed, list(seed)),
        "BPCellsRowBindMatrixSeed"
    )
    obj <- BPCellsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_s4_class(rbind2(obj, obj), "BPCellsMatrix")
    testthat::expect_s4_class(rbind(obj, obj), "BPCellsMatrix")
    testthat::expect_s4_class(arbind(obj, obj), "BPCellsMatrix")
    testthat::expect_s4_class(
        bindROWS(obj, list(obj)),
        "BPCellsMatrix"
    )
})

testthat::test_that("`cbind` for `BPCellsRenameDims` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_s4_class(cbind2(seed, seed), "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(cbind(seed, seed), "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(acbind(seed, seed), "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(
        bindCOLS(seed, list(seed)),
        "BPCellsColBindMatrixSeed"
    )
    obj <- BPCellsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_s4_class(cbind2(obj, obj), "BPCellsMatrix")
    testthat::expect_s4_class(cbind(obj, obj), "BPCellsMatrix")
    testthat::expect_s4_class(acbind(obj, obj), "BPCellsMatrix")
    testthat::expect_s4_class(bindCOLS(obj, list(obj)), "BPCellsMatrix")
})

testthat::test_that("`+` for `BPCellsRenameDimsSeed` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_s4_class(seed + 1, "BPCellsTransformScaleShiftSeed")
    testthat::expect_s4_class(seed + 1 + 10, "BPCellsTransformScaleShiftSeed")
    testthat::expect_s4_class(
        seed + seq_len(nrow(seed)),
        "BPCellsTransformScaleShiftSeed"
    )
    testthat::expect_s4_class(
        t(seed) + seq_len(ncol(seed)),
        "BPCellsTransformScaleShiftSeed"
    )
    testthat::expect_s4_class(1 + seed, "BPCellsTransformScaleShiftSeed")
    testthat::expect_s4_class(
        seq_len(nrow(seed)) + seed,
        "BPCellsTransformScaleShiftSeed"
    )
    testthat::expect_s4_class(
        seq_len(ncol(seed)) + t(seed),
        "BPCellsTransformScaleShiftSeed"
    )
})

testthat::test_that("`+` for `BPCellsMatrix` object works as expected", {
    obj <- BPCellsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_s4_class(obj + 1, "BPCellsMatrix")
    testthat::expect_s4_class(obj + 1 + 10, "BPCellsMatrix")
    testthat::expect_s4_class(obj + seq_len(nrow(obj)), "BPCellsMatrix")
    testthat::expect_s4_class(t(obj) + seq_len(ncol(obj)), "BPCellsMatrix")
    testthat::expect_s4_class(1 + obj, "BPCellsMatrix")
    testthat::expect_s4_class(seq_len(nrow(obj)) + obj, "BPCellsMatrix")
    testthat::expect_s4_class(seq_len(ncol(obj)) + t(obj), "BPCellsMatrix")
})

testthat::test_that("`-` for `BPCellsRenameDimsSeed` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_s4_class(seed - 1, "BPCellsTransformScaleShiftSeed")
    testthat::expect_s4_class(seed - 1 - 10, "BPCellsTransformScaleShiftSeed")
    testthat::expect_s4_class(
        seed - seq_len(nrow(seed)),
        "BPCellsTransformScaleShiftSeed"
    )
    testthat::expect_s4_class(
        t(seed) - seq_len(ncol(seed)),
        "BPCellsTransformScaleShiftSeed"
    )
    testthat::expect_s4_class(1 - seed, "BPCellsTransformScaleShiftSeed")
    testthat::expect_s4_class(
        seq_len(nrow(seed)) - seed,
        "BPCellsTransformScaleShiftSeed"
    )
    testthat::expect_s4_class(
        seq_len(ncol(seed)) - t(seed),
        "BPCellsTransformScaleShiftSeed"
    )
})

testthat::test_that("`-` for `BPCellsMatrix` object works as expected", {
    obj <- BPCellsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_s4_class(obj - 1, "BPCellsMatrix")
    testthat::expect_s4_class(obj - 1 - 10, "BPCellsMatrix")
    testthat::expect_s4_class(
        obj - seq_len(nrow(obj)),
        "BPCellsMatrix"
    )
    testthat::expect_s4_class(t(obj) - seq_len(ncol(obj)), "BPCellsMatrix")
    testthat::expect_s4_class(1 - obj, "BPCellsMatrix")
    testthat::expect_s4_class(seq_len(nrow(obj)) - obj, "BPCellsMatrix")
    testthat::expect_s4_class(seq_len(ncol(obj)) - t(obj), "BPCellsMatrix")
})

testthat::test_that("`*` for `BPCellsRenameDimsSeed` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_s4_class(seed * 1, "BPCellsTransformScaleShiftSeed")
    testthat::expect_s4_class(seed * 1 * 10, "BPCellsTransformScaleShiftSeed")
    testthat::expect_s4_class(
        seed * seq_len(nrow(seed)),
        "BPCellsTransformScaleShiftSeed"
    )
    testthat::expect_s4_class(
        t(seed) * seq_len(ncol(seed)),
        "BPCellsTransformScaleShiftSeed"
    )
    testthat::expect_s4_class(1 * seed, "BPCellsTransformScaleShiftSeed")
    testthat::expect_s4_class(
        seq_len(nrow(seed)) * seed,
        "BPCellsTransformScaleShiftSeed"
    )
    testthat::expect_s4_class(
        seq_len(ncol(seed)) * t(seed),
        "BPCellsTransformScaleShiftSeed"
    )
})

testthat::test_that("`*` for `BPCellsMatrix` object works as expected", {
    obj <- BPCellsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_s4_class(obj * 1, "BPCellsMatrix")
    testthat::expect_s4_class(obj * 1 * 10, "BPCellsMatrix")
    testthat::expect_s4_class(
        obj * seq_len(nrow(obj)),
        "BPCellsMatrix"
    )
    testthat::expect_s4_class(
        t(obj) * seq_len(ncol(obj)),
        "BPCellsMatrix"
    )
    testthat::expect_s4_class(1 * obj, "BPCellsMatrix")
    testthat::expect_s4_class(seq_len(nrow(obj)) * obj, "BPCellsMatrix")
    testthat::expect_s4_class(seq_len(ncol(obj)) * t(obj), "BPCellsMatrix")
})

testthat::test_that("`/` for `BPCellsRenameDimsSeed` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_s4_class(seed / 1, "BPCellsTransformScaleShiftSeed")
    testthat::expect_s4_class(seed / 1 / 10, "BPCellsTransformScaleShiftSeed")
    testthat::expect_s4_class(
        seed / seq_len(nrow(seed)),
        "BPCellsTransformScaleShiftSeed"
    )
    testthat::expect_s4_class(
        t(seed) / seq_len(ncol(seed)),
        "BPCellsTransformScaleShiftSeed"
    )
    testthat::expect_error(1 / seed)
})

testthat::test_that("`/` for `BPCellsMatrix` object works as expected", {
    obj <- BPCellsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsMatrix")
    testthat::expect_s4_class(obj / 1, "BPCellsMatrix")
    testthat::expect_s4_class(obj / 1 / 10, "BPCellsMatrix")
    testthat::expect_s4_class(obj / seq_len(nrow(obj)), "BPCellsMatrix")
    testthat::expect_s4_class(t(obj) / seq_len(ncol(obj)), "BPCellsMatrix")
    testthat::expect_error(1 / obj)
})
