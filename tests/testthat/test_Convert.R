mat <- mock_matrix(2000, 200)
path <- tempfile()
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
obj <- BPCells::convert_matrix_type(obj, "uint32_t")

testthat::test_that("`BPCellsConvertSeed()` works as expected", {
    seed <- BPCellsConvertSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsConvertSeed")
    obj <- BPCellsConvertArray(seed)
    testthat::expect_s4_class(obj, "BPCellsConvertMatrix")
    testthat::expect_identical(path(seed), path)
    testthat::expect_identical(path(obj), path)
})

testthat::test_that("subset `BPCellsConvertSeed` object works as expected", {
    seed <- BPCellsConvertSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsConvertSeed")
    testthat::expect_identical(path(seed), path)
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsConvertSeed")
})

testthat::test_that("subset `BPCellsConvertMatrix` object works as expected", {
    obj <- BPCellsConvertArray(obj)
    testthat::expect_s4_class(obj, "BPCellsConvertMatrix")
    testthat::expect_identical(path(obj), path)
    testthat::expect_s4_class(obj[1:10, 1:10], "BPCellsConvertMatrix")
})

testthat::test_that("`convert_type` for `BPCellsConvertSeed` object works as expected", {
    seed <- BPCellsConvertSeed(obj)
    float_seed <- convert_type(seed, "numeric")
    testthat::expect_s4_class(float_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(float_seed), "double")
    integer_seed <- convert_type(float_seed, "integer")
    testthat::expect_s4_class(integer_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(integer_seed), "integer")
})

testthat::test_that("`convert_type` for `BPCellsConvertMatrix` object works as expected", {
    obj <- BPCellsConvertArray(obj)
    float_obj <- convert_type(obj, "numeric")
    testthat::expect_s4_class(float_obj, "BPCellsConvertMatrix")
    testthat::expect_identical(type(float_obj), "double")
    integer_obj <- convert_type(float_obj, "integer")
    testthat::expect_s4_class(integer_obj, "BPCellsConvertMatrix")
    testthat::expect_identical(type(integer_obj), "integer")
})

testthat::test_that("`t()` for `BPCellsConvert` object works as expected", {
    seed <- BPCellsConvertSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsConvertSeed")
    testthat::expect_s4_class(t(seed), "BPCellsConvertSeed")
    obj <- BPCellsConvertArray(obj)
    testthat::expect_s4_class(obj, "BPCellsConvertMatrix")
    testthat::expect_s4_class(t(obj), "BPCellsConvertMatrix")
})

testthat::test_that("`dimnames<-` for `BPCellsConvert` object works as expected", {
    seed <- BPCellsConvertSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsConvertSeed")
    dimnames(seed) <- list(
        paste0("G", seq_len(nrow(seed))),
        paste0("C", seq_len(ncol(seed)))
    )
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    obj <- BPCellsConvertArray(obj)
    testthat::expect_s4_class(obj, "BPCellsConvertMatrix")
    dimnames(obj) <- list(
        paste0("G", seq_len(nrow(obj))),
        paste0("C", seq_len(ncol(obj)))
    )
    testthat::expect_s4_class(obj, "BPCellsRenameDimsMatrix")
})

testthat::test_that("`%*%` for `BPCellsConvert` object works as expected", {
    seed <- BPCellsConvertSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsConvertSeed")
    testthat::expect_warning(temp <- seed %*% t(seed))
    testthat::expect_s4_class(temp, "BPCellsMultiplySeed")
    testthat::expect_true(is.matrix(seed %*% as.matrix(t(seed))))
    testthat::expect_true(is.matrix(seed %*% seq_len(ncol(seed))))
    testthat::expect_true(is.matrix(seq_len(nrow(seed)) %*% seed))
    obj <- BPCellsConvertArray(obj)
    testthat::expect_s4_class(obj, "BPCellsConvertMatrix")
    testthat::expect_warning(temp <- obj %*% t(obj))
    testthat::expect_s4_class(temp, "BPCellsMultiplyMatrix")
    testthat::expect_true(is.matrix(obj %*% as.matrix(t(obj))))
    testthat::expect_true(is.matrix(obj %*% seq_len(ncol(obj))))
    testthat::expect_true(is.matrix(seq_len(nrow(obj)) %*% obj))
})

testthat::test_that("`rbind` for `BPCellsConvert` object works as expected", {
    seed <- BPCellsConvertSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsConvertSeed")
    testthat::expect_s4_class(rbind2(seed, seed), "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(rbind(seed, seed), "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(arbind(seed, seed), "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(
        bindROWS(seed, list(seed)),
        "BPCellsRowBindMatrixSeed"
    )
    obj <- BPCellsConvertArray(obj)
    testthat::expect_s4_class(obj, "BPCellsConvertMatrix")
    testthat::expect_s4_class(rbind2(obj, obj), "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(rbind(obj, obj), "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(arbind(obj, obj), "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(
        bindROWS(obj, list(obj)),
        "BPCellsRowBindMatrixMatrix"
    )
})

testthat::test_that("`cbind` for `BPCellsConvert` object works as expected", {
    seed <- BPCellsConvertSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsConvertSeed")
    testthat::expect_s4_class(cbind2(seed, seed), "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(cbind(seed, seed), "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(acbind(seed, seed), "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(
        bindCOLS(seed, list(seed)),
        "BPCellsColBindMatrixSeed"
    )
    obj <- BPCellsConvertArray(obj)
    testthat::expect_s4_class(obj, "BPCellsConvertMatrix")
    testthat::expect_s4_class(cbind2(obj, obj), "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(cbind(obj, obj), "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(acbind(obj, obj), "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(
        bindCOLS(obj, list(obj)),
        "BPCellsColBindMatrixMatrix"
    )
})

testthat::test_that("`+` for `BPCellsConvertSeed` object works as expected", {
    seed <- BPCellsConvertSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsConvertSeed")
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

testthat::test_that("`+` for `BPCellsConvertMatrix` object works as expected", {
    obj <- BPCellsConvertArray(obj)
    testthat::expect_s4_class(obj, "BPCellsConvertMatrix")
    testthat::expect_s4_class(obj + 1, "BPCellsTransformScaleShiftMatrix")
    testthat::expect_s4_class(obj + 1 + 10, "BPCellsTransformScaleShiftMatrix")
    testthat::expect_s4_class(
        obj + seq_len(nrow(obj)),
        "BPCellsTransformScaleShiftMatrix"
    )
    testthat::expect_s4_class(
        t(obj) + seq_len(ncol(obj)),
        "BPCellsTransformScaleShiftMatrix"
    )
    testthat::expect_s4_class(1 + obj, "BPCellsTransformScaleShiftMatrix")
    testthat::expect_s4_class(
        seq_len(nrow(obj)) + obj,
        "BPCellsTransformScaleShiftMatrix"
    )
    testthat::expect_s4_class(
        seq_len(ncol(obj)) + t(obj),
        "BPCellsTransformScaleShiftMatrix"
    )
})

testthat::test_that("`-` for `BPCellsConvertSeed` object works as expected", {
    seed <- BPCellsConvertSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsConvertSeed")
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

testthat::test_that("`-` for `BPCellsConvertMatrix` object works as expected", {
    obj <- BPCellsConvertArray(obj)
    testthat::expect_s4_class(obj, "BPCellsConvertMatrix")
    testthat::expect_s4_class(obj - 1, "BPCellsTransformScaleShiftMatrix")
    testthat::expect_s4_class(obj - 1 - 10, "BPCellsTransformScaleShiftMatrix")
    testthat::expect_s4_class(
        obj - seq_len(nrow(obj)),
        "BPCellsTransformScaleShiftMatrix"
    )
    testthat::expect_s4_class(
        t(obj) - seq_len(ncol(obj)),
        "BPCellsTransformScaleShiftMatrix"
    )
    testthat::expect_s4_class(1 - obj, "BPCellsTransformScaleShiftMatrix")
    testthat::expect_s4_class(
        seq_len(nrow(obj)) - obj,
        "BPCellsTransformScaleShiftMatrix"
    )
    testthat::expect_s4_class(
        seq_len(ncol(obj)) - t(obj),
        "BPCellsTransformScaleShiftMatrix"
    )
})

testthat::test_that("`*` for `BPCellsConvertSeed` object works as expected", {
    seed <- BPCellsConvertSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsConvertSeed")
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

testthat::test_that("`*` for `BPCellsConvertMatrix` object works as expected", {
    obj <- BPCellsConvertArray(obj)
    testthat::expect_s4_class(obj, "BPCellsConvertMatrix")
    testthat::expect_s4_class(obj * 1, "BPCellsTransformScaleShiftMatrix")
    testthat::expect_s4_class(obj * 1 * 10, "BPCellsTransformScaleShiftMatrix")
    testthat::expect_s4_class(
        obj * seq_len(nrow(obj)),
        "BPCellsTransformScaleShiftMatrix"
    )
    testthat::expect_s4_class(
        t(obj) * seq_len(ncol(obj)),
        "BPCellsTransformScaleShiftMatrix"
    )
    testthat::expect_s4_class(1 * obj, "BPCellsTransformScaleShiftMatrix")
    testthat::expect_s4_class(
        seq_len(nrow(obj)) * obj,
        "BPCellsTransformScaleShiftMatrix"
    )
    testthat::expect_s4_class(
        seq_len(ncol(obj)) * t(obj),
        "BPCellsTransformScaleShiftMatrix"
    )
})

testthat::test_that("`/` for `BPCellsConvertSeed` object works as expected", {
    seed <- BPCellsConvertSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsConvertSeed")
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

testthat::test_that("`/` for `BPCellsConvertMatrix` object works as expected", {
    obj <- BPCellsConvertArray(obj)
    testthat::expect_s4_class(obj, "BPCellsConvertMatrix")
    testthat::expect_s4_class(obj / 1, "BPCellsTransformScaleShiftMatrix")
    testthat::expect_s4_class(obj / 1 / 10, "BPCellsTransformScaleShiftMatrix")
    testthat::expect_s4_class(
        obj / seq_len(nrow(obj)),
        "BPCellsTransformScaleShiftMatrix"
    )
    testthat::expect_s4_class(
        t(obj) / seq_len(ncol(obj)),
        "BPCellsTransformScaleShiftMatrix"
    )
    testthat::expect_error(1 / obj)
})
