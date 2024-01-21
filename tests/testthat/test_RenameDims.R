mat <- mock_matrix(2000, 200)
path <- tempfile()
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
dimnames(obj) <- list(
    paste0("G", seq_len(2000L)),
    paste0("C", seq_len(200))
)

testthat::test_that("`BPCellsRenameDimsSeed()` works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    obj <- BPCellsRenameDimsArray(seed)
    testthat::expect_s4_class(obj, "BPCellsRenameDimsMatrix")
    testthat::expect_identical(path(seed), path(obj))
})

testthat::test_that("subset `BPCellsRenameDimsSeed` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsRenameDimsSeed")
})

testthat::test_that("subset `BPCellsRenameDimsMatrix` object works as expected", {
    obj <- BPCellsRenameDimsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsRenameDimsMatrix")
    testthat::expect_s4_class(obj[1:10, 1:10], "BPCellsRenameDimsMatrix")
})

testthat::test_that("`convert_type` for `BPCellsConvertSeed` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    float_seed <- convert_type(seed, "numeric")
    testthat::expect_s4_class(float_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(float_seed), "double")
    BPCells::convert_matrix_type(float_seed, type = "uint32_t")
    integer_seed <- convert_type(float_seed, "integer")
    testthat::expect_s4_class(integer_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(integer_seed), "integer")
})

testthat::test_that("`convert_type` for `BPCellsRenameDimsMatrix` object works as expected", {
    obj <- BPCellsRenameDimsArray(obj)
    float_obj <- convert_type(obj, "numeric")
    testthat::expect_s4_class(float_obj, "BPCellsConvertMatrix")
    testthat::expect_identical(type(float_obj), "double")
    integer_obj <- convert_type(float_obj, "integer")
    testthat::expect_s4_class(integer_obj, "BPCellsConvertMatrix")
    testthat::expect_identical(type(integer_obj), "integer")
})

testthat::test_that("`t()` for `BPCellsRenameDims` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_s4_class(t(seed), "BPCellsRenameDimsSeed")
    obj <- BPCellsRenameDimsArray(obj)
    testthat::expect_s4_class(t(obj), "BPCellsRenameDimsMatrix")
})

testthat::test_that("`dimnames<-` for `BPCellsRenameDims` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    dimnames(seed) <- list(
        paste0("G", seq_len(nrow(seed))),
        paste0("C", seq_len(ncol(seed)))
    )
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    obj <- BPCellsRenameDimsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsRenameDimsMatrix")
    dimnames(obj) <- list(
        paste0("G", seq_len(nrow(obj))),
        paste0("C", seq_len(ncol(obj)))
    )
    testthat::expect_s4_class(obj, "BPCellsRenameDimsMatrix")
})

testthat::test_that("`%*%` for `BPCellsRenameDims` object works as expected", {
    seed <- BPCellsRenameDimsSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    testthat::expect_warning(temp <- seed %*% t(seed))
    testthat::expect_s4_class(temp, "BPCellsMultiplySeed")
    testthat::expect_true(is.matrix(seed %*% as.matrix(t(seed))))
    testthat::expect_true(is.matrix(seed %*% seq_len(ncol(seed))))
    testthat::expect_true(is.matrix(seq_len(nrow(seed)) %*% seed))
    obj <- BPCellsRenameDimsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsRenameDimsMatrix")
    testthat::expect_warning(temp <- obj %*% t(obj))
    testthat::expect_s4_class(temp, "BPCellsMultiplyMatrix")
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
    obj <- BPCellsRenameDimsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsRenameDimsMatrix")
    testthat::expect_s4_class(rbind2(obj, obj), "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(rbind(obj, obj), "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(arbind(obj, obj), "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(
        bindROWS(obj, list(obj)),
        "BPCellsRowBindMatrixMatrix"
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
    obj <- BPCellsRenameDimsArray(obj)
    testthat::expect_s4_class(obj, "BPCellsRenameDimsMatrix")
    testthat::expect_s4_class(cbind2(obj, obj), "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(cbind(obj, obj), "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(acbind(obj, obj), "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(
        bindCOLS(obj, list(obj)),
        "BPCellsColBindMatrixMatrix"
    )
})
