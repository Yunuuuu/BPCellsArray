mat <- mock_matrix(2000, 200)
mat2 <- mock_matrix(2000, 200)
path <- c(tempfile(), tempfile())
mat <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path[1L])
mat2 <- BPCells::write_matrix_dir(mat = as(mat2, "dgCMatrix"), dir = path[2L])
obj <- rbind(mat, mat2)

testthat::test_that("`BPCellsRowBindMatrixSeed()` works as expected", {
    seed <- BPCellsRowBindMatrixSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRowBindMatrixSeed")
    obj <- BPCellsRowBindMatrixArray(seed)
    testthat::expect_s4_class(obj, "BPCellsRowBindMatrixMatrix")
    testthat::expect_identical(path(seed), path)
    testthat::expect_identical(path(obj), path)
})

testthat::test_that(
    "subset `BPCellsRowBindMatrixSeed` object works as expected",
    {
        seed <- BPCellsRowBindMatrixSeed(obj)
        testthat::expect_s4_class(seed, "BPCellsRowBindMatrixSeed")
        testthat::expect_identical(path(seed), path)
        testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsSubsetSeed")
    }
)

testthat::test_that("subset `BPCellsRowBindMatrixMatrix` object works as expected", {
    obj <- BPCellsRowBindMatrixArray(obj)
    testthat::expect_s4_class(obj, "BPCellsRowBindMatrixMatrix")
    testthat::expect_identical(path(obj), path)
    testthat::expect_s4_class(obj[1:10, 1:10], "BPCellsSubsetMatrix")
})

testthat::test_that("`convert_type` for `BPCellsRowBindMatrixSeed` object works as expected", {
    seed <- BPCellsRowBindMatrixSeed(obj)
    float_seed <- convert_type(seed, "numeric")
    testthat::expect_s4_class(float_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(float_seed), "double")
    integer_seed <- convert_type(float_seed, "integer")
    testthat::expect_s4_class(integer_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(integer_seed), "integer")
})

testthat::test_that("`convert_type` for `BPCellsRowBindMatrixMatrix` object works as expected", {
    obj <- BPCellsRowBindMatrixArray(obj)
    float_obj <- convert_type(obj, "numeric")
    testthat::expect_s4_class(float_obj, "BPCellsConvertMatrix")
    testthat::expect_identical(type(float_obj), "double")
    integer_obj <- convert_type(float_obj, "integer")
    testthat::expect_s4_class(integer_obj, "BPCellsConvertMatrix")
    testthat::expect_identical(type(integer_obj), "integer")
})

testthat::test_that("`t()` for `BPCellsRowBindMatrix` object works as expected", {
    seed <- BPCellsRowBindMatrixSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(t(seed), "BPCellsRowBindMatrixSeed")
    obj <- BPCellsRowBindMatrixArray(obj)
    testthat::expect_s4_class(obj, "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(t(obj), "BPCellsRowBindMatrixMatrix")
})

testthat::test_that("`dimnames<-` for `BPCellsRowBindMatrix` object works as expected", {
    seed <- BPCellsRowBindMatrixSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRowBindMatrixSeed")
    dimnames(seed) <- list(
        paste0("G", seq_len(nrow(seed))),
        paste0("C", seq_len(ncol(seed)))
    )
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    obj <- BPCellsRowBindMatrixArray(obj)
    testthat::expect_s4_class(obj, "BPCellsRowBindMatrixMatrix")
    dimnames(obj) <- list(
        paste0("G", seq_len(nrow(obj))),
        paste0("C", seq_len(ncol(obj)))
    )
    testthat::expect_s4_class(obj, "BPCellsRenameDimsMatrix")
})

testthat::test_that("`%*%` for `BPCellsRowBindMatrix` object works as expected", {
    seed <- BPCellsRowBindMatrixSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRowBindMatrixSeed")
    testthat::expect_warning(temp <- seed %*% t(seed))
    testthat::expect_s4_class(temp, "BPCellsMultiplySeed")
    testthat::expect_true(is.matrix(seed %*% as.matrix(t(seed))))
    testthat::expect_true(is.matrix(seed %*% seq_len(ncol(seed))))
    testthat::expect_true(is.matrix(seq_len(nrow(seed)) %*% seed))
    obj <- BPCellsRowBindMatrixArray(obj)
    testthat::expect_s4_class(obj, "BPCellsRowBindMatrixMatrix")
    testthat::expect_warning(temp <- obj %*% t(obj))
    testthat::expect_s4_class(temp, "BPCellsMultiplyMatrix")
    testthat::expect_true(is.matrix(obj %*% as.matrix(t(obj))))
    testthat::expect_true(is.matrix(obj %*% seq_len(ncol(obj))))
    testthat::expect_true(is.matrix(seq_len(nrow(obj)) %*% obj))
})

testthat::test_that("`rbind` for `BPCellsRowBindMatrix` object works as expected", {
    seed <- BPCellsRowBindMatrixSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(rbind2(seed, seed), "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(rbind(seed, seed), "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(arbind(seed, seed), "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(
        bindROWS(seed, list(seed)),
        "BPCellsRowBindMatrixSeed"
    )
    obj <- BPCellsRowBindMatrixArray(obj)
    testthat::expect_s4_class(obj, "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(rbind2(obj, obj), "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(rbind(obj, obj), "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(arbind(obj, obj), "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(
        bindROWS(obj, list(obj)),
        "BPCellsRowBindMatrixMatrix"
    )
})

testthat::test_that("`cbind` for `BPCellsRowBindMatrix` object works as expected", {
    seed <- BPCellsRowBindMatrixSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(cbind2(seed, seed), "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(cbind(seed, seed), "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(acbind(seed, seed), "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(
        bindCOLS(seed, list(seed)),
        "BPCellsColBindMatrixSeed"
    )
    obj <- BPCellsRowBindMatrixArray(obj)
    testthat::expect_s4_class(obj, "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(cbind2(obj, obj), "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(cbind(obj, obj), "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(acbind(obj, obj), "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(
        bindCOLS(obj, list(obj)),
        "BPCellsColBindMatrixMatrix"
    )
})
