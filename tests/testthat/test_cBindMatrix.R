mat1 <- mock_matrix(2000, 200)
mat2 <- mock_matrix(2000, 200)
path <- c(tempfile(), tempfile())
obj1 <- BPCells::write_matrix_dir(mat = as(mat1, "dgCMatrix"), dir = path[1L])
obj2 <- BPCells::write_matrix_dir(mat = as(mat2, "dgCMatrix"), dir = path[2L])
obj <- cbind(obj1, obj2)


testthat::test_that("`BPCellsColBindMatrixSeed()` works as expected", {
    seed <- BPCellsColBindMatrixSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsColBindMatrixSeed")
    obj <- BPCellsColBindMatrixArray(seed)
    testthat::expect_s4_class(obj, "BPCellsColBindMatrixMatrix")
    testthat::expect_identical(path(seed), path)
    testthat::expect_identical(path(obj), path)
})

testthat::test_that(
    "subset `BPCellsColBindMatrixSeed` object works as expected",
    {
        seed <- BPCellsColBindMatrixSeed(obj)
        testthat::expect_s4_class(seed, "BPCellsColBindMatrixSeed")
        testthat::expect_identical(path(seed), path)
        testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsSubsetSeed")
    }
)

testthat::test_that("subset `BPCellsColBindMatrixMatrix` object works as expected", {
    obj <- BPCellsColBindMatrixArray(obj)
    testthat::expect_s4_class(obj, "BPCellsColBindMatrixMatrix")
    testthat::expect_identical(path(obj), path)
    testthat::expect_s4_class(obj[1:10, 1:10], "BPCellsSubsetMatrix")
})

testthat::test_that("`convert_type` for `BPCellsColBindMatrixSeed` object works as expected", {
    seed <- BPCellsColBindMatrixSeed(obj)
    float_seed <- convert_type(seed, "numeric")
    testthat::expect_s4_class(float_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(float_seed), "double")
    integer_seed <- convert_type(float_seed, "integer")
    testthat::expect_s4_class(integer_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(integer_seed), "integer")
})

testthat::test_that("`convert_type` for `BPCellsColBindMatrixMatrix` object works as expected", {
    obj <- BPCellsColBindMatrixArray(obj)
    float_obj <- convert_type(obj, "numeric")
    testthat::expect_s4_class(float_obj, "BPCellsConvertMatrix")
    testthat::expect_identical(type(float_obj), "double")
    integer_obj <- convert_type(float_obj, "integer")
    testthat::expect_s4_class(integer_obj, "BPCellsConvertMatrix")
    testthat::expect_identical(type(integer_obj), "integer")
})

testthat::test_that("`t()` for `BPCellsColBindMatrix` object works as expected", {
    seed <- BPCellsColBindMatrixSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(t(seed), "BPCellsColBindMatrixSeed")
    obj <- BPCellsColBindMatrixArray(obj)
    testthat::expect_s4_class(obj, "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(t(obj), "BPCellsColBindMatrixMatrix")
})

testthat::test_that("`dimnames<-` for `BPCellsColBindMatrix` object works as expected", {
    seed <- BPCellsColBindMatrixSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsColBindMatrixSeed")
    dimnames(seed) <- list(
        paste0("G", seq_len(nrow(seed))),
        paste0("C", seq_len(ncol(seed)))
    )
    testthat::expect_s4_class(seed, "BPCellsRenameDimsSeed")
    obj <- BPCellsColBindMatrixArray(obj)
    testthat::expect_s4_class(obj, "BPCellsColBindMatrixMatrix")
    dimnames(obj) <- list(
        paste0("G", seq_len(nrow(obj))),
        paste0("C", seq_len(ncol(obj)))
    )
    testthat::expect_s4_class(obj, "BPCellsRenameDimsMatrix")
})

testthat::test_that("`%*%` for `BPCellsColBindMatrix` object works as expected", {
    seed <- BPCellsColBindMatrixSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(seed %*% t(seed), "BPCellsMultiplySeed")
    testthat::expect_true(is.matrix(seed %*% as.matrix(t(seed))))
    testthat::expect_true(is.matrix(seed %*% seq_len(ncol(seed))))
    testthat::expect_true(is.matrix(seq_len(nrow(seed)) %*% seed))
    obj <- BPCellsColBindMatrixArray(obj)
    testthat::expect_s4_class(obj, "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(obj %*% t(obj), "BPCellsMultiplyMatrix")
    testthat::expect_true(is.matrix(obj %*% as.matrix(t(obj))))
    testthat::expect_true(is.matrix(obj %*% seq_len(ncol(obj))))
    testthat::expect_true(is.matrix(seq_len(nrow(obj)) %*% obj))
})

testthat::test_that("`rbind` for `BPCellsColBindMatrix` object works as expected", {
    seed <- BPCellsColBindMatrixSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(rbind2(seed, seed), "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(rbind(seed, seed), "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(arbind(seed, seed), "BPCellsRowBindMatrixSeed")
    testthat::expect_s4_class(
        bindROWS(seed, list(seed)),
        "BPCellsRowBindMatrixSeed"
    )
    obj <- BPCellsColBindMatrixArray(obj)
    testthat::expect_s4_class(obj, "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(rbind2(obj, obj), "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(rbind(obj, obj), "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(arbind(obj, obj), "BPCellsRowBindMatrixMatrix")
    testthat::expect_s4_class(
        bindROWS(obj, list(obj)),
        "BPCellsRowBindMatrixMatrix"
    )
})

testthat::test_that("`cbind` for `BPCellsColBindMatrix` object works as expected", {
    seed <- BPCellsColBindMatrixSeed(obj)
    testthat::expect_s4_class(seed, "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(cbind2(seed, seed), "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(cbind(seed, seed), "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(acbind(seed, seed), "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(
        bindCOLS(seed, list(seed)),
        "BPCellsColBindMatrixSeed"
    )
    obj <- BPCellsColBindMatrixArray(obj)
    testthat::expect_s4_class(obj, "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(cbind2(obj, obj), "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(cbind(obj, obj), "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(acbind(obj, obj), "BPCellsColBindMatrixMatrix")
    testthat::expect_s4_class(
        bindCOLS(obj, list(obj)),
        "BPCellsColBindMatrixMatrix"
    )
})
