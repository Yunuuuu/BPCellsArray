mat <- mock_matrix(2000, 200)
path <- tempfile()
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
obj <- BPCells::convert_matrix_type(obj, "uint32_t")
obj <- obj %*% BPCells::transpose_storage_order(t(obj))

testthat::test_that("`BPCellsMultiplySeed()` works as expected", {
    seed <- BPCellsMultiplySeed(obj)
    testthat::expect_s4_class(seed, "BPCellsMultiplySeed")
    obj <- BPCellsMultiplyArray(seed)
    testthat::expect_s4_class(obj, "BPCellsMultiplyMatrix")
    testthat::expect_identical(path(seed), path(obj))
})

testthat::test_that("subset `BPCellsMultiplySeed` object works as expected", {
    seed <- BPCellsMultiplySeed(obj)
    testthat::expect_s4_class(seed, "BPCellsMultiplySeed")
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsMultiplySeed")
})

testthat::test_that("subset `BPCellsMultiplyMatrix` object works as expected", {
    obj <- BPCellsMultiplyArray(obj)
    testthat::expect_s4_class(obj, "BPCellsMultiplyMatrix")
    testthat::expect_s4_class(obj[1:10, 1:10], "BPCellsMultiplyMatrix")
})

testthat::test_that("`convert_type` for `BPCellsConvertSeed` object works as expected", {
    seed <- BPCellsMultiplySeed(obj)
    float_seed <- convert_type(seed, "numeric")
    testthat::expect_s4_class(float_seed, "BPCellsConvertSeed")
    testthat::expect_identical(type(float_seed), "double")
    BPCells::convert_matrix_type(float_seed, type = "uint32_t")
    integer_seed <- convert_type(float_seed, "integer")
    testthat::expect_s4_class(integer_seed, "BPCellsRenameDimsSeed")
    testthat::expect_identical(type(integer_seed), "integer")
})

testthat::test_that("`convert_type` for `BPCellsMultiplyMatrix` object works as expected", {
    obj <- BPCellsMultiplyArray(obj)
    float_obj <- convert_type(obj, "numeric")
    testthat::expect_s4_class(float_obj, "BPCellsConvertMatrix")
    testthat::expect_identical(type(float_obj), "double")
    integer_obj <- convert_type(float_obj, "integer")
    testthat::expect_s4_class(integer_obj, "BPCellsRenameDimsMatrix")
    testthat::expect_identical(type(integer_obj), "integer")
})
