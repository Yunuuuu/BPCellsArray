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
