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
