mat <- mock_matrix(20, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
obj <- BPCells::convert_matrix_type(obj, "uint32_t")

common_test(
    obj, path,
    mat = mat, 
    seed_fn = BPCellsConvertSeed, 
    name = "Convert"
)
testthat::test_that("`subset()` BPCellsConvertSeed object works as expected", {
    seed <- BPCellsConvertSeed(obj)
    testthat::expect_s4_class(seed[1:10, ], "BPCellsConvertSeed")
    testthat::expect_s4_class(seed[, 1:10], "BPCellsConvertSeed")
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsConvertSeed")
})
