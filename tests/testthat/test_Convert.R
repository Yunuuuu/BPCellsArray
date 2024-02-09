mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
obj <- BPCells::convert_matrix_type(obj, "uint32_t")
mat <- matrix_to_integer(mat)

common_test(
    obj, path,
    mode = "uint32_t",
    mat = mat, name = "Convert"
)
testthat::test_that("`subset()` BPCellsConvertSeed object works as expected", {
    seed <- BPCellsSeed(obj)
    testthat::expect_s4_class(seed[1:10, ], "BPCellsConvertSeed")
    testthat::expect_s4_class(seed[, 1:10], "BPCellsConvertSeed")
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsConvertSeed")
})
