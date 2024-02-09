mat1 <- mock_matrix(30, 20)
mat2 <- mock_matrix(30, 20)
path <- normalizePath(c(tempfile(tmpdir = tmpdir), tempfile(tmpdir = tmpdir)), mustWork = FALSE)
obj1 <- BPCells::write_matrix_dir(mat = as(mat1, "dgCMatrix"), dir = path[1L])
obj2 <- BPCells::write_matrix_dir(mat = as(mat2, "dgCMatrix"), dir = path[2L])
obj <- cbind(obj1, obj2)
mat <- cbind(mat1, mat2)


common_test(
    obj, path,
    mat = mat, 
    name = "ColBindMatrix"
)
testthat::test_that("`subset()` BPCellsBindMatrixSeed object works as expected", {
    seed <- BPCellsSeed(obj)
    testthat::expect_s4_class(seed[1:10, ], "BPCellsColBindMatrixSeed")
    testthat::expect_s4_class(seed[, 1:10], "BPCellsSubsetSeed")
    testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsSubsetSeed")
})
