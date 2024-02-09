mat1 <- mock_matrix(30, 20)
mat2 <- mock_matrix(30, 20)
path <- normalizePath(c(tempfile(tmpdir = tmpdir), tempfile(tmpdir = tmpdir)), mustWork = FALSE)
mat <- rbind(mat1, mat2)
mat1 <- BPCells::write_matrix_dir(mat = as(mat1, "dgCMatrix"), dir = path[1L])
mat2 <- BPCells::write_matrix_dir(mat = as(mat2, "dgCMatrix"), dir = path[2L])
obj <- rbind(mat1, mat2)

common_test(
    obj, path,
    mat = mat, 
    name = "RowBindMatrix"
)
testthat::test_that(
    "`subset()` BPCellsBindMatrixSeed object works as expected",
    {
        seed <- BPCellsSeed(obj)
        testthat::expect_s4_class(seed[1:10, ], "BPCellsSubsetSeed")
        testthat::expect_s4_class(seed[, 1:10], "BPCellsRowBindMatrixSeed")
        testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsSubsetSeed")
    }
)
