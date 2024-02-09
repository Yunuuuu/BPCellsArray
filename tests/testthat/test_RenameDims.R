mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
dimnames(obj) <- list(
    paste0("G", seq_len(nrow(obj))),
    paste0("C", seq_len(ncol(obj)))
)
dimnames(mat) <- list(
    paste0("G", seq_len(nrow(obj))),
    paste0("C", seq_len(ncol(obj)))
)

common_test(
    obj, path,
    mat = mat, 
    name = "RenameDims"
)
testthat::test_that(
    "`subset()` BPCellsRenameDimsSeed object works as expected",
    {
        seed <- BPCellsSeed(obj)
        testthat::expect_s4_class(seed[1:10, ], "BPCellsRenameDimsSeed")
        testthat::expect_s4_class(seed[, 1:10], "BPCellsRenameDimsSeed")
        testthat::expect_s4_class(seed[1:10, 1:10], "BPCellsRenameDimsSeed")
    }
)
