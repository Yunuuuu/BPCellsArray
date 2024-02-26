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

test_BPCellsArray(
    obj, path,
    mat = mat,
    name = "RenameDims"
)
