mat1 <- mock_matrix(30, 20)
mat2 <- mock_matrix(ncol(mat1), 100)
path <- normalizePath(
    c(tempfile(tmpdir = tmpdir), tempfile(tmpdir = tmpdir)),
    mustWork = FALSE
)
obj <- BPCells::write_matrix_dir(mat = as(mat1, "dgCMatrix"), dir = path[1L])
obj2 <- BPCells::write_matrix_dir(mat = as(mat2, "dgCMatrix"), dir = path[2L])
obj <- obj %*% obj2
mat <- mat1 %*% mat2

test_BPCellsArray(
    obj, path,
    mat = mat,
    name = "Multiply"
)
