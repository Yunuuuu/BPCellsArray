mat1 <- mock_matrix(30, 20)
mat2 <- mock_matrix(30, 20)
path <- normalizePath(c(tempfile(tmpdir = tmpdir), tempfile(tmpdir = tmpdir)), mustWork = FALSE)
mat <- rbind(mat1, mat2)
mat1 <- BPCells::write_matrix_dir(mat = as(mat1, "dgCMatrix"), dir = path[1L])
mat2 <- BPCells::write_matrix_dir(mat = as(mat2, "dgCMatrix"), dir = path[2L])
obj <- rbind(mat1, mat2)

test_methods(obj, mat = mat, name = "RowBindMatrices")
