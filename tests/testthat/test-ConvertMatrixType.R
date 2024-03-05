mat <- mock_matrix(30, 20)
path <- normalizePath(tempfile(tmpdir = tmpdir), mustWork = FALSE)
obj <- BPCells::write_matrix_dir(mat = as(mat, "dgCMatrix"), dir = path)
obj <- BPCells::convert_matrix_type(obj, "uint32_t")
mat <- matrix_to_integer(mat)

test_methods(obj, mode = "uint32_t", mat = mat, name = "ConvertMatrixType")
