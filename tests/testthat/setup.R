# Creating temporary directory
tmpdir <- testthat::test_path("_TEMP")
if (!dir.exists(tmpdir)) dir.create(tmpdir)
tmpdir <- normalizePath(tmpdir, mustWork = TRUE)
