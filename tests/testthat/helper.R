# When devtools::load_all is run, create symbolic link for sql directory
# Allows testing with devtools::test on osx and linux
if (Sys.getenv("DEVTOOLS_LOAD") == "true" & .Platform$OS.type == "unix") {
  print("setting sql folder symobolic link")
  packageRoot <- normalizePath(system.file("..", package = "SelfControlledCohort"))
  # Create symbolic link so code can be used in devtools::test()
  linkPath <- file.path(packageRoot, "sql")
  if (!file.exists(linkPath)) {
    R.utils::createLink(link = linkPath, system.file("sql", package = "SelfControlledCohort"))
    options("use.devtools.sql_shim" = TRUE)
    withr::defer(unlink(linkPath), testthat::teardown_env())
  }
}

checkManifestFiles <- function(rootDir) {
  checkmate::expect_file_exists(file.path(rootDir, "manifest.json"))
  manifest <- jsonlite::read_json(file.path(rootDir, "manifest.json"))

  for (file in manifest$files) {
    checkmate::assertFileExists(file.path(rootDir, file))
  }
}
