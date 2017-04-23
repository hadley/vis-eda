cache <- function(name, code) {
  path <- paste0(name, ".rds")

  if (file.exists(path)) {
    readRDS(path)
  } else {
    code <- rlang::enquo(code)

    # Create a function so return() works
    fun <- rlang::new_function(list(), rlang::get_expr(code), rlang::get_env(code))
    result <- fun()

    saveRDS(result, path)
    result
  }
}
