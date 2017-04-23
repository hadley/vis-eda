map_progress <- function(.x, .f, ...) {
  names <- names(.x)
  name_width <- 30L

  format <- "[:bar] :percent ETA: :eta"
  if (!is.null(names)) {
    format <- paste0(format, " - :name")
  }

  p <- progress::progress_bar$new(
    total = length(.x),
    format = format
  )

  i <- 1
  .f <- purrr::as_mapper(.f, ...)
  pf <- function(...) {
    out <- .f(...)
    if (!is.null(names)) {
      p$tick(tokens = list(name = str_align(names[[i]], name_width)))
    } else {
      p$tick()
    }
    i <<- i + 1
    out
  }

  purrr::map(.x, pf, ...)

}

str_align <- function(x, n = 30) {
  if (nchar(x) == n) {
    x
  } else if (nchar(x) > n) {
    paste0(substr(x, 1, n - 3), "...")
  } else {
    paste0(x, paste0(rep(" ", n - nchar(x)), collapse = ""))
  }
}

# map_progress(1:20, ~ Sys.sleep(0.5))
# map_progress(setNames(1:26, letters), ~ Sys.sleep(0.5))

