.onLoad <- function(libname, pkgname) {
  # Force NULL device for all non-interactive sessions and when env var is set.
  # Do NOT wait for user preference because rgl may already be loading.
  if (!interactive() || identical(Sys.getenv("RGL_USE_NULL"), "TRUE")) {
    options(rgl.useNULL = TRUE, rgl.printRglwidget = FALSE)
  }
}
