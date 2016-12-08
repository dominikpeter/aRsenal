
.onAttach <- function(...) {

  if (!interactive()) return()

  packageStartupMessage(paste0("rsenal is under *active* development. ",
                               "See https://github.com/dominikpeter/rsenal for changes"))

}
