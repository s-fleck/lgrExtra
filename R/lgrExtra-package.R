#' @keywords internal
#' @import lgr
#' @importFrom stats setNames
#' @importFrom data.table data.table :=
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL



.onLoad <- function(...){
  assign(
    "lg",
    lgr::get_logger("lgrExtra"),
    envir = parent.env(environment())
  )
}



# for R CMD Check
r6_import_workaround <- function(){
  R6::is.R6(NULL)
  NULL
}
