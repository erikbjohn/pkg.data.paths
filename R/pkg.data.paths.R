#' \code{pkg.data.paths} package
#'
#' pkg.data.paths
#'
#' See the Vignette on 
#'
#' @docType package
#' @name pkg.data.paths
#' @importFrom data.table :=
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @title dt
#'
#' @description creates data table with path infor
#' @param path.root root path for dropbox package data
#' @keywords points clean geocode
#' @export
#' @import stringr
#'     data.table
dt <- function(path.root='~/Dropbox/pkg.data'){
  pkg.data <- lapply(c(TRUE,FALSE), function(x) list.files(path.root, full.names = x, recursive = TRUE))
  names(pkg.data) <- c('sys.path', 'pkg.path')
  pkg.data$pkg.path <- str_replace_all(pkg.data$pkg.path, '\\\r', '')
  pkg.data$pkg.name <- str_replace(pkg.data$pkg.path, regex('\\/.+', perl=TRUE), '')
  l.split <- str_split(pkg.data$pkg.path, '\\/')
  name.ind <- sapply(l.split,  length)
  pkg.data$file.name <- mapply(function(x, y) x[y], l.split, name.ind)
  pkg.data$file.body <- str_replace(pkg.data$pkg.path, regex(paste0('(',paste0(unique(pkg.data$pkg.name), collapse='|'),')\\/'), perl=TRUE), '')
  pkg.data$file.body <- mapply(function(x,y ) str_replace(x, y, ''), pkg.data$file.body, pkg.data$file.name)
  pkg.data$pkg.root <- paste0(path.root, '/',pkg.data$pkg.name)
  dt.pkg.data <- as.data.table(do.call(cbind, pkg.data))
  return(dt.pkg.data)
}