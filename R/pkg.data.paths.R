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
#' @param str.pkg.name optional if null return all file paths
#' @keywords points clean geocode
#' @export
#' @import stringr
#'     data.table
dt <- function(path.root=NULL, str.pkg.name = NULL){
  ls.pkg.data <- list()
  # Collect all dropbox pkg.data stored locally
  if (is.null(path.root)){
    path.root  <- '~/Dropbox/pkg.data'
    cat('dropbox path missing in pkg.data.paths::path.root, using', path.root, '\n ')
  }

  # Build initial base paths with all dropbox pkg.data files
  dt.full <- build.paths(path.root, str.pkg.name)

  # If importing specific package
  if (!is.null(str.pkg.name)){
    # Check and intialize 'clean' directories for new import
    dt.pkg <- build.pkg.paths(dt.full, str.pkg.name, path.root)
    return(dt.pkg)
    # Rebuild 
  } else {
    return(dt.full)
  }
}
#' @title build.paths
#'
#' @description Build dropbox mapping for all data in pkg.data directory
#' @param path.root root local dropbox directory
#' @param str.pkg.name package name of files
#' @keywords path dropbox check package
#' @export
#' @import stringr
#'     data.table
build.paths <- function(path.root, str.pkg.name){
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
  dt <- as.data.table(do.call(cbind, pkg.data))
}
#' @title build.pkg.paths
#'
#' @description Checks specific package paths for raw and clean directories and rebuilds dt
#' @param dt.full datatable offull root paths for all local dropbox data
#' @param str.pkg.name package name
#' @param path.root root dropbox path
#' @keywords path dropbox check package
#' @export
#' @import stringr
#'     data.table
build.pkg.paths <- function(dt.full, str.pkg.name, path.root){
  pkg.name <- NULL; file.name <- NULL; file.body <- NULL
  dt.pkg <- dt.full[str_detect(pkg.name, regex(paste0('(?i)', str.pkg.name), perl=TRUE)) &
                      !str_detect(file.name, 'Icon')]
  dir.pkg.root <- dt.pkg$pkg.root[1]
  dir.pkg.raw <- paste0(dir.pkg.root, '/raw')
  dir.pkg.clean <- paste0(dir.pkg.root, '/clean')
  
  # Check to see if base package data directory exists
  if (!dir.exists(dir.pkg.root)){
    dir.create(dir.pkg.root)
    l.msg$dir.pkg.root <- paste0('Creatied data directory for package in ', dir.pkg.root, '\n \n')
  }
  
  # Check for raw and clean data data
  l <- list()
  l.msg <- list()
  # Build raw clean and assigned locations
  l$dirs <- list.dirs(dir.pkg.root, recursive = FALSE, full.names = FALSE)
  l$raw <- sapply(dt.pkg$sys.path, function(x) str_detect(x, regex('(?i)\\/raw\\/', perl=TRUE)))
  l$clean <- sapply(dt.pkg$sys.path, function(x) str_detect(x, regex('(?i)\\/clean\\/', perl=TRUE)))
  l$unassigned.files <- dt.pkg[l$raw == FALSE & l$clean == FALSE & file.body == '']
  l$unassigned.dirs <- l$dirs[!str_detect(l$dirs, regex('(?i)(clean|raw)', perl=TRUE))]

  # Check for raw data directory
  if (length(which(l$raw)) == 0){
    dir.create(dir.pkg.raw, showWarnings = FALSE)
    l.msg$dir.pkg.raw <- paste0('Creatied raw directory for package in ', dir.pkg.raw)
  }
  
  # Check for clean data directory
  if (length(which(l$clean))== 0){
    dir.create(dir.pkg.clean, showWarnings = FALSE)
    l.msg$dir.pkg.clean <- paste0('Creatied clean directory for package in ', dir.pkg.clean)
  }
  
  # Move unassigned files to raw
  if (nrow(l$unassigned.files)!=0){
    from.locs <- l$unassigned.files$sys.path
    to.locs <- paste0(dir.pkg.raw, '/', l$unassigned.files$file.name)
    files.copied <- mapply(file.copy, from = from.locs, to = to.locs)
    files.removed <- lapply(from.locs, file.remove)
    l.msg$files.moved.raw <- mapply(function(x, y) paste0('Moved file ', x , ' to \n ', y), from.locs, to.locs, USE.NAMES = FALSE)
  }
  
  # Move unassigned directories to raw
  if (length(l$unassigned.dirs)!=0){
    from.dirs <- paste(dir.pkg.root, l$unassigned.dirs, sep='/')
    to.dir <- dir.pkg.raw
    dirs.copied <- lapply(from.dirs, function(x) file.copy(from = x, to = to.dir, recursive=TRUE))
    dirs.removed <- lapply(from.dirs, function(x) unlink(x, recursive=TRUE, force=TRUE))
    l.msg$dirs.moved.raw <- sapply(from.dirs, function(x) paste0('Moved dir recursively ', x , ' to \n ', to.dir), USE.NAMES = FALSE)
  }
 cat(unlist(l.msg,use.names = FALSE), labels=NULL, sep='\n \n')
 # Rebuild locations
 return(build.paths(path.root, str.pkg.name))
}
