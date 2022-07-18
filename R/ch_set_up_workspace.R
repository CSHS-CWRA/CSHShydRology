#' @title Create directories to hold a project
#' 
#' @description This function creates sub-directories within the root directory 
#' for a project. It first checks whether the specified root directory exists;
#' if not, the root directory is created. 
#' 
#' The default names for the sub-directories are "data", "output", "code", 
#' "document", and "figures"; these can be over-ridden by specifying
#' alternatives via the `dir_names` argument.
#' 
#' If desired, the function will create a .Rproj file in the root directory.
#' To do so, the path to an existing .Rproj file must be specified via the
#' `rproj_source` argument.
#' 
#' @author R.D. (Dan) Moore <dan.moore@ubc.ca>
#' 
#' @param super_dir Name of directory within which to create the root directory
#' @param root_dir Name of the root directory for the project
#' @param dir_names  names of directories within the root directory; 
#'   default names are "data", "output", "code", "document", "figures"
#' @param make_project If TRUE, create a .Rproj file in the root directory
#' @param rproj_source Path to an existing .Rproj file to use for content
#' @return Does not return a value, simply creates the specified directories, and
#' (optionally) .Rproj file.
#' @export
#' @examples 
##' \donttest{
##' ch_set_up_workspace("~", "new_project")
##' }
#' 
ch_set_up_workspace <- function(super_dir, root_dir, make_project = FALSE,
                                dir_names = NULL, rproj_source = NULL) {
  if (!dir.exists(super_dir)) {
    stop("super_dir does not exist")
  }
  root_path <- file.path(super_dir, root_dir)
  if (!dir.exists(root_path)) {
    root_path <- file.path(super_dir, root_dir)
    dir.create(root_path)
  }
  if (is.null(dir_names)) {
    dir_names <- c("data", "output", "code", "document", "figures")
  }
  dir_paths <- file.path(root_path, dir_names)
  for (i in seq_along(dir_paths)) {
    if (!dir.exists(dir_paths[i])) dir.create(dir_paths[i])
  }
  if (make_project) {
    # check for existence of .Rproj file
    root_files <- list.files(root_dir)
    nc <- nchar(root_files)
    exts <- substr(root_files, nc - 4, nc)
    # if file does not exist, create one based on existing .Rproj file
    if (!("Rproj" %in% exts)) {
      if (is.null(rproj_source)) {
        warning("No path to .Rproj source file; .Rproj file not created")
      } else {
        proj_file <- file.path(root_path, paste0(root_dir, ".Rproj"))
        if (file.exists(rproj_source)) {
          rproj_text <- readLines(rproj_source)
          writeLines(rproj_text, con = proj_file)
          sprintf("A default .Rproj file has been created.")
        } else {
          warning(paste(rproj_source, "does not exist; .Rproj file not created"))
        }
      }
    }
  }
}