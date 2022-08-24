#' Checks for WhiteboxTools executable
#'
#' @return If \code{WhiteboxTools} cannot be found, a message explaining what to do is displayed.
#' @export
#' @keywords internal
#' @author Kevin Shook
#' @importFrom whitebox check_whitebox_binary
#'
#' @examples 
#' # Only proceed if Whitebox executable is installed
#' library(whitebox)
#' if (check_whitebox_binary()){
#'   ch_wbt_check_whitebox()
#' } else {
#'   message("Example not run as Whitebox executable not found")
#' }
ch_wbt_check_whitebox <- function() {
  wb_found <- check_whitebox_binary(silent = TRUE)
  msg <- paste("The WhiteboxTools executable could not be found.\n", 
               "Make sure that you have run install_whitebox().\n", 
               "If you have already done this, try setting the path to the executable using wbt_init().", sep = "")
  if (!wb_found) {
    stop(msg)
  }
}