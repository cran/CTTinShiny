#' shinyCTT
#' 
#' Launches the Shiny interface for CTT
#'
#' Launches a Shiny interface that was developed in conjunction with the CTT package.
#' The interface was designed to be intuitive and guide users through analyses. Some
#' options are hidden until data are loaded. Also, some options (distractor analysis)
#' are hidden unless raw data AND a key file are loaded.
#'
#' @return No values are returned to R. After the Shiny interface launches
#' all values are returned within the Shiny interface itself.
#' 
#' @examples
#' \dontrun{
#' shinyCTT()
#' }
#' 
#' @export

shinyCTT <- function(){
  shinyApp(ui = uiCTT, server = serverCTT)
}  