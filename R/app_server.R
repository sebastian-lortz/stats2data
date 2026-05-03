#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  observeEvent(input$show_workflow, {
    updateNavbarPage(session, "main", selected = "High Level Workflow")
  })

  observeEvent(input$go_to_tab, {
    updateNavbarPage(session, "main", selected = input$go_to_tab)
  })

  mod_optim_vec_server("optim_vec")
  mod_optim_lm_server("optim_lm")
  mod_optim_aov_server("optim_aov")

}
