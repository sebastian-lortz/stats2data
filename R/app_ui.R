#' The application User-Interface
#' tools::showNonASCIIfile("R/app_ui.R")
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # External resources
    golem_add_external_resources(),

    fluidPage(
      lang = "en",
      navbarPage(
        title = tags$img(
          src    = "www/logo.png",
          alt    = "stats2data",
          height = "30px",
          style  = "margin-top:-5px;"
        ),
        id = "main",
        windowTitle = "stats2data",

        # home
        tabPanel(
          title = "Home",
          fluidRow(
            column(width = 12,
                   h1("Welcome to the stats2data App"),
                   p(HTML(
                  "In this App and corresponding R package, we implemented a new method: Non-parametric Data Simulation from Summary Statistics. The primary scope of the method is to simulate complete datasets using only summary statistics, giving researchers a way to generate plausible raw datasets when original data are unavailable.
                  "
                      )),
                   h3("Modular Structure"),
                   p("The package is composed of three simulation modules, Descriptives, ANOVA and multiple linear regression (MLR). The modules are tailored to different data structures and statistical models, each following a similar high-level workflow."),
                    br(),
                   tags$a(
                     href   = "https://example.com/research_article.pdf",
                     target = "_blank",
                     class  = "btn btn-primary",
                     "Research Article"
                   )
            )
          )
        ),
        # workflow
        tabPanel(
          title = "High Level Workflow",
          fluidRow(
            column(width = 6,
                   h2("High Level Workflow"),
                   p("
                     The procedure (summarized in the Figure) begins with the candidate initialization, that is, the creation of an initial simulated dataset. The algorithm then iteratively refines the candidate by minimizing an objective function f that quantifies the discrepancy between the summary statistics of the candidate and the reported summary statistics, which we collect in a target vector. Because the targets are fixed, we treat f as a function of the candidate data alone. At each iteration, the following three steps are performed: \n\n
                     Candidate Modification: The data are modified through different types of moves within the search space. \n\n
                     Candidate Evaluation: Each candidate is evaluated by f. \n\n
                     Candidate Acceptance: The candidate is accepted based on a stochastic optimization criterion that favors reductions in f.
                     "),
                   h4("Convergence"),
                   p("The algorithm is deemed to have met the convergence criteria as soon as the best objective function value falls below the user-specified absolute threshold.
                   In addition, the modules use the threshold for targets that are enforced as constraints. If convergence is not reached within the iteration limit, the algorithm restarts from the best candidate found so far, with the adaptive optimization parameter reset to its starting value. When all allowed iterations and restarts have been exhausted, the optimization routine stops.")
             ),
            column(width = 6,
                   tags$img(
                     src   = "www/workflow.png",
                     alt   = "High Level Workflow",
                     class = "img-responsive center-block",
                     style = "border:1px solid #ddd; padding:4px; border-radius:4px; max-height:80vh;"
                   )
            )
          )
        ),
        # Dropdown
        navbarMenu(
          title = "Modules",
          tabPanel(title = "Descriptives", mod_optim_vec_ui("optim_vec")),
          tabPanel(title = "ANOVA", mod_optim_aov_ui("optim_aov")),
          tabPanel(title = "Linear Regression", mod_optim_mlr_ui("optim_mlr")),
        ),
         # about
        tabPanel("About",
                includeMarkdown(app_sys("app/www/about.md"))
        )
      )
    )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "stats2data"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
