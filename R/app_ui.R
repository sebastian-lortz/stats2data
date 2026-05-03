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

        # Home
        tabPanel(
          title = "Home",
          fluidRow(
            column(width = 12,
                   h1("Welcome to the stats2data App"),
                   p(HTML(
                  "We introduce the stats2data package: <strong>N</strong>onparametric <strong>D</strong>ata <strong>S</strong>imulation from <strong>S</strong>ummary <strong>S</strong>tatistics.
                  The primary scope of this algorithmic framework is to simulate complete datasets using only summary statistics, giving researchers a way to generate plausible sample data when original data are unavailable."
                      )),
                   h3("Modular Structure"),
                   p("The package is composed of two main simulation modules, ANOVA and multiple linear regression (LM), with a Descriptives sub-module. The modules are tailored to different data structures and statistical models, each following a similar high-level workflow. The ANOVA and LM modules produce datasets as a matrix, while the Descriptives sub-module generates a single variable. The modules can operate independently or sequentially, depending on the specific requirements of the optimization context."),
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
        # High Level Workflow
        tabPanel(
          title = "High Level Workflow",
          fluidRow(
            column(width = 6,
                   h2("High Level Workflow"),
                   p("The algorithmic framework simulates and adjusts data until its  summary statistics closely match the reported targets. The process (see Figure) begins with the candidate initialization, that is, the creation of an initial simulated dataset. The algorithm then iteratively refines the candidate by optimizing an objective function f that quantifies the discrepancy between the summary statistics of the candidate and the reported targets. At each iteration, the following two steps are performed. Candidate Modification: Modifications to the data are produced through different types of moves (i.e., adjustments of the data) within the search space. Candidate Evaluation: Each candidate is evaluated by f and accepted based on a stochastic optimization criterion, ensuring that modifications progressively reduce the  value of f."),
                   h4("Convergence"),
                   p("The algorithm is deemed to have met the convergence criteria as soon as the best objective function value f_best falls below the user‐specified thresh. If convergence is not reached within the iteration limit, the algorithm restarts from the best candidate found so far (with updated optimization settings). Only when all allowed iterations and restarts have been exhausted without achieving the thresh does the routine stop due to iteration limits rather than discrepancy criteria.")
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
        # Modules Dropdown
        navbarMenu(
          title = "Modules",
          tabPanel(title = "Descriptives", mod_optim_vec_ui("optim_vec")),
          tabPanel(title = "ANOVA", mod_optim_aov_ui("optim_aov")),
          tabPanel(title = "Linear Regression", mod_optim_lm_ui("optim_lm")),
        ),
        # About
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
