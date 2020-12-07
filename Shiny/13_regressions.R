
clear()
library(lfe)
library(stargazer)
library("shiny")
library("shinyWidgets")

real_estates <-
  read_file("real_estate_offers") %>%
  dplyr::relocate(total_rent) %>%
  dplyr::mutate(
    log_total_rent = log(total_rent)
  )

exclude_vars <- c("total_rent", "city", "date", "user_id", "asset_id", "street",
                  "zip_code", "city_region", "bus", "latitude", "longitude",
                  "city_part", "house_number") %>%
  paste0("^", ., "$", collapse = "|") %>%
  paste0("(", ., ")")

choices_list <- names(real_estates) %>%
  stringr::str_remove_all(exclude_vars) %>%
  stringi::stri_remove_empty_na()

names(choices_list) <- snakecase::to_sentence_case(choices_list)


ui <-
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        multiInput(
          inputId = "dependent_vars",
          label = "Dependent Variables:",
          choices = choices_list,
          selected = "property_size",
          width = "400px",
          options = list(
            enable_search = FALSE,
            non_selected_header = "Dependent Variables:",
            selected_header = "Currently Selected:"
          )
        ),
        actionButton(
          inputId = "estimate_button",
          label = "Estimate Regression"
        )
      ),
      mainPanel(htmlOutput("html_regression"))
    )
  )

server <- function(input, output) {

  var_list <- reactiveValues()

  observeEvent(
    input$estimate_button, {
      var_list$dependent_var <- syms(c("log_total_rent", input$dependent_vars))
    }, ignoreInit = TRUE
  )

  formula_vars <- reactive({
    DF2formula(dplyr::select(real_estates, !!!var_list$dependent_var)) %>%
      Formula() %>% update(., ~ . | city)
  })

  observeEvent(
    input$estimate_button, {
      output$html_regression <- renderPrint({
        formula_obj <- formula_vars()
        felm_results <- felm(formula_obj, data = real_estates)
        stargazer::stargazer(felm_results, type = "html")
      })
    })

}
shinyApp(ui, server)










# ui <-
#   fluidPage(
#     sidebarLayout(
#       sidebarPanel(
#         selectInput(
#           inputId = "dependent_vars",
#           label = "Dependent Variables:",
#           choices = choices_list,
#           selected  = "property_size",
#           multiple = TRUE,
#           selectize = FALSE,
#           size = length(real_estates)
#         ),
#         actionButton(
#           inputId = "estimate_button",
#           label = "Estimate Regression"
#         )
#       ),
#       mainPanel(htmlOutput("html_regression"))
#     )
#   )
#
# server <- function(input, output) {
#
#   var_list <- reactiveValues()
#
#   observeEvent(
#     input$dependent_vars, {
#       var_list$dependent_var <- syms(c("total_rent", input$dependent_vars))
#     }, ignoreInit = TRUE
#   )
#
#   formula_vars <- reactive({
#     DF2formula(dplyr::select(real_estates, !!!var_list$dependent_var)) %>%
#       Formula() %>% update(., ~ . | city)
#   })
#
#   observeEvent(
#     input$estimate_button, {
#       output$html_regression <- renderPrint({
#         formula_obj <- formula_vars()
#         felm_results <- felm(formula_obj, data = real_estates)
#         stargazer::stargazer(felm_results, type = "html")
#       })
#     })
#
# }
# shinyApp(ui, server)
