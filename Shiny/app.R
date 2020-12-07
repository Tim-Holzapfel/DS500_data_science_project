
clear()
# output$text <-
#   renderPrint({
#     reactiveValuesToList(input)
#   })
#verbatimTextOutput(outputId = "text")


real_estate_objects <- readRDS(file.path(output_dir, "real_estate_objects.RDS"))
feature_data <- readRDS(file.path(output_dir, "feature_data_city.RDS"))
plz_data <- readRDS(file.path(output_dir, "zip_codes_data.RDS"))
icon_list <- readRDS(file.path(output_dir, "icon_list.RDS"))
var_group_table <- readRDS(file.path(output_dir, "var_group_table.RDS"))
germany_centroid <- readRDS(file.path(output_dir, "germany_centroid.RDS"))
bundesland_coords <- readRDS(file.path(output_dir, "bundesland_coords.RDS"))
bundesland_centroids <- readRDS(file.path(output_dir, "bundesland_centroids.RDS"))
stadt_coords <- readRDS(file.path(output_dir, "stadt_coords.RDS"))
stadt_centroids <-
  stadt_coords %>%
  dplyr::filter(name %in% big_cities) %>%
  create_centroids()
labs <- as.list(feature_data$address)

ui <-
  navbarPage(
    title = "Real Estate Objects",
    theme = shinytheme("flatly"),
    tabPanel(
      title = "test",
      fluidPage(
        useShinyjs(),
        tags$head(
          tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/latest.js?config=TeX-MML-AM_CHTML")
        ),
        fluidRow(
          column(
            width = 2,
            selectizeInput(
              inputId = "active_state",
              label = "Select Bundesland",
              choices = c("Choose one of" = "", bundesland_centroids$name)
            ),
            selectizeInput(
              inputId = "active_city",
              label = "Select City",
              choices = c("Choose one of" = "", stadt_centroids$name)
            ),
            sliderInput(
              inputId = "price_range",
              min = 0,
              max = max(real_estate_objects$total_rent),
              label = "Price Range",
              value = c(0, max(real_estate_objects$total_rent)),
              ticks = FALSE
            )
          ),
          column(
            width = 10,
            leafletOutput(outputId = "map", height = 800)
          )
        )
      )
    ),
    tabPanel("Data", DT::DTOutput("data"))
  )

server <- function(input, output, session) {

  #   ____________________________________________________________________________
  #   Base Map                                                                ####

  output$map <- renderLeaflet({
    leaflet(data = stadt_coords) %>%
      addTiles() %>%
      setView(
        lng = germany_centroid$X,
        lat = germany_centroid$Y,
        zoom = 6
      )
  })

  #   ____________________________________________________________________________
  #   Dynamic Icon creation                                                   ####

  # Bounding box of the currently visible area (rectangle).
  map_bbox <- reactive({
    bbox <- c(
      xmin = input$map_bounds$west,
      ymin = input$map_bounds$south,
      xmax = input$map_bounds$east,
      ymax = input$map_bounds$north
    )
    class(bbox) <- "bbox"
    return(bbox)
  })

  # Reactive object holding the name of the active city
  active_city_id <- reactiveValues()

  price_interval <- reactiveValues()
  observeEvent(input$price_range, {
    price_interval$min <- input$price_range[1]
    price_interval$max <- input$price_range[2]
  })

  # Observe the current bound box of the map and change the city_index if
  # necessary, otherwise do not.
  observeEvent(
    input$map_bounds,
    {
      bbox <- map_bbox()
      index_id <-
        suppressMessages({
          sf::st_crop(plz_data, bbox) %>%
            magrittr::extract2("id")
        })
      active_city_id$city_index <- index_id
    },
    ignoreInit = TRUE
  )

  # Reactive expression to change the visibility of the real estate markers.
  city_real_estates <- reactive({
    if (sjmisc::is_empty(input$price_range) | sjmisc::is_empty(active_city_id$city_index)) {
      return()
    }
    print(input$price_range)
    real_estate_objects %>%
      dplyr::filter(
        id %in% active_city_id$city_index,
        dplyr::between(total_rent, price_interval$min, price_interval$max)
      )
  })

  observeEvent(
    city_real_estates(),
    {
      if (input$map_zoom < 13) {
        leafletProxy("map") %>%
          clearMarkers()
      } else {
        leafletProxy("map", data = city_real_estates()) %>%
          clearMarkers() %>%
          addAwesomeMarkers()
      }
    },
    ignoreNULL = TRUE
  )

  city_features <- reactive({
    feature_data %>%
      dplyr::filter(id %in% active_city_id$city_index)
  })

  # Observer for the icon markers.
  observeEvent(
    active_city_id$city_index,
    {
      if (input$map_zoom < 16) {
        leafletProxy("map") %>%
          clearMarkers()
      } else {
        feature_data <- city_features()
        leafletProxy("map", data = feature_data) %>%
          clearMarkers() %>%
          addAwesomeMarkers(
            group = ~var_category,
            icon = ~ icon_list[var_group]
            # popup = lapply(labs, HTML),
            # label = lapply(labs, HTML),
            # options = markerOptions(riseOnHover = TRUE)
          ) %>%
          addLayersControl(
            overlayGroups = ~var_category,
            options = layersControlOptions(
              collapsed = FALSE
            )
          ) %>%
          hideGroup(group = ~ as.character(levels(var_category)))
      }
    },
    ignoreInit = TRUE
  )

  #   ____________________________________________________________________________
  #   Legend Data                                                             ####

  # Reactive value for the legend icons
  legend_data <- reactive({
    feature_data <- city_features()
    var_group_table %>%
      dplyr::filter(
        (var_category %in% input$map_groups) & (var_group %in% feature_data$var_group)
      ) %>%
      magrittr::extract2("legend_html_code") %>%
      paste0(., collapse = "<br>")
  })

  observeEvent(
    input$map_groups,
    {
      if (input$map_zoom < 16) {
        return()
      }
      html_legend <- legend_data()
      leafletProxy("map", data = html_legend) %>%
        removeControl(layerId = "legend") %>%
        addControl(html = html_legend, position = "bottomright", layerId = "legend")
    },
    ignoreInit = TRUE, ignoreNULL = FALSE
  )

  #   ____________________________________________________________________________
  #   Changing View                                                           ####

  bundesland_coords <- reactive({
    if (!sjmisc::is_empty(input$active_state)) {
      bundesland_centroids %>%
        dplyr::filter(name == input$active_state) %>%
        sf::st_coordinates() %>%
        as.data.frame() %>%
        dplyr::rename(lng = X, lat = Y)
    }
  })

  city_coords <- reactive({
    if (!sjmisc::is_empty(input$active_city)) {
      stadt_centroids %>%
        dplyr::filter(name == input$active_city) %>%
        sf::st_coordinates() %>%
        as.data.frame() %>%
        dplyr::rename(lng = X, lat = Y)
    }
  })

  # Fly to the center of the selected Bundesland.
  observeEvent(
    input$active_state,
    {
      view_coords_event <- bundesland_coords()

      duration <- 3

      leafletProxy("map") %>%
        flyTo(
          lng = view_coords_event$lng,
          lat = view_coords_event$lat,
          zoom = 10,
          options = list(
            duration = duration
          )
        )

      delay(duration * 1000, reset("active_state"))
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )

  # Fly to the center of the selected city.
  observeEvent(
    input$active_city,
    {
      view_coords_event <- city_coords()

      duration <- 3

      leafletProxy("map") %>%
        flyTo(
          lng = view_coords_event$lng,
          lat = view_coords_event$lat,
          zoom = 14,
          options = list(
            duration = duration
          )
        )

      delay(duration * 1000, reset("active_city"))
    },
    ignoreInit = TRUE,
    ignoreNULL = TRUE
  )
  ##  ............................................................................
  ##  Data Tabpanel                                                           ####


  output$data <- DT::renderDT({
    DT::datatable(
      real_estate_objects,
      extensions = c("FixedHeader", "KeyTable"),
      options = list(
        deferRender = TRUE,
        keys = TRUE,
        columnDefs = list(
          list(
            className = "dt-center",
            targets = "_all"
          )
        ),
        pageLength = 500,
        lengthMenu = c(5, 20, 50, 100, 200, 500, 1000),
        fixedHeader = TRUE,
        autoWidth = TRUE,
        language = list(search = "Filter:")
      )
    )
  })

  session$onSessionEnded(function() {
    stopApp()
  })
}
shinyApp(ui, server)
