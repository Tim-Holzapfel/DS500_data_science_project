library(shiny, quietly = TRUE)
library(leaflet, quietly = TRUE)
library(rlang, quietly = TRUE)
library(magrittr, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(ggthemes, quietly = TRUE)
library(scales, quietly = TRUE)
library(tikzDevice, quietly = TRUE)
library(sf, quietly = TRUE)
library(ggrepel, quietly = TRUE)
library(sp, quietly = TRUE)
library(Hmisc, quietly = TRUE)
library(ggnewscale, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(rgeos, quietly = TRUE)
library(rgdal, quietly = TRUE)
library(gdalUtilities, quietly = TRUE)
library(forcats, quietly = TRUE)
library(raster, quietly = TRUE)
library(furrr, quietly = TRUE)
library(purrr, quietly = TRUE)
library(future, quietly = TRUE)
library(xml2, quietly = TRUE)
library(rlang, quietly = TRUE)
library(enhancedView, quietly = TRUE)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
conflicted::conflict_prefer("View", "enhancedView", quiet = TRUE)
options(enhancedView.pageLength = 1000, enhancedView.standard_view = FALSE)

options(warn = -1)
options(scipen = 999)
options(max.print = 500)
conflicted::conflict_prefer("filter", "dplyr", quiet = TRUE)
conflicted::conflict_prefer("select", "dplyr", quiet = TRUE)
data.table::setDTthreads(6)

local(
  {
    root <- rprojroot::find_root(rprojroot::as.root_criterion(".Rprofile"))

    setwd(root)

    project_root <- root

    temp_dir <- tempdir()

    data_dir <- file.path(root, "data")

    scrape_dir <- file.path(data_dir, "wg_gesucht/page_sources_search")

    scrape_dir_single <- file.path(data_dir, "wg_gesucht/page_sources_real_estates")

    output_dir <- file.path(root, "output")

    figure_dir <- file.path(root, "figures")

    functions_dir <- file.path(root, "R/functions")

    exe_dir <- file.path(root, "executables")

    TA <- 4326

    `%notin%` <- Negate(`%in%`)

    `%>%` <- magrittr::`%>%`

    ggsave <- ggplot2::ggsave

    cairo_pdf <- grDevices::cairo_pdf

    federal_states <-
      c(
        "Baden-W\u00FCrttemberg", "Bayern", "Berlin", "Brandenburg", "Bremen",
        "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen",
        "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen",
        "Sachsen-Anhalt", "Schleswig-Holstein", "Th\u00FCringen"
      )

    big_cities <- c(
      "Berlin", "Bielefeld", "Bochum", "Bonn", "Bremen", "Dortmund",
      "Dresden", "D\u00FCsseldorf", "Duisburg", "Essen", "Frankfurt am Main",
      "Hamburg", "Hannover", "Karlsruhe", "K\u00F6ln", "Leipzig", "Mannheim",
      "M\u00FCnchen", "N\u00FCrnberg", "Stuttgart", "Wuppertal"
    )

    federal_states_mod <-
      federal_states %>%
      snakecase::to_snake_case(transliterations = "de-ASCII")

    federal_states_pattern <-
      federal_states_mod %>%
      stringr::str_replace("^sachsen$", "sachsen(?!_anhalt)") %>%
      paste0(collapse = "|") %>%
      paste0("(", ., ")") %>%
      stringr::regex(ignore_case = TRUE)

    cores <- parallelly::availableCores(which = "min")

    date_snakecase <-
      lubridate::today() %>%
      as.character() %>%
      snakecase::to_snake_case()

  },
  envir = rlang::base_env()
)

R.utils::sourceDirectory(
  path = "R/functions",
  envir = baseenv()
)


.Last <- function() {
  unlink(temp_dir, force = TRUE)
  system("java_kill.bat")
}
