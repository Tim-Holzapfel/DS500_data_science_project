retrieve_proxies <-
  function(
    values_n = 0,
    priorities_order = "uptime"
    ) {

  gen_proxy_list <- function(values_n, priorities_order) {
    system("java_kill.bat")
    shell("java -jar selenium-server-standalone-4.0.0-alpha-2.jar", wait = F)

    library(RSelenium)
    library(rvest)
    library(xml2)

    # Changing path so that RSelenium can find geckodriver and chromedriver
    withr::local_path(new = list(project_root))

    eCaps <- list(chromeOptions = list(
      args = c("--headless", "--disable-gpu", "--window-size=1280,800")
    ))

    remDr <- remoteDriver(
      remoteServerAddr = "localhost",
      port = 4444,
      browserName = "chrome",
      javascript = TRUE,
      nativeEvents = TRUE,
      extraCapabilities = eCaps
    )

    proxy_list_url <- "https://spys.one/en/socks-proxy-list/"

    remDr$open(silent = TRUE)
    remDr$navigate(proxy_list_url)

    # Using the drop down menu select the option that shows 500 observations
    drop_down_value <- paste0("//*/option[@value = '", values_n, "']")
    option_show_all <- remDr$findElement(using = "xpath", drop_down_value)
    option_show_all$clickElement()

    proxy_table <-
      remDr$findElements("css", "td[colspan='10'] td[colspan='1']") %>%
      sapply(., function(x) {
        x$getElementText()
      }) %>%
      unlist() %>%
      matrix(., ncol = 9, byrow = T) %>%
      as.data.frame() %>%
      janitor::row_to_names(1) %>%
      dplyr::rename_with(.fn = snakecase::to_snake_case) %>%
      dplyr::mutate(
        ip_address = stringr::str_extract(proxy_address_port, "[^\\:]*"),
        port = stringr::str_extract(proxy_address_port, "(?<=\\:)\\d{4}") %>%
          as.integer(),
        latency = as.double(latency),
        uptime = stringr::str_extract(uptime, "\\d+(?=\\%)") %>% as.double()
      ) %>%
      dplyr::arrange(dplyr::desc(uptime))

    remDr$close()

    return(proxy_table)
  }

  environment(gen_proxy_list) <- rlang::global_env()

  proxy_results <-
    invisible({
      gen_proxy_list(values_n, priorities_order)
    })

  return(proxy_results)

  system("java_kill.bat")

}
