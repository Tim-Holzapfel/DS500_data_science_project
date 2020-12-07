proxy_table_download <- function() {
  proxy_table <-
    xml2::read_html("https://free-proxy-list.net/anonymous-proxy.html") %>%
    rvest::html_node(css = "#proxylisttable") %>%
    rvest::html_table() %>%
    dplyr::rename_with(.fn = snakecase::to_snake_case) %>%
    dplyr::mutate(
      ip_port = paste0(ip_address, ":", port)
    )
  return(proxy_table)
}
