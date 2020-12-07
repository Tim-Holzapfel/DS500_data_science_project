make_firefox_proxy <- function(firefox_proxy) {
  library(RSelenium)

  firefox_address_port <- firefox_proxy

  make_profile <- function(firefox_address_port) {
    system("java_kill.bat")
    shell("java -jar selenium-server-standalone-4.0.0-alpha-2.jar", wait = F)

    host <- stringr::str_extract(firefox_address_port, "[^\\:]*")

    port <- stringr::str_extract(firefox_address_port, "(?<=\\:)\\d{4}") %>%
      as.integer()

    firefox_profile.me <- makeFirefoxProfile(
      list(
        # marionette = TRUE,
        # webdriver_accept_untrusted_certs = TRUE,
        # webdriver_assume_untrusted_issuer = TRUE,
        network.proxy.socks = host,
        network.proxy.socks_port = port,
        network.proxy.type = 1L
        # security.enterprise_roots.enabled = "false"
      )
    )

    remDr <- remoteDriver(
      remoteServerAddr = "localhost",
      port = 4444,
      browserName = "firefox",
      javascript = TRUE,
      nativeEvents = TRUE,
      extraCapabilities = firefox_profile.me
    )

    return(remDr)
  }

  environment(make_profile) <- rlang::global_env()

  remDr <- make_profile(firefox_address_port)

  return(remDr)
}
