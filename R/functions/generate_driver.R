generate_driver <-
  function(
           browser = "chrome",
           headless = FALSE,
           use_proxy = FALSE,
           proxy_ip = NULL,
           proxy_port = NULL,
           proxy_type = "socks",
           socks_version = 5L) {
    library(RSelenium)

    # Changing path so that RSelenium can find geckodriver and chromedriver
    withr::local_path(new = list(project_root))

    make_profile <-
      function(browser, headless, use_proxy, proxy_ip, proxy_port, proxy_type, socks_version) {
        system("java_kill.bat")
        shell("java -jar selenium-server-standalone-4.0.0-alpha-2.jar", wait = F)

        if (browser == "chrome") {

          if (headless == TRUE) {

            eCaps <- list(chromeOptions = list(
              args = c("--headless", "--disable-gpu", "--window-size=1936,1056")
            ))

            remDr <- remoteDriver(
              remoteServerAddr = "localhost",
              port = 4444,
              browserName = "chrome",
              javascript = TRUE,
              nativeEvents = TRUE,
              extraCapabilities = eCaps
            )

          } else {

            remDr <- remoteDriver(
              remoteServerAddr = "localhost",
              port = 4444,
              browserName = "chrome",
              javascript = TRUE,
              nativeEvents = TRUE
            )

          }

        } else if (browser == "firefox") {
          firefox_profile <- makeFirefoxProfile(
            list(
              # Important function! Otherwise an error is returned if you want
              # to move to an element that is not visible.
              # accessibility.browsewithcaret = TRUE,
              # accessibility.accesskeycausesactivation = FALSE,
              # accessibility.mouse_focuses_formcontrol = TRUE,
              # accessibility.typeaheadfind = TRUE,
              marionette = TRUE
            )
          )

          if (use_proxy == FALSE) {
            remDr <- remoteDriver(
              remoteServerAddr = "localhost",
              port = 4444,
              browserName = "firefox",
              javascript = TRUE,
              nativeEvents = TRUE,
              extraCapabilities = firefox_profile
            )

            return(remDr)
          } else if (use_proxy == TRUE) {
            if (proxy_type == "socks") {
              firefox_profile <- makeFirefoxProfile(
                list(
                  # Important function! Otherwise an error is returned if you want
                  # to move to an element that is not visible.
                  accessibility.browsewithcaret = TRUE,
                  marionette = TRUE,
                  network.proxy.socks = proxy_ip,
                  network.proxy.socks_port = proxy_port,
                  network.proxy.type = 1L,
                  network.proxy.socks_remote_dns = TRUE,
                  network.proxy.socks_version = socks_version,
                  # Enable Https over DNS
                  network.trr.mode = 3L
                )
              )
            } else {
              firefox_profile <- makeFirefoxProfile(
                list(
                  # Important function! Otherwise an error is returned if you want
                  # to move to an element that is not visible.
                  accessibility.browsewithcaret = TRUE,
                  marionette = TRUE,
                  network.proxy.http = proxy_ip,
                  network.proxy.http_port = proxy_port,
                  network.proxy.type = 1L,
                  network.proxy.share_proxy_settings = TRUE,
                  # Enable Https over DNS
                  network.trr.mode = 3L,
                )
              )
            }
            remDr <- remoteDriver(
              remoteServerAddr = "localhost",
              port = 4444,
              browserName = "firefox",
              javascript = TRUE,
              nativeEvents = TRUE,
              extraCapabilities = firefox_profile
            )
            return(remDr)
          }
        }
      }

    environment(make_profile) <- rlang::global_env()
    withr::with_path(
      new = list(project_root),
      code = {
        remDr <- make_profile(browser, headless, use_proxy, proxy_ip, proxy_port, proxy_type, socks_version)
      },
      action = "prefix"
    )
    return(remDr)
  }
