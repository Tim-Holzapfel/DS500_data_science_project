generate_tor_driver <- function(headless = FALSE) {
  library(RSelenium)

  gc(full = TRUE)

  # Changing path so that RSelenium can find geckodriver and chromedriver.
  withr::local_path(new = list(exe_dir))

  # Temporarily changing the working directory to the folder containing the
  # executable files.
  withr::local_dir(new = exe_dir)

  # Forcefully stop any processes associated with the scraping.
  system("java_kill.bat")

  # Path to the tor executable
  tor_path <-
    list.files(
      path = file.path(normalizePath(Sys.getenv("USERPROFILE"), winslash = "/"), "Desktop"),
      recursive = TRUE,
      pattern = "\\.exe$",
      full.names = TRUE
    ) %>%
    stringr::str_subset("(?<=Tor).*firefox\\.exe$")


  shell.exec(tor_path)
  shell("java -jar selenium-server-standalone-4.0.0-alpha-2.jar", wait = F)

  generate_tor <- function(headless) {
    if (headless == FALSE) {
      args_moz <- list("")
    } else if (headless == TRUE) {
      args_moz <- list("--headless")
    }

    remDr <- remoteDriver(
      remoteServerAddr = "localhost",
      port = 4444L,
      browserName = "firefox",
      javascript = TRUE,
      nativeEvents = TRUE,
      extraCapabilities = list(
        "moz:firefoxOptions" = list(
          args = args_moz,
          prefs = list(
            "browser.download.folderList" = 2,
            "browser.download.manager.showWhenStarting" = FALSE,
            "browser.download.dir" = "C:/Users/Tim/Downloads",
            "browser.helperApps.neverAsk.saveToDisk" = "png",
            "network.proxy.socks" = "127.0.0.1",
            "network.proxy.socks_port" = 9150L,
            "network.proxy.type" = 1L,
            "network.proxy.socks_version" = 5L,
            "network.proxy.socks_remote_dns" = TRUE,
            "permissions.default.image" = 2L,
            "permissions.default.geo" = 2L,
            "services.sync.prefs.sync.permissions.default.image" = FALSE,
            "browser.display.show_image_placeholders" = FALSE,
            "editor.use_css" = FALSE,
            "dom.event.clipboardevents.enabled" = FALSE,
            "dom.events.asyncClipboard.dataTransfer" = TRUE
          )
        )
      )
    )

    return(remDr)
  }

  environment(generate_tor) <- rlang::global_env()

  remDr <- generate_tor(headless)
  remDr$open(silent = TRUE)
  return(remDr)
}
