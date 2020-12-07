#   ____________________________________________________________________________
#   Entering city into search box                                           ####

search_box <- remDr$findElement("css", "input#autocompinp")
search_box$clickElement()
search_box$clearElement()

# remDr$mouseMoveToLocation(webElement = search_box)
# remDr$click()

city_split <- strsplit(cities$stadt[i], "")[[1]]

# In order to simulate realistic key strokes, the time it takes a person to find
# the key is not always the same, therefore the time between key strokes needs
# to vary. To simulate this, the waiting time between key strokes is simulated
# using the normal distribution.
wait_time <- runif(length(city_split), 0.3, 0.6)

k <- 1
for (city_i in city_split) {
  search_box$sendKeysToElement(list(city_i))
  Sys.sleep(wait_time[k])
  k <- k + 1
}
Sys.sleep(1)
#   ____________________________________________________________________________
#   Selecting city from drop down menu                                     ####

drop_down_elements <- remDr$findElements(using = "css", ".autocomplete-suggestion")

drop_down_elements_values <-
  sapply(drop_down_elements, function(x) {
    x$getElementText()
  }) %>% unlist()

drop_down_number <- which(drop_down_elements_values %in% city)

# In case the city in question cannot be found, write the name of the city to
# a missing_cities file and continue with the next city
if (sjmisc::is_empty(drop_down_number)) {
  write.table(
    city_norm,
    file = "R/missing_cities.txt",
    sep = ",",
    append = TRUE,
    quote = FALSE,
    col.names = FALSE,
    row.names = FALSE
  )
  next
}

drop_down_select <- drop_down_elements[[drop_down_number]]
Sys.sleep(1)
drop_down_select$clickElement()

