dropdown_categories <- remDr$findElement("css", "#categories")
dropdown_categories$clickElement()

# dropdown_values <- remDr$findElement("css", ".selectpicker")
# dropdown_values_tags <- dropdown_values$selectTag()

dropdown_menu_open <- remDr$findElement("css", ".dropdown-menu.open")

# Select all dropdown elements but the one currently selected
dropdown_menu_links <- dropdown_menu_open$findChildElements("css", "a:not([class='selected'])")

dropdown_wohnung <- dropdown_menu_links[[2]]
dropdown_wohnung$clickElement()
