library(rvest)
library(RSelenium)
library(tidyverse)


# Start the RSelenium driver
rD <- rsDriver(browser = "chrome", port = 4544L, chromever = NULL)
remDr <- rD[["client"]]

# Navigate to the webpage
remDr$navigate("https://www.allsides.com/media-bias/ratings")

# Function to attempt clicking a "Load More" button
attemptLoadMore <- function() {
  tryCatch({
    button <- remDr$findElement(using = 'css selector', value = '.changeFilter.btn.btn-large.btn-success, .load-more-button-selector') # Combine selectors if possible
    button$clickElement()
    Sys.sleep(2) # Wait for content to load
    TRUE
  }, error = function(e) { FALSE })
}

# Initial click to load more content
attemptLoadMore()

# Scroll and attempt to load more until no new content loads
repeat {
  last_height <- remDr$executeScript("return document.body.scrollHeight;")
  remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(3)
  
  # Check for new scroll height and attempt to load more if scrolled to bottom
  new_height <- remDr$executeScript("return document.body.scrollHeight;")
  if (last_height == new_height && !attemptLoadMore()) {
    break
  }
}

# Get the page source and extract the information we need
page_source <- remDr$getPageSource()[[1]]
remDr$close()

news_source <- read_html(page_source) %>%
  html_elements(".view-content .views-field-title a") %>%
  html_text()

media_rating <- read_html(page_source) %>%
  html_elements(".views-field-field-bias-image a img") %>%
  html_attr("alt")

data <- cbind(news_source, media_rating) %>%
  as.data.frame() %>%
  mutate(media_rating = str_replace(media_rating, "AllSides Media Bias Rating:", ""),
         news_source = trimws(news_source, which = "left"))

write.csv(data, "media_bias.csv", row.names = F)
