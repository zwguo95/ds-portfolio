library(rvest)
library(tidyverse)

# Initialize variables
base_url <- "https://www.allsides.com/media-bias/ratings?page="
params <- "&field_featured_bias_rating_value=All&field_news_source_type_tid%5B0%5D=2&field_news_bias_nid_1%5B1%5D=1&field_news_bias_nid_1%5B2%5D=2&field_news_bias_nid_1%5B3%5D=3&title="
page_num <- 0
has_content <- TRUE

# Initialize placeholder for scraped data
news_sources <- list()
media_ratings <- list()

start = proc.time()
while(has_content){
  current_url <- paste0(base_url, page_num, params)
  
  # Load the webpage content
  page <- read_html(current_url)
  print(paste("Scraping:", current_url))
  
  # Extract names of media outlets
  news_source <- page %>%
    html_elements(".view-content .views-field-title a") %>%
    html_text()
  
  # Extract leanings of media outlets 
  media_rating <- page %>%
    html_elements(".views-field-field-bias-image a img") %>%
    html_attr("alt")
  
  # Check if the page has content
  if (length(news_source) == 0) {
    has_content <- FALSE
    print("No more content.")
  } else {
    news_sources[[page_num + 1]] <- news_source
    media_ratings[[page_num + 1]] <- media_rating
    
    print(paste("Page", page_num, "scraped successfully."))
    page_num <- page_num + 1
  }
}
proc.time() - start # 20s

data <- map2(news_sources, media_ratings, ~data.frame(news_source = .x, media_rating = .y)) %>%
  bind_rows() %>%
  mutate(media_rating = str_replace(media_rating, "AllSides Media Bias Rating:", ""))

write.csv(data, "media_bias.csv", row.names = F)
