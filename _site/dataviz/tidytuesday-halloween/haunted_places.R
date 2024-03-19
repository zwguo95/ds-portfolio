library(ggbeeswarm)
library(ggimage)
library(tidytext)
library(showtext)
library(tidyverse)

# load spooky fonts
font_add_google("Creepster")  
font_add_google("Shadows Into Light")
showtext.auto()

# load raw data and images
haunted_places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-10/haunted_places.csv')

images <- data.frame(loc_type = c("school", "dwelling", "cemetery", "nature", "social"),
                     img = c("plot/book.png",
                             "plot/dwelling.png",
                             "plot/cemetery.png",
                             "plot/tree.png",
                             "plot/theater.png"))

# extract haunted places in washington
wa <- haunted_places %>%
  filter(state_abbrev == "WA") %>%
  select(-city_latitude, -city_longitude) %>%
  mutate(loc_type = case_when(
    grepl("School|Elementary|University|Lacrosse|steps|Sedrowooly", location) == T ~ "school",
    grepl("Hotel|Motel|Inn|House|Mansion|mansion|Castle|Studio|Institute|Ave.|Shepard Center|Row|Rivoli|homestead|Apartments|Home", location) == T ~ "dwelling",
    grepl("Graveyard|Cemetery|graveyard|Church|Campus", location) == T ~ "cemetery",
    grepl("Garden|Park|park|Greenlake|Fort|Tunnel|Dell|Lakewood|Boneylake|Wetlands|Island|Lake|Bridge|Creek", location) == T ~ "nature",
    grepl("Theater|theater|Theatre|Bar|Library|Museum|restaurant|Restaurant|Town|Club", location) == T ~ "social",
    T ~ NA
  ))

# sentiment analysis on fear
sentiments <- get_sentiments("nrc")
nrc_fear <- sentiments %>%
  filter(sentiment == "fear") %>%
  rename(description_unnested = word)

tidy_wa <- wa %>%
  mutate(id = row_number()) %>%
  select(id, description) %>%
  rowwise() %>%
  unnest_tokens(output = description_unnested, input = description, drop = F) %>%
  filter(!description_unnested %in% get_stopwords(source = "snowball")$word)

tidy_wa_all <- tidy_wa %>%
  group_by(id) %>%
  summarise(all_word = n())

tidy_wa_fear <- tidy_wa %>%
  group_by(id) %>%
  inner_join(nrc_fear) %>%
  count(description_unnested, sort = T) %>%
  summarise(score = sum(n)) %>%
  left_join(tidy_wa_all) %>%
  mutate(score_standard = round(scales::rescale(score/all_word), 2))

wa_scores <- wa %>%
  mutate(id = row_number()) %>%
  left_join(tidy_wa_fear) %>%
  left_join(images) %>%
  na.omit()

p1 <- ggplot(wa_scores) +
  geom_beeswarm(aes(x = score_standard, y = "y", color = loc_type), size = 5, method = "hex", cex = 5) +
  theme_gray() +
  theme(
    plot.background = element_rect(fill = "grey99", color = NA)
  )

p2 <- ggplot_build(p1)

p2_df <- data.frame(x = p2$data[[1]]$x, 
                    y = p2$data[[1]]$y,
                    r = 0.03,
                    img = wa_scores$img
) %>% 
  arrange(x) %>% 
  mutate(id = row_number())  

# create a beeswarm plot
ggplot(p2_df, aes(x, y, image = img)) + 
  geom_image(size = 0.035, asp=12/6) + 
  scale_x_continuous(limits = c(0, 1)) +
  annotate("text", x = seq(0, 0.9, 0.1), y = 0.55, label = seq(0.1, 1, 0.1), color = "#CED4E4", size = 10) +
  annotate("label", x = 0.78, y = 0.63, label = "Sentiment Scores of Fear -->", color = "black", size = 10,
           fill = "grey80" ,label.size = 0, label.padding = unit(0.5, "lines"), lineheight = 0.69)  +
  annotate(geom = "text", x = 0.8, y = 0.85, label = "Civil War Cemetery, Seattle",
           hjust = "center", vjust = "top", family = "Shadows Into Light", size = 12, color = "white") +
  annotate(geom = "curve", x = 0.8, y = 0.86, xend = 0.86, yend = 0.95, linewidth = 0.3, 
           curvature = -0.2, arrow = arrow(length = unit(1.25, "mm")), color = "white") +
  annotate(geom = "text", x = 0.68, y = 1.16, label = "DoubleTree Hotel, Spokane",
           hjust = "center", vjust = "top", family = "Shadows Into Light", size = 12, color = "white") +
  annotate(geom = "curve", x = 0.68, y = 1.12, xend = 0.74, yend = 1.05, linewidth = 0.3, 
           curvature = 0.2, arrow = arrow(length = unit(1.25, "mm")), color = "white") +
  theme_void() +
  labs(title = "SCARY ADVENTURES IN WASHINGTON") +
  theme(plot.margin = margin(l=10,r=10,t=10,b=10),
        plot.background = element_rect(fill = "#1a1a13", color = NA),
        plot.title = element_text(family = "Creepster", color = "white", size = 70, hjust = 0.5))

ggsave("spooky_wa.png", width = 8.5, height = 5)
