library(camcorder)
library(tidyverse)
library(ggimage)
library(magick)
background <- image_read("black_switch.jpg")

library(showtext)
font_add_google("Gochi Hand", "gochi")
font_add_google("Roboto","Roboto", regular.wt=300)
font_add_google("Racing Sans One")
showtext_auto()

data <- data.frame(
  names = c("It Takes Two",
            "Kirby and the Forgotten Land",
            "Brothers",
            "The Great ACE Attorney",
            "Xenoblade",
            "Fire Emblem: Three Houses", 
            "Fire Emblem: Engage"
            ),
  images = c("it_takes_two.png", 
             "kirby.png",
             "brothers.png",
             "ace.png",
             "xenoblade.png",
             "three_houses.png",
             "engage.png"),
  hours = c(12, 10.5, 4.5, 50, 58, 49, 39)
) %>%
  arrange(desc(hours))
data$index <- seq(5, 2, -0.5)

y_axis <- data.frame(values = seq(-45, by = 30, length.out = 7),
                     labels = rep(NA, 7))

x_axis <- data.frame(index = 1:7,
                     labels = rep(NA,7))

gg_record(
  dir = file.path("switch", "recording"), 
  device = "png",
  width = 1066,     
  height = 692,   
  units = "px",  
  dpi = 300      
)

library(ggpubr)
ggplot() +
  background_image(background)

ggplot() +
  background_image(background) +
  geom_bar(data = data, aes(x = index, y = hours), stat = "identity",
           size = 0.2) 

ggplot() +
  background_image(background) +
  geom_bar(data = data, aes(x = index, y = hours), stat = "identity",
           size = 0.2) +
  geom_text(data = data, aes(x = index, y = hours, label = hours), vjust = 1, color = "white", size = 3) + 
  geom_text(data = y_axis, aes(x = 0, y = values, 
                               label = labels)) +
  geom_text(data = x_axis, aes(x = index, y = 0, 
                               label = labels)) 

ggplot() +
  background_image(background) +
  geom_bar(data = data, aes(x = index, y = hours), stat = "identity",
           size = 0.2) +
  geom_text(data = data, aes(x = index, y = hours, label = hours), vjust = 1, color = "white", size = 3) + 
  geom_text(data = y_axis, aes(x = 0, y = values, 
                               label = labels)) +
  geom_text(data = x_axis, aes(x = index, y = 0, 
                               label = labels)) + 
  geom_image(data = data, aes(x=index, y = hours+10, image = images), asp=1.6, size=0.05) 



ggplot() +
  background_image(background) +
  geom_bar(data = data, aes(x = index, y = hours), stat = "identity",
           size = 0.2) +
  geom_text(data = data, aes(x = index, y = hours, label = hours), vjust = 1, color = "white", size = 3) + 
  geom_text(data = y_axis, aes(x = 0, y = values, 
                               label = labels)) +
  geom_text(data = x_axis, aes(x = index, y = 0, 
                               label = labels)) + 
  geom_image(data = data, aes(x=index, y = hours+10, image = images), asp=1.6, size=0.05) + 
  annotate(geom = "text", x = 3.5, y = 140, label = "GAME TIME",
           hjust = "center", vjust = "top", family = "Racing Sans One", size = 15, color = "white") +
  annotate(geom = "text", x = 3.5, y = 125, label = "Hours spent playing video games together from 2021 to 2022",
           hjust = "center", vjust = "top", family = "Roboto", size = 5, color = "grey80")


ggplot() +
  background_image(background) +
  geom_bar(data = data, aes(x = index, y = hours), stat = "identity",
           size = 0.2) +
  geom_text(data = data, aes(x = index, y = hours, label = hours), vjust = 1, color = "white", size = 3) + 
  geom_text(data = y_axis, aes(x = 0, y = values, 
                               label = labels)) +
  geom_text(data = x_axis, aes(x = index, y = 0, 
                               label = labels)) + 
  geom_image(data = data, aes(x=index, y = hours+10, image = images), asp=1.6, size=0.05) + 
  annotate(geom = "text", x = 3.5, y = 140, label = "GAME TIME",
           hjust = "center", vjust = "top", family = "Racing Sans One", size = 15, color = "white") +
  annotate(geom = "text", x = 3.5, y = 125, label = "Hours spent playing video games together from 2021 to 2022",
           hjust = "center", vjust = "top", family = "Roboto", size = 5, color = "grey80") +
  annotate(geom = "text", x = 1, y = -25, label = "It Takes Two is our favorite co-op game",
           hjust = "center", vjust = "top", family = "gochi", size = 5, color = "grey80")

ggplot() +
  background_image(background) +
  geom_bar(data = data, aes(x = index, y = hours), stat = "identity",
           size = 0.2) +
  geom_text(data = data, aes(x = index, y = hours, label = hours), vjust = 1, color = "white", size = 3) + 
  geom_text(data = y_axis, aes(x = 0, y = values, 
                               label = labels)) +
  geom_text(data = x_axis, aes(x = index, y = 0, 
                               label = labels)) + 
  geom_image(data = data, aes(x=index, y = hours+10, image = images), asp=1.6, size=0.05) + 
  annotate(geom = "text", x = 3.5, y = 140, label = "GAME TIME",
           hjust = "center", vjust = "top", family = "Racing Sans One", size = 15, color = "white") +
  annotate(geom = "text", x = 3.5, y = 125, label = "Hours spent playing video games together from 2021 to 2022",
           hjust = "center", vjust = "top", family = "Roboto", size = 5, color = "grey80") +
  annotate(geom = "text", x = 1, y = -25, label = "It Takes Two is our favorite co-op game",
           hjust = "center", vjust = "top", family = "gochi", size = 5, color = "grey80") +
  annotate(geom = "curve", x = 2.2, y = -23, xend = 3, yend = 0, linewidth = 0.3, 
           curvature = 0.25, arrow = arrow(length = unit(1.25, "mm")), color = "white") +
  annotate(geom = "text", x = 3.5, y = -50, label = "Data source: play history on the Nintendo Switch | Visualization by Zhaowen Guo",
           hjust = "center", vjust = "top", family = "Roboto", size = 4, color = "grey80") +
  theme_void()

gg_playback(
  name = file.path("switch", "switch.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  image_resize = 800,
  frame_duration = .25
)