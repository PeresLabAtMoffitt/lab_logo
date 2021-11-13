# Import library

library(tidyverse)
library(imager)
# library(ggpubr)
library(magick)
library(gganimate)

peres <- load.image("Peres.png")

plot(peres)

# Get coordinates
df <- as.data.frame(peres)

date1 <- "01-08-2018"
date2 <- "22-10-2021"
a <- seq(as.Date(date1, format = "%d-%m-%Y"), as.Date(date2, format = "%d-%m-%Y"),
       by = "month")

df_3 <- df %>% filter(cc == 1) %>% # select channel 1, 2 or 3
  filter(x >19, x < 375, y > 25, y < 125) %>%
  filter(value < 0.05) %>%  # filter threshold
  mutate(col = sample(c(1,2,3,4), size = nrow(.), replace = TRUE)) %>% 
  mutate(date = sample(a, size = nrow(.), replace = TRUE))

df_3 %>% 
  sample_frac(0.1) %>% 
  arrange(x) %>% 
  mutate(letter = case_when(
    x < 110 ~ 1,
    x >= 110 &
      x < 170 ~ 2,
    x >= 170 &
      x< 240 ~ 3,
    x >= 240 &
      x < 300 ~ 4,
    TRUE ~ 5
  )) %>% 
  group_by(letter) %>% 
  mutate(n = row_number()) %>%
  ggplot(aes(x=x, y=y, color = col, group = n
             )) +
  # geom_rect(aes(xmin=0, xmax= 1, ymin=0, ymax = 1))+
  # scale_color_gradient(low="navyblue", high="blue")+
  geom_point(size = 4, alpha = 0.7) +
  geom_line( alpha = 0.3)+
  labs(x= "Disparities", y= "Ovarian")+
  scale_color_viridis_b()+
  scale_y_reverse() + 
  theme_classic()+
  theme(legend.position = "none",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),
        # panel.background = element_rect(fill = 'black'),
        axis.text = element_blank(),
        axis.title.y = element_text(hjust = 1))


df_3 %>% 
  sample_frac(0.1) %>% 
  arrange(x) %>% 
  mutate(letter = case_when(
    x < 110 ~ 1,
    x >= 110 &
      x < 170 ~ 2,
    x >= 170 &
      x< 240 ~ 3,
    x >= 240 &
      x < 300 ~ 4,
    TRUE ~ 5
  )) %>% 
  group_by(letter) %>% 
  mutate(n = row_number()) %>%
  ggplot(aes(x=x, y=y, color = col, group = n
  )) +
  geom_point(size = 4, alpha = 0.7) +
  # geom_line(alpha = 0.3, aes(group= n))+
  scale_color_viridis_b()+
  scale_y_reverse() + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title.y = element_text(hjust = 1))+
  transition_states(x)+
  shadow_mark()

p <- df_3 %>% 
  sample_frac(0.1) %>% 
  arrange(x) %>% 
  mutate(letter = case_when(
    x < 110 ~ 1,
    x >= 110 &
      x < 170 ~ 2,
    x >= 170 &
      x< 240 ~ 3,
    x >= 240 &
      x < 300 ~ 4,
    TRUE ~ 5
  )) %>% 
  group_by(letter) %>% 
  mutate(n = row_number()) %>%
  ggplot(aes(x=x, y=y, color = col
  )) +
  geom_point(size = 4, alpha = 0.7, aes(group = seq_along(n))) +
  geom_line(alpha = 0.3, aes(group = n))+
  labs(x= "Disparities", y= "Ovarian")+
  transition_reveal(x)+
  scale_color_viridis_b()+
  scale_y_reverse() + 
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title.y = element_text(hjust = 1))

anim_save("peres.gif", p)
