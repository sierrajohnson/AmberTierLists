
library(readxl)
library(tidyverse)


path <- "Grocery Tier List.xlsx"
data <- excel_sheets(path) %>%
  map_df(~read_excel(path, sheet = ., .name_repair = function(x) tolower(str_trim(x))) %>%
           mutate(Sheet = .x))  %>%
  mutate(Type = case_when(Sheet %in% c("Heaven", "Ice Cream") ~ "Ice Cream",
                          Sheet %in% c("Drinks", "Tea", "Kuluka", "Coffee beans") ~ "Beverages",
                          Sheet %in% c("Butter", "Cheese", "Yogurt") ~ "Other non-dairy",
                          Sheet %in% c("Dinner", "Meds") ~ "Misc",
                          TRUE ~ "Food"),
         Type = factor(Type, levels = c("Beverages", "Food", "Ice Cream", "Other non-dairy", "Misc"))) %>%
  mutate(rating = str_to_title(rating))


sheet_summ <- data %>%
  group_by(Sheet, Type) %>%
  summarize(Count = n())


rating_summ <- data %>%
  select(Sheet, Type, rating) %>%
  distinct() %>%
  drop_na(rating) %>%
  group_by(Sheet, Type) %>%
  mutate(ycoord = row_number())

ratings <- ggplot(rating_summ, aes(x = Sheet, y = ycoord, label = rating, color = Type)) +
  geom_text() +
  scale_color_viridis_d() +
  theme_bw() +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  guides(x = guide_axis(angle = 45, title = ""),
         y = guide_axis(title = ""))


tabcount <- ggplot(sheet_summ, aes(x = Sheet, y = Count, fill = Type)) +
  geom_col() +
  geom_text(aes(label = Count), vjust = -.1) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA)) +
  guides(x = guide_axis(angle = 45, title = ""))

ggsave("Tab Count.png", tabcount)
ggsave("Rating.png", ratings, width = 8, height = 4, units = c("in"))


