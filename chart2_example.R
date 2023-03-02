# Load relevant libraries
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

# Setting things up:
# Load 5 Checkouts SPL dataset
five_checkouts_df <- read.csv("2013-2023-5-Checkouts-SPL.csv",                               stringsAsFactors = FALSE)

# Filter out materials that aren't book, ebook, or audiobook
five_checkouts_df <- five_checkouts_df %>%
  filter(MaterialType %in% c("AUDIOBOOK", "EBOOK", "BOOK"))

# Find harry potter books
hp_df <- five_checkouts_df %>%
  filter(str_detect(Title, "(?i)harry potter")) %>% 
  filter(str_detect(Creator, "(?i)rowling"))

# Add properly formatted date column
hp_df <- hp_df %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
hp_df$date <- as.Date(hp_df$date, format = "%Y-%m-%d")

# Pie chart values
audiobook_checkouts <- hp_df %>%
  select(MaterialType, Checkouts) %>%
  filter(MaterialType == "AUDIOBOOK") %>%
  summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  pull(Checkouts)
ebook_checkouts <- hp_df %>%
  select(MaterialType, Checkouts) %>%
  filter(MaterialType == "EBOOK") %>%
  summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  pull(Checkouts)
book_checkouts <- hp_df %>%
  select(MaterialType, Checkouts) %>%
  filter(MaterialType == "BOOK") %>%
  summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  pull(Checkouts)
all_checkouts <- hp_df %>%
  summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  pull(Checkouts)
audiobook_percent <- (audiobook_checkouts / all_checkouts) * 100
ebook_percent <- (ebook_checkouts / all_checkouts) * 100
book_percent <- (book_checkouts / all_checkouts) * 100
material_types_df <- data.frame(
  types = c("AUDIOBOOK", "EBOOK", "BOOK"),
  pie_chart_values = c(53.61835, 21.3229, 25.05875)
  )

# Graphing:
# Make pie chart of audiobook, ebook, and book
ggplot(material_types_df, aes(x = "",
                              y = pie_chart_values,
                              fill = types)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste0(pie_chart_values, "%")),
            position = position_stack(vjust=0.5)) +
  labs(title = "Percentages of Audiobook vs. Ebook vs. Book Checkouts
              from Jan. 2013 to Jan. 2023",
       fill = "Material Type")

