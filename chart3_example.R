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

# Just audiobook data
audiobook_data <- hp_df %>%
  select(MaterialType, Checkouts, CheckoutYear) %>%
  filter(MaterialType == "AUDIOBOOK")

# Graphing:
# Make line chart of audiobook trends
ggplot(data = audiobook_data) +
  geom_line(mapping = aes(x = CheckoutYear,
                          y = Checkouts,
                          color = MaterialType)) +
  geom_point(mapping = aes(x = CheckoutYear,
                           y = Checkouts,
                           color = MaterialType)) +
  labs(title = "Harry Potter Audiobook Checkouts from Jan. 2013 - Jan. 2023",
       x = "Year",
       y = "Number of Checkouts",
       color = "Material Type") +
  scale_x_continuous(breaks = seq(2013, 2023, 2))

