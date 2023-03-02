# Load libraries
library(dplyr)
library(ggplot2)
library(scales)
library(stringr) 

# Load 5 Checkouts SPL dataset
five_checkouts_df <- read.csv("2013-2023-5-Checkouts-SPL.csv",
                              stringsAsFactors = FALSE)

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

# Five data points for reference in summary below:
# 1. What year was a book (any MaterialType) checked out the most over the past
# ten years? How many checkouts occurred on that date? What kind of book was it?
date_max_checkouts <- hp_df %>%
  select(Checkouts, date) %>%
  filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>%
  pull(date)material_max_checkouts <- hp_df %>%
  select(Checkouts, MaterialType) %>%
  filter(Checkouts == max(Checkouts, na.rm = TRUE)) %>%
  pull(MaterialType)
max_checkouts <- max(hp_df$Checkouts, na.rm = TRUE)

# 2. How many Harry Potter audiobook, ebook, and regular book checkouts have
# there been over the past ten years?
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

# 3. What percentage of checkouts over the last ten years were audiobooks? What
# percentage were ebooks? What percentage were regular books?
all_checkouts <- hp_df %>%
  summarize(Checkouts = sum(Checkouts, na.rm = TRUE)) %>%
  pull(Checkouts)
audiobook_percent <- (audiobook_checkouts / all_checkouts) * 100
ebook_percent <- (ebook_checkouts / all_checkouts) * 100
book_percent <- (book_checkouts / all_checkouts) * 100

# 4. In what year did the lowest amount of ebook checkouts occur? (There were
# multiple years, filtered for the year with the most amount of least amount of
# checkouts)
year_min_ebook <- hp_df %>%
  filter(MaterialType == "EBOOK") %>%
  select(Checkouts, CheckoutYear) %>%
  filter(Checkouts == min(Checkouts, na.rm = TRUE)) %>%
  count(CheckoutYear)
year_min_ebook <- year_min_ebook %>%
  select(CheckoutYear, n) %>%
  filter(n == max(n, na.rm = TRUE)) %>%
  pull(CheckoutYear)

# 5. In what year did the lowest amount of book checkouts occur? (There were
# multiple years, filtered for the year with the most amount of least amount of
# checkouts)
year_min_book <- hp_df %>%
  filter(MaterialType == "BOOK") %>%
  select(Checkouts, CheckoutYear) %>%
  filter(Checkouts == min(Checkouts, na.rm = TRUE)) %>%
  count(CheckoutYear)
year_min_book <- year_min_book %>%
  select(CheckoutYear, n) %>%
  filter(n == max(n, na.rm = TRUE)) %>%
  pull(CheckoutYear)

