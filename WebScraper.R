# web scraping
library(rvest)
library(stringr)

# get home depot locations
chr.link = "https://www.homedepot.com/l/AL"

page <- read_html(chr.link)

html_element(page, "store-directory__store-link--a2mro") %>%
  html_table()


html_element(page,css=".store-directory")
div.store-directory__store--a2mro:nth-child(1) > ul:nth-child(2)



html_element(page,xpath="//*[contains(@class, 'store-directory')]") %>% html_text() %>% 
  str_match_all(paste0(", AL ","\\s*(.*?)\\s*","\\(")) %>% .[[1]] %>% .[,2] %>% str



