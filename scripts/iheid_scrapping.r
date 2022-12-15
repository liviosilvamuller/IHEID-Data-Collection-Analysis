# Title: Scrapping tool for IHEID MA and PhD Theses data.
# Purpose: script to scrappe iheid's repository for theses.
# Authors: Livio Silva Muller
# Date: December 2022

setwd("~/Documents/GitHub/IHEIData/data")
links <- readRDS("~/Documents/GitHub/IHEIData/data/links.rds")

#packages
library(rvest)
library(dplyr)
library(purrr)
library(xml2)

#general url for phd theses repository
url_p <- "https://repository.graduateinstitute.ch/search?ln=en&fct__1=PhD+Theses&c=Academic+Departments&c=Academic+Reports&c=Book+Chapters&c=Books&c=Centres+and+Programmes&c=Dataset&c=Journal+Articles&c=Multimedia&c=Thematics&c=Working+Papers&rg=100&jrec=%d"

#creating vector with links to each individual PhD thesis
map_df(seq(0, 1400, by=100), function(i){
  
  page <- read_html(sprintf(url_p,i))
  
  data.frame(links=html_nodes(page, ".result-title a") %>% html_attr("href"),
             stringsAsFactors = FALSE)%>%
              mutate(links=paste0("https://repository.graduateinstitute.ch",links))
  
}) -> links

links_vector <- links$links

PhD_Theses <- data.frame()

for(i in 1:length(links_vector)) {
  message(str_c('scraping ', i, ' of ', length(links_vector) ))
  title <- links_vector[i] %>%
    read_html() %>%
    html_nodes("#body-wrapper > div.container.detailed-view-content > div > div.detailed-top-section.row > div > div.metadata-details.col-sm-10.col-xs-12 > div.full-record-title") %>%
    html_text()
  title <- title[1]
  author <- links_vector[i] %>%
    read_html() %>%
    html_nodes("#body-wrapper > div.container.detailed-view-content > div > div.detailed-top-section.row > div > div.metadata-details.col-sm-10.col-xs-12 > div:nth-child(2) > div > a") %>%
    html_text()
  author <- author[1]
  year <- links_vector[i] %>%
    read_html() %>%
    html_nodes(".icon-calendar-2+ .value") %>%
    html_text()
  year <- year[1]
  year2 <- links_vector[i] %>%
    read_html() %>%
    html_nodes(".metadata-row:nth-child(3) .col-md-10") %>%
    html_text()
  year2 <- year2[1]
  abstract <- links_vector[i] %>%
    read_html() %>%
    html_nodes("#abstract-collapse") %>%
    html_text()
  abstract <- abstract[1]
  program <- links_vector[i] %>%
    read_html() %>%
    html_nodes(".navtrail~ .navtrail+ .navtrail") %>%
    html_text()
  program <- program[1]
  PhD_Theses <- rbind(PhD_Theses, tibble(title = title,
                                         author = author,
                                         year = year,
                                         year2=year2,
                                         abstract=abstract,
                                         program=program))
  Sys.sleep(3)
}

saveRDS(PhD_Theses, file = "phd_theses.rds")

#Alternative----------------------------------------------------

# pages <- links_vector %>% map(read_html)
# PhD_Theses <- pages %>% map_df(~{
#   title <- html_text(html_nodes(., "#body-wrapper > div.container.detailed-view-content > div > div.detailed-top-section.row > div > div.metadata-details.col-sm-10.col-xs-12 > div.full-record-title"))
#   author <- html_text(html_nodes(., "#body-wrapper > div.container.detailed-view-content > div > div.detailed-top-section.row > div > div.metadata-details.col-sm-10.col-xs-12 > div:nth-child(2) > div > a"))
#   year <- html_text(html_nodes(., ".icon-calendar-2+ .value"))
#   abstract <- html_text(html_nodes(., "#abstract-collapse"))
#   program <- html_text(html_nodes(., ".navtrail~ .navtrail+ .navtrail"))
#   df <- tibble(title, author,year,abstract,program)
# })

