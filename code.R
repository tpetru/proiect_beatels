#install.packages("htmltab")
#install.packages("rvest")
#install.packages("tidyverse")
#install.packages("xml2")
library(htmltab)
library(rvest)
library(tidyverse)
library(xml2)

rm(list=ls())


url_wikipedia = "https://en.wikipedia.org/"
url_discography = "https://en.wikipedia.org/wiki/The_Beatles_discography"
url_beatels = "https://en.wikipedia.org/wiki/The_Beatles"


# citesc site-urile
rh_raw <- read_html(url_beatels)
beatels_discography_raw <- xml2::read_html(url_discography)



# extragere de tabele
extract_table <- function(tb_index, beatels_discography) {
  album_list <- beatels_discography %>%
    html_nodes("table") %>%
    #html_nodes("wikitable plainrowheaders")# %>%
    .[[tb_index]] %>%
    html_table(fill=TRUE , header=FALSE)

  column_names <- as.character(album_list[1,])
  column_to_eliminate = c()
  
  for(i in seq(1, length(column_names), 1)) {  # merg pe fiecare coloana
    #print(i)
    #print(column_names[i])
    if(column_names[i] == "Peak chart positions") { # scot coloana are numele "Peak chart positions" o elimin
      column_to_eliminate <- c(column_to_eliminate, i) # salvez pozitiile in care se afla coloana  Peak chart positions
    }
  }
  
  column_names <- column_names[-column_to_eliminate] # ELIMIN coloanele cu numele Peak chart positions
  #print(column_names)
  return (album_list)
}

a <- extract_table(3, beatels_discography_raw) # Studio albums
