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
extract_table <- function(tableIndex, beatels_discography, keepCol1, keepCol2, keepCol3, keepCol4, removeRow1, removeRow2) {
  wiki_tabel <- beatels_discography %>%
    html_nodes("table") %>%
    #html_nodes("wikitable plainrowheaders")# %>%
    .[[tableIndex]] %>%
    html_table(fill=TRUE , header=FALSE)
  
  #column_names <- as.character(wiki_tabel[1,])
  b <- keepCol3 + keepCol4 + removeRow1 + removeRow2
  contentCol1 <- wiki_tabel[keepCol1] # Extrag coloana 1
  contentCol2 <- wiki_tabel[keepCol2] # Extrag coloana 2
  #contentCol3 <- wiki_tabel[keepCol3] # Extrag coloana 3
  #contentCol4 <- wiki_tabel[keepCol4] # Extrag coloana 4
  
  lastRowId <- length(wiki_tabel[[1]]) - 1
  startRowId <- 3
  
  contentCol1 <- contentCol1[[1]][startRowId:lastRowId]
  contentCol2 <- contentCol2[[1]][startRowId:lastRowId]
  #contentCol3 <- contentCol3[[1]][startRowId:lastRowId]
  #contentCol4 <- contentCol4[[1]][startRowId:lastRowId]
  
  #print(albumTitle)
#  column_to_eliminate = c()
  
#  for(i in seq(1, length(column_names), 1)) {  # merg pe fiecare coloana
#    #print(i)
#    #print(column_names[i])
#    if(column_names[i] == "Peak chart positions") { # scot coloana are numele "Peak chart positions" o elimin
#      column_to_eliminate <- c(column_to_eliminate, i) # salvez pozitiile in care se afla coloana  Peak chart positions
#    }
#  }
#  column_names <- column_names[-column_to_eliminate] # ELIMIN coloanele cu numele Peak chart positions
  
  dataframeToReturn <- data.frame("Album" = contentCol1, "Release" = contentCol2)

  return (dataframeToReturn)
}

combine_albums <- function(album1, album2) {
  # convert data from album1 in character
  album1$Album <- as.character(album1$Album)
  album1$Release <- as.character(album1$Release)
  
  # convert data from album2 in character
  album2$Album <- as.character(album2$Album)
  album2$Release <- as.character(album2$Release)
  
  # Extract Album from album1, album2
  album1_column1 <- c(album1$Album)
  album2_column1 <- c(album2$Album)
  
  # Extract DATE from Release from album1
  album1_column2 <- c(album1$Release)
  album1_column2 <- str_extract(album1_column2, '([0-9][0-9]? [A-Za-z]* [0-9]{4})|([0-9]{4})')
  
  # Extract DATE from Release from album2
  album2_column2 <- c(album2$Release)
  album2_column2 <- str_extract(album2_column2, '([0-9][0-9]? [A-Za-z]* [0-9]{4})|([0-9]{4})')
  
  # combine album1_column1 with album2_column1:
  new_album_column1 <- c(album1_column1, album2_column1)
  
  # combine album1_column1 with album2_column2
  new_album_column2 <- c(album1_column2, album2_column2)
  
  newAlbum <- data.frame("Album" = new_album_column1, "Release" = new_album_column2)
  
  return (newAlbum)
}

album_studio <- extract_table(tableIndex = 3, beatels_discography_raw, keepCol1=1, keepCol2=2, keepCol3=0, keepCol4=0, removeRow1=1, removeRow2=2) # Studio albums
album_live <- extract_table(tableIndex = 4, beatels_discography_raw, keepCol1=1, keepCol2=2, keepCol3=0, keepCol4=0, removeRow1=1, removeRow2=2) # Live albums
album_compilation <- extract_table(tableIndex = 5, beatels_discography_raw, keepCol1=1, keepCol2=2, keepCol3=0, keepCol4=0, removeRow1=1, removeRow2=2) # Compilation albums
album_mashup<- extract_table(tableIndex = 6, beatels_discography_raw, keepCol1=1, keepCol2=2, keepCol3=0, keepCol4=0, removeRow1=1, removeRow2=2) # MashUp albums

# combine album_studio with album_live
final_album <- combine_albums(album_studio, album_live)

# add album_compilation to final_album
final_album <- combine_albums(final_album, album_compilation)

# add album_mashup to final_album
final_album <- combine_albums(final_album, album_mashup)

final_album
