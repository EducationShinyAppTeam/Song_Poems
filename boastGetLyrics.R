library(rlang)
# library(geniusr)
library(dplyr)
# library(tidytext)
library(rvest)
library(xml2)


boastGetLyrics2 <- function(songDB, artist, song) {
  url <- songDB %>%
    filter(Artist == artist, Song == song) %>%
    select(url) %>%
    as.character()

  mainPage <- read_html(url)
  lyrics <- mainPage %>% html_elements(xpath = '//div[contains(@class, "Lyrics__Container")]')
  xml_find_all(lyrics, ".//br") %>% xml_add_sibling("p", "\n")
  xml_find_all(lyrics, ".//br") %>% xml_remove()
  lyrics <- html_text(lyrics, trim = TRUE)
  lyrics <- unlist(strsplit(lyrics, split = "\n"))
  lyrics <- grep(pattern = "[[:alnum:]]", lyrics, value = TRUE)
  if (is_empty(lyrics)) {
    return(tibble(line = NA, section_name = NA, section_artist = NA,
                  song_name = song, artist_name = artist))
  }
  section_tags <- nchar(gsub(pattern = "\\[.*\\]", "", lyrics)) == 0
  sections <- geniusr:::repeat_before(lyrics, section_tags)
  sections <- gsub("\\[|\\]", "", sections)
  sections <- strsplit(sections, split = ": ", fixed = TRUE)
  section_name <- sapply(sections, "[", 1)
  section_artist <- sapply(sections, "[", 2)
  section_artist[is.na(section_artist)] <- artist

  lyricsOut <- tibble(line = lyrics[!section_tags], section_name = section_name[!section_tags],
         section_artist = section_artist[!section_tags], song_name = song,
         artist_name = artist)
  return(lyricsOut)
}
