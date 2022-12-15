# Scraping Lyrics from the Genius website ----

# Packages ----
library(geniusr)
library(dplyr)
library(tidytext)
library(rvest)
library(xml2)
library(rlang)

# Fix to genius::get_lyrics ----
## The original fix is from Malcolm Mashig; see post on geniusr's repo at
## https://github.com/ewenme/geniusr/issues/17#issuecomment-989302741

## Additional modifications made by Neil Hatfield (7/6/22):
### replace html_nodes (superseded) with html_elements
### replace "SongHeaderVariantdesktop__" with "SongHeaderdesktop__"
### replace "SongHeaderVariantdesktop__Artist" with SongHeaderdesktop__Artist"

get_lyrics <- function(session) {
  lyrics <-  session %>% html_elements(xpath = '//div[contains(@class, "Lyrics__Container")]')
  song <-  session %>% html_elements(xpath = '//span[contains(@class, "SongHeaderdesktop__")]') %>% html_text(trim = TRUE)
  artist <-  session %>% html_elements(xpath = '//a[contains(@class, "SongHeaderdesktop__Artist")]') %>% html_text(trim = TRUE)
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
  tibble(line = lyrics[!section_tags], section_name = section_name[!section_tags], 
         section_artist = section_artist[!section_tags], song_name = song, 
         artist_name = artist)
}
assignInNamespace("get_lyrics", get_lyrics, "geniusr")

# Scrape Genius Webpage with lyrics ----
## Based upon genius::get_song_lyrics

replacementCharacters <- c(
  "\\s*\\(Ft.[^\\)]+\\)" = "",
  "&" = "and",
  "\\$" = " ",
  "'" = "",
  # "\u00e9" = "e",
  # "\u00f6" = "o",
  "[[:punct:]]" = "",
  "[[:blank:]]+" = " ",
  " " = "-"
)

boastGetLyrics <- function(artistName, songTitle, newPage=FALSE) {
  artistName <- stringr::str_replace_all(string = artistName, replacementCharacters)
  songTitle <- stringr::str_replace_all(string = songTitle, replacementCharacters)
  path <- sprintf(
    fmt = ifelse(
      test = newPage,
      yes = "https://genius.com/%s-%s-lyrics?react=1",
      no = "https://genius.com/%s-%s-lyrics"
    ),
    artistName,
    songTitle
  )
  session <- xml2::read_html(path)
  return(get_lyrics(session))
  closeAllConnections()
}


