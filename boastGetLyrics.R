library(rlang)
library(dplyr)
library(rvest)
library(xml2)
library(stringr)
library(curl)

print("This is when the script gets loaded.")

# Test 1A ----
geturl <- function(url, handle) {
  curl::curl_fetch_memory(url = url, handle = handle)$content
}

boastGetLyrics2 <- function(songDB, artist, song) {
  print("getLyrics Running")
  url <- songDB %>%
    filter(Artist == artist, Song == song) %>%
    dplyr::select(url) %>%
    as.character()

  # Test 1B ----
  # h <- curl::new_handle()
  # curl::handle_setopt(h, ssl_verifypeer = 0)

  # mainPage <- read_html(url)
  # Test 1C ----
  # mainPage <- read_html(geturl(url, h))
  mainPage <- read_html(url)
  # rm(h)

  lyrics <- mainPage %>% html_elements(xpath = '//div[contains(@class, "Lyrics__Container")]')
  xml_find_all(lyrics, ".//br") %>% xml_add_sibling("p", "\n")
  xml_find_all(lyrics, ".//br") %>% xml_remove()
  lyrics <- html_text(lyrics, trim = TRUE)
  # Strip out the additional text that is getting captured ----
  firstPosition <- str_locate(string = lyrics[1], pattern = "\\[")
  str_sub(string = lyrics[1], start = 1, end = firstPosition[1, 1] - 1) <- ""

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


boastGetLyrics3 <- function(database, artist, song) {
  # Temporary function that bypasses scrapping

  mainPage <- xml_unserialize(
    connection = database[[paste(song, artist, sep = "--")]]$page,
    as_html =TRUE,
    options = c("NOWARNING")
  )
  lyrics <- mainPage %>% html_elements(xpath = '//div[contains(@class, "Lyrics__Container")]')
  xml_find_all(lyrics, ".//br") %>% xml_add_sibling("p", "\n")
  xml_find_all(lyrics, ".//br") %>% xml_remove()
  lyrics <- html_text(lyrics, trim = TRUE)
  # Strip out the additional text that is getting captured ----
  firstPosition <- str_locate(string = lyrics[1], pattern = "\\[")
  str_sub(string = lyrics[1], start = 1, end = firstPosition[1, 1] - 1) <- ""

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

# test <- boastGetLyrics3(songDatabase, artist = "Shawn Mendes", song = "Stitches")
# test0 <- boastGetLyrics2(songDB = songDB, artist = "Rihanna", song = "American Oxygen")


