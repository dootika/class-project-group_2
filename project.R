library (tidyverse)
library (dplyr)
library (rvest)
# installed.packages("stringr")
library(stringr)

# Data Preparation and Cleaning
links <- NULL
for (i in seq(0,0,by=50)) #will change upper limit to 25350
{
  links <- c(links, paste0("https://myanimelist.net/topanime.php?limit=",i))
}
anime.names <- NULL
score.numbers <- NULL
anime.links <- NULL
for (i in 1:length(links))
{
  html <- read_html(links[i])
  anime.name <- html %>% html_elements(".fl-l.fs14.fw-b.anime_ranking_h3 a") %>% html_text()
  anime.link <- html %>% html_elements(".fl-l.fs14.fw-b.anime_ranking_h3 a") %>% html_attr("href")
  score.number <- html %>% html_elements(".js-top-ranking-score-col.di-ib.al span") %>% html_text()
  anime.names <- c(anime.names, anime.name)
  score.numbers <- c(score.numbers, as.numeric(score.number))
  anime.links <- c(anime.links, anime.link)
}
information_vec <- NULL
type_cleaned <- NULL
episode_cleaned <- NULL
status_cleaned<- NULL
aired_uncleaned<- NULL
premiered_cleaned <- NULL
broadcast_uncleaned<- NULL
producers_cleaned<- NULL
licensors_cleaned <- NULL
studio_cleaned <- NULL
source_cleaned <- NULL
genre_cleaned <- NULL
theme_cleaned <- NULL
demographic_uncleaned <- NULL
duration_uncleaned <- NULL
rating_uncleaned <- NULL
ranked_cleaned <- 1:50
popularity_cleaned <- NULL
members_cleaned <- NULL
favorites_cleaned <- NULL


##
#anime.links <- anime.links[1:10] ##delete this line later
##
for (i in 1:length(anime.links))
{
  html <- read_html(anime.links[i])
  print(i)
  information <- html %>% html_elements(".spaceit_pad") %>% html_text()
  information <- str_squish(information)
  len <- nchar(information)
  
  information_finder <- function (words)
  {
    if ( sum( grepl(words,information)==1 ) )
    return (information[grep(words,information)])
    else
      return (NA)
  }
  
  type_cleaned <- c(type_cleaned,substring(information_finder("Type: "),7))
  episode_cleaned <- c(episode_cleaned,as.numeric(substring(information_finder("Episodes: "),11)))
  status_cleaned <- c(status_cleaned, substring(information_finder("Status: "),9))
  aired_uncleaned <- c(aired_uncleaned,substring(information_finder("Aired: "),8)) # replace ? to ongoing
  premiered_cleaned <- c(premiered_cleaned,substring(information_finder("Premiered:"), 12))
  broadcast_uncleaned <- c(broadcast_uncleaned,substring(information_finder("Broadcast: "),12))
  producers_cleaned <- c(producers_cleaned, substring(information_finder("Producers:") ,12) )
  licensors_cleaned <- c(licensors_cleaned,substring(information_finder("Licensors:"),12))
  studio_cleaned <- c(studio_cleaned,substring(information_finder("Studios: "),10))
  source_cleaned <- c(source_cleaned,substring(information_finder("Source: "),9))
  genre_cleaned <- c(genre_cleaned,substring(information_finder("Genres:"),9))
  theme_cleaned <- c(theme_cleaned,substring(information_finder("Themes: "),9))
  demographic_uncleaned <- c(demographic_uncleaned,substring(information_finder("Demographic: "),14))
  duration_uncleaned <- c(duration_uncleaned,substring(information_finder("Duration: "),11))
  rating_uncleaned <- c(rating_uncleaned,substring(information_finder("Rating: "),9))
  popularity_cleaned <- c(popularity_cleaned,as.numeric(substring(information_finder("Popularity: "),14)))
  members_cleaned <- c(members_cleaned,as.numeric(gsub(",","", substring(information_finder("Members: "),10))))
  favorites_cleaned <- c(favorites_cleaned,as.numeric(gsub(",","", substring(information_finder("Favorites: "), 12)))) 
  
# rough
  information_vec <- c(information_vec, information)
  
}


# Creating Dataframe
anime_df <- data.frame("Ranked"=ranked_cleaned, "Anime Title"=anime.names,
                       "Score"=score.numbers, "Type"=type_cleaned, 
                       "Source"= source_cleaned,
                       "Episode Count"=episode_cleaned,
                       "Status"=status_cleaned, "Aired"=aired_uncleaned,
                       "Premiered"=premiered_cleaned, "Broadcast"=broadcast_uncleaned,
                       "Producers"=producers_cleaned, "Licensors"=licensors_cleaned,
                       "Studio"= studio_cleaned, "Genre"=genre_cleaned,
                       "Theme"=theme_cleaned, "Demographic"=demographic_uncleaned,
                       "Duration"=duration_uncleaned,"Rating"=rating_uncleaned,
                       "Popularity Ranking"=popularity_cleaned,
                       "Member Views"=members_cleaned, "Favorite marks"=favorites_cleaned,
                       "URLs"=anime.links)
 write.csv(anime_df,"Anime Dataframe.csv",row.names=FALSE)