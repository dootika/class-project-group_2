library(tidyverse)
library(rvest)
library(dplyr)

# Reading the data from the website for top 50 anime series
html <- read_html("https://myanimelist.net/topanime.php")
Anime.Table <- html%>%html_table()
Anime.Table <- Anime.Table[[1]]
Anime.Table <- rename(Anime.Table, Rank = X1, Title = X2 , Score = X3)
Anime.Table <- Anime.Table[-1,-c(4,5)] #Eliminating the first row and a useless column
Anime.Table$Rank <- c(1:50)
Anime.Table$Score <- as.numeric(Anime.Table$Score)
# Meta - Data:
# Rank(int)[1:50] , 
# Title(char)[Name , TV(no of eps.) or Movie , airtime, voting members]
# Score(dbl) [Avg. Rating]

##########################

#Cleaning the Data
Anime.Table = Anime.Table%>%
  mutate(ShowTitle = Title, 
         Catagory = Title, 
         Airtime = Title,
         Members = Title, 
         )
for(i in 1:50){
  words = Anime.Table$Title[i]
  words = strsplit(words, split = "\n")
  words = words[[1]]
  anime.name = words[1]
  anime.catag = substring(words[2],9)
  anime.airtime = substring(words[3],9)
  anime.members = substring(words[4],9)
  
  Anime.Table$ShowTitle[i] = anime.name
  Anime.Table$Members[i] = anime.members 
  Anime.Table$Catagory[i] = anime.catag
  Anime.Table$Airtime[i] = anime.airtime
}
# Removing the title column
Anime.Table = Anime.Table[-2]
# Correcting the members column
Anime.Table$Members <- as.numeric(gsub("\\D", "", Anime.Table$Members))
# Getting the number of episodes
foo <- as.numeric(gsub("\\D", "", Anime.Table$Catagory))
Anime.Table <- mutate(Anime.Table, Eps = foo)
#Sorting the Catagory
for(i in 1:50){
  words = Anime.Table$Catagory[i]
  words = strsplit(words, split = " ")
  words = words[[1]]
  anime.catag = words[1]

  Anime.Table$Catagory[i] = anime.catag
}
##########################

#Adding the detailed description link for the Anime(s)
URL.data <- html %>% html_elements(".hoverinfo_trigger.fl-l")%>%html_attr("href")
Anime.Table <- mutate(Anime.Table, DetailURL = URL.data)


##########################

# Writing the data in a csv file
write_csv(Anime.Table,file = "AnimeData.csv")







