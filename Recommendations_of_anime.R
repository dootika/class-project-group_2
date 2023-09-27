library(rvest)
library(tidyverse)
library(dplyr)

html = read_html("https://myanimelist.net/topanime.php")
name = html %>% html_elements(".fl-l.fs14.fw-b.anime_ranking_h3") %>% html_text()
link = html %>% html_elements(".hoverinfo_trigger") %>% html_attr("href")
recommended = numeric()
total.reviews = numeric()
for(i in 1:50)
{
  recommended[i] = as.numeric(read_html(link[i]) %>% html_elements(".recommended strong") %>% html_text())
  total.reviews[i] = as.numeric(read_html(link[i]) %>% html_elements(".right strong") %>% html_text())
  print(i)
}
write.csv(data.frame(name, recommended, total.reviews), "Recommendations_of_anime.csv")
