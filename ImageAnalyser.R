# Loading the Libraries
library(imager)
library(magick)

# Defining the function
diff.col <- function(img)
{
  col.mat <- as.array(img[, , 1, ])
  dims <- dim(col.mat)
  # Calculates the amt of each color
  rtot <- 0
  gtot <- 0
  btot <- 0
  
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      # calculates the partial sum of all the colors
      rtot <- rtot + col.mat[i,j, 1] 
      gtot <- gtot + col.mat[i,j, 2] 
      btot <- btot + col.mat[i,j, 3] 
    }
  }
  # return the distance matrix of each pixel
  tot = sum(c(rtot, gtot, btot))
  return(c(rtot/tot, gtot/tot, btot/tot))
}

# Loading the data
dat <- read.csv("Data\ Scraping\ and\ Cleaning/Anime\ Dataframe.csv")

# Iterating through all the links
img.links <- dat$Image.URLs
dat$Percent.red <- 1:1e3
dat$Percent.blue <- 1:1e3
dat$Percent.green <- 1:1e3
for(i in 1:1e3){
  my.img <- image_read(img.links[i])
  my.img <- magick2cimg(my.img)
  cols <- diff.col(my.img)
  dat$Percent.red[i] <- cols[1]
  dat$Percent.blue[i] <- cols[2]
  dat$Percent.green[i] <- cols[3]
}

# Grey Scale Analysis 
dist.gray <- function(poster)
{
  col.mat <- as.array(poster[, , 1, ])
  dims <- dim(col.mat)
  # Calculate distance to given color
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      # distance from the col give by user
      dist[i,j] <- (norm(col.mat[i,j, ] - c(0,0,0), "2") > 0.5)
    }
  }
  # return the distance matrix of each pixel
  out.num <- sum(dist)/ (dims[1]*dims[2])
  return(out.num)
}
for(i in 1:1e3){
  my.img <- image_read(MainData$Image.URLs[i])
  my.img <- magick2cimg(my.img)
  white.percent <- dist.gray(my.img)
  MainData$Percent.white[i] <- white.percent
  MainData$Percent.black[i] <- 1-white.percent
}
write.csv(MainData,file="Finaldata.csv")
save(MainData, file = "Finaldata")