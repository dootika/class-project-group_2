library(imager)
library(dplyr)
library(magick)
load("../Finaldata.Rdata")
dat <- as_tibble(dat [c(1:33)])
dat1 <- dat %>% filter(Source == "Manga")
dat2 <- dat %>% filter(Source == "Original")
dat3 <- dat %>% filter(Source == "Light novel")


dat1<- dat1[1:10, ]
dat2<- dat2[1:10, ]
dat3<- dat3[1:10, ]
diff.col <- function(poster)
{
  col.mat <- poster
  dims <- dim(col.mat)
  # Calculate distance to given color
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      # distance from the col give by user
      dist[i,j] <- norm(col.mat[i,j, ] - col, "2")
    }
  }
  # return the distance matrix of each pixel
  return(dist)
}

which.color <- function(poster)
{
  dist.cols <- array(0, dim = c(dim(poster)[1], dim(poster)[2], 2) )
  colors.matrix <- matrix (c(0, 0, 0, 1, 1, 1), nrow = 2, ncol = 3)  
  dist.cols[ , , 1] <- diff.col(poster, col = colors.matrix[1, ])
  dist.cols[ , ,2] <- diff.col(poster, col = colors.matrix[2, ])
  dist.cols <- round(dist.cols, 2 )
  return(dist.cols)
}

for  (p in 1:1e3)
{
  my.img <- image_read(MainData$Image.URLs[p])
  my.img <- magick2cimg(my.img)
  poster1 <- as.array(my.img[ , ,1, ])
  distance1 <- which.color(poster1)
  print(distance1)
}

for  (p in 1:length(dat2$Image.URLs))
{
  poster2 <- load.image(dat2$Image.URLs[p])
  poster2 <- as.array(poster2[ , ,1, ])
  distance2 <- which.color(poster2)
  dark2 <- sum( apply(distance2, c(1,2), function (i) i<=0.2 & i>=0) )
  white2 <- sum( apply(distance2, c(1,2), function (i) i<=0.2 & i>=0) )
}

for  (p in 1:length(dat3$Image.URLs))
{
  poster3 <- load.image(dat3$Image.URLs[p])
  poster3 <- as.array(poster3[ , ,1, ])
  distance3 <- which.color(poster3)
  dark3 <- sum( apply(distance3, c(1,2), function (i) i<=0.2 & i>=0) )
  white3 <- sum( apply(distance3, c(1,2), function (i) i<=0.2 & i>=0) )
}
par(mfrow = c(1,2))
data1 <- data.frame(dark1,dark2,dark3)
names(data1) <- c("Manga", "Original", "Light Novel")
# plotting multiple bar plots
barplot(as.matrix(data1),
        main="Darkness of posters based on counts", 
        
        # setting y label only 
        # because x-label will be our
        # barplots name
        ylab="Count", 
        col= c("Blue","Yellow", "Green"),
        # to plot the bars vertically
        beside=TRUE, 
)
data2 <- data.frame(white1,white2,white3)
names(data2) <- c("Manga", "Original", "Light Novel")
# plotting multiple bar plots
barplot(as.matrix(data2),
        main="Whiteness of posters based on counts", 
        
        # setting y label only 
        # because x-label will be our
        # barplots name
        ylab="Count", 
        col= c("Blue","Yellow", "Green"),
        # to plot the bars vertically
        beside=TRUE, 
)
