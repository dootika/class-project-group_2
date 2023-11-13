library(imager)
dat <- dat [c(1:33)]
poster <- load.image(dat$Image.URLs[1])
poster <- as.array(poster[ , ,1, ])

diff.col <- function(poster, col)
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
      dist[i,j] <- norm(col.mat[i,j, ] - col, "2")
    }
  }
  # return the distance matrix of each pixel
  return(dist)
}

which.color <- function(poster)
{
  dist.cols <- array(0, dim = c(dim(poster)[1], dim(poster)[2], 2))
  colors.matrix <- matrix (c(0, 0, 0, 1, 1, 1), nrow = 2, ncol = 3)  
  for(k in 1:dim(poster)[1])
  {
    for (j in 1:dim(poster)[2])
{
print(j)  
      dist.cols[k,j,1] <- diff.col(poster, col = colors.matrix[1, ])
  dist.cols[k,j,2] <- diff.col(poster, col = colors.matrix[2, ])
    }
  }
return(dist.cols)
}

