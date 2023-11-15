# Reading the Data 
MainData <- read.csv("Finaldata.csv")

# Cleaning the Genre Column
for(i in 1:1e3){
  MainData$Genre[i] <- strsplit(MainData$Genre[i],',')[[1]][1]
}

write.csv(MainData,file="Finaldata.csv")