pollutantmean <- function(directory, pollutant, id = 1:332){
  dataframe <- data.frame()
  path <- c(getwd(),"/",directory,"/")
  for (i in seq_along(id)){
     if (id[i] < 10) {
      filename <- paste(c("00",id[i],".csv"), collapse="")
    }
    else if (id[i] < 100) {
      filename <- paste(c("0",id[i],".csv"), collapse="")
    }
    else {
      filename <- paste(c(id[i],".csv"), collapse="")
    }
    nextfile <- paste(c(path,filename),collapse="")
    dataframe <- rbind(dataframe, read.csv(file = nextfile))

  }            
  round(mean(dataframe[[pollutant]], na.rm=TRUE), 3)
}