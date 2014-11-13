complete <- function(directory, id = 1:332){
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
    filedata <- read.csv(file = nextfile)
    # проходимся по всем строкам и столбцам
    nobs <- 0 
    nafound <- FALSE  #Если нашли NA вываливаемся из цикла
    for (j in 1:nrow(filedata)){
        for (k in 1:ncol(filedata)){
          if (is.na(filedata[j,k])){
            nafound=TRUE
            break
          }
          else{
            nafound=FALSE
            next                  
          }
        }
        if (!nafound) {
          nobs <- nobs+1
        }  
    }
    dataframecurrentfile <- data.frame(id = id[i], nobs = nobs)
    dataframe <- rbind(dataframe, dataframecurrentfile)
  }
  print (dataframe)
}