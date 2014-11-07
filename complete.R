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
    nafound <- FALSE
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
    #print (id[i])
    print (nobs)
   
  #dataframe <- rbind(dataframe, read.csv(file = nextfile))
  
  #names(dataframe) <- c("id","nobs") 
  #dataframe <- data.frame(id=id[3], nobs=id[9])
  #print(dataframe)
  #print(rbind(dataframe, c(1,2)))
  }            
  #round(mean(dataframe[[pollutant]], na.rm=TRUE), 3)
}

#print(paste(c("ncol=",k), collapse=""))