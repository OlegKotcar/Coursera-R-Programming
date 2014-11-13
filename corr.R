corr <- function (directory, treshold=0){
  dataframe <- data.frame()
  completecase <- data.frame()
  cordata <- vector("numeric")
  corfile <- vector("numeric")
  id <- 1:332  # ----------------> файлы для чтения
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

    completecasecurrentfile <-data.frame() # Обнуляем данные текущего файла
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
        completecasecurrentfile <- rbind(completecasecurrentfile, filedata[j, ])
      }  
    }
    
    if (nobs > treshold){
      #print(nobs)
      cordata <- cor(completecasecurrentfile[, 3],completecasecurrentfile[, 2])
      #print(paste(c(filename," ",cordata), collapse=""))
      corfile  <- c(corfile, cordata)
      #print(corfile)      
    }    
  }
  if (length(corfile) < 1) corfile <- vector("numeric")
  return(corfile)
}
