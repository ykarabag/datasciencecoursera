myFunction <- function(x) {
  
  y <- rnorm(100)
  mean(y)
}

second <- function(x){
  
  x+ rnorm(length(x))
}


y<-subset.matrix(x,x$Ozone>31 & x$Temp>90)

mean(y[,2])


good<-complete.cases(y[,1])
y[good,]
as.data.frame(y[good,2])

y[good,1]

max(y[good,1])



library(swirl)


complete <-function(directory,id=1:332){
        
        
        res<-data.frame("ID"=numeric(),"nobs"=numeric())
        mydata<-load_data(directory)
        
        idList<-as.character(c(id))
        mydata<-subset.data.frame(mydata,mydata$ID %in% idList & complete.cases(mydata[,]))
        
      
        z<-1
        for (i in unique(mydata[,4])){
                
                
                res[z,1]=i
                newData<-(subset.data.frame(mydata,mydata[,4]==as.character(i)))
                res[z,2]=length(newData[,1])
                
                
                z<-z+1
        
        }
        
        return (res)
        
    
}

corr<-function(directory,threshold = 0){
        
        mydata<-load_data(directory)
        res<-data.frame("Date"=character(),"sulfate"=numeric(),"nitrate"=numeric(),"ID"=numeric())
        res2<-c()
        mydata<-subset.data.frame(mydata,complete.cases(mydata[,]))
        
        
        z<-1
        for (i in unique(mydata[,4])){
                
                
             
                newData<-(subset.data.frame(mydata,mydata[,4]==as.character(i)))
                
                if (length(newData[,1])>=threshold){
                        
                        
                       
                        res2<-c(res2,cor(newData[,2],newData[,3]))  
                        
                }
                
            
             
                
        }
        
        return(res2)     
        
}




pollutantmean <- function(directory, pollutant, id=1:332){
  
mydata<-load_data(directory)
 
idList<-as.character(c(id))

mydata<-subset.data.frame(mydata,mydata$ID %in% idList & !is.na(mydata[,pollutant]))

mean<-mean(mydata[,pollutant])

}





load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

subset.data.frame(myData,myData$ID=='6')


