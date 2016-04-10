## Load necessary libraries
library(dplyr)
library(tidyr)

## prepare filepath constants
        pathtest <-"cleandata/dataset/test"
        pathtrain<-"cleandata/dataset/train"
        pathroot <-"cleandata/dataset"
        pathoutput <-"cleandata/output"
        

## load test data files, assign column name to subject

        subject_test<-read.table(paste(pathtest,"/subject_test.txt",sep=""))
        colnames(subject_test)[1]<-"Subject_ID"
        
        x_test<- read.table(paste(pathtest,"/X_test.txt",sep=""))
        y_test<- read.table(paste(pathtest,"/Y_test.txt",sep=""))

## load train data files, assign column name to subject

        subject_train<-read.table(paste(pathtrain,"/subject_train.txt",sep=""))
        colnames(subject_train)[1]<-"Subject_ID"

        x_train<- read.table(paste(pathtrain,"/X_train.txt",sep=""))
        y_train<- read.table(paste(pathtrain,"/Y_train.txt",sep=""))
        
        
## load features and activity labels
        
        feat  <- read.table(paste(pathroot,"/features.txt",sep=""))
        labels<- read.table(paste(pathroot,"/activity_labels.txt",sep=""))
        
## assign activity labels to train and test data 
        
        a_train <- data.frame(Activity_Name = labels$V2[y_train$V1])
        a_test <- data.frame(Activity_Name = labels$V2[y_test$V1])
        
## assign features to train and test data 
        
        colnames(x_train)<- feat$V2
        colnames(x_test) <- feat$V2

## consolidate train and test data
        
        c_train <- bind_cols(subject_train,a_train,x_train)
        c_test <- bind_cols(subject_test,a_test,x_test)

## merge consolidated test and train data 
        
        c_data <- rbind(c_train,c_test)
        
## collect key columns along with mean() and std() labels only
        
        final_cols<-c(1,2, grep("(mean|std)\\(\\)", colnames(c_data)))
        final_data<-c_data[final_cols]
        
## store final tidy data on disk
        
        write.table(final_data, file=paste(pathoutput,"/final_data.txt",sep=""), row.name=FALSE) 
        
## create another data set with average values. Create two new columns named variable & value
        
## Group final_data by Subject Id And Activity Name
        
        final_data_grouped <-group_by(final_data,Subject_ID,Activity_Name)
        
## Calculate average using summarise_each function 
        
        ave<-summarise_each(final_data_grouped,funs(mean))
        
## store calculated average data file on disk

        write.table(ave, file=paste(pathoutput,"/average.txt",sep=""), row.name=FALSE) 
        
        