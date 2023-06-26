
run_analysis <- function() {
## Step 1: load the required special package: reshape2
##
     if (!("reshape2" %in% rownames(installed.packages())) ) {
        stop ("Please install required package: reshape2!\n")
     } 
     library(reshape2)

## Step 2: Merges the training and the test sets to create one data set (X_train.txt, X_test.txt; (data set), 
## y_train.txt, y_test.txt; (label-data set), and subject_train.txt, subject_test.txt (subject-data set))
##   
     cat("\n")
     cat("Step2: Merges the training and the test set to create on data set.\n")
     traindata <- read.table("./train/X_train.txt")
     testdata  <- read.table("./test/X_test.txt")
     joindata  <- rbind(traindata, testdata) 
     dim(traindata) ##for  (7352, 561)
     dim(testdata)  ##for  (2947, 561)
     dim(joindata)  ##for (10299, 561)

     trainlabel <- read.table("./train/y_train.txt")
     testlabel  <- read.table("./test/y_test.txt")
     joinlabel  <- rbind(trainlabel, testlabel)

## cross-check dimensions: (observations/rows, variables/columns)
     dim(trainlabel) ##(7352, 1)
     dim(testlabel)  ##(2947, 1)
     dim(joinlabel)  ##(10299, 1)

     trainsubject <- read.table("./train/subject_train.txt")
     testsubject  <- read.table("./test/subject_test.txt")
     joinsubject  <- rbind(trainsubject, testsubject)

     dim(trainsubject) ##  (7352, 1)
     dim(testsubject)  ##  (2947, 1)
     dim(joinsubject)  ## (10299, 1)

## Step 3: takes only the measurements on the mean and standard deviation for 
##measurement.
##
     cat("\n")
     cat("Step3: Extracts only the measurements on the mean and standard deviation for each measurement.\n")

     features <- read.table("features.txt") ##read the file
     dim(features) ## (561, 2)
     meanstdindex <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])

     length(meanstdindex) 

     joindatanew <- joindata[, meanstdindex] 

     dim(joindatanew)  

     colnames(joindatanew) <- features[meanstdindex, 2] 

     colnames(joindatanew) <- gsub("\\(|\\)", "", colnames(joindatanew)) 
     colnames(joindatanew) <- gsub("-", ".", colnames(joindatanew))
     colnames(joindatanew) <- tolower(colnames(joindatanew))

## Step 4: name the activities in the data set.

     cat("\n")
     cat("Step4: Uses descriptive activity names to name the activities in the data set.\n")

     activity <- read.table("activity_labels.txt")

     activity[, 2] <- tolower(gsub("_", "", activity[, 2]))

     activitylabel <- activity[joinlabel[, 1], 2]

     joinlabel[, 1] <- activitylabel 
    colnames(joinlabel) <- "activity"


## Step 5: labels the data set with descriptive activity names

     cat("\n")
     cat("Step5: Appropriately labels the data set with descriptiv activity names.\n")

## give a column name to the column of the joinsubject data frame 
     colnames(joinsubject) <- "subject"

##Combine three working dataframes (joinsubject, joinlabel and joindatanew) into one 
##  single data frame via command cbind

     cleandata <- cbind(joinsubject, joinlabel, joindatanew)

     dim(cleandata) 

     write.table(cleandata, "combinedcleandata.txt")

#########################################################################################
## Step 6: Creates a second, independent tidy data set with the average of each variable
##      for each activity and each subject. 
##
     cat("\n")
     cat("Step6: Creates a independent tidy data set with the average of each variable for each activity and each subject.\n")

## Reshape the data: generate skinny data via melt function
     meltdfrm <- melt(cleandata, id=c("activity", "subject"))
     tidydfrm <- dcast(meltdfrm, activity + subject ~ variable, mean)

## Create a file containing the tidy data set
     write.table(tidydfrm, "tidy_average_data.txt", row.names = F, col.names= T, sep = "\t")

     cat("")
     cat("DONE: a tidy data file has been created in the working directory!\n")
     cat("")
     workdone <- "TRUE"
} 


