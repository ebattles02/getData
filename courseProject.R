#Course Project for Coursera Getting and Cleaning Data Course
#working directory
dir <- 'D:/Documents/Coursera/Getting and Cleaning Data/CP'
setwd(dir)

#read activity labels and name columns
activityLabels <- read.table('activity_labels.txt')
names(activityLabels) <- c('code', 'activityName')

#read features and rename columns
features<- read.table('features.txt')
names(features) <- c("number", "feature")

#read in test data
test_y <- read.table('./test/y_test.txt')
test_x <- read.table('./test/X_test.txt')
test_subject <- read.table('./test/subject_test.txt')

#rename columns
names(test_x)<- features$feature
names(test_y) <- 'activity'
names(test_subject) <- 'subject'

#combine test tables
test <- cbind(test_subject, test_x, test_y)

#read in train data
train_y <- read.table('./train/y_train.txt')
train_x <- read.table('./train/X_train.txt')
train_subject <- read.table('./train/subject_train.txt')

#rename columns
names(train_x)<- features$feature
names(train_y) <- 'activity'
names(train_subject) <- 'subject'

#combine train tables
train <- cbind(train_subject, train_x, train_y)

#combine all test and train data
data <- rbind(test, train)

#extract column numbers for mean and std
c <- grep('mean|std', names(data))

#new data set with subject, mean, std, and activity
data2 <- data[,c(1,c,563)]

#merge activity code with activity name and remove activity code
tidyData <- merge(data2, activityLabels, by.x = 'activity', by.y = 'code')
tidyData$activity<-NULL

#create summary table of averages of each variable for each subject
subjects<- as.integer(names(table(tidyData$subject)))
subjects.df<- data.frame(subjects)
names(subjects.df) = 'subject.activity'
n <-ncol(tidyData) -1
for(i in 2:n){
    temp1 <- tapply(tidyData[,i], tidyData$subject, mean)
    temp2<-data.frame(as.integer(names(temp1)),temp1)
    temp.name <- names(tidyData)[i]
    names(temp2) <- c('subject.activity',temp.name)
    subjects.df <- merge(subjects.df,temp2)
}
subjects.df[,1]<-as.factor(subjects.df[,1]) #convert integers into factors

#create summary table of averages of each variable for each activity
activities <- names(table(tidyData$activityName))
activities.df<- data.frame(activities)
names(activities.df) = 'subject.activity'
for(i in 2:n){
    temp1 <- tapply(tidyData[,i], tidyData$activityName, mean)
    temp2<-data.frame((names(temp1)),temp1)
    temp.name <- names(tidyData)[i]
    names(temp2) <- c('subject.activity',temp.name)
    activities.df <- merge(activities.df,temp2)
}

output <-rbind(activities.df, subjects.df)
