setwd("~/coursera/course3/project/")

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              destfile = "rawdata.zip",
              method = "wget")
unzip("rawdata.zip")

## Reading test data from the "test" folder
subject_test     <- read.table("UCI HAR Dataset/test/subject_test.txt", sep="")
y_test           <- read.table("UCI HAR Dataset/test/y_test.txt", sep="")
x_test           <- read.table("UCI HAR Dataset/test/X_test.txt", sep="")
body_acc_x_test  <- read.table("UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt", sep="")
body_acc_y_test  <- read.table("UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt", sep="")
body_acc_z_test  <- read.table("UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt", sep="")
body_gyro_x_test <- read.table("UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt", sep="")
body_gyro_y_test <- read.table("UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt", sep="")
body_gyro_z_test <- read.table("UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt", sep="")
total_acc_x_test <- read.table("UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt", sep="")
total_acc_y_test <- read.table("UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt", sep="")
total_acc_z_test <- read.table("UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt", sep="")

## Reading train data from the "train" folder
subject_train     <- read.table("UCI HAR Dataset/train/subject_train.txt", sep="")
y_train           <- read.table("UCI HAR Dataset/train/y_train.txt", sep="")
x_train           <- read.table("UCI HAR Dataset/train/X_train.txt", sep="")
body_acc_x_train  <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt", sep="")
body_acc_y_train  <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt", sep="")
body_acc_z_train  <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt", sep="")
body_gyro_x_train <- read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt", sep="")
body_gyro_y_train <- read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt", sep="")
body_gyro_z_train <- read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt", sep="")
total_acc_x_train <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt", sep="")
total_acc_y_train <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt", sep="")
total_acc_z_train <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt", sep="")

#######################################################################################################
###### 1. Merging the training and the test sets to create one data set ###############################
#######################################################################################################

## Combining "test" data with all inertial signals
## Note: cbind is used here as all inertial signal data have the same number of rows as the x_test data
test_all  <- cbind(rep("test", times=nrow(x_test)),
                   subject_test,
                   y_test,
                   x_test,
                   body_acc_x_test, 
                   body_acc_y_test,
                   body_acc_z_test,
                   body_gyro_x_test,
                   body_gyro_y_test, 
                   body_gyro_z_test,
                   total_acc_x_test,
                   total_acc_y_test,
                   total_acc_z_test)

## Combining "train" data with all inertial signals
## Note: cbind is used here as all inertial signal data have the same number of rows as the x_train data
train_all <- cbind(rep("train", times=nrow(x_train)),
                   subject_train,
                   y_train,
                   x_train,
                   body_acc_x_train,
                   body_acc_y_train,
                   body_acc_z_train,
                   body_gyro_x_train,
                   body_gyro_y_train,
                   body_gyro_z_train,
                   total_acc_x_train,
                   total_acc_y_train,
                   total_acc_z_train)
## Temporarily aligning the names to make the rbind happen.
## The actual names will be updated shortly in section 4
names(test_all) <- names(train_all)
## Combining all test and train data into a single data set
## Note: rbind is used here as combined test data and combined train data have the same number of columns
grand_all <- rbind(test_all, train_all)

#######################################################################################################
###### 3. Generating descriptive activity names to name the activities in the data set ################
#######################################################################################################

## Reading initial feature names
features <- read.table("UCI HAR Dataset/features.txt", sep="")
feature_names <- as.vector(features[,2])

## Replacing "BodyBody" with "Body" which makes more sense
feature_names <- sub("BodyBody", "Body", feature_names,)

## Replacing single small letter 't' with "time: to make the variable name more readable
feature_names <- sub("tBody", "timeBody", feature_names)
feature_names <- sub("tGravity", "timeGravity", feature_names)

## Replacing single small letter 'f' with "freq" to make the variable name more readable
feature_names <- sub("fBody", "freqBody", feature_names)

## replacing "BodyGyro" with "AnglVel" as the latter is more descriptive
feature_names <- sub("BodyGyro", "AnglVel", feature_names)

## removing all "()" in feature names
feature_names <- gsub("\\(\\)", "", feature_names)

## finding the patterns of "17,24" or "X,Y"
## and replacing the comma in the patterns with "to"
i <- c(grep("[0-9]\\,[0-9]", feature_names), grep("[X|Y|Z]\\,[X|Y|Z]", feature_names))
feature_names[i] <- sub("\\,", "to", feature_names[i])

## finding the patterns of "X,1" and removing and comma in the patterns
j <- grep("[X|Y|Z]\\,[0-9]$", feature_names)
feature_names[j] <- sub("\\,", "", feature_names[j])

## for patterns like angle(X, Y), replace it with something like "angleBtwnXAndY"
k <- grep("angle", feature_names)
for (x in k) {
  ## finding location of the first opening bracket '('
  start_location <- regexpr("\\(", feature_names[x])  
  ## finding location of comma
  end_location <- regexpr("\\,", feature_names[x]) 
  ## finding the location of last closing bracket ')'
  last_location <- regexpr("\\)$", feature_names[x])
  ## transforming the name from angle(X, Y) to angleBetwnXAndY
  feature_names[x] <- paste0("angleBtwn",
                            substr(feature_names[x], start_location[1]+1, end_location[1]-1),
                            "And",
                            substr(feature_names[x], end_location[1]+1, last_location[1]-1))
}

## removing redundant ")"
feature_names <- gsub("\\)", "", feature_names)

## removing all dashes
feature_names <- gsub("\\-", "", feature_names)

## changing "bandsEnergy" to "EnergyBands" for better readability
feature_names <- gsub("bandsEnergy", "EnergyBands", feature_names)

## Generating descriptive names for inertial signals.
## There should be 128 sample data for each type of inertial signal
names_body_acc_x  <- paste0("bodyAccXMeasure", 1:128)
names_body_acc_y  <- paste0("bodyAccYMeasure", 1:128)
names_body_acc_z  <- paste0("bodyAccZMeasure", 1:128)
names_body_gyro_x <- paste0("anglVelXMeasure", 1:128)
names_body_gyro_y <- paste0("anglVelYMeasure", 1:128)
names_body_gyro_z <- paste0("anglVelZMeasure", 1:128)
names_total_acc_x  <- paste0("totalAccXMeasure", 1:128)
names_total_acc_y  <- paste0("totalAccYMeasure", 1:128)
names_total_acc_z  <- paste0("totalAccZMeasure", 1:128)

## Combining all names into a single vector
names_all <- c("dataTypes",
               "subjects",
               "activities",
               feature_names,
               names_body_acc_x, 
               names_body_acc_y,
               names_body_acc_z,
               names_body_gyro_x,
               names_body_gyro_y,
               names_body_gyro_z,
               names_total_acc_x,
               names_total_acc_y,
               names_total_acc_z)

## Saving all variable names in codebook.md for later use
write.table(names_all, file = "codebook.md", row.names = FALSE)

#######################################################################################################
###### 4. Labelling the data set with descriptive variable names ######################################
#######################################################################################################

## Applying new names to the unified data set
names(grand_all) <- names_all

#######################################################################################################
###### 2. Extracting only the measurements on the mean and standard deviation for each measurement. ###
#######################################################################################################

## Finding indices of variables having std/mean in their names
index <- c(grep("std", names(grand_all)), grep("mean", names(grand_all)))
## Subsetting data based on those indices
subdata <- grand_all[,index]

#######################################################################################################
###### 5. Creating a second, independent tidy data set with the average of each variable ##############
###### for each activity and each subject. ############################################################
#######################################################################################################

## Expected resultant tidy data:
## dataType subject activities  var1  var2  var3  ...
##  test      1       1           x1    y1    z1  ...
##  test      1       2           x2    y2    z2  ...
##  test      1       3           x3    y3    z3  ...
##  test      1       4           x4    y4    z4  ...
##  ...       ...     ...         ...   ...   ... ...
##  train     30      1           xn    yn    zn  ...
##  ...       ...     ...         ...   ...   ... ...

no_columns <- ncol(grand_all)                         ## Number of columns in the unified dataset
no_activities <- length(unique(grand_all$activities)) ## Number of different activities (equal to 6)
no_subjects <- length(unique(grand_all$subjects))     ## Number of subjects involved in experiment (equal to 30)

## Initializing an empty data frame that will be updated with tidy data
final_result <- data.frame()

## First level split of the unified dataset with regard to subjects
s <- split(grand_all, grand_all$subjects)

for(x in s) {
  data_type <- as.character(unique(x$dataType)) ## Getting name of the data type, either "test" or "train"
  subject <- unique(x$subjects)                 ## Getting the label of the subject, from 1 to 30
  
  ## Second level of split based on the 6 types of activies
  ## Calculate the mean based on the split data
  ## sapply should return us a matrix with x axis being the 6 different activities and y axis being all the variables
  temp_content <- sapply(split(x, x$activities), function(y) colMeans(y[, 4:no_columns]))
  
  ## Transposing the matrix so that the variables will show up in x axis, and turning the matrix into a data.frame
  temp_content <- as.data.frame(t(temp_content))
  
  ## Adding extra columns such as data type, subjects, activities so that the tidy data is more complete
  temp_content <- cbind(rep(data_type, no_activities), rep(subject, no_activities), 1:no_activities, temp_content)
  
  ##names(temp_content) <- names(grand_all)
  
  ## Using rbind to stitch up all tidy data from all 30 subjects
  final_result <- rbind(final_result, temp_content)
}

## Naming the variables in the tidy data in the same way as the unified dataset
names(final_result) <- names(grand_all)

## Finally writing the tidy dataset into a file called "tidydata.txt
write.table(final_result, file="tidydata.txt", row.names=FALSE)