library(caret)

# Load data from pml-training.csv
colclasses <- c("numeric","character","numeric","numeric","character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","character")

pmlframe <- read.csv("pml-training.csv", header=TRUE, sep=",",
    colClasses=colclasses, na.strings = c("NA","#DIV/0!"))

# convert timestamp from string
pmlframe$cvtd_timestamp <- as.POSIXct(pmlframe$cvtd_timestamp, tz="", "%m/%d/%Y %H:%M")

# remove some columns, filter down to summary rows
pcakeep <- c("roll_belt","pitch_belt","yaw_belt","total_accel_belt",
    "kurtosis_roll_belt","kurtosis_picth_belt", "skewness_roll_belt","skewness_roll_belt.1", "max_roll_belt","max_picth_belt","max_yaw_belt", "min_roll_belt","min_pitch_belt","min_yaw_belt", "amplitude_roll_belt","amplitude_pitch_belt", "var_total_accel_belt","avg_roll_belt","stddev_roll_belt", "var_roll_belt","avg_pitch_belt","stddev_pitch_belt", "var_pitch_belt","avg_yaw_belt","stddev_yaw_belt", "var_yaw_belt","gyros_belt_x","gyros_belt_y", "gyros_belt_z","accel_belt_x","accel_belt_y", "accel_belt_z","magnet_belt_x","magnet_belt_y", "magnet_belt_z","roll_arm","pitch_arm", "yaw_arm","total_accel_arm","var_accel_arm", "avg_roll_arm","stddev_roll_arm","var_roll_arm", "avg_pitch_arm","stddev_pitch_arm","var_pitch_arm", "avg_yaw_arm","stddev_yaw_arm","var_yaw_arm", "gyros_arm_x","gyros_arm_y","gyros_arm_z", "accel_arm_x","accel_arm_y","accel_arm_z", "magnet_arm_x","magnet_arm_y","magnet_arm_z", "kurtosis_roll_arm","kurtosis_picth_arm","kurtosis_yaw_arm", "skewness_roll_arm","skewness_pitch_arm","skewness_yaw_arm", "max_roll_arm","max_picth_arm","max_yaw_arm", "min_roll_arm","min_pitch_arm","min_yaw_arm", "amplitude_roll_arm","amplitude_pitch_arm","amplitude_yaw_arm", "roll_dumbbell","pitch_dumbbell","yaw_dumbbell", "kurtosis_roll_dumbbell","kurtosis_picth_dumbbell", "skewness_roll_dumbbell","skewness_pitch_dumbbell", "max_roll_dumbbell","max_picth_dumbbell","max_yaw_dumbbell", "min_roll_dumbbell","min_pitch_dumbbell","min_yaw_dumbbell", "amplitude_roll_dumbbell","amplitude_pitch_dumbbell", "total_accel_dumbbell","var_accel_dumbbell","avg_roll_dumbbell", "stddev_roll_dumbbell","var_roll_dumbbell","avg_pitch_dumbbell", "stddev_pitch_dumbbell","var_pitch_dumbbell","avg_yaw_dumbbell", "stddev_yaw_dumbbell","var_yaw_dumbbell","gyros_dumbbell_x", "gyros_dumbbell_y","gyros_dumbbell_z","accel_dumbbell_x", "accel_dumbbell_y","accel_dumbbell_z","magnet_dumbbell_x", "magnet_dumbbell_y","magnet_dumbbell_z","roll_forearm", "pitch_forearm","yaw_forearm","kurtosis_roll_forearm", "kurtosis_picth_forearm","skewness_roll_forearm", "skewness_pitch_forearm","max_roll_forearm", "max_picth_forearm","max_yaw_forearm","min_roll_forearm", "min_pitch_forearm","min_yaw_forearm","amplitude_roll_forearm", "amplitude_pitch_forearm","total_accel_forearm", "var_accel_forearm","avg_roll_forearm","stddev_roll_forearm", "var_roll_forearm","avg_pitch_forearm","stddev_pitch_forearm", "var_pitch_forearm","avg_yaw_forearm","stddev_yaw_forearm", "var_yaw_forearm","gyros_forearm_x","gyros_forearm_y", "gyros_forearm_z","accel_forearm_x","accel_forearm_y", "accel_forearm_z","magnet_forearm_x","magnet_forearm_y", "magnet_forearm_z","classe")

# filter down to summary rows
pmlsummary <- pmlframe[pmlframe$new_window == "yes", ]
# filter down to summary columns
pmlsummary <- pmlsummary[, pcakeep]

# PCA analysis - skipped in presi
pcaframe<-pmlsummary

# set NAs to 0
pcaframe[is.na(pcaframe)] <- 0

#scale
preProc1 <- preProcess(pcaframe[-144], method = c("center", "scale"))
train1 <- predict(preProc1, pcaframe[-144])

# do PCA, summarize to see signicant cols, plot
pcadecomp <- prcomp(train1)

# we see magnet_belt_x, max_picth_dumbbell,  biggest contributors to PCA1
sort(pcadecomp$rotation[,1])
# accel_belt_z, roll_belt biggest contributors to PCA2
sort(pcadecomp$rotation[,2])

# plot PCA1 vs. PCA2, color/shape by classe
plotcolor <- data.frame(classe=pmlsummary$classe)
plotcolor$classe2 <- as.integer(0)
plotcolor[plotcolor$classe=="A", "classe2"] <- 1
plotcolor[plotcolor$classe=="B", "classe2"] <- 2
plotcolor[plotcolor$classe=="C", "classe2"] <- 3
plotcolor[plotcolor$classe=="D", "classe2"] <- 4
plotcolor[plotcolor$classe=="E", "classe2"] <- 5
plotshape <- data.frame(classe=pmlsummary$classe)
plotshape$classe2 <- as.integer(0)
plotshape[plotshape$classe=="A", "classe2"] <- 16
plotshape[plotshape$classe=="B", "classe2"] <- 17
plotshape[plotshape$classe=="C", "classe2"] <- 18
plotshape[plotshape$classe=="D", "classe2"] <- 19
plotshape[plotshape$classe=="E", "classe2"] <- 15

plot(pcadecomp$x[,1], pcadecomp$x[,2], col=plotcolor[, "classe2"], pch=plotshape[, "classe2"])

# a little bit of grouping but not so promising LOL

# predict with gbm (boost)
pmlsummary2<-pmlsummary
pmlsummary2$numclass=0
pmlsummary2[pmlsummary2$classe=="A", "numclass"] <- 1
pmlsummary2[pmlsummary2$classe=="B", "numclass"] <- 2
pmlsummary2[pmlsummary2$classe=="C", "numclass"] <- 3
pmlsummary2[pmlsummary2$classe=="D", "numclass"] <- 4
pmlsummary2[pmlsummary2$classe=="E", "numclass"] <- 5
pmlsummary2 <- pmlsummary2[-144]
pmlsummary2[is.na(pmlsummary2)] <- 0

modelFitGbm <- train(pmlsummary2$numclass ~ .,method="gbm",data=pmlsummary2)
predictGbm <- round(predict(modelFitGbm, pmlsummary2))
table(predictGbm, pmlsummary2$numclass)

#plot predict vs. actual
#jitter so dots don't all overlap, fill the box
predRightGbm <- predictGbm==pmlsummary2$numclass
qplot(predictGbm, numclass, data=pmlsummary2, col=predRightGbm, position="jitter")

#predcit with rf (random forest)
modelFitRF <- train(pmlsummary2$numclass ~ .,method="gbm",data=pmlsummary2)
predictRF <- round(predict(modelFitRF, pmlsummary2))
table(predictRF, pmlsummary2$numclass)

#plot predict vs. actual
#jitter so dots don't all overlap, fill the box
predRightRF <- predictRF==pmlsummary2$numclass
qplot(predictRF, numclass, data=pmlsummary2, col=predRightRF, position="jitter")

# RF has fewer incorrect - 31 v. 50 out of 406 rows 
# 31/406= 7.6%
# best guess is, in test will have 30-50% more errors 
# in any case under 15%, with luck under 10%

# run v. test data

testframe <- read.csv("pml-testing.csv", header=TRUE, sep=",",
    colClasses=colclasses, na.strings = c("NA","#DIV/0!"))

# convert timestamp from string
testframe$cvtd_timestamp <- as.POSIXct(testframe$cvtd_timestamp, tz="", "%m/%d/%Y %H:%M")

# and here it turns out I'm a bit screwed because test data doesn't have summary rows ... 
# this was a bit unexpected. test cases don't run on this mode. D'oh!

testsummary <- testframe

# make classe a numeric
testsummary$numclass=0
testsummary[testsummary$classe=="A", "numclass"] <- 1
testsummary[testsummary$classe=="B", "numclass"] <- 2
testsummary[testsummary$classe=="C", "numclass"] <- 3
testsummary[testsummary$classe=="D", "numclass"] <- 4
testsummary[testsummary$classe=="E", "numclass"] <- 5

predictRFtest <- round(predict(modelFitRF, testsummary))
table(predictRFtest, testsummary$numclass)
