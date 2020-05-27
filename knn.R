Wbcd<-read.csv("C:\\Users\\Administrator\\Downloads\\wbcd_new.csv")
View(Wbcd)  

# Table of diagnosis
prop.table(table(Wbcd$diagnosis))*100

#recode diagnosis as a factor
Wbcd$diagnosis<-factor(Wbcd$diagnosis, levels = c("B","M"), labels = c("Benign", "Malignant"))
Wbcd$diagnosis

#Create training and test data
Wbcd_train<-Wbcd[1:469,]
Wbcd_test<-Wbcd[470:569,]
# Creating labels for train and test dataset
Wbcd_train_lables<-Wbcd[1:469,1]
Wbcd_test_lables<-Wbcd[470:569, 1]

#Loading the "class library"
install.packages("class")
library(class)
Wbcd_test_pred<-knn(train=Wbcd_train[,-1], test=Wbcd_test[,-1], cl=Wbcd_train_lables, k=25)

# Evaluating model performance
#Loading the "gmodel" library
install.packages("gmodels")
library(gmodels)

# Create the cross tabulation of predicted v/s actual
CrossTable(x= Wbcd_test_lables, y= Wbcd_test_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)



##improving model performance
# use the scale() function to z-score standardise a data frame
Wbcd_z<-as.data.frame(scale(Wbcd[-1]))

#Confirm that the transformation was applied correctly
summary(Wbcd_z$area_mean)


# Create training and test datasets
wbcd_train<-Wbcd_z[1:469,]
wbcd_test<-Wbcd_z[470:569,]

# re-classify test cases
wbcd_test_pred<-knn(train = wbcd_train[,-1], test=wbcd_test[,-1], cl=Wbcd_train_lables, k=21)

#create the cross tabulation of predicted v/s actual

CrossTable(x=Wbcd_test_lables, y=wbcd_test_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)


# try with several different  k values
wbcd_test_pred<-knn(train = wbcd_train[,-1], test=wbcd_test[,-1], cl=Wbcd_train_lables, k=1)
CrossTable(x=Wbcd_test_lables, y=wbcd_test_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)



wbcd_test_pred<-knn(train = wbcd_train[,-1], test=wbcd_test[,-1], cl=Wbcd_train_lables, k=11)
CrossTable(x=Wbcd_test_lables, y=wbcd_test_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)




wbcd_test_pred<-knn(train = wbcd_train[,-1], test=wbcd_test[,-1], cl=Wbcd_train_lables, k=15)
CrossTable(x=Wbcd_test_lables, y=wbcd_test_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

wbcd_test_pred<-knn(train = wbcd_train[,-1], test=wbcd_test[,-1], cl=Wbcd_train_lables, k=21)
CrossTable(x=Wbcd_test_lables, y=wbcd_test_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
