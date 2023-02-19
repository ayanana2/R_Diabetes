
########################
#prepare before using R
########################

#install some packages
install.packages("dplyr")
install.packages("moments")
install.packages("MCMCpack")
install.packages("GGally")

#prepare library
library(plyr)
#Data wrangling 
library(dplyr)
#Visualisation
library(ggplot2)
#GGally of pairs = ggplt2 of pairs
library(GGally)

#confirm the current folder
getwd()
#move to the folder
setwd("C:/Users/Naito/Documents/MScDataSciense/2.Statistic and Machine Learning 1 DATA70121/★assinment_12Nov1700")


####################################
# 2. exploratory data analysis(EDA)
####################################

#read the CSV file
auto.data <- read.csv("PimaDiabetes.csv", header=TRUE, na.strings = c(""))
auto.data

#data of 1st column
auto.data[,1]
#data of 1st row
auto.data[1,]
summary( auto.data)

head(auto.data)
#dim(row, col) -> 750, 10
dim(auto.data)

#search missing values
is.na(auto.data)
auto.data[!complete.cases(auto.data)] 
#result' there is no missing values

#To replace 0 values, I create another variable
re0na <- auto.data
#Replace 0 to NA
#re0na$x1[my_df1$x1 == 0] <- NA 
re0na[, 2:6][re0na[, 2:6] == 0] <- NA
#confirm each variable
summary(auto.data)
summary(re0na)

#Replace NA to means in each column
#2nd column Glucose
re0na$Glucose[is.na(re0na$Glucose)] <- mean(re0na$Glucose, na.rm = TRUE)
re0na

#3rd Blood Pressure
re0na$BloodPressure[is.na(re0na$BloodPressure)] <- mean(re0na$BloodPressure, na.rm = TRUE)
re0na

#4th Skin Thickness
re0na$SkinThickness[is.na(re0na$SkinThickness)] <- mean(re0na$SkinThickness, na.rm = TRUE)
re0na

#5th Insulin
re0na$Insulin[is.na(re0na$Insulin)] <- mean(re0na$Insulin, na.rm = TRUE)
re0na

#6th BMI
re0na$BMI[is.na(re0na$BMI)] <- mean(re0na$BMI, na.rm = TRUE)
re0na

summary(re0na)

#Visualization
install.packages("VIM")
library(VIM)



vim_plot <- aggr(auto.data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(auto.data), 
                 cex.axis=0.5, gap=1.0, ylab=c("Histogram of missing data","Pattern"))

#pair plot
pairs(~Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin
      + BMI + DiabetesPedigree + Age + Outcome, data = re0na,
      main = "Scatterplot Matrices")

ggpairs(re0na)
summary(re0na)

##corelation
#without replace
cor(auto.data, auto.data$Outcome)
#[,1]
#Pregnancies      0.22923467
#Glucose          0.46030994
#BloodPressure    0.06086034
#SkinThickness    0.08220532
#Insulin          0.13092845
#BMI              0.28983170
#DiabetesPedigree 0.17068834
#Age              0.23289168
#Outcome          1.00000000


#after replace
cor(re0na, re0na$Outcome)
#[,1]
#Pregnancies      0.2292347
#Glucose          0.4869989
#BloodPressure    0.1624341
#SkinThickness    0.2165774
#Insulin          0.2109712
#BMI              0.3090573
#DiabetesPedigree 0.1706883
#Age              0.2328917
#Outcome          1.0000000

#(>0.2)


ggcorr(auto.data)
ggcorr(re0na)

#ggplot
attach(re0na)
boxplot(Pregnancies, BloodPressure, SkinThickness, Insulin, BMI, DiabetesPedigree, Age, Outcome)

#depict each column with outcome
plot(re0na$Pregnancies, re0na$Outcome, pch=16)
plot(re0na$Glucos, re0na$Outcome, pch=16)
plot(re0na$BloodPressure, re0na$Outcome, pch=16)
plot(re0na$SkinThickness, re0na$Outcome, pch=16)
plot(re0na$Insulin, re0na$Outcome, pch=16)
plot(re0na$BMI, re0na$Outcome, pch=16)
plot(re0na$DiabetesPedigree, re0na$Outcome, pch=16)
plot(re0na$Age, re0na$Outcome, pch=16)

#####Pregnancies
x1 <- re0na[[1]]
mean(x1) #summaryで確認済み
#Unbiased
var(x1)
#-->11.35747
#biased
library(moments)
moment(x1, order=2, central=TRUE)
#-->11.34233
#Skewness
ss = sqrt(moment(x1, order=2, central=TRUE))
moment(x1, order=3, central=TRUE)/(ss^3)
#-->0.9088167
#Kurtosis
(moment(x1, order=4, central=TRUE)/(ss^4))-3
#-->0.183347 

n <- length(x1)
n
#scatter plot
preg1 <- Pregnancies
ggplot(re0na, aes(x=Pregnancies,y=rep(1,n),stroke=0)) +
  geom_jitter(width=0.1,height=0.7) +
  ylim(0,2.0) + ylab("Random Jitter") + xlab("Pregnancies")
#histogram
ggplot(re0na, aes(x=Pregnancies)) +
  geom_histogram(bins=12)
#histogram with density
ggplot(re0na, aes(x=Pregnancies)) +
  geom_histogram(aes(y=..density..),colour="violetred4", fill="thistle2") +
  geom_density() + xlab("Pregnancies")
#boxplot
boxplot(preg1, main = "Pregnancies")$out


#####2 Glucose
x2 <- re0na[[2]]
mean(x2) #summaryで確認済み
#Unbiased
var(x2)
#biased
moment(x2, order=2, central=TRUE)
#Skewness
ss = sqrt(moment(x2, order=2, central=TRUE))
moment(x2, order=3, central=TRUE)/(ss^3)
#Kurtosis
(moment(x2, order=4, central=TRUE)/(ss^4))-3
n <- length(x2)
n
#scatter plot
preg2 <- re0na$Glucose
ggplot(re0na, aes(x=Glucose,y=rep(1,n),stroke=0)) +
  geom_jitter(width=0.1,height=0.7) +
  ylim(0,2.0) + ylab("Random Jitter") + xlab("Glucose")
#histogram
ggplot(re0na, aes(x=Glucose)) +
  geom_histogram(bins=12)
#histogram with density
ggplot(re0na, aes(x=Glucose)) +
  geom_histogram(aes(y=..density..),colour="violetred4", fill="thistle2") +
  geom_density() + xlab("Glucose")
#boxplot
boxplot(preg2, main = "Glucose")$out



#####3 BloodPressure
x3 <- re0na[[3]]
mean(x3) #summaryで確認済み
#Unbiased
var(x3)
#biased
moment(x3, order=2, central=TRUE)
#Skewness
ss = sqrt(moment(x3, order=2, central=TRUE))
moment(x3, order=3, central=TRUE)/(ss^3)
#Kurtosis
(moment(x3, order=4, central=TRUE)/(ss^4))-3
n <- length(x3)
n
#scatter plot
preg3 <- re0na$Glucose
ggplot(re0na, aes(x=BloodPressure,y=rep(1,n),stroke=0)) +
  geom_jitter(width=0.1,height=0.7) +
  ylim(0,2.0) + ylab("Random Jitter") + xlab("BloodPressure")
#histogram
ggplot(re0na, aes(x=BloodPressure)) +
  geom_histogram(bins=12)
#histogram with density
ggplot(re0na, aes(x=BloodPressure)) +
  geom_histogram(aes(y=..density..),colour="violetred4", fill="thistle2") +
  geom_density() + xlab("BloodPressure")
#boxplot
boxplot(preg3, main = "BloodPressure")$out


#####4 SkinThickness
x4 <- re0na[[4]]
mean(x4) #summaryで確認済み
#Unbiased
var(x4)
#biased
moment(x4, order=2, central=TRUE)
#Skewness
ss = sqrt(moment(x4, order=2, central=TRUE))
moment(x4, order=3, central=TRUE)/(ss^3)
#Kurtosis
(moment(x4, order=4, central=TRUE)/(ss^4))-3
n <- length(x4)
n
#scatter plot
preg4 <- re0na$SkinThickness
ggplot(re0na, aes(x=BloodPressure,y=rep(1,n),stroke=0)) +
  geom_jitter(width=0.1,height=0.7) +
  ylim(0,2.0) + ylab("Random Jitter") + xlab("SkinThickness")
#histogram
ggplot(re0na, aes(x=SkinThickness)) +
  geom_histogram(bins=30)
#histogram with density
ggplot(re0na, aes(x=SkinThickness)) +
  geom_histogram(aes(y=..density..),colour="violetred4", fill="thistle2") +
  geom_density() + xlab("SkinThickness")
#boxplot
boxplot(preg4, main = "SkinThickness")$out

#####5 Insulin
x5 <- re0na[[5]]
mean(x5) #summaryで確認済み
#Unbiased
var(x5)
#biased
moment(x5, order=2, central=TRUE)
#Skewness
ss = sqrt(moment(x5, order=2, central=TRUE))
moment(x5, order=3, central=TRUE)/(ss^3)
#Kurtosis
(moment(x5, order=4, central=TRUE)/(ss^4))-3
n <- length(x5)
n
#scatter plot
preg5 <- re0na$Insulin
ggplot(re0na, aes(x=Insulin,y=rep(1,n),stroke=0)) +
  geom_jitter(width=0.1,height=0.7) +
  ylim(0,2.0) + ylab("Random Jitter") + xlab("Insulin")
#histogram
ggplot(re0na, aes(x=Insulin)) +
  geom_histogram(bins=30)
#histogram with density
ggplot(re0na, aes(x=Insulin)) +
  geom_histogram(aes(y=..density..),colour="violetred4", fill="thistle2") +
  geom_density() + xlab("SkinThickness")
#boxplot
boxplot(preg5, main = "Insulin")$out

###After that, replace the columns and analyse the value of each column


####################################
# 3. Simple regression analysis
####################################

####################################
#Create the ThreeOrMoreKids
####################################
add.data <- mutate(re0na,ThreeOrMoreKids =
                     ifelse(
                       Pregnancies >=3,
                       1,
                       0
                     ))
add.data
summary(add.data)

re0na
head(add.data)
#dim(row, col) -> 750, 10
dim(add.data)

#check the outlier
is.na(add.data)
add.data[!complete.cases(add.data),] 


#####ThreeOrMoreKids
x10 <- add.data[[10]]
mean(x10) #summaryで確認済み
#Unbiased
var(x10)
#biased
moment(x10, order=2, central=TRUE)
#Skewness
ss = sqrt(moment(x10, order=2, central=TRUE))
moment(x10, order=3, central=TRUE)/(ss^3)
#Kurtosis
(moment(x10, order=4, central=TRUE)/(ss^4))-3
n <- length(x10)
n
#scatter plot
preg10 <- add.data$ThreeOrMoreKids
ggplot(add.data, aes(x=ThreeOrMoreKids,y=rep(1,n),stroke=0)) +
  geom_jitter(width=0.1,height=0.7) +
  ylim(0,2.0) + ylab("Random Jitter") + xlab("ThreeOrMoreKids")
#histogram
ggplot(add.data, aes(x=ThreeOrMoreKids)) +
  geom_histogram(bins=30)
#histogram with density
ggplot(add.data, aes(x=ThreeOrMoreKids)) +
  geom_histogram(aes(y=..density..),colour="violetred4", fill="thistle2") +
  geom_density() + xlab("ThreeOrMoreKids")
#boxplot
boxplot(preg4, main = "ThreeOrMoreKids")$out


######################################
#diabetes outcomes and ThreeOrMorekids
#apply simple logistic regression
######################################
model1 = glm(Outcome ~ ThreeOrMoreKids,data = add.data, family = binomial)

summary(model1)
exp(coef(model1))
exp(-1.1462)
exp(-0.2649)

#ThreeOrMoreKids = 0
probability0 = exp(-1.1462 + 0.8813*0)/(1 + exp(-1.1462 + 0.8813*0))
probability0
#ThreeOrMoreKids = 1
probability1 = exp(-1.1462 + 0.8813*1)/(1 + exp(-1.1462 + 0.8813*1))
probability1



###########
# 4.outcome, ThreeOrMorekids and others
#multiple logistic regression
###########
#Use the significance of coeicients and the AIC to guide in model selection and assessment.
#(hint: for comparison, use ‘AIC’ function in R.).


attach(add.data)

#Before multiple regression: note:execute 'attach(add.data)
cor(add.data, Outcome)


####1 more than 0.2 from correlation result
add.data
dat2 <- add.data[,c(1, 2, 4, 5, 6, 8)]
model2 <- glm(Outcome ~.,data = dat2, family = binomial)
summary(model2)

####2 choose the all various
dat3 <- add.data[,c(1, 2, 3, 4, 5, 6, 7, 8)]
model3 <- glm(Outcome ~.,data = dat3, family = binomial)
summary(model3)

####3 remove the lowest correlation various ***lowest
dat4 <- add.data[,c(1, 2, 4, 5, 6, 7, 8)]
model4 <- glm(Outcome ~.,data = dat4, family = binomial)
summary(model4)

####4 remove the second lowest correlation various 
dat5 <- add.data[,c(1, 2, 3, 4, 5, 6, 8)]
model5 <- glm(Outcome ~.,data = dat5, family = binomial)
summary(model5)

####5 remove the highest correlation various
dat6 <- add.data[,c(1, 3, 4, 5, 6, 7, 8)]
model6 <- glm(Outcome ~.,data = dat6, family = binomial)
summary(model6)

####6 choose from the highest to the third highest variouses
dat7 <- add.data[,c(3, 6, 8)]
model7 <- glm(Outcome ~.,data = dat7, family = binomial)
summary(model7)

####7 remove the 8 age 
dat8 <- add.data[,c(1, 2, 3, 4, 5, 6, 7)]
model8 <- glm(Outcome ~.,data = dat8, family = binomial)
summary(model8)

####8 remove the 8 age and 7 DiabetsPedigree
dat9 <- add.data[,c(1, 2, 3, 4, 5, 6)]
model9 <- glm(Outcome ~.,data = dat9, family = binomial)
summary(model9)

######compare the models
AIC(model2, model3, model4, model5, model6, model7, model8, model9)
#low score is a good model
#Then I chose model4

#AIC(model2, model3, model4, model5, model6, model7, model8, model9)
#df      AIC
#model2  7 721.3241
#model3  9 716.6202
#model4  8 715.4831
#model5  8 722.1954
#model6  8 828.0721
#model7  4 859.5257
#model8  8 715.7630
#model9  7 721.6478


###Q5  predict with 'ToPredict.csv' - not replacing 0 with mean
pre1 = read.csv("ToPredict.csv", header=TRUE)
pre1

summary(add.data)
summary( pre1 )

#predictData
predictData4 <- predict(model4, pre1)
predictData4
#-->
#         1           2           3           4           5 
#0.03455116 -0.80956796 -2.22477533  1.28273200  1.05752702 

odds1 <- exp (0.03455116)
odds1
probability1 = odds1/(1+odds1)
probability1
#--> 0.5086369
#-->50.9%

odds2 <- exp (-0.80956796)
odds2
probability2 = odds2/(1+odds2)
probability2
#-->0.3079826
#-->30.8%

odds3 <- exp (-2.22477533)
odds3
probability3 = odds3/(1+odds3)
probability3
#-->0.09754761
#-->9.8%

odds4 <- exp (1.28273200)
odds4
probability4 = odds4/(1+odds4)
probability4
#-->0.7829145
#-->78.3%

odds5 <- exp (1.05752702)
odds5
probability5 = odds5/(1+odds5)
probability5
#-->0.7422177
#-->74.2%


###Q5  predict with 'ToPredict.csv' - replacing 0 with mean

#To replace 0 values, I create another variable
pre2 <- pre1
#Replace 0 to NA
pre2[, 2:6][pre2[, 2:6] == 0] <- NA
#confirm each variable
summary(pre1)
summary(pre2)
pre2

#Replace NA to means in each column
#2nd column Glucose
pre2$Glucose[is.na(pre2$Glucose)] <- mean(re0na$Glucose, na.rm = TRUE)

#3rd Blood Pressure
pre2$BloodPressure[is.na(pre2$BloodPressure)] <- mean(re0na$BloodPressure, na.rm = TRUE)

#4th Skin Thickness
pre2$SkinThickness[is.na(pre2$SkinThickness)] <- mean(re0na$SkinThickness, na.rm = TRUE)

#5th Insulin
pre2$Insulin[is.na(pre2$Insulin)] <- mean(re0na$Insulin, na.rm = TRUE)

#6th BMI
pre2$BMI[is.na(pre2$BMI)] <- mean(re0na$BMI, na.rm = TRUE)

pre2
summary(pre2)
summary(re0na)

#apply to model4
predictData4 <- predict(model4, pre2)
predictData4
#-->
#        1          2          3          4          5 
#0.0615853 -0.8095680 -2.3361501  1.2827320  0.9461522

odds1 <- exp (0.0615853)
odds1
probability1 = odds1/(1+odds1)
probability1
#--> 0.5153915
#-->51.5%

odds2 <- exp (-0.8095680)
odds2
probability2 = odds2/(1+odds2)
probability2
#--> 0.3079826
#-->30.8%

odds3 <- exp (-2.3361501)
odds3
probability3 = odds3/(1+odds3)
probability3
#--> 0.08817295
#-->8.8%

odds4 <- exp (1.2827320)
odds4
probability4 = odds4/(1+odds4)
probability4
#--> 0.7829145
#-->78.3%

odds5 <- exp (0.9461522)
odds5
probability5 = odds5/(1+odds5)
probability5
#--> 0.7203407
#-->72.0%


