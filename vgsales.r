library(dplyr)
library(ggplot2)
library(doBy)

mydata<-read.csv('vgsales-12-4-2019-short.csv')

mydata<-mydata[mydata$Year!="2020",]
mydata$User_Score<-NULL
mydata$Total_Shipped<-NULL

glimpse(mydata)

row_to_keep<-c()

for (i in seq_along(mydata[[1]]))
{
    if(!is.na(mydata$NA_Sales[i]) && !is.na(mydata$PAL_Sales[i]) && !is.na(mydata$JP_Sales[i]) && !is.na(mydata$Other_Sales[i]))
        {
            row_to_keep<-c(row_to_keep,i);
        }
}

vgsales<- mydata[row_to_keep,]

q1<-count(vgsales, Year,Genre)

rowremain<-c()
for(i in seq_along(q1$Genre)){
    if( q1$Genre[i]=="Action"  || q1$Genre[i]=="Shooter" || q1$Genre[i]=="Role-Playing"  )
    rowremain <- c(rowremain, i)
}

q1main <- q1[rowremain,]

ggplot(q1, aes(x = factor(Year), y = n, colour=Genre, group=Genre)) + geom_line()+ggtitle("Trend of Genre by Year")+theme(axis.text.x = element_text(angle = 90,hjust = 1))

ggplot(q1main, aes(x = factor(Year), y = n, colour=Genre, group=Genre)) + geom_line()+ggtitle("Trend of Genre by Year")+theme(axis.text.x = element_text(angle = 90,hjust = 1))

q2<-count(vgsales, Year,Platform)

ggplot(q2, aes(x = factor(Year), y = n, colour=Platform, group=Platform)) + geom_line()+ggtitle("Trend of Platform by Year")+theme(axis.text.x = element_text(angle = 90,hjust = 1))

q3_ps<-q2[q2$Platform=="PS" | q2$Platform=="PS2" | q2$Platform=="PS3"| q2$Platform=="PS4",]
ggplot(q3_ps, aes(x = factor(Year), y = n, colour=Platform, group=Platform)) + geom_line()+ggtitle("Trend of 'PS' Platform by Year")+theme(axis.text.x = element_text(angle = 90,hjust = 1))

q4_xb<-q2[q2$Platform=="XB" | q2$Platform=="X360" | q2$Platform=="XOne",]
ggplot(q4_xb, aes(x = factor(Year), y = n, colour=Platform, group=Platform)) + geom_line()+ggtitle("Trend of 'Xbox' Platform by Year")+theme(axis.text.x = element_text(angle = 90,hjust = 1))

q5_mix<-q2[q2$Platform=="XB" | q2$Platform=="X360"| q2$Platform=="XOne" | q2$Platform=="PS"|q2$Platform=="PS2" | q2$Platform=="PS3" | q2$Platform=="PS4" ,]
ggplot(q5_mix, aes(x = factor(Year), y = n, colour=Platform, group=Platform)) + geom_line()+ggtitle(" The competition between SONY and Microsoft")+theme(axis.text.x = element_text(angle = 90,hjust = 1))

sum_sale<-summaryBy(NA_Sales+PAL_Sales+JP_Sales+Other_Sales~Genre,data=vgsales,FUN=sum)
sum_sale

label_PAL<-paste('(', round(sum_sale$PAL_Sales.sum/sum(sum_sale$PAL_Sales.sum) * 100, 1), '%)', sep = '')
label_PAL

labelPAL <- paste(sum_sale$Genre, label_PAL, sep = '')
labelPAL

ggplot(data = sum_sale, mapping = aes(x = 'Content', y = PAL_Sales.sum, fill = Genre )) + geom_bar(stat = 'identity', position = 'stack', width = 1)+ coord_polar(theta ="y") + ggtitle("Pie chart for PAL_Sales")+ theme(axis.text = element_blank())+ scale_fill_discrete(labels =labelPAL)

label_JP<-paste('(', round(sum_sale$JP_Sales.sum/sum(sum_sale$JP_Sales.sum) * 100, 1), '%)', sep = '')

labelJP <- paste(sum_sale$Genre, label_JP, sep = '')

ggplot(data = sum_sale, mapping = aes(x = 'Content', y = JP_Sales.sum, fill = Genre )) + geom_bar(stat = 'identity', position = 'stack', width = 1)+ coord_polar(theta ="y") + ggtitle("Pie chart for JP_Sales")+ theme(axis.text = element_blank())+ scale_fill_discrete(labels =labelJP)

label_OTHER<-paste('(', round(sum_sale$Other_Sales.sum/sum(sum_sale$Other_Sales.sum) * 100, 1), '%)', sep = '')

labelOTHER <- paste(sum_sale$Genre, label_OTHER, sep = '')

ggplot(data = sum_sale, mapping = aes(x = 'Content', y = Other_Sales.sum, fill = Genre )) + geom_bar(stat = 'identity', position = 'stack', width = 1)+ coord_polar(theta ="y") + ggtitle("Pie chart for Other_Sales")+ theme(axis.text = element_blank())+ scale_fill_discrete(labels =labelOTHER)  

vg_Nintendo<- subset(vgsales,Publisher =="Nintendo" )

summary(vgsales)

vg_nintendo_NA <- vg_Nintendo[,-c(9,11,12,13)]

colnames(vg_nintendo_NA)[9] <- "sales" 

vg_nintendo_NA$region<-rep("NORTH AMERICA",nrow(vg_nintendo_NA))
set.seed(1000)
vg_nintendo_NA_sample <- sample(2, nrow(vg_nintendo_NA), replace=TRUE, prob=c(0.67, 0.33))
vg_nintendo_NA_training <- vg_nintendo_NA[vg_nintendo_NA_sample==1,]
vg_nintendo_NA_test <- vg_nintendo_NA[vg_nintendo_NA_sample==2,]




vg_nintendo_PAL <- vg_Nintendo[,-c(9,10,12,13)]

colnames(vg_nintendo_PAL)[9] <- "sales" 

vg_nintendo_PAL$region<-rep("EUROPEAN",nrow(vg_nintendo_PAL))
set.seed(1000)
vg_nintendo_PAL_sample <- sample(2, nrow(vg_nintendo_PAL), replace=TRUE, prob=c(0.67, 0.33))
vg_nintendo_PAL_training <- vg_nintendo_PAL[vg_nintendo_PAL_sample==1,]
vg_nintendo_PAL_test <- vg_nintendo_PAL[vg_nintendo_PAL_sample==2,]




vg_nintendo_JP <- vg_Nintendo[,-c(9,10,11,13)]

colnames(vg_nintendo_JP)[9] <- "sales" 

vg_nintendo_JP$region<-rep("JAPAN",nrow(vg_nintendo_JP))
set.seed(1000)
vg_nintendo_JP_sample <- sample(2, nrow(vg_nintendo_JP), replace=TRUE, prob=c(0.67, 0.33))
vg_nintendo_JP_training <- vg_nintendo_JP[vg_nintendo_JP_sample==1,]
vg_nintendo_JP_test <- vg_nintendo_JP[vg_nintendo_JP_sample==2,]



vg_nintendo_O <- vg_Nintendo[,-c(9,10,11,12)]

colnames(vg_nintendo_O)[9] <- "sales" 

vg_nintendo_O$region<-rep("OTHER",nrow(vg_nintendo_O))
set.seed(1000)
vg_nintendo_O_sample <- sample(2, nrow(vg_nintendo_O), replace=TRUE, prob=c(0.67, 0.33))
vg_nintendo_O_training <- vg_nintendo_O[vg_nintendo_O_sample==1,]
vg_nintendo_O_test <- vg_nintendo_O[vg_nintendo_O_sample==2,]



#rbind

new_video_nintendo_training<-rbind(vg_nintendo_NA_training,vg_nintendo_PAL_training,vg_nintendo_JP_training,vg_nintendo_O_training)
new_video_nintendo_test<-rbind(vg_nintendo_NA_test,vg_nintendo_PAL_test,vg_nintendo_JP_test,vg_nintendo_O_test)

#linear Regression

lm1 <- lm(sales~Platform+Genre+Critic_Score+region-1,data=new_video_nintendo_training)
summary(lm1)

plot(lm1)

new_video_nintendo_training$sales_sqrt <- sqrt(new_video_nintendo_training$sales)
new_video_nintendo_test$sales_sqrt <- sqrt(new_video_nintendo_test$sales)

lm2 <- lm(sales_sqrt~Platform+Genre+Critic_Score+region-1,data=new_video_nintendo_training)
summary(lm2)

plot(lm2)

row_to_keep<-c()

for (i in seq_along(new_video_nintendo_test[[1]]))
{
    if(new_video_nintendo_test$Platform[i]!='NES')
        {
            row_to_keep<-c(row_to_keep,i);
        }
}

new_video_nintendo_test1 <- new_video_nintendo_test[row_to_keep,]

pred<- predict(lm2,new_video_nintendo_test1)
Eval <- data.frame(Game= new_video_nintendo_test1$Name, Actual = new_video_nintendo_test1$sales_sqrt)
pred <- round(pred,2)
Eval <- Eval[1:length(pred),]
Eval$Predicted <- abs(pred)
Eval$diff <- abs(Eval$Predicted - Eval$Actual)
row <- !is.na(Eval$Predicted)
eval1 <- Eval[row,]
head(eval1)

RMSE <- sqrt(mean(eval1$diff^2))
RMSE

#Hypothesis: There is no significant change in NA_Sales with respect to genre and platform.
fit <- lm( NA_Sales ~ Genre , data = vgsales)
summary(fit)

#Hypothesis: There is no significant change in EU_Sales with respect to genre and platform.
fit <- lm( PAL_Sales ~ Genre , data = vgsales)
summary(fit)

#Hypothesis: There is no significant change in JP_Sales with respect to genre and platform.
fit <- lm( JP_Sales ~ Genre , data = vgsales)
summary(fit)


