install.packages("dplyr")
install.packages('plyr')
install.packages('car')
#install.packages("corrplot")
#install.packages("caret")
#install.packages("caretEnsemble")
install.packages("ggplot2")
#install.packages("randomForest")
#install.packages("xgboost")
library(dplyr)
library(plyr)
#library(car)
#library(corrplot)
#library(caret)
#library(caretEnsemble)
library(ggplot2)
#library(randomForest)
#library(xgboost)

df <- read.csv('oasis_longitudinal.csv')
head(df)
#df

dim(df)
print('/n')
str(df)
print('/n')
summary(df)

#Drop hand
table(df$Hand)
df$Hand<-NULL

#Drop Ids
subject_id<-df$Subject.ID
MRI_id<-df$MRI.ID

df$Subject.ID<-NULL
df$MRI.ID<-NULL

sort(apply(df, 2, function(x){sum(is.na(x))}), decreasing = TRUE)

table(df$SES)

df <- df[order(df$Group),]
head(df)
#df %>% group_by(MMSE) %>% arrange(Group) 
count(df, 'Group')

df1 <- df[order(df$Group),][c(0:37),c(1,11,12)]
#df1
df2 <- df[order(df$Group),][c(38:183),c(1,11,12)]
#df2
df3 <- df[order(df$Group),][c(184:373),c(1,11,12)]
#df3

hist(df1$ASF)
hist(df2$ASF)
hist(df3$ASF)

shapiro.test(df1$ASF)
shapiro.test(df2$ASF)
shapiro.test(df3$ASF)

hist(df1$nWBV)
hist(df2$nWBV)
hist(df3$nWBV)

shapiro.test(df1$nWBV)
shapiro.test(df2$nWBV)
shapiro.test(df3$nWBV)

ggplot(df, aes(Group, EDUC))+
  geom_boxplot(col = "blue")+
  geom_point(stat = "summary", fun.y = "mean", col = "red", size = 4)+
  ggtitle("Education and Dementia")+
  theme(plot.title = element_text(hjust = .5))

#No real difference
#ggplot(MRI_data, aes(M.F, EDUC))+
#  geom_boxplot(col = "blue")+
#  ggtitle("Years of Education by Sex")+
#  theme(plot.title = element_text(hjust = .5))

anova_one_way <- aov(nWBV~Group, data = df)
summary(anova_one_way)

anova(aov(ASF~Group, data = df))