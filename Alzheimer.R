#install.packages("dplyr")
#install.packages('plyr')
#install.packages('car')
#install.packages("corrplot")
#install.packages("caret")
#install.packages("caretEnsemble")
#install.packages("ggplot2")
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

########################## Editado por Fabricio #############################
################## Plotagem de grafico para Lillie Test #####################

lista_de_valores_do_atributo <- c(93.45, 94.46, 94.93, 96.17, 96.74, 97.07, 97.68, 97.93, 99.1, 99.3, 100.73, 103.29, 103.6, 103.83, 105.2)
min_lista_valores_atributo <- min(unlist(lista_de_valores_do_atributo))
max_lista_valores_atributo <- max(unlist(lista_de_valores_do_atributo))
media_lista_valores_atributo <- mean(lista_de_valores_do_atributo)
desvio_padrao_lista_valores_atributo <- sd(lista_de_valores_do_atributo)

windows.options(width=10, height=20)
plot(seq(min_lista_valores_atributo,max_lista_valores_atributo,0.01), pnorm(seq(min_lista_valores_atributo,max_lista_valores_atributo,0.01), media_lista_valores_atributo, desvio_padrao_lista_valores_atributo, lower.tail = TRUE, log.p = FALSE),ylab = "probability", xlab = "mm", type = "l", lwd=2)

#############################################################################
#############################################################################