#install.packages("dplyr")
#install.packages('plyr')
#install.packages('car')
# install.packages("cowplot")
library(cowplot)
library(nortest)
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


dataset_escolhido <- read.csv('oasis_longitudinal.csv')
head(dataset_escolhido)
#df

# dim(df)
# print('/n')
# str(df)
# print('/n')
# summary(df)

#Drop hand
table(dataset_escolhido$Hand)
dataset_escolhido$Hand<-NULL

#Drop Ids
subject_id<-dataset_escolhido$Subject.ID
MRI_id<-dataset_escolhido$MRI.ID

dataset_escolhido$Subject.ID<-NULL
dataset_escolhido$MRI.ID<-NULL

sort(apply(dataset_escolhido, 2, function(x){sum(is.na(x))}), decreasing = TRUE)

table(dataset_escolhido$SES)

dataset_escolhido <- dataset_escolhido[order(dataset_escolhido$Group),]
head(dataset_escolhido)
#df %>% group_by(MMSE) %>% arrange(Group) 
count(dataset_escolhido, 'Group')

###############################################################
############## DATAFRAMES SEPARADOS EM GRUPOS #################
###############################################################

demented_and_converted_df <- dataset_escolhido[order(dataset_escolhido$Group),][c(0:183),c(1,10,11,12)]
non_demented_df <- dataset_escolhido[order(dataset_escolhido$Group),][c(184:373),c(1,10,11,12)]

###############################################################
##################### SHAPIRO TESTE ###########################
###############################################################

shapiro.test(demented_and_converted_df$ASF)
shapiro.test(non_demented_df$ASF)

shapiro.test(demented_and_converted_df$nWBV)
shapiro.test(non_demented_df$nWBV)

shapiro.test(demented_and_converted_df$eTIV)
shapiro.test(non_demented_df$eTIV)

################################################################
##################### HISTOGRAMAS ##############################
################################################################

hist(demented_and_converted_df$ASF)
hist(non_demented_df$ASF)

hist(demented_and_converted_df$nWBV)
hist(non_demented_df$nWBV)

hist(demented_and_converted_df$eTIV)
hist(non_demented_df$eTIV)

################################################################
##################### LILLIE TESTE #############################
################################################################

lillie.test(demented_and_converted_df$ASF)
lillie.test(non_demented_df$ASF)

lillie.test(demented_and_converted_df$nWBV)
lillie.test(non_demented_df$nWBV)

lillie.test(demented_and_converted_df$eTIV)
lillie.test(non_demented_df$eTIV)

################################################################
####################### BOXPLOTS ###############################
################################################################

boxplot(demented_and_converted_df$eTIV)
boxplot(non_demented_df$eTIV)

# print(lillie.test(df1$nWBV))
# print(nrow(df1))

# ggplot(df, aes(Group, EDUC))+
#   geom_boxplot(col = "blue")+
#   geom_point(stat = "summary", fun.y = "mean", col = "red", size = 4)+
#   ggtitle("Education and Dementia")+
#   theme(plot.title = element_text(hjust = .5))

#No real difference
#ggplot(MRI_data, aes(M.F, EDUC))+
#  geom_boxplot(col = "blue")+
#  ggtitle("Years of Education by Sex")+
#  theme(plot.title = element_text(hjust = .5))

# anova_one_way <- aov(nWBV~Group, data = df)
# summary(anova_one_way)
# 
# anova(aov(ASF~Group, data = df))

########################## Editado por Fabricio #############################
################## Plotagem de grafico para Lillie Test #####################

lista_de_valores_do_atributo <- c(93.45, 94.46, 94.93, 96.17, 96.74, 97.07, 97.68, 97.93, 99.1, 99.3, 100.73, 103.29, 103.6, 103.83, 105.2)
lista_de_valores_do_atributo <- demented_and_converted_df$eTIV
lista_de_valores_do_atributo <- sort(lista_de_valores_do_atributo)
min_lista_valores_atributo <- min(unlist(lista_de_valores_do_atributo))
max_lista_valores_atributo <- max(unlist(lista_de_valores_do_atributo))
media_lista_valores_atributo <- mean(lista_de_valores_do_atributo)
desvio_padrao_lista_valores_atributo <- sd(lista_de_valores_do_atributo)


#plotagem de curva normal de gauss com media e desvio padrao fornecidos sem utilizar ggplot
plot(seq(min_lista_valores_atributo,max_lista_valores_atributo,0.01), pnorm(seq(min_lista_valores_atributo,max_lista_valores_atributo,0.01), media_lista_valores_atributo, desvio_padrao_lista_valores_atributo, lower.tail = TRUE, log.p = FALSE),ylab = "probability", xlab = "mm", type = "l", lwd=2)
#plotagem de curva normal de gauss com media e desvio padrao fornecidos utilizando ggplot
plot(ggplot(data = data.frame(x = seq(min_lista_valores_atributo, max_lista_valores_atributo)), aes(x)) + stat_function(fun = pnorm, n = 101, args = list(mean = media_lista_valores_atributo, sd = desvio_padrao_lista_valores_atributo)) + ylab("aaa") + xlab("fwefew"))

# vetor teste
vetor <- lista_de_valores_do_atributo
# vetor soma acumulada
soma_acumulada <- cumsum(vetor)
# frequencias relativas
freq_relativas <- as.vector(table(vetor)/length(vetor))
# frequencia acumulada
freq_relativas_acumuladas <- cumsum(freq_relativas)
# retira os valores repetidos
lista_de_valores_do_atributo <- unique(lista_de_valores_do_atributo)

#plotagem da curva normal junto com a frequencia relativa acumulada.
plot(ggplot(data = data.frame(x = seq(min_lista_valores_atributo, max_lista_valores_atributo)), aes(x))
     + stat_function(fun = pnorm, n = 101, args = list(mean = media_lista_valores_atributo, sd = desvio_padrao_lista_valores_atributo), linetype="longdash")
     + ylab("aaa")
     + xlab("fwefew")
     + geom_step(data = data.frame(x = lista_de_valores_do_atributo[1:length(freq_relativas_acumuladas)]), aes(x=x, y=freq_relativas_acumuladas), color="blue"))
 

#############################################################################
#############################################################################

funcao_criada <- as.function(alist(x=, pnorm(x - media_lista_valores_atributo)))

