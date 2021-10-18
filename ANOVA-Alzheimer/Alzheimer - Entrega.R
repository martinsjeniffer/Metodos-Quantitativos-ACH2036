library(cowplot)
library(nortest)
library(dplyr)
library(plyr)
library(ggplot2)

dataset_escolhido <- read.csv('oasis_longitudinal.csv')
head(dataset_escolhido)

# Esta funcao controi um grafico contendo as distribuicoes de frequencias
# relativas acumuladas e de probabilidades da normal sobrepostas, para
# critério de comparação. O eixo das ordenadas se refere às distribuicoes
# acumuladas de 0 a 1, enquanto o eixo das abscissas se refere aos valores
# da variável variando do mín ou máx entre eles.

plot_cumulative_distribution_lilliefors <- function(values_list)
{
    lista_de_valores_do_atributo <- values_list
    # lista em ordem crescente
    lista_de_valores_do_atributo <- sort(lista_de_valores_do_atributo)
    # valor maximo da lista
    min_lista_valores_atributo <- min(unlist(lista_de_valores_do_atributo))
    # valor mínimo da lista
    max_lista_valores_atributo <- max(unlist(lista_de_valores_do_atributo))
    # media dos valores da lista
    media_lista_valores_atributo <- mean(lista_de_valores_do_atributo)
    # desvio padrao da lista
    desvio_padrao_lista_valores_atributo <- sd(lista_de_valores_do_atributo)
    
    # soma acumulada dos valores do variavel
    soma_acumulada <- cumsum(lista_de_valores_do_atributo)
    # frequencias relativas
    freq_relativas <- as.vector(table(lista_de_valores_do_atributo)/length(lista_de_valores_do_atributo))
    # frequencia acumulada
    freq_relativas_acumuladas <- cumsum(freq_relativas)
    # retira os valores repetidos
    lista_de_valores_do_atributo <- unique(lista_de_valores_do_atributo)
    
    #plotagem da curva normal junto com a frequencia relativa acumulada.
    plot(ggplot(data = data.frame(x = seq(min_lista_valores_atributo, max_lista_valores_atributo)), aes(x))
         + stat_function(fun = pnorm, n = 101, args = list(mean = media_lista_valores_atributo, sd = desvio_padrao_lista_valores_atributo), linetype="longdash")
         + ylab("Distribuição acumulada")
         + xlab("Valor da variável")
         + geom_step(data = data.frame(x = lista_de_valores_do_atributo[1:length(freq_relativas_acumuladas)]), aes(x=x, y=freq_relativas_acumuladas), color="blue"))
}

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
############## GRUPOS SEPARADOS A PARTIR DO DATASET ###########
###############################################################

demented_and_converted_df   <-  dataset_escolhido[order(dataset_escolhido$Group),][c(0:183),c(1,10,11,12)]
non_demented_df             <-  dataset_escolhido[order(dataset_escolhido$Group),][c(184:373),c(1,10,11,12)]

################################################################
##################### HISTOGRAMAS ##############################
################################################################

hist(demented_and_converted_df$nWBV)
hist(non_demented_df$nWBV)

################################################################
############### LILLIEFORS TESTE - resultados ##################
################################################################

print(lillie.test(log(demented_and_converted_df$nWBV)))
print(lillie.test(log(non_demented_df$nWBV)))

################################################################
####################### BOXPLOTS ###############################
################################################################

boxplot(demented_and_converted_df$eTIV)
boxplot(non_demented_df$eTIV)

################################################################
########## Distribuicoes acumuladas para Lilliefors ############
################################################################

plot_cumulative_distribution_lilliefors(log(demented_and_converted_df$nWBV))
plot_cumulative_distribution_lilliefors(log(non_demented_df$nWBV))
