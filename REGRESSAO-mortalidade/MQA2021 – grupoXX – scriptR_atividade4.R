library(car)
library(ggplot2)
library(dplyr)


dataset_mortalidade <- read.csv('mortality.csv')
summary(dataset_mortalidade)

# - Hematocrit (Percentagem de volume ocupado pelas hemaceas no volume total de sangue)
# - Chloride (Taxa de Cloreto no sangue)
# - Bicarbonate (Taxa de Bicarbonato no sangue)
# - RBC (Numero de Hemaceas)
# - RDW (Score normalizado da media da largura das Hemaceas)
# - Creatinine (Creatinina Fosforilada)

#	Escolhas feitas para estudar o impacto de determinadas variaveis no resultado do teste Hematocrit
#	Cloreto e Bicarbonato fazem parte da troca de substÃÂ¢ncias na forma ionica quando as hemaceas captam o O2
#	O numero de Hemaceas e sua largura possivelmente influenciam diretamente no teste
#		 por estarmos tratando do numero de hemaceas total estimado e a largura delas, fatores que influenciam no volume ocupado
#	Creatinina Fosforilada eh uma substancia que se mostra mais presente em pessoas com mais massa muscular, 
#		e massa muscular pode implicar em maior numero de hemaceas dependendo do esporte praticado

print("---------------------------------------------------------------")
print("-                ESTATASTICA DESCRITIVA                       -")
print("---------------------------------------------------------------")
print(">>>>>>>>>>>>>>>>>>>>>> HEMATOCRIT:")
ggplot(data=dataset_mortalidade, aes(hematocrit)) + 
  geom_histogram(col = "blue", fill = "steelblue2")+
  ggtitle("Histograma: Hematocrit - Percentual de Volume de hemaceas em relacao ao Volume de sangue")

ggplot(dataset_mortalidade, aes(,hematocrit))+
  geom_boxplot(col = "blue")+
  ggtitle("Hematocrit: Percentual de Volume de hemaceas em relacao ao Volume de sangue")+
  theme(plot.title = element_text(hjust = .5))+
  coord_cartesian(xlim = c(-.75, .75))

print(">>>>>>>>>>>>>>>>>>>>>> CHLORIDE:")
ggplot(data=dataset_mortalidade, aes(Chloride)) + 
  geom_histogram(col = "blue", fill = "steelblue2")+
  ggtitle("Histograma: Cloreto no Sangue")

ggplot(dataset_mortalidade, aes(,Chloride))+
  geom_boxplot(col = "blue")+
  ggtitle("Cloreto no Sangue")+
  theme(plot.title = element_text(hjust = .5))+
  coord_cartesian(xlim = c(-.75, .75))

print(">>>>>>>>>>>>>>>>>>>>>> BICARBONATE:")
ggplot(data=dataset_mortalidade, aes(Bicarbonate)) + 
  geom_histogram(col = "blue", fill = "steelblue2")+
  ggtitle("Histograma: Bicarbonato no Sangue")

ggplot(dataset_mortalidade, aes(,Bicarbonate))+
  geom_boxplot(col = "blue")+
  ggtitle("Bicarbonato no Sangue")+
  theme(plot.title = element_text(hjust = .5))+
  coord_cartesian(xlim = c(-.75, .75))

print(">>>>>>>>>>>>>>>>>>>>>> RBC:")
ggplot(data=dataset_mortalidade, aes(RBC)) + 
  geom_histogram(col = "blue", fill = "steelblue2")+
  ggtitle("Histograma: RBC - Numero estimado de hemaceas no sangue")

ggplot(dataset_mortalidade, aes(,RBC))+
  geom_boxplot(col = "blue")+
  ggtitle("RBC - Numero estimado de hemaceas no sangue")+
  theme(plot.title = element_text(hjust = .5))+
  coord_cartesian(xlim = c(-.75, .75))

print(">>>>>>>>>>>>>>>>>>>>>> RDW:")
ggplot(data=dataset_mortalidade, aes(RDW)) + 
  geom_histogram(col = "blue", fill = "steelblue2")+
  ggtitle("Histograma: RDW - Score normalizado da largura media das hemaceas")  

ggplot(dataset_mortalidade, aes(,RDW))+
  geom_boxplot(col = "blue")+
  ggtitle("RDW - Score normalizado da largura media das hemaceas")+
  theme(plot.title = element_text(hjust = .5))+
  coord_cartesian(xlim = c(-.75, .75))

print(">>>>>>>>>>>>>>>>>>>>>> CREATININE:")
ggplot(data=dataset_mortalidade, aes(Creatinine)) + 
  geom_histogram(col = "blue", fill = "steelblue2")+
  ggtitle("Histograma: Creatinine - Creatinina Fosforilada")  

ggplot(dataset_mortalidade, aes(,Creatinine))+
  geom_boxplot(col = "blue", varwidth = TRUE)+
  ggtitle("Creatinine - Creatinina Fosforilada")+
  theme(plot.title = element_text(hjust = .5))+
  coord_cartesian(xlim = c(-.75, .75))
  

print("---------------------------------------------------------------")
print("-               REGRESSAO LINEAR SIMPLES                      -")
print("---------------------------------------------------------------")

# Para todas as regressoes, tanto lineares simples quanto lineares 
# multiplas, nossa variavel dependente sera a `hematocrit`.


print(">>>>>>>>>>>>>>>>>>>>>> CHLORIDE:")
summary(lm(log(hematocrit) ~ log(Chloride), data = dataset_mortalidade))
ggplot(dataset_mortalidade, aes(log(Chloride), log(hematocrit))) + 
	geom_point() + stat_smooth(method = lm, level = 0) + 
        scale_x_continuous(n.breaks = 10) + 
        scale_y_continuous(n.breaks = 10) +
	ggtitle("Hematocrit x Cloreto", subtitle = "Hematocrit - Percentagem de volume ocupado pelas hemaceas no volume total de sangue\nChloride - Taxa de Cloreto no Sangue") 


print(">>>>>>>>>>>>>>>>>>>>>> BICARBONATE:")
summary(lm(log(hematocrit) ~ log(Bicarbonate), data = dataset_mortalidade))
ggplot(dataset_mortalidade, aes(log(Bicarbonate), log(hematocrit))) + 
	geom_point() + stat_smooth(method = lm, level = 0) + 
        scale_x_continuous(n.breaks = 10) + 
        scale_y_continuous(n.breaks = 10) +
	ggtitle("Hematocrit x Bicarbonate", subtitle = "Hematocrit - Percentagem de volume ocupado pelas hemaceas no volume total de sangue\nBicarbonate - Taxa de Bicarbonato no Sangue") 

print(">>>>>>>>>>>>>>>>>>>>>> RBC:")
summary(lm(log(hematocrit) ~ log(RBC), data = dataset_mortalidade))
ggplot(dataset_mortalidade, aes(log(RBC), log(hematocrit))) + 
	geom_point() + stat_smooth(method = lm, level = 0) + 
        scale_x_continuous(n.breaks = 10) + 
        scale_y_continuous(n.breaks = 10) +
	ggtitle("Hematocrit x RBC", subtitle = "Hematocrit - Percentagem de volume ocupado pelas hemaceas no volume total de sangue\nRBC - Numero estimado de hemaceas no sangue") 


print(">>>>>>>>>>>>>>>>>>>>>> RDW:")
summary(lm(log(hematocrit) ~ log(RDW), data = dataset_mortalidade))
ggplot(dataset_mortalidade, aes(log(RDW), log(hematocrit))) + 
	geom_point() + stat_smooth(method = lm, level = 0) + 
        scale_x_continuous(n.breaks = 10) + 
        scale_y_continuous(n.breaks = 10) +
	ggtitle("Hematocrit x RDW", subtitle = "Hematocrit - Percentagem de volume ocupado pelas hemaceas no volume total de sangue\nRDW - Score normalizado da media da largura das Hemaceas") 


print(">>>>>>>>>>>>>>>>>>>>>> Creatinine:")
summary(lm(log(hematocrit) ~ log(Creatinine), data = dataset_mortalidade))
ggplot(dataset_mortalidade, aes(log(Creatinine), log(hematocrit))) + 
	geom_point() + stat_smooth(method = lm, level = 0) + 
        scale_x_continuous(n.breaks = 10) + 
        scale_y_continuous(n.breaks = 10) +
	ggtitle("Hematocrit x Creatinine", subtitle = "Hematocrit - Percentagem de volume ocupado pelas hemaceas no volume total de sangue\nCreatinine - Creatinina Fosforilada") 



print("---------------------------------------------------------------")
print("-               REGRESSAO LINEAR MULTIPLA                     -")
print("---------------------------------------------------------------")
#	Retira-se um elemento por vez de acordo com o p valor, 
#		p valor < 0.05 indica significancia baixa na representacao da variavel dependente.

print(">>>>>>>>>>>>>>>> RDW, RBC, Bicarbonate, Chloride e Creatinine:")
summary(lm(log(hematocrit)~log(RDW) + log(RBC) + log(Bicarbonate) + log(Chloride) + log(Creatinine), data = dataset_mortalidade))

print(">>>>>>>>>>>>>>>> RDW, RBC, Bicarbonate, e Chloride:")
summary(lm(log(hematocrit)~log(RDW) + log(RBC) + log(Bicarbonate) + log(Chloride), data = dataset_mortalidade))

print(">>>>>>>>>>>>>>>> RDW, RBC e Bicarbonate:")
summary(lm(log(hematocrit)~log(RDW) + log(RBC) + log(Bicarbonate), data = dataset_mortalidade))

print(">>>>>>>>>>>>>>>> RDW e RBC:")
summary(lm(log(hematocrit)~log(RDW) + log(RBC), data = dataset_mortalidade))


# TESTE DE NORMALIDADE DE SHAPIRO
shapiro.test(residuals(lm(log(hematocrit)~log(RDW) + log(RBC), data = dataset_mortalidade)))

# TESTE DE HOMOCEDASTICIDADE
ncvTest(lm(log(hematocrit)~log(RDW) + log(RBC), data = dataset_mortalidade))

# PLOT RESIDUOS
plot(residuals(lm(log(hematocrit)~log(RDW) + log(RBC), data = dataset_mortalidade)))
plot(lm(log(hematocrit)~log(RDW) + log(RBC), data = dataset_mortalidade))
