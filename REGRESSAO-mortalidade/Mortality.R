install.packages("car")
library(car)
library(ggplot2)

dataset_mortalidade <- read.csv('mortality.csv')
summary(dataset_mortalidade)


print("---------------------------------------------------------------")
print("-               REGRESSAO LINEAR SIMPLES                      -")
print("---------------------------------------------------------------")

# Para todas as regressoes, tanto lineares simples quanto lineares 
# multiplas, nossa variavel dependente sera a `hematocrit`.


print(">>>>>>>>>>>>>>>>>>>>>> CHLORIDE:")
summary(lm(log(hematocrit) ~ log(Chloride), data = dataset_mortalidade))
plot(ggplot(dataset_mortalidade, aes(log(Chloride), log(hematocrit))) + 
         geom_point() + stat_smooth(method = lm, level = 0) + 
         scale_x_continuous(n.breaks = 10) + 
         scale_y_continuous(n.breaks = 10))

print(">>>>>>>>>>>>>>>>>>>>>> BICARBONATE:")
summary(lm(log(hematocrit) ~ log(Bicarbonate), data = dataset_mortalidade))
plot(ggplot(dataset_mortalidade, aes(log(Bicarbonate), log(hematocrit))) + 
         geom_point() + stat_smooth(method = lm, level = 0) + 
         scale_x_continuous(n.breaks = 10) + 
         scale_y_continuous(n.breaks = 10))

print(">>>>>>>>>>>>>>>>>>>>>> RBC:")
summary(lm(log(hematocrit) ~ log(RBC), data = dataset_mortalidade))
plot(ggplot(dataset_mortalidade, aes(log(RBC), log(hematocrit))) + 
         geom_point() + stat_smooth(method = lm, level = 0) + 
         scale_x_continuous(n.breaks = 10) + 
         scale_y_continuous(n.breaks = 10))

print(">>>>>>>>>>>>>>>>>>>>>> RDW:")
summary(lm(log(hematocrit) ~ log(RDW), data = dataset_mortalidade))
plot(ggplot(dataset_mortalidade, aes(log(RDW), log(hematocrit))) + 
         geom_point() + stat_smooth(method = lm, level = 0) + 
         scale_x_continuous(n.breaks = 10) + 
         scale_y_continuous(n.breaks = 10))

print(">>>>>>>>>>>>>>>>>>>>>> Creatinine:")
summary(lm(log(hematocrit) ~ log(Creatinine), data = dataset_mortalidade))
plot(ggplot(dataset_mortalidade, aes(log(Creatinine), log(hematocrit))) + 
         geom_point() + stat_smooth(method = lm, level = 0) + 
         scale_x_continuous(n.breaks = 10) + 
         scale_y_continuous(n.breaks = 10))

print("---------------------------------------------------------------")
print("-               REGRESSAO LINEAR MULTIPLA                     -")
print("---------------------------------------------------------------")
print(">>>>>>>>>>>>>>>> RDW, RBC, Bicarbonate, Chloride e Creatinine:")
summary(lm(log(hematocrit)~log(RDW) + log(RBC) + log(Bicarbonate) + log(Chloride) + log(Creatinine), data = dataset_mortalidade))


print(">>>>>>>>>>>>>>>> RDW e RBC:")
summary(lm(log(hematocrit)~log(RDW) + log(RBC), data = dataset_mortalidade))


# TESTE DE NORMALIDADE DE SHAPIRO
shapiro.test(residuals(lm(log(hematocrit)~log(RDW) + log(RBC), data = dataset_mortalidade)))

# PLOT RESIDUOS
ncvTest(lm(log(hematocrit)~log(RDW) + log(RBC), data = dataset_mortalidade))
plot(residuals(lm(log(hematocrit)~log(RDW) + log(RBC), data = dataset_mortalidade)))
plot(lm(log(hematocrit)~log(RDW) + log(RBC), data = dataset_mortalidade))
