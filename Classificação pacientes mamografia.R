base = read.csv("mammographic.csv", header=FALSE)

colnames(base) <- c("Birads","Age","Shape","Margin","Density","Severity")

summary (base)

table (base$Birads)

base$Birads[base$Birads==55]<-5

base[base=="?"]<-NA

base[is.na(base$Age),]

#Colunas para numérico
base$Birads <- as.numeric(as.character(base$Birads))
base$Age <- as.numeric(as.character(base$Age))
base$Shape <- as.numeric(as.character(base$Shape))
base$Margin <- as.numeric(as.character(base$Margin))
base$Density <- as.numeric(as.character(base$Density))

#substituindo a NA com a média
base$Birads = ifelse(is.na(base$Birads), mean(base$Birads, na.rm = TRUE), base$Birads)
base$Age = ifelse(is.na(base$Age), mean(base$Age, na.rm = TRUE), base$Age)
base$Shape = ifelse(is.na(base$Shape), mean(base$Shape, na.rm = TRUE), base$Shape)
base$Margin = ifelse(is.na(base$Margin), mean(base$Margin, na.rm = TRUE), base$Margin)
base$Density = ifelse(is.na(base$Density), mean(base$Density, na.rm = TRUE), base$Density)

sapply(base, class) 

library(ggplot2)
ggplot(base, aes(x=Age))+
  geom_histogram(color="darkblue", fill="lightblue")

ggplot(base, aes(x=Age, color=Severity)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.1, fill="blue")

ggplot(base, aes(x=Severity))+ 
  geom_histogram(binwidth=0.5,color="black", fill="purple")
table(base$Severity) 

# Divisão entre treinamento e teste
library(caTools)
set.seed(1)
divisao = sample.split(base$Severity, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

library(e1071)
classificador = svm(formula = Severity ~ ., data = base_treinamento, type = 'nu-classification',
                    kernel = 'polynomial')
previsoes = predict(classificador, newdata = base_teste[-6])
matriz_confusao = table(base_teste[, 6], previsoes)
library(caret)
confusionMatrix(matriz_confusao)

# Acurácia de 82.08 %

