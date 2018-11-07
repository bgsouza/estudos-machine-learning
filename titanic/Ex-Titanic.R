# Leitura dos arquivos
train <- read.csv("train.csv")
test <- read.csv("test.csv")

d <- train

# Probabilidade de ser mulher
nrow(d[d$Sex=="female",]) / nrow(d) # [filtro de linha, filtro de coluna]
install.packages("sqldf")  #wrapper p/ sqllite
library(sqldf)

sqldf("select Sex,count(Survived) from d group by Sex")

install.packages("plyr")
library(plyr)
count(d, c("Sex"))
count(d, c("Survived"))/891 # Probabilidade de sobrevivência
count(d[d$Survived==1,],"Sex") # De casa 3 sobreviventes temos 2 mulheres e 1 homem
count(d[d$Pclass==1 & d$Sex=='female',], "Survived") # 1ª classe sobrevivei + 90%

count(d[d$Survived==1 & d$Sex=='male',], "Survived")

# Arvores de decisão no R
install.packages("party")
library(party)
#mod <- ctree(Survived ~ Sex, data=d)
mod <- ctree(Survived ~ Sex + Pclass + Age, data=d)
plot(mod, type="simple") #analizar entropia

# Modelo Preditivo AD (ML)
d <- read.csv("trains.csv")
set.seed(33)
va <- sample(nrow(d))
treino <- d[va[1:600],]
teste <- d[va[601:891],]
#mod <- ctree(Survived ~ Sex + Pclass + Age, data=treino)
#mod <- ctree(Survived ~ Sex + Pclass + Embarked , data=treino)
mod <- ctree(Survived ~ Sex + Age + Pclass + Embarked + Fare, data=treino)
p <- predict(mod, newdata=teste)
prev <- ifelse(p<.5,0,1)

# Comparação Resultados
head(cbind(prev, teste$Survived)) # head traz o cabeçalho do DataFrame
# Quantidade de acertos de previsão
sum(ifelse(prev==teste$Survived,1,0)) # 217/291 => 0,7475 (ACC: acurácia)
# Matriz de Confusão525
table(prev, teste$Survived)
count(teste$Survived)
count(prev)


# Execução do modelo
#m1<-lm(Survived ~ Sex + Age + as.factor(Pclass), data=train)

#debug
#summary(m1)
# Previsão dos dados
#p<-predict(m1, newdata=test)

# Classificação de Survived, através do gatilho 0.5
#pr <- ifelse(p<0.5, 0, 1)

# Montagem do dataframe para saída de dados
pr_data <- cbind(teste$PassengerId, prev)
pr_data <- as.data.frame(pr_data)

# Nomes das colunas
names(pr_data) <- c("PassengerId", "Survived")

# Gravação em disco do arquivo a ser submetido no site Kaggle
write.csv(pr_data, file="predict.csv", row.names = FALSE)