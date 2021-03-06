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
count(d, c("Survived"))/891 # Probabilidade de sobreviv�ncia
count(d[d$Survived==1,],"Sex") # De casa 3 sobreviventes temos 2 mulheres e 1 homem
count(d[d$Pclass==1 & d$Sex=='female',], "Survived") # 1� classe sobrevivei + 90%

count(d[d$Survived==1 & d$Sex=='male',], "Survived")

# Arvores de decis�o no R
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

# Compara��o Resultados
head(cbind(prev, teste$Survived)) # head traz o cabe�alho do DataFrame
# Quantidade de acertos de previs�o
sum(ifelse(prev==teste$Survived,1,0)) # 217/291 => 0,7475 (ACC: acur�cia)
# Matriz de Confus�o525
table(prev, teste$Survived)
count(teste$Survived)
count(prev)


# Execu��o do modelo
#m1<-lm(Survived ~ Sex + Age + as.factor(Pclass), data=train)

#debug
#summary(m1)
# Previs�o dos dados
#p<-predict(m1, newdata=test)

# Classifica��o de Survived, atrav�s do gatilho 0.5
#pr <- ifelse(p<0.5, 0, 1)

# Montagem do dataframe para sa�da de dados
pr_data <- cbind(teste$PassengerId, prev)
pr_data <- as.data.frame(pr_data)

# Nomes das colunas
names(pr_data) <- c("PassengerId", "Survived")

# Grava��o em disco do arquivo a ser submetido no site Kaggle
write.csv(pr_data, file="predict.csv", row.names = FALSE)