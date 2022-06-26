library(tidyr)
library(stringr)

#Juntei os dados de treino e teste para modificar os dois ao mesmo tempo

dados_full <- bind_rows(train,test)

########## Exploracao e tratamento de dados ##########

names(dados_full)

##### PassengereId #####

head(dados_full$PassengerId) #ID nao precisa de nada

##### Survived #####

head(dados_full$Survived) 
levels(dados_full$Survived) #temos que transformar essa variavel em categorica
dados_full$Survived <- as.factor(dados_full$Survived)
levels(dados_full$Survived)

##### Pclass #####

head(dados_full$Pclass)
dados_full$Pclass <- as.factor(dados_full$Pclass) #transformando em categorica
levels(dados_full$Pclass)
sum(is.na(dados_full$Pclass)) #Nenhum dado faltante

##### Name #####

head(dados_full$Name)
sum(is.na(dados_full$Name))
#Nao temos nenhum nome faltante, o q eh muito bom, mas acho que podemos criar uma variavel categorica "titulo" para as pessoas com algum titulo diferente

##### Titulo #####

head(dados_full$Name) #Observei que os titulos vem depois de uma virgula, vou tentar usar isso para me ajudar
#atraves do site regex101.com eu cheguei a seguinte expressao \,(.*?)\., vou tentar usa-la
str_extract(dados_full$Name,"\\,(.*?)\\.") #nao sei pq, mas tive que usar \\ no R
#agora vou manter apenas as letras 
dados_full$titulo<-dados_full$Name
str_extract(str_extract(dados_full$Name,"\\,(.*?)\\."),"\\w+.") #prontinho, agora podemos ter nossa categoria "titulo" :D
dados_full$titulo <- str_extract(str_extract(dados_full$Name,"\\,(.*?)\\."),"\\w+.")
dados_full$titulo<-as.factor(dados_full$titulo) 
levels(dados_full$titulo) #Vou combinar os titulos em 4 categorias, Dif. Master. Mr. e Ms.
levels(dados_full$titulo) <-  c("Dif."  ,   "Dif."      ,"Dif."      ,"Dif."     ,"Dif."       ,"Dif." ,"Dif.",     "Dif.",    "Master.",  
                                "Ms."   ,  "Dif."     ,"Dif."      ,"Mr."       ,"Ms."    ,  "Ms."     ,  "Dif."     , "Dif."   ,   "Dif.")
levels(dados_full$titulo)

##### Sex #####

head(dados_full$Sex)
levels(dados_full$Sex) #temos que transformar essa variavel em categorica
dados_full$Sex <- as.factor(dados_full$Sex)
levels(dados_full$Sex)

##### Age #####
sum(is.na(dados_full$Age)) #temos 177 dados faltantes na nossa variavel age, vou substituir os faltantes pela media
dados_full$Age[is.na(dados_full$Age)]<-round(mean(dados_full$Age,na.rm = T))

##### Family #####
#As colunas Sibsp e Parch indicam familia em geral, entao vou somar as duas e fazer uma nova variavel "family"
dados_full$family <- dados_full$SibSp + dados_full$Parch
dados_full$family


##### Ticket #####
#Nao sei qual a utilidade dessa variavel, nao vou utiliza-la

##### Fare #####

sum(is.na(dados_full$Fare)) #1 dado faltante
dados_full$Fare[which(is.na(dados_full$Fare))] <- median(dados_full$Fare,na.rm=TRUE)

##### Cabin #####
sum(is.na(dados_full$Cabin)) #temos muitos dados faltantes, mas n sei como substitui-los
dados_full$Cabin <- as.factor(dados_full$Cabin)
levels(dados_full$Cabin)
#Os dados das cabines estao extremamente desorganizados, nao vou utiliza-los

##### Embarked #####
sum(is.na(dados_full$Embarked)) #somente dois faltantes, vou coloca-los na maior categoria (S)
dados_full$Embarked[is.na(dados_full$Embarked)] <- "S"
dados_full$Embarked <- as.factor(dados_full$Embarked)
levels(dados_full$Embarked)

print(sapply(dados_full, typeof)) #vamos ver a "cara" dos dados tratados
#aparentemente as variaves que vamos utilizar estao nos tipos adequados

########## Fim da exploracao e tratamento de dados ##########

### Voltando com os dados de treino e teste ###

dados_treino <- dados_full[1:891,]
dados_teste <- dados_full[892:1309,]

##### vou utilizar um general linear model (glm) para classificacao #####
## No primeiro modelo vou adicionar uma interacao entre sexo e idade

mod1 <- glm(Survived ~ Pclass + Sex + Age + Sex*Age + Fare + Embarked + titulo + family, data = dados_treino, family = "binomial")
summary(mod1) 

as.integer(predict(mod1,dados_teste) >.5) #vou utilizar um limiar de 0.5 para classificar

resposta <- data.frame(dados_teste$PassengerId,as.integer(predict(mod1,dados_teste) >.5))
names(resposta) <- c("PassengerId", "Survived")
resposta #temos nossas respostas, vamos exportar para upar no kaggle e ver se o modelo ficou +- bom

write_csv(resposta, "Caminho")
