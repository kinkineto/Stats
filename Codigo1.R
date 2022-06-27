# PassengerId - A unique Id for each passenger. Each Id takes the form gggg_pp where gggg indicates a group the passenger is travelling with 
#and pp is their number within the group. People in a group are often family members, but not always.
# HomePlanet - The planet the passenger departed from, typically their planet of permanent residence.
# CryoSleep - Indicates whether the passenger elected to be put into suspended animation for the duration of the voyage. Passengers in
#cryosleep are confined to their cabins.
# Cabin - The cabin number where the passenger is staying. Takes the form deck/num/side, where side can be either P for Port or S for
#Starboard.
# Destination - The planet the passenger will be debarking to.
# Age - The age of the passenger.
# VIP - Whether the passenger has paid for special VIP service during the voyage.
# RoomService, FoodCourt, ShoppingMall, Spa, VRDeck - Amount the passenger has billed at each of the Spaceship Titanic's many luxury amenities.
# Name - The first and last names of the passenger.
# Transported - Whether the passenger was transported to another dimension. This is the target, the column you are trying to predict.

library(tidyverse)
library(gender)

train <- read_csv("C:/Users/kinki/OneDrive/Estudos/Kaggle/Spaceship Titanic/train.csv")
test <- read_csv("C:/Users/kinki/OneDrive/Estudos/Kaggle/Spaceship Titanic/test.csv")
test$Transported <- NA #adicionei a coluna transported ao banco de testes

dadosfull <- bind_rows(train,test) #banco completo para tratar ambos os dados ao mesmo tempo

### PassengerId ###
sum(is.na(dadosfull$PassengerId)) #Sem dados faltantes
#Como essa variavel indica tambem um grupo, vou criar a variavel grupo que deve ser mais util


### Group ###
dadosfull$group <- str_extract(dadosfull$PassengerId, "^([^_])+")

### HomePlanet ###
head(dadosfull$HomePlanet)
sum(is.na(dadosfull$HomePlanet)) #288 dados faltantes
##### Tratando dados faltantes #####

dadosfull$HomePlanet <- as.factor(dadosfull$HomePlanet) #Transformamos a variavel em categorica
levels(dadosfull$HomePlanet)
summary(dadosfull$HomePlanet)


### CryoSleep ###
head(dadosfull$CryoSleep)
sum(is.na(dadosfull$CryoSleep)) #310 dados faltantes
##### Tratando dados faltantes #####

dadosfull$CryoSleep <- as.factor(as.integer(dadosfull$CryoSleep))
levels(dadosfull$CryoSleep)
summary(dadosfull$CryoSleep)


##### Cabin #####
#Aqui temos 3 variaveis em uma, vou criar uma variavel deck e uma variavel side para o deck e o lado que a pessoa esta deck/num/lado
sum(is.na(dadosfull$Cabin)) #299 dados faltantes


##### Deck #####
dadosfull$Deck <- str_extract(dadosfull$Cabin, "^(.)")
dadosfull$Deck <- as.factor(dadosfull$Deck)
summary(dadosfull$Deck)


##### Side #####
dadosfull$Side <- str_extract(dadosfull$Cabin, "(.)$")
dadosfull$Side <- as.factor(dadosfull$Side)
summary(dadosfull$Side)


##### Destination #####
sum(is.na(dadosfull$Destination)) #274 dados faltantes
##### Tratando dados faltantes #####

head(dadosfull$Destination)
dadosfull$Destination <- as.factor(dadosfull$Destination)
levels(dadosfull$Destination)
summary(dadosfull$Destination)


##### Age #####
sum(is.na(dadosfull$Age)) #270 dados faltantes
##### Tratando dados faltantes #####
dadosfull$Age[is.na(dadosfull$Age)] <- round(mean(dadosfull$Age, na.rm = T))
summary(dadosfull$Age)

##### VIP #####
head(dadosfull$VIP)
dadosfull$VIP <- as.factor(as.integer(dadosfull$VIP))
summary(dadosfull$VIP)
sum(is.na(dadosfull$VIP)) #296 dados faltantes


##### RoomService #####
sum(is.na(dadosfull$RoomService)) #263 dados faltantes
##### Tratando dados faltantes #####
dadosfull$RoomService[is.na(dadosfull$RoomService)] <- median(dadosfull$RoomService, na.rm = T)


##### FoodCourt #####
sum(is.na(dadosfull$FoodCourt)) #289 dados faltantes
##### Tratando dados faltantes #####
dadosfull$FoodCourt[is.na(dadosfull$FoodCourt)] <- median(dadosfull$FoodCourt, na.rm = T)


##### ShoppingMall #####
sum(is.na(dadosfull$ShoppingMall)) #306 dados faltantes
##### Tratando dados faltantes #####
dadosfull$ShoppingMall[is.na(dadosfull$ShoppingMall)] <- median(dadosfull$ShoppingMall, na.rm = T)


##### Spa #####
sum(is.na(dadosfull$Spa)) #284 dados faltantes
##### Tratando dados faltantes #####
dadosfull$Spa[is.na(dadosfull$Spa)] <- median(dadosfull$Spa, na.rm = T)


##### VRDeck #####
sum(is.na(dadosfull$VRDeck)) #268 dados faltantes
##### Tratando dados faltantes #####
dadosfull$VRDeck[is.na(dadosfull$VRDeck)] <- median(dadosfull$VRDeck, na.rm = T)

##### Total #####
#Criei uma variavel para o gasto total dentro da nave
total <- dadosfull$VRDeck + dadosfull$Spa + dadosfull$ShoppingMall + dadosfull$FoodCourt + dadosfull$RoomService
dadosfull$total <- total

##### Name #####
#Nao temos a variavel sexo e acho que podemos criar essa variavel atraves do nome
# firstName <- str_extract(dadosfull$Name, "^([^ ])+")
# dadosfull$firstName <- firstName
# sum(is.na(firstName)) #Temos 294 pessoas sem nome
# gender <- gender(dadosfull$firstName)
# 
# ##### Gender #####
# 
# gender$proportion_male <- NULL
# gender$proportion_female <- NULL
# gender$year_min <- NULL
# gender$year_max <- NULL
# names(gender) <- c("firstName","gender")
# names(gender)
# dadosfull <- inner_join(dadosfull,gender,by = "firstName")
# dadosfull$gender <- as.factor(dadosfull$gender)
# levels(dadosfull$gender)

##### Transported #####
#Variavel resposta, nao deve ser modificada


########## Visualizacao rapida ##########

plot(dadosfull$HomePlanet,dadosfull$gender)
plot(dadosfull$gender)

########## Modelo ##########
dados_treino <- dadosfull[1:8693,]
dados_teste <- dadosfull[8694:12970,]

modelo1 <- glm(Transported ~ HomePlanet + CryoSleep + Destination +
                 Age + VIP + total + Deck + Side,data = dados_treino, family = "binomial")
summary(modelo1)

as.integer(predict(modelo1,dados_teste) >.5) #vou utilizar um limiar de 0.5 para classificar
sum(is.na(as.integer(predict(modelo1,dados_teste) >.5)))

resposta <- data.frame(dados_teste$PassengerId,as.integer(predict(modelo1,dados_teste) >.5))
names(resposta) <- c("PassengerId", "Transported")
resposta
resposta$Transported[is.na(resposta$Transported)] <- 0
resposta$Transported <- str_replace(resposta$Transported, "0", "False")
resposta$Transported <- str_replace(resposta$Transported, "1", "True")
resposta

write_csv(resposta, "C:\\Users\\kinki\\OneDrive\\Estudos\\Kaggle\\Spaceship Titanic\\resposta1.csv")
