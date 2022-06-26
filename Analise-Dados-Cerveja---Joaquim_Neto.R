---
title: "Dados cerveja"
author: "Joaquim Neto"
date: "21/04/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


## Importação dos dados


```{r}
library(readr)
consumo_cerveja <- read_delim("C:/Users/Usuario/Downloads/consumo_cerveja.csv", 
    ";", escape_double = FALSE, col_types = cols(Data = col_date(format = "%d/%m/%Y"), 
        `Final de Semana` = col_factor(levels = c("0", 
            "1"))), locale = locale(decimal_mark = ","), 
    trim_ws = TRUE)
View(consumo_cerveja)

#usamos o pacote readr para importarmos os dados de maneira correta (datas no formato "Date", categorias como "factor" e NA como dados ausentes)
```                   


## Análise descritiva dos dados


```{r}
library(dplyr)
library(tidyr)
library(ggplot2)

attach(consumo_cerveja)

summary(consumo_cerveja) #análise unidimensional geral de todas as variáveis da tabela

sd(consumo_cerveja$`Consumo de cerveja (litros)`) #desvio padrao da qtd de cerveja consumida

plot(`Consumo de cerveja (litros)` ~ `Final de Semana`) #análise bivariada simples, consumo de cerveja nos fins de semana e nos dias de semana

plot(`Consumo de cerveja (litros)` ~ `Temperatura Media (C)`)
abline(lm(`Consumo de cerveja (litros)` ~ `Temperatura Media (C)`),col='red')

#análise do consumo de cerveja pela temperatura média com a presença de um outlier que provavelmente foi um erro de digitação pela temperatura extremamente alta
```


## Gráficos pertinentes


```{r}
ggplot(consumo_cerveja, aes(x = `Precipitacao (mm)`, y = `Consumo de cerveja (litros)`)) + geom_point(color = 'blue') + ggtitle("Consumo de cerveja em relação à precipitação") + theme (plot.title = element_text(hjust= 0.5)) #consumo de cerveja em relação à precipitação


`Temperatura Media (C)`[17] = 21.3
`Temperatura Media (C)`[78] = 21.3 #removendo o outlier e o NA


ggplot(consumo_cerveja, aes(x = `Consumo de cerveja (litros)`, color = `Final de Semana`)) + geom_histogram(fill = "grey", alpha = 0.4, position = "identity") + ggtitle("Consumo de cerveja em dias de semana e em finais de semana") #consumo de cerveja em dias de semana e finais de semana
```


## Análise de regressão / Testes de hipóteses


```{r}
modelo1 <- lm(`Consumo de cerveja (litros)`~`Temperatura Media (C)`) #fazendo o modelo linear para as variáveis desejadas
plot(`Consumo de cerveja (litros)`~`Temperatura Media (C)`)
abline(modelo1, col = 'red') #plotando a regressão linear no modelo

cor(`Temperatura Media (C)`, `Consumo de cerveja (litros)`) #observando a correlação entre as variáveis

summary(modelo1) #sumario dos erros, estimativa do ponto de interceptação e da curva, estatística de teste e valor-p para o teste de hipoteses de que o graf passa por (0,0), tbm mostra o valor-p para o teste de que a inclinação é 0. Também podemos ver um R^2 de 0.31, o que indica que apenas 31% da variação é explicada pelo modelo.

plot(modelo1) #no segundo grafico podemos checar a normalidade do modelo
plot(modelo1$residuals) #os resíduos tbm são bem randomizados

modelo2 <- lm(`Consumo de cerveja (litros)`~`Final de Semana`)
plot(`Consumo de cerveja (litros)`~`Final de Semana`)
t.test(`Consumo de cerveja (litros)`~`Final de Semana`) #testando a hipotese de que as medias dos dois grupos são iguais (p-valor mt baixo, portanto n aceitamos a hipotese)
```
