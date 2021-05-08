Titanic kaggle
================
Joaquim Neto
08/05/2021

# Importação dos dados

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 4.0.5

``` r
titanic.test <- read_csv("C:/Users/Rosiane/Desktop/Neto/R anotações/Coisinhas/test.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   PassengerId = col_double(),
    ##   Pclass = col_double(),
    ##   Name = col_character(),
    ##   Sex = col_character(),
    ##   Age = col_double(),
    ##   SibSp = col_double(),
    ##   Parch = col_double(),
    ##   Ticket = col_character(),
    ##   Fare = col_double(),
    ##   Cabin = col_character(),
    ##   Embarked = col_character()
    ## )

``` r
titanic.train <- read_csv("C:/Users/Rosiane/Desktop/Neto/R anotações/Coisinhas/train.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   PassengerId = col_double(),
    ##   Survived = col_double(),
    ##   Pclass = col_double(),
    ##   Name = col_character(),
    ##   Sex = col_character(),
    ##   Age = col_double(),
    ##   SibSp = col_double(),
    ##   Parch = col_double(),
    ##   Ticket = col_character(),
    ##   Fare = col_double(),
    ##   Cabin = col_character(),
    ##   Embarked = col_character()
    ## )

## Visualização do banco de dados

``` r
head(titanic.train)
```

    ## # A tibble: 6 x 12
    ##   PassengerId Survived Pclass Name    Sex     Age SibSp Parch Ticket  Fare Cabin
    ##         <dbl>    <dbl>  <dbl> <chr>   <chr> <dbl> <dbl> <dbl> <chr>  <dbl> <chr>
    ## 1           1        0      3 Braund~ male     22     1     0 A/5 2~  7.25 <NA> 
    ## 2           2        1      1 Cuming~ fema~    38     1     0 PC 17~ 71.3  C85  
    ## 3           3        1      3 Heikki~ fema~    26     0     0 STON/~  7.92 <NA> 
    ## 4           4        1      1 Futrel~ fema~    35     1     0 113803 53.1  C123 
    ## 5           5        0      3 Allen,~ male     35     0     0 373450  8.05 <NA> 
    ## 6           6        0      3 Moran,~ male     NA     0     0 330877  8.46 <NA> 
    ## # ... with 1 more variable: Embarked <chr>

``` r
tail(titanic.train)
```

    ## # A tibble: 6 x 12
    ##   PassengerId Survived Pclass Name    Sex     Age SibSp Parch Ticket  Fare Cabin
    ##         <dbl>    <dbl>  <dbl> <chr>   <chr> <dbl> <dbl> <dbl> <chr>  <dbl> <chr>
    ## 1         886        0      3 "Rice,~ fema~    39     0     5 382652 29.1  <NA> 
    ## 2         887        0      2 "Montv~ male     27     0     0 211536 13    <NA> 
    ## 3         888        1      1 "Graha~ fema~    19     0     0 112053 30    B42  
    ## 4         889        0      3 "Johns~ fema~    NA     1     2 W./C.~ 23.4  <NA> 
    ## 5         890        1      1 "Behr,~ male     26     0     0 111369 30    C148 
    ## 6         891        0      3 "Doole~ male     32     0     0 370376  7.75 <NA> 
    ## # ... with 1 more variable: Embarked <chr>

``` r
summary(titanic.train)
```

    ##   PassengerId       Survived          Pclass          Name          
    ##  Min.   :  1.0   Min.   :0.0000   Min.   :1.000   Length:891        
    ##  1st Qu.:223.5   1st Qu.:0.0000   1st Qu.:2.000   Class :character  
    ##  Median :446.0   Median :0.0000   Median :3.000   Mode  :character  
    ##  Mean   :446.0   Mean   :0.3838   Mean   :2.309                     
    ##  3rd Qu.:668.5   3rd Qu.:1.0000   3rd Qu.:3.000                     
    ##  Max.   :891.0   Max.   :1.0000   Max.   :3.000                     
    ##                                                                     
    ##      Sex                 Age            SibSp           Parch       
    ##  Length:891         Min.   : 0.42   Min.   :0.000   Min.   :0.0000  
    ##  Class :character   1st Qu.:20.12   1st Qu.:0.000   1st Qu.:0.0000  
    ##  Mode  :character   Median :28.00   Median :0.000   Median :0.0000  
    ##                     Mean   :29.70   Mean   :0.523   Mean   :0.3816  
    ##                     3rd Qu.:38.00   3rd Qu.:1.000   3rd Qu.:0.0000  
    ##                     Max.   :80.00   Max.   :8.000   Max.   :6.0000  
    ##                     NA's   :177                                     
    ##     Ticket               Fare           Cabin             Embarked        
    ##  Length:891         Min.   :  0.00   Length:891         Length:891        
    ##  Class :character   1st Qu.:  7.91   Class :character   Class :character  
    ##  Mode  :character   Median : 14.45   Mode  :character   Mode  :character  
    ##                     Mean   : 32.20                                        
    ##                     3rd Qu.: 31.00                                        
    ##                     Max.   :512.33                                        
    ## 

``` r
str(titanic.train)
```

    ## spec_tbl_df [891 x 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ PassengerId: num [1:891] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Survived   : num [1:891] 0 1 1 1 0 0 0 0 1 1 ...
    ##  $ Pclass     : num [1:891] 3 1 3 1 3 3 1 3 3 2 ...
    ##  $ Name       : chr [1:891] "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
    ##  $ Sex        : chr [1:891] "male" "female" "female" "female" ...
    ##  $ Age        : num [1:891] 22 38 26 35 35 NA 54 2 27 14 ...
    ##  $ SibSp      : num [1:891] 1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch      : num [1:891] 0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Ticket     : chr [1:891] "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
    ##  $ Fare       : num [1:891] 7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Cabin      : chr [1:891] NA "C85" NA "C123" ...
    ##  $ Embarked   : chr [1:891] "S" "C" "S" "S" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   PassengerId = col_double(),
    ##   ..   Survived = col_double(),
    ##   ..   Pclass = col_double(),
    ##   ..   Name = col_character(),
    ##   ..   Sex = col_character(),
    ##   ..   Age = col_double(),
    ##   ..   SibSp = col_double(),
    ##   ..   Parch = col_double(),
    ##   ..   Ticket = col_character(),
    ##   ..   Fare = col_double(),
    ##   ..   Cabin = col_character(),
    ##   ..   Embarked = col_character()
    ##   .. )

``` r
head(titanic.test)
```

    ## # A tibble: 6 x 11
    ##   PassengerId Pclass Name    Sex     Age SibSp Parch Ticket  Fare Cabin Embarked
    ##         <dbl>  <dbl> <chr>   <chr> <dbl> <dbl> <dbl> <chr>  <dbl> <chr> <chr>   
    ## 1         892      3 Kelly,~ male   34.5     0     0 330911  7.83 <NA>  Q       
    ## 2         893      3 Wilkes~ fema~  47       1     0 363272  7    <NA>  S       
    ## 3         894      2 Myles,~ male   62       0     0 240276  9.69 <NA>  Q       
    ## 4         895      3 Wirz, ~ male   27       0     0 315154  8.66 <NA>  S       
    ## 5         896      3 Hirvon~ fema~  22       1     1 31012~ 12.3  <NA>  S       
    ## 6         897      3 Svenss~ male   14       0     0 7538    9.22 <NA>  S

``` r
tail(titanic.test)
```

    ## # A tibble: 6 x 11
    ##   PassengerId Pclass Name   Sex     Age SibSp Parch Ticket   Fare Cabin Embarked
    ##         <dbl>  <dbl> <chr>  <chr> <dbl> <dbl> <dbl> <chr>   <dbl> <chr> <chr>   
    ## 1        1304      3 Henri~ fema~  28       0     0 347086   7.78 <NA>  S       
    ## 2        1305      3 Spect~ male   NA       0     0 A.5. ~   8.05 <NA>  S       
    ## 3        1306      1 Oliva~ fema~  39       0     0 PC 17~ 109.   C105  C       
    ## 4        1307      3 Saeth~ male   38.5     0     0 SOTON~   7.25 <NA>  S       
    ## 5        1308      3 Ware,~ male   NA       0     0 359309   8.05 <NA>  S       
    ## 6        1309      3 Peter~ male   NA       1     1 2668    22.4  <NA>  C

``` r
summary(titanic.test)
```

    ##   PassengerId         Pclass          Name               Sex           
    ##  Min.   : 892.0   Min.   :1.000   Length:418         Length:418        
    ##  1st Qu.: 996.2   1st Qu.:1.000   Class :character   Class :character  
    ##  Median :1100.5   Median :3.000   Mode  :character   Mode  :character  
    ##  Mean   :1100.5   Mean   :2.266                                        
    ##  3rd Qu.:1204.8   3rd Qu.:3.000                                        
    ##  Max.   :1309.0   Max.   :3.000                                        
    ##                                                                        
    ##       Age            SibSp            Parch           Ticket         
    ##  Min.   : 0.17   Min.   :0.0000   Min.   :0.0000   Length:418        
    ##  1st Qu.:21.00   1st Qu.:0.0000   1st Qu.:0.0000   Class :character  
    ##  Median :27.00   Median :0.0000   Median :0.0000   Mode  :character  
    ##  Mean   :30.27   Mean   :0.4474   Mean   :0.3923                     
    ##  3rd Qu.:39.00   3rd Qu.:1.0000   3rd Qu.:0.0000                     
    ##  Max.   :76.00   Max.   :8.0000   Max.   :9.0000                     
    ##  NA's   :86                                                          
    ##       Fare            Cabin             Embarked        
    ##  Min.   :  0.000   Length:418         Length:418        
    ##  1st Qu.:  7.896   Class :character   Class :character  
    ##  Median : 14.454   Mode  :character   Mode  :character  
    ##  Mean   : 35.627                                        
    ##  3rd Qu.: 31.500                                        
    ##  Max.   :512.329                                        
    ##  NA's   :1

``` r
str(titanic.test)
```

    ## spec_tbl_df [418 x 11] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ PassengerId: num [1:418] 892 893 894 895 896 897 898 899 900 901 ...
    ##  $ Pclass     : num [1:418] 3 3 2 3 3 3 3 2 3 3 ...
    ##  $ Name       : chr [1:418] "Kelly, Mr. James" "Wilkes, Mrs. James (Ellen Needs)" "Myles, Mr. Thomas Francis" "Wirz, Mr. Albert" ...
    ##  $ Sex        : chr [1:418] "male" "female" "male" "male" ...
    ##  $ Age        : num [1:418] 34.5 47 62 27 22 14 30 26 18 21 ...
    ##  $ SibSp      : num [1:418] 0 1 0 0 1 0 0 1 0 2 ...
    ##  $ Parch      : num [1:418] 0 0 0 0 1 0 0 1 0 0 ...
    ##  $ Ticket     : chr [1:418] "330911" "363272" "240276" "315154" ...
    ##  $ Fare       : num [1:418] 7.83 7 9.69 8.66 12.29 ...
    ##  $ Cabin      : chr [1:418] NA NA NA NA ...
    ##  $ Embarked   : chr [1:418] "Q" "S" "Q" "S" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   PassengerId = col_double(),
    ##   ..   Pclass = col_double(),
    ##   ..   Name = col_character(),
    ##   ..   Sex = col_character(),
    ##   ..   Age = col_double(),
    ##   ..   SibSp = col_double(),
    ##   ..   Parch = col_double(),
    ##   ..   Ticket = col_character(),
    ##   ..   Fare = col_double(),
    ##   ..   Cabin = col_character(),
    ##   ..   Embarked = col_character()
    ##   .. )

# Limpando os dados

## Primeiro vamos juntar os datasets

``` r
titanic.train$IsTrainSet <- TRUE   #coluna para sabermos que é de treino
titanic.test$IsTrainSet <- FALSE
```

Vamos ver se ambos possuem as mesmas colunas

``` r
names(titanic.test) #nao tem a coluna "Survived"
```

    ##  [1] "PassengerId" "Pclass"      "Name"        "Sex"         "Age"        
    ##  [6] "SibSp"       "Parch"       "Ticket"      "Fare"        "Cabin"      
    ## [11] "Embarked"    "IsTrainSet"

``` r
names(titanic.train)
```

    ##  [1] "PassengerId" "Survived"    "Pclass"      "Name"        "Sex"        
    ##  [6] "Age"         "SibSp"       "Parch"       "Ticket"      "Fare"       
    ## [11] "Cabin"       "Embarked"    "IsTrainSet"

Vamos adicionar a coluna survied aos dados teste

``` r
titanic.test$Survived <- NA
```

Agora que a formatação de ambos é a mesma, podemos juntá-los

``` r
titanic.full <- rbind(titanic.train, titanic.test)

head(titanic.full)
```

    ## # A tibble: 6 x 13
    ##   PassengerId Survived Pclass Name    Sex     Age SibSp Parch Ticket  Fare Cabin
    ##         <dbl>    <dbl>  <dbl> <chr>   <chr> <dbl> <dbl> <dbl> <chr>  <dbl> <chr>
    ## 1           1        0      3 Braund~ male     22     1     0 A/5 2~  7.25 <NA> 
    ## 2           2        1      1 Cuming~ fema~    38     1     0 PC 17~ 71.3  C85  
    ## 3           3        1      3 Heikki~ fema~    26     0     0 STON/~  7.92 <NA> 
    ## 4           4        1      1 Futrel~ fema~    35     1     0 113803 53.1  C123 
    ## 5           5        0      3 Allen,~ male     35     0     0 373450  8.05 <NA> 
    ## 6           6        0      3 Moran,~ male     NA     0     0 330877  8.46 <NA> 
    ## # ... with 2 more variables: Embarked <chr>, IsTrainSet <lgl>

``` r
table(titanic.full$IsTrainSet)
```

    ## 
    ## FALSE  TRUE 
    ##   418   891

## Agora vamos dar uma olhada nos valores faltantes

``` r
table(titanic.full$Pclass)
```

    ## 
    ##   1   2   3 
    ## 323 277 709

``` r
1309 - (323+277+709)
```

    ## [1] 0

``` r
table(titanic.full$Embarked)
```

    ## 
    ##   C   Q   S 
    ## 270 123 914

``` r
1309 - (270+123+914) #dois valores faltantes
```

    ## [1] 2

``` r
library(tidyr)
```

    ## Warning: package 'tidyr' was built under R version 4.0.5

``` r
titanic.full$Embarked = replace_na(titanic.full$Embarked, "S") #Método q achei

table(is.na(titanic.full$Age)) #263 NA em idade
```

    ## 
    ## FALSE  TRUE 
    ##  1046   263

``` r
titanic.full$Age = replace_na(titanic.full$Age, median(titanic.full$Age, na.rm = T)) #Vlw tidyR

table(is.na(titanic.full$Fare)) #1 valor faltante
```

    ## 
    ## FALSE  TRUE 
    ##  1308     1

``` r
titanic.full$Fare = replace_na(titanic.full$Fare, median(titanic.full$Fare, na.rm = T)) #substituido pela mediana
```

Vamos mudar algumas coisas pra factor pra facilitar nossa vida

``` r
str(titanic.full)
```

    ## spec_tbl_df [1,309 x 13] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ PassengerId: num [1:1309] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Survived   : num [1:1309] 0 1 1 1 0 0 0 0 1 1 ...
    ##  $ Pclass     : num [1:1309] 3 1 3 1 3 3 1 3 3 2 ...
    ##  $ Name       : chr [1:1309] "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
    ##  $ Sex        : chr [1:1309] "male" "female" "female" "female" ...
    ##  $ Age        : num [1:1309] 22 38 26 35 35 28 54 2 27 14 ...
    ##  $ SibSp      : num [1:1309] 1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch      : num [1:1309] 0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Ticket     : chr [1:1309] "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
    ##  $ Fare       : num [1:1309] 7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Cabin      : chr [1:1309] NA "C85" NA "C123" ...
    ##  $ Embarked   : chr [1:1309] "S" "C" "S" "S" ...
    ##  $ IsTrainSet : logi [1:1309] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   PassengerId = col_double(),
    ##   ..   Survived = col_double(),
    ##   ..   Pclass = col_double(),
    ##   ..   Name = col_character(),
    ##   ..   Sex = col_character(),
    ##   ..   Age = col_double(),
    ##   ..   SibSp = col_double(),
    ##   ..   Parch = col_double(),
    ##   ..   Ticket = col_character(),
    ##   ..   Fare = col_double(),
    ##   ..   Cabin = col_character(),
    ##   ..   Embarked = col_character()
    ##   .. )

``` r
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
```

# Vamos separar os dados novamente

``` r
titanic.train = titanic.full[titanic.full$IsTrainSet == "TRUE", ]
titanic.test = titanic.full[titanic.full$IsTrainSet == "FALSE", ]
```

Vamos colocar a coluna “Survived” como factor nos dados de treino

``` r
titanic.train$Survived <- as.factor(titanic.train$Survived)
```

# Agora vamos fazer o modelo Random Forest

``` r
library("randomForest")
```

    ## Warning: package 'randomForest' was built under R version 4.0.5

``` r
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

survived.formula <- as.formula(survived.equation)


titanic.model = randomForest(formula = survived.formula,
                             data = titanic.train,
                             ntree = 500,
                             mtry = 3,
                             nodesize = 0.01*nrow(titanic.train))

Sobreviveu = predict(titanic.model, newdata = titanic.test)
```

# Fazendo a pasta com os dados gerados

``` r
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)

output.df$Survived <- Sobreviveu

write.csv(output.df,"titanic_kaggle_submission.csv",row.names = FALSE)
```
