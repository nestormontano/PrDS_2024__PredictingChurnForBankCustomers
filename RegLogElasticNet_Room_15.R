
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(tidymodels)
library(skimr)
library(factoextra)
library(FactoMineR)
library(rpart)
library(rpart.plot)
library(parallel)
library(doParallel)


# Data --------------------------------------------------------------------
data <- read_csv('Data/Churn_Modelling.csv',
                 col_types = list("n","n","c","n","f","f","n","n","n","i","f","f","n","f"))




# EDA ---------------------------------------------------------------------
novars <- c("RowNumber", "CustomerId", "Surname")#, "HasCrCard", "IsActiveMember"
data <- data %>% select(-one_of(novars))
summary(data)
glimpse(data)
# Credit score
#Medio normal con media=650, unos outliers inferiores y un grupo de clientes con credit score alto
#Se ve inariante el credit score para cada nivel de exited
par(mfrow=c(2,1));hist(data$CreditScore,breaks=30);boxplot(data$CreditScore,horizontal=T);par(mfrow=c(1,1));summary(data$CreditScore)
boxplot(data$CreditScore~data$Exited)

# Geography
barplot(table(data$Geography),las=1)
table(data$Geography);round(prop.table(table(data$Geography))*100,2)






# Chequeo de supuestos para análisis multivariante -------------------------
# Variabilidad: Se observa distribuciones sesgadas, 
boxplot(data[,c(4,5,7)],horizontal = T)
plot(data[,c(4,5,7)])
boxplot(data[,c(6,8)], horizontal = T)
plot(data[data$Balance>20000,c(6,8)])
abc <- hist(data$Balance,plot = F)
abc$breaks;abc$counts

# Correlación: Casi no hay muchas correlaciones
round(cor(data[,c(1,4,5,6,7,8)]),2)


#PCA
datanum <- data %>% select_if(is.numeric)
datanumExi <- data %>% select_if(is.numeric) %>% mutate(Exited=data$Exited)
PCA1 <- PCA(data,quali.sup = c(2,3,8,9,11,12),graph = F)
fviz_pca_biplot(PCA1, repel = TRUE,geom.ind = "point",label = "var",habillage = "Exited")
fviz_pca_biplot(PCA1, repel = TRUE,geom.ind = "point",label = "var",habillage = "IsActiveMember")
fviz_pca_biplot(PCA1, repel = TRUE,geom.ind = "point",label = "var",habillage = "HasCrCard")
fviz_pca_biplot(PCA1, repel = TRUE,geom.ind = "point",label = "var",habillage = "Gender")
fviz_pca_biplot(PCA1, repel = TRUE,geom.ind = "point",label = "var",habillage = "Geography")
fviz_pca_biplot(PCA1, repel = TRUE,geom.ind = "point",label = "var",habillage = "cluster",addEllipses = T)





# Analysis ----------------------------------------------------------------
# Receta de preprocesamiento.
# 1. Elimina variables.
# 2. Crea nuevas variables.
# 3. Imputa valores.
# 4. Haz dummy? Esto no debería ser en la fase de "crear variables"
# 5. Escala los datos en caso de ser necesario.
# 6. Elimina variables por multicolinealidad.


# Ver video para definir bien los pasos.
## 1. Eliminar variables ----
# novars <- c("RowNumber", "CustomerId", "Surname")#, "HasCrCard", "IsActiveMember"
# data <- data %>% select(-one_of(novars))


## 2. Crear variable de cluster ----
datanum <- data %>% select_if(is.numeric)
datanumExi <- data %>% select_if(is.numeric) %>% mutate(Exited=data$Exited)
regtree2 <- rpart(datanumExi$Exited ~., data = datanumExi)
s2 <- rpart.plot(regtree2,extra = 101)

dataclus <- data %>% 
  select_if(is.numeric) %>% 
  scale() %>% 
  kmeans(centers = 5)
data$cluster <- as.factor(dataclus$cluster)
fviz_cluster(dataclus, data = datanum,geom = "point")

d<- dist(datanum,method = "euclidean")
{
par(mfrow=c(2,2))
plot(hclust(d, method = "complete"), cex=0.6, hang = -1, labels=F)# da k=4
plot(hclust(d, method = "ward.D2"), cex=0.6, hang = -1, labels=F)# da k=4
plot(hclust(d, method = "single"), cex=0.6, hang = -1, labels=F)# da k=4
plot(hclust(d, method = "average"), cex=0.6, hang = -1, labels=F)# da k=4
par(mfrow=c(1,1))
}


## 3. Imputa (No hay valores que imputar) ----


## 4. Train - Test ----
set.seed(1234) # Semilla para aleatorios
split <- data %>%
  initial_split(
    prop = 0.75, # Porcentaje al train
    strata = Exited # Estratificación del muestreo
  )

train <- training(split);dim(train)
test <- testing(split);dim(test)


# 5. Balancea datos ----
train %>% 
  mutate(
    ## crear la variable con los pesos
    case_wts = ifelse(Exited == "1", 4, 1),
    ## crea el vector de importancia ponderada
    case_wts = importance_weights(case_wts)
  ) -> train


## 6. Crear receta ----
receta <- train %>%
  recipe(Exited ~ . ) %>% ## Crea la receta
  ## Eliminar variables que no usaremos
  # step_rm() %>%
  ## Crear nuevas variables (insight desde el EDA)
  # step_mutate() %>% 
  ## Imputar los datos
  # step_impute_mean() %>% 
  ## Imputar datos
  # step_impute_knn( all_predictors() ) %>%
  ## Estandarizacion/Normalizacion de numericas
  step_normalize( all_numeric(), -all_outcomes()) %>%
  ## Crear una categoría "otros" que agrupe a categorias pequeñas
  step_other(all_nominal(), -all_outcomes() , threshold = 0.07, other = "otros") %>%
  ## Crear una categoría "new" para observaciones con labels "no muestreados"
  step_novel(all_nominal(), -all_outcomes() , new_level = "new") %>%
  ## Crear variables indicadoras para cada categoría
  step_dummy(all_nominal(), -all_outcomes() ) %>% # Dummy
  ## Eliminar automáticamente variables con alta correlacion
  ## para evitar la multicolinealidad xi ~ xj
  step_corr(all_numeric(), -all_outcomes(), threshold = 0.9) %>%
  ## Tambien podemos eliminar variables con multicolinealidad "a mano"
  # step_rm() %>%
  ## Eliminar columnas con varianza cercana a cero
  step_nzv(all_predictors());receta


#    7.    Training y ajuste de hiperparámetros ----
###  7.1   Definir estrategia de Remuestreo (cross validation) ----
###  7.2   Métricas ----
# Para la combinación de hiperparámetros y para la elección del modelo final
###  7.3   Especificación del Modelo ----
###  7.4   Workflow: Receta, modelo y pesos ----
###  7.5   Afinamiento de hiperparámetros ----
#### 7.5.1 Definir Malla de búsqueda ----
#### 7.5.2 Iniciar Paralelización ----
#### 7.5.3 Entrenar malla de búsqueda en el remuestreo ----
#Aquí se puede repetir la malla de búsqueda y estos primeros 3 pasos.
#### 7.5.3 Entrenar malla de búsqueda en el remuestreo ----
###  7.6   Modelo final ----
#### 7.6.1 Seleccionar la mejor combinación de hiperparámetros ----
#### 7.6.2 Finalizar (darles valores a los parámetros tuneables) el workflow ----
#### 7.6.3 Ahora sí, entrenar el modelo final con los datos----
###  7.7   Evaluación del modelo ----
#Comparación de las métricas en el train y en el test para ver si hubo sobreajuste o no.
#### 7.7.1 Finalizar la paralelización ----




{int1 <- glm(Exited~Age+NumOfProducts,family = "binomial",data = datanumExi)
  summary(int1)
}
