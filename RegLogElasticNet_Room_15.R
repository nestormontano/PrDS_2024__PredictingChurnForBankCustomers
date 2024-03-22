
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
library(glmnet)
library(vip)



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



hist(data$Balance,breaks = 30)


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


# 1. Eliminar variables ----
# novars <- c("RowNumber", "CustomerId", "Surname")#, "HasCrCard", "IsActiveMember"
# data <- data %>% select(-one_of(novars))


# 2. Crear variable de cluster -------------------------------------------
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


# 3. Imputa (No hay valores que imputar) ---------------------------------


# 4. Train - Test --------------------------------------------------------
set.seed(1234) # Semilla para aleatorios
split <- data %>%
  initial_split(
    prop = 0.75, # Porcentaje al train
    strata = Exited # Estratificación del muestreo
  )

train <- training(split);dim(train)
test <- testing(split);dim(test)


# 5. Balancea datos ------------------------------------------------
train %>% 
  mutate(
    ## crear la variable con los pesos
    case_wts = ifelse(Exited == "1", 4, 1),
    ## crea el vector de importancia ponderada
      case_wts = importance_weights(case_wts)
  ) -> train


# 6. Crear receta ----
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


#    7.    Training y ajuste de hiperparámetros -----------------------
###  7.1   Definir estrategia de Remuestreo (cross validation) ----
set.seed(1234)
cv <- vfold_cv(train, v = 5, repeats = 2, strata = Exited)
cv


###  7.2   Métricas ----
# Para la combinación de hiperparámetros y para la elección del modelo final
metricas <- metric_set(accuracy, sens, spec, bal_accuracy, precision)
metricas
# La métrica estratégica para este proyecto es la sensibilidad,
# en otras palabras, la proporción de verdaderos positivos,
# o la efectividad del modelo para detectar los clientes que
# abandonan la empresa, de entre todos aquellos que realmente lo hacen.


###  7.3   Especificación del Modelo ----
reglog_elasnet <- logistic_reg(penalty = tune(), mixture = tune()) %>% 
                      set_engine("glmnet") %>% 
                      translate() #Tiene un solo modo:Clasificar - Entonces, family="binomial"?


###  7.4   Workflow: Receta, modelo y pesos ----
rlen_wflow <-
    workflow() %>%
    add_recipe(receta) %>%
    add_model(reglog_elasnet) %>%
    add_case_weights(case_wts) ## Aquí agregamos los pesos


###  7.5   Afinamiento de hiperparámetros ----
#### 7.5.1 Definir Malla de búsqueda ----
set.seed(123)
rlen_grid <- reglog_elasnet %>%
  ## preguntamos los parametros tuneables del modelo
  parameters() %>%
  ## Vamos a definir un rango para el penalty(regularizacion) y mixture(alpha)
  update(penalty= penalty( range= c(0, 1) ), # Penalty(lambda) es la cantidad de regularización (shrinkage)
         mixture= mixture( range= c(0, 1)) ) %>% #Mixture es el parámetro alpha que indica si es 1=Lasso, 0=Ridge, o ElasticNet.
  grid_latin_hypercube(size = 10)


#### 7.5.2 Iniciar Paralelización ----
parallel::detectCores(logical=FALSE)
cl <- makePSOCKcluster(4) #El número detectado
registerDoParallel(cl)
#parallel::stopCluster(cl) ## Esto se debe ejecutar al final


#### 7.5.3 Entrenar malla de búsqueda en el remuestreo ----
set.seed(123)
rlen_tuned <- tune_grid(
  rlen_wflow, ## Workflow: Receta, modelo y case_weights
  resamples= cv, ## Crossvalidation
  grid = rlen_grid, ## Malla de Busqueda
  metrics = metricas, ## Metricas
  control= control_grid(allow_par = T, save_pred = T) ## Paralel y Pred
)
rlen_tuned

show_best(rlen_tuned, metric = "accuracy", n = 10)
show_best(rlen_tuned, metric = "sens", n = 10)
show_best(rlen_tuned, metric = "spec", n = 10)
show_best(rlen_tuned, metric = "precision", n = 10)
show_best(rlen_tuned, metric = "bal_accuracy", n = 10)


#De esta primera malla se vio que los valores de penalty son grandes y se que quiere que sean más pequeños.


#.....................................................................
#Aquí se puede repetir la malla de búsqueda y estos primeros 3 pasos.
# Segunda malla ajustando penalty(regularizacion) en valores pequeños.
{
set.seed(123)
rlen_grid2 <- reglog_elasnet %>%
  ## preguntamos los parametros tuneables del modelo
  parameters() %>%
  ## Vamos a definir un rango para el penalty(regularizacion) y mixture(alpha)
  update(penalty= penalty( range= c(0.01, 0.2) ), # Penalty es la cantidad de regularización (shrinkage)
         mixture= mixture( range= c(0, 1)) ) %>% #Mixture es el parámetro alpha que indica si es 1=Lasso, 0=Ridge, o ElasticNet.
  grid_latin_hypercube(size = 10)

#### 7.5.3.1 Entrenar malla 2 de búsqueda en el remuestreo ----
set.seed(123)
rlen_tuned2 <- tune_grid(
  rlen_wflow, ## Workflow: Receta, modelo y case_weights
  resamples= cv, ## Crossvalidation
  grid = rlen_grid2, ## Malla de Busqueda
  metrics = metricas, ## Metricas
  control= control_grid(allow_par = T, save_pred = T) ## Paralel y Pred
)

show_best(rlen_tuned2, metric = "accuracy", n = 10)
show_best(rlen_tuned2, metric = "sens", n = 10)
show_best(rlen_tuned2, metric = "spec", n = 10)
#De esta segunda malla se observa que los valores de mixture más pequeños tienen mejor rendimiento. Se combinará tanto con valores de penalty grandes y pequeños.
}

#.....................................................................
# Tercera malla ajustando mixture(alpha:LASSO-Ridge) en valores pequeños y penalty en valores grandes.
{
set.seed(123)
rlen_grid3 <- reglog_elasnet %>%
  ## preguntamos los parametros tuneables del modelo
  parameters() %>%
  ## Vamos a definir un rango para el penalty(regularizacion) y mixture(alpha)
  update(penalty= penalty( range= c(0, 1) ), # Penalty es la cantidad de regularización (shrinkage)
         mixture= mixture( range= c(0.01, 0.3)) ) %>% #Mixture es el parámetro alpha que indica si es 1=Lasso, 0=Ridge, o ElasticNet.
  grid_latin_hypercube(size = 10)

#### 7.5.3.2 Entrenar malla 3 de búsqueda en el remuestreo ----
set.seed(123)
rlen_tuned3 <- tune_grid(
  rlen_wflow, ## Workflow: Receta, modelo y case_weights
  resamples= cv, ## Crossvalidation
  grid = rlen_grid3, ## Malla de Busqueda
  metrics = metricas, ## Metricas
  control= control_grid(allow_par = T, save_pred = T) ## Paralel y Pred
)

show_best(rlen_tuned3, metric = "accuracy", n = 10)
show_best(rlen_tuned3, metric = "sens", n = 10)
show_best(rlen_tuned3, metric = "spec", n = 10)
#De esta tercera malla, se ve bastante bien que alcanzo un buen accuracy y una mejor sensibilidad, además afortunadamente también salió buena la especificidad
}

#.....................................................................
# Cuarta malla ajustando mixture(alpha:LASSO-Ridge) en valores pequeños y penalty en valores pequeños.
{
set.seed(123)
rlen_grid4 <- reglog_elasnet %>%
  ## preguntamos los parametros tuneables del modelo
  parameters() %>%
  ## Vamos a definir un rango para el penalty(regularizacion) y mixture(alpha)
  update(penalty= penalty( range= c(0.01, 0.2) ), # Penalty es la cantidad de regularización (shrinkage)
         mixture= mixture( range= c(0.01, 0.3)) ) %>% #Mixture es el parámetro alpha que indica si es 1=Lasso, 0=Ridge, o ElasticNet.
  grid_latin_hypercube(size = 10)

#### 7.5.3.3 Entrenar malla 4 de búsqueda en el remuestreo ----
set.seed(123)
rlen_tuned4 <- tune_grid(
  rlen_wflow, ## Workflow: Receta, modelo y case_weights
  resamples= cv, ## Crossvalidation
  grid = rlen_grid4, ## Malla de Busqueda
  metrics = metricas, ## Metricas
  control= control_grid(allow_par = T, save_pred = T) ## Paralel y Pred
)

show_best(rlen_tuned4, metric = "accuracy", n = 10)
show_best(rlen_tuned4, metric = "sens", n = 10)
show_best(rlen_tuned4, metric = "spec", n = 10)
#Aparentemente en esta cuarta malla no se mejoró la búsqueda y en la tercera malla fue mejor, obteniendo la sensibilidad más alta hasta el momento.
}


#.....................................................................
# Quinta malla ajustando tendiendo a que sea una reg LASSO y penalty valores variados (grandes).
{
set.seed(123)
rlen_grid5 <- reglog_elasnet %>%
  ## preguntamos los parametros tuneables del modelo
  parameters() %>%
  ## Vamos a definir un rango para el penalty(regularizacion) y mixture(alpha)
  update(penalty= penalty( range= c(0, 1) ), # Penalty es la cantidad de regularización (shrinkage)
         mixture= mixture( range= c(0.8, 1)) ) %>% #Mixture es el parámetro alpha que indica si es 1=Lasso, 0=Ridge, o ElasticNet.
  grid_latin_hypercube(size = 10)

#### 7.5.3.3 Entrenar malla 5 de búsqueda en el remuestreo ----
set.seed(123)
rlen_tuned5 <- tune_grid(
  rlen_wflow, ## Workflow: Receta, modelo y case_weights
  resamples= cv, ## Crossvalidation
  grid = rlen_grid5, ## Malla de Busqueda
  metrics = metricas, ## Metricas
  control= control_grid(allow_par = T, save_pred = T) ## Paralel y Pred
)


show_best(rlen_tuned5, metric = "accuracy", n = 10)
show_best(rlen_tuned5, metric = "sens", n = 10)
show_best(rlen_tuned5, metric = "spec", n = 10)
#En esta quinta malla se descarta la idea de que el modelo pueda tender más a ser de tipo LASSO y que deseche ciertas variables
#porque los valores alpha mejores resultados han dado cuando tienden a cero, como en la malla dos, tres y cuatro.
#Si no descarto variables, y me quedo con todas aunque regularizadas, valdrías revisar si hay multicolinealidad, antes de hacer una sexta malla
# Lo de la multicolinealidad se elimina QUITANDO la variable cluster.
#Además aquí SE RECUERDA que:
# 1. Me permito bajar mi accuracy, para elevar mi sensibilidad.
# 2. Pero, tampoco voy a elevar mucho los predichos positivos y que estén mal, entonces también debería considerar "precision=TP/(TP+FP)"
# De esa manera elevo sensibilidad sin afectar tanto la precisión.
# Sin embargo, como la data no está balanceada, la métrica de precision podría no reflejar correctamente lo que se espera.
}

#.....................................................................
# Sexta malla buscando mixture(alpha) en valores pequeños(Ridge) y explorando con penalty.

set.seed(123)
rlen_grid6 <- reglog_elasnet %>%
  ## preguntamos los parametros tuneables del modelo
  parameters() %>%
  ## Vamos a definir un rango para el penalty(regularizacion) y mixture(alpha)
  update(penalty= penalty( range= c(0, 1) ), # Penalty es la cantidad de regularización (shrinkage)
         mixture= mixture( range= c(0, 1)) ) %>% #Mixture es el parámetro alpha que indica si es 1=Lasso, 0=Ridge, o ElasticNet.
  grid_latin_hypercube(size = 200)
#Elevamos la cantidad de divisiones en grid_latin_hypercube de tal manera puedo obtener más detalle en espacios pequeños que pueden elevar las métricas.

#### 7.5.3.3 Entrenar malla 6 de búsqueda en el remuestreo ----
set.seed(123)

#  EL SIGUIENTE TUNING TOMA VARIOS MINUTOS EN CORRER.

                  # rlen_tuned6 <- tune_grid(
                  #   rlen_wflow, ## Workflow: Receta, modelo y case_weights
                  #   resamples= cv, ## Crossvalidation
                  #   grid = rlen_grid6, ## Malla de Busqueda
                  #   metrics = metricas, ## Metricas
                  #   control= control_grid(allow_par = T, save_pred = T) ## Paralel y Pred
                  # )
#saveRDS(rlen_tuned6,file = "TuningSextaMalla.rds")
#rlen_tuned6 <- readRDS("Exports/TuningSextaMalla.rds")

show_best(rlen_tuned6, metric = "accuracy", n = 10)
show_best(rlen_tuned6, metric = "sens", n = 10)
show_best(rlen_tuned6, metric = "precision", n = 10)
show_best(rlen_tuned6, metric = "spec", n = 10)
show_best(rlen_tuned6, metric = "bal_accuracy", n = 10)
#Basta
# Se observó que siempre era posible encontrar una combinación lambda-alpha que alcance una sensibilidad de 1 independientemente de sus valores, por lo tanto,
# se buscó maximizar el balanced_acuracy siempre y cuando mantenga una elevada sensibilidad.
# (lambda,alpha) = (penalty,mixture) = (1.748948 , 0.011986)
# Esta combinaión de hiperparámetros es la elegida por observación propia, siendo la segunda con mejor bal_accuracy, ya que mantiene el parámetro mixture lo más alejado de ser una reg LASSO.


# malla <- rlen_tuned6[[4]][[1]]
# mallasens1 <- malla %>%
#                 filter(
#                   .metric=="sens",
#                   .estimate > 0.999
#                 )
# plot(malla[malla$.metric=="sens",c(1,2)])
# plot(mallasens1[,c(1,2)])


#Multicolinealidad (considerando la variable cluster)
# m2 <- glm(Exited~.-case_wts,family = "binomial",data = train,weights = case_wts)
# summary(m2)
# vif(m2)
# train3 <- train[,-c(2,3,8,9,11,12,13)] %>% mutate(clust=as.double(train$cluster))
# GGally::ggpairs(train3)
# Con esto se finaliza lo interpretado en la quinta malla, donde se decidió dejar fuera la variable "cluster".


###  7.6   Modelo final ----
#### 7.6.1 Seleccionar la mejor combinación de hiperparámetros ----
#best_rlen_par <- select_best(rlen_tuned6, metric = "bal_accuracy")
best_rlen_par <- rlen_tuned6[[4]][[1]] %>% 
  filter(.config=="Preprocessor1_Model003", .metric=="bal_accuracy") %>% 
  select(penalty,mixture,.config)
# A pesar que inicialmente la estrategia era de sacrificar un poco de accuracy para
# obtener mayor sensibilidad, por el objetivo de detectar más clientes que podrían
# retirarse, pero se identificó que independientemente del valor que tome un
# hiperparámetro siempre era posible obtener una combinación que alcance una sensibilidad
# de 1, aunque dichas combinaciones no producían un accuracy alto. Por lo tanto se buscó
# el mejor balanced accuracy que mantenga una alta sensibilidad.


#### 7.6.2 Finalizar (darles valores a los parámetros tuneables) el workflow ----
rlen_wfow_final <- rlen_wflow %>% finalize_workflow(best_rlen_par)


#### 7.6.3 Ahora sí, entrenar el modelo final con los datos----
rlen_fitted <- fit(rlen_wfow_final, data = train) #Este es el workflow ajustado a la data de training
rlen_model <- extract_fit_parsnip(rlen_fitted) #Esto solo es el modelo


###  7.7   Evaluación del modelo ----
#Comparación de las métricas tanto en el train y en el test para ver si hubo sobreajuste o no.
train %>% 
  predict(rlen_fitted, new_data = .) %>% 
  mutate(Real=train$Exited) %>% 
  conf_mat(truth = Real, estimate = .pred_class) %>% 
  summary()

test %>% 
  predict(rlen_fitted, new_data = .) %>% 
  mutate(Real=test$Exited) %>% 
  conf_mat(truth = Real, estimate = .pred_class) %>% 
  summary()
#Podemos ver que las métricas no son muy diferentes, son levemente más bajas y, podría ser algo esperado,
#pero tiene en general, valores cercanos y esto indica que el modelo NO SE HA SOBREAJUSTADO
# Y dado que no hay evidencia de overfitting, aquí termina la búsqueda del modelo!


#### 7.7.1 Finalizar la paralelización ----
# parallel::detectCores(logical=FALSE)
# cl <- makePSOCKcluster(4)
# registerDoParallel(cl)
parallel::stopCluster(cl) ## Esto se debe ejecutar al final



#    8.    Análisis posteriores al modelamiento -----------------------
rlen_model$spec
rlen_model %>% vip(geom = "point")
# Las variables que más aportaron para ajustar un buen modelo que prediga la fuga de clientes fueron:
# Si el cliente no es miembro activo.
# LA edad.
# Si es de Alemania.
# Si son hombres.
# Y sus datos de Balance.


# Fin. --------------------------------------------------------------------