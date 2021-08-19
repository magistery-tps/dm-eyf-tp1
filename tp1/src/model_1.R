options(warn=-2)
library(pacman)
p_load(this.path, dplyr, data.table, rpart)
setwd(this.path::this.dir())
source('../../lib/import.R')
import('../../lib/csv.R')


#cargo los datos de 202009 que es donde voy a ENTRENAR el modelo
setwd(this.path::this.dir())
dtrain <- loadcsv("../../dataset/paquete_premium_202009.csv")
dapply  <- loadcsv("../../dataset/paquete_premium_202011.csv")

#genero el modelo
modelo  <- rpart("clase_ternaria ~ .",
                 data = dtrain,
                 xval=0,
                 cp=        -0.3, 
                 minsplit=  80,
                 minbucket=  1,
                 maxdepth=   8 )


#aplico al modelo  a los datos de 202011

prediccion  <- predict(modelo, dapply , type = "prob") #aplico el modelo

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

entrega <- dapply %>%
  mutate(prob_baja2 = prediccion[, "BAJA+2"]) %>%
  mutate(Predicted  = as.numeric(prob_baja2 > 0.025)) %>%
  select(numero_de_cliente, Predicted)
  
#genero el archivo para Kaggle
fwrite(entrega, file="./K101_001.csv", sep="," )
