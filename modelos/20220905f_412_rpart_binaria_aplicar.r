#20220905b pruebo la corrección de fecha ultimo cierre (no me cambió nada, lo dejo)
#20220905c pruebo unificar los status de visa y master (no funcionó)
#20220905d2 pruebo crear el ctrx_quarter < 14 (FUNCIONÓ) (Funcionó porque me olvidé de ponerlo, ahora hay que ver sacando que cosas va mejor)
#Pruebo la iteración 529 (Se mezcló con la d2, queda esta).
#20220905f pruebo crear el visa_status < x y master_status <x (?)
-----------------------------------------------------------------------
#Peluquería de features:
#20220905g saque CTRX, mcaja_ahorros y master_mpagado







#Aplicacion de los mejores hiperparametros encontrados en una bayesiana
#Utilizando clase_binaria =  [  SI = { "BAJA+1", "BAJA+2"} ,  NO="CONTINUA ]

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")


#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\gdrive\\UBA2022\\")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv" )


#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]

#Transformo las primeras ramas del árbol en categorías binarias
#dataset[ ,ctrx_quarter_bin :=  ifelse( ctrx_quarter < 14, 0, 1 ) ] da peor con esto que sin
dataset[ ,Visa_status_bin :=  ifelse( Visa_status < 8, 0, 1 ) ]
dataset[ ,Master_status_bin :=  ifelse( Master_status < 8, 0, 1 ) ]


dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103, ]  #defino donde voy a aplicar el modelo


# Entreno el modelo
# obviamente rpart no puede ver  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

modelo  <- rpart(formula=   "clase_binaria ~ . -mcaja_ahorro -Master_mpagado -Visa_mpagado -ctrx_quarter -mcomisiones_mantenimiento -clase_ternaria -Visa_status -Master_status",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=         0,
                 cp=          -0.075,#  -0.89
                 minsplit=  899,   # 621
                 minbucket=  31,   # 309
                 maxdepth=     7 )  #  12

#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)

#----------------------------------------------------------------------------
# habilitar esta seccion si el Fiscal General  Alejandro Bolaños  lo autoriza
#----------------------------------------------------------------------------

# corrijo manualmente el drifting de  Visa_fultimo_cierre
 dapply[ Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
 dapply[ Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
 dapply[ Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
 dapply[ Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
 dapply[ Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
 dapply[ Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
 dapply[ Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]

# corrijo manualmente el drifting de  Visa_fultimo_cierre
 dapply[ Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
 dapply[ Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
 dapply[ Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
 dapply[ Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
 dapply[ Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
 dapply[ Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
 dapply[ Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]


#aplico el modelo a los datos nuevos
prediccion  <- predict( object=  modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con DOS columnas, llamadas "NO", "SI"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal  <- copy( dapply[ , list(numero_de_cliente) ] )
dfinal[ , prob_SI := prediccion[ , "SI"] ]


# por favor cambiar por una semilla propia
# que sino el Fiscal General va a impugnar la prediccion
set.seed(113111)  
dfinal[ , azar := runif( nrow(dapply) ) ]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder( dfinal, -prob_SI, azar )


dir.create( "./exp/" )
dir.create( "./exp/KA113111" )


#for( corte  in  c( 4000, 5000, 6000, 7000, 8000, 9000, 9500, 10000, 10500, 11000 ) )
for( corte  in  c( 10000, 10500 ) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]


  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
           file= paste0( "./exp/KA113111/20220905g_KA113111_005_",  corte, ".csv"),
           sep=  "," )
}
