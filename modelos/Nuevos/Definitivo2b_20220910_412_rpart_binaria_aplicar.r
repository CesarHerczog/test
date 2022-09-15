#20220908 Abandono la normalización y empiezo de cero
#1 Corroboro que puedo replicar mi mejor marca leaderboard público (OK)
## 20220908_X1 sin Fiscal Bolaños
#2 Pruebo Fiscal Bolaños (Mismo resultado, lo dejo)
#3 Pruebo 200 menos y mejora (Falta sintonía fina)
#20220908_x2_KA113111_8800
#4Pruebo creando ctrx_quarter (NO FUNCIONÓ)
  
##2 Probar la contrucción de algunas features:
#Principal_1[ , campo1 := as.integer( ctrx_quarter >= 49 & mpayroll >= 7043 & (ccaja_ahorro < 4 | is.na(ccaja_ahorro) ) & mpayroll < 1000000 & ( mcheques_depositados < 45000 | is.na(mcheques_depositados) ) & ( Visa_fechaalta < 8986 | is.na(Visa_fechaalta) ) & ( mtarjeta_master_consumo < 77000 | is.na(mtarjeta_master_consumo) ) ]
#Principal_2[ , campo1 := as.integer( ctrx_quarter >= 49 & (mpayroll < 7043 | is.na(mpayroll) ) & mtarjeta_visa_consumo >= 2130 & mpasivos_margen >= 699 & mcaja_ahorro >= 325 & ( cliente_edad < 78 | is.na(cliente_edad) ) & ( ccallcenter_transacciones < 10 | is.na(ccallcenter_transacciones) ) ]
#Principal_3[ , campo1 := as.integer( ctrx_quarter >= 49 & (mpayroll < 7043 | is.na(mpayroll) ) & mtarjeta_visa_consumo >= 2130 & (mpasivos_margen < 699 | is.na(mpasivos_margen) ) & Visa_msaldototal >= 5997 & mrentabilidad_annual >= 11000 & ( Visa_msaldodolares < 3026 | is.na(Visa_msaldodolares) )]
#Principal_4[ , campo1 := as.integer( ctrx_quarter >= 49 & (mpayroll < 7043 | is.na(mpayroll) ) & mtarjeta_visa_consumo >= 2130 & (mpasivos_margen < 699 | is.na(mpasivos_margen) ) & Visa_msaldototal >= 5997 & ( mrentabilidad_annual < 11000 | is.na(mrentabilidad_annual) ) & ( ctrx_quarter < 148 | is.na(ctrx_quarter) )]


#Aplicacion de los mejores hiperparametros encontrados en una bayesiana
#Utilizando clase_binaria =  [  SI = { "BAJA+1", "BAJA+2"} ,  NO="CONTINUA ]

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
require("dplyr")


#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:\\gdrive\\UBA2022\\")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")


#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]


#dataset[ ,feature2:= as.integer( ctrx_quarter+mpayroll )]
#dataset[ ,feature13:= as.integer( ctrx_quarter+mcaja_ahorro )]
#dataset[ ,feature23:= as.integer( mpayroll+mtarjeta_visa_consumo )]
#dataset[ ,feature24:= as.integer( mpayroll+mpasivos_margen )]
#dataset[ ,feature69:= as.integer( Visa_fechaalta+mcaja_ahorro )]
#dataset[ ,feature80:= as.integer( mtarjeta_master_consumo+mpasivos_margen )]
#dataset[ ,feature125:= as.integer( mtarjeta_visa_consumo+mcaja_ahorro )]
#dataset[ ,feature207:= as.integer( ctrx_quarter*ccallcenter_transacciones )]
#dataset[ ,feature290:= as.integer( ctarjeta_visa_transacciones*mpasivos_margen )]
#dataset[ ,feature294:= as.integer( ctarjeta_visa_transacciones*Visa_msaldototal )]
#dataset[ ,feature321:= as.integer( mtarjeta_visa_consumo*mcaja_ahorro )]
#dataset[ ,feature336:= as.integer( mpasivos_margen*Visa_msaldototal )]
#Transformo las primeras ramas del árbol en categorías binarias

#Nuevas Features
#dataset[ ,principal_1 := as.integer( ctrx_quarter >= 49 & mpayroll >= 7043 & (ccaja_ahorro < 4 | is.na(ccaja_ahorro) ) & mpayroll < 1000000 & ( mcheques_depositados < 45000 | is.na(mcheques_depositados) ) & ( Visa_fechaalta < 8986 | is.na(Visa_fechaalta) ) & ( mtarjeta_master_consumo < 77000 | is.na(mtarjeta_master_consumo) )) ]
#dataset[ ,principal_3 := as.integer( ctrx_quarter >= 49 & (mpayroll < 7043 | is.na(mpayroll) ) & mtarjeta_visa_consumo >= 2130 & (mpasivos_margen < 699 | is.na(mpasivos_margen) ) & Visa_msaldototal >= 5997 & mrentabilidad_annual >= 11000 & ( Visa_msaldodolares < 3026 | is.na(Visa_msaldodolares) ))]
#dataset[ ,principal_2 := as.integer( ctrx_quarter >= 49 & (mpayroll < 7043 | is.na(mpayroll) ) & mtarjeta_visa_consumo >= 2130 & mpasivos_margen >= 699 & mcaja_ahorro >= 325 & ( cliente_edad < 78 | is.na(cliente_edad) ) & ( ccallcenter_transacciones < 10 | is.na(ccallcenter_transacciones) )) ]
#dataset[ ,principal_4 := as.integer( ctrx_quarter >= 49 & (mpayroll < 7043 | is.na(mpayroll) ) & mtarjeta_visa_consumo >= 2130 & (mpasivos_margen < 699 | is.na(mpasivos_margen) ) & Visa_msaldototal >= 5997 & ( mrentabilidad_annual < 11000 | is.na(mrentabilidad_annual) ) & ( ctrx_quarter < 148 | is.na(ctrx_quarter) ))]


#dataset[ ,ctrx_quarter_bin :=  ifelse( ctrx_quarter >= 49, 1, 0 ) ] 
#dataset[ ,Visa_status_bin :=  ifelse( Visa_status < 8, 0, 1 ) ]
#dataset[ ,Master_status_bin :=  ifelse( Master_status < 8, 0, 1 ) ]
#dataset[ ,mcaja_ahorro_bin :=  ifelse( mcaja_ahorro < 260, 0, 1 ) ]




dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar


#Creo nuevas features



dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo
dapply[ ,mcuentas_saldo := mcuentas_saldo * 0.9523 ] 
dapply[ ,ctrx_quarter := ctrx_quarter * 0.9947 ] 
dapply[ ,mcaja_ahorro := mcaja_ahorro * 0.99517 ] 

#Creo nuevas features




# Entreno el modelo
# obviamente rpart no puede ver  clase_ternaria para predecir  clase_binaria
#  #no utilizo Visa_mpagado ni  mcomisiones_mantenimiento por drifting

modelo  <- rpart(formula=   "clase_binaria ~ .  -clase_ternaria ", #-ctrx_quarter
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=          0,
                 cp=        -0.54,   #-0.89 -0.54
                 minsplit=   1073,   # 621  1073
                 minbucket=   278,   # 309   278
                 maxdepth=      9 )  #  12     9

#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)

rpart.plot(modelo)

vector_importantes <- names(modelo$variable.importance)

vector_importantes

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


for( corte  in  c( 301, 1151, 2023, 2842, 4317, 4200, 5172, 5706, 6316, 7063, 7481, 8280, 9117, 9956, 9125 ) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]


  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
           file= paste0( "./exp/KA113111/20220910_Definitivo2_",  corte, ".csv"),
           sep=  "," )
}

