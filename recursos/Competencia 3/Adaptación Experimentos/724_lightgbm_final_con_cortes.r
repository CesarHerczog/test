# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")


#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "1002-6-FINAL_distintos_cortes"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$finalmodel$max_bin           <-    31
PARAM$finalmodel$learning_rate     <-    0.005167243 #0.010376197	0.005074585	0.005102191	0.02369966	0.007466308	0.005016956	0.010981193	0.005167243	0.005182794	0.005079523	0.012797102	0.006354138	0.008976899	0.005016553	0.005012027	0.009322211	0.005007473	0.005082847	0.005233427
PARAM$finalmodel$num_iterations    <-    750 #340	840	1142	158	228	1033	387	750	500	680	789	434	796	2043	992	266	1457	994	1436
PARAM$finalmodel$num_leaves        <-    41 #226	51	196	241	160	24	41	266	120	38	108	154	230	164	84	87	52	209
PARAM$finalmodel$min_data_in_leaf  <-    788 #927	1222	493	893	190	110	217	788	793	6	221	291	131	218	1542	510	50	1854	5
PARAM$finalmodel$feature_fraction  <-    0.423893723 #0.360452324	0.351841123	0.218797907	0.305032999	0.448039575	0.42430585	0.445989705	0.423893723	0.414326274	0.577729208	0.409136838	0.598187826	0.400119277	0.574452032	0.52458149	0.583304805	0.921187198	0.645851898	0.233161143

PARAM$finalmodel$semilla           <-    113111


  

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
#setwd( "~/buckets/b1" )
setwd("C:\\gdrive\\UBA2022\\")  #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)


#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO

#data drifting
dataset[ ,ctrx_quarter:=ifelse(foto_mes==202103,ifelse((ctrx_quarter<0),ctrx_quarter*1,ctrx_quarter*1.009),ctrx_quarter)]
dataset[ ,mcuentas_saldo:=ifelse(foto_mes==202103,ifelse((mcuentas_saldo<0),mcuentas_saldo*1.1283,mcuentas_saldo*1.0132),mcuentas_saldo)]
dataset[ ,cdescubierto_preacordado:=ifelse(foto_mes==202103,ifelse((cdescubierto_preacordado<0),cdescubierto_preacordado*1,cdescubierto_preacordado*0.9999),cdescubierto_preacordado)]
dataset[ ,mprestamos_personales:=ifelse(foto_mes==202103,ifelse((mprestamos_personales<0),mprestamos_personales*1,mprestamos_personales*1.0644),mprestamos_personales)]
dataset[ ,active_quarter:=ifelse(foto_mes==202103,ifelse((active_quarter<0),active_quarter*1,active_quarter*1.0016),active_quarter)]
dataset[ ,cprestamos_personales:=ifelse(foto_mes==202103,ifelse((cprestamos_personales<0),cprestamos_personales*1,cprestamos_personales*0.958),cprestamos_personales)]
dataset[ ,mcaja_ahorro:=ifelse(foto_mes==202103,ifelse((mcaja_ahorro<0),mcaja_ahorro*4.8481,mcaja_ahorro*1.0491),mcaja_ahorro)]
dataset[ ,mcuenta_corriente:=ifelse(foto_mes==202103,ifelse((mcuenta_corriente<0),mcuenta_corriente*1.1405,mcuenta_corriente*1.0132),mcuenta_corriente)]
dataset[ ,mactivos_margen:=ifelse(foto_mes==202103,ifelse((mactivos_margen<0),mactivos_margen*1,mactivos_margen*0.8113),mactivos_margen)]
dataset[ ,ccomisiones_otras:=ifelse(foto_mes==202103,ifelse((ccomisiones_otras<0),ccomisiones_otras*1,ccomisiones_otras*0.9063),ccomisiones_otras)]
dataset[ ,mpasivos_margen:=ifelse(foto_mes==202103,ifelse((mpasivos_margen<0),mpasivos_margen*1.8146,mpasivos_margen*1.056),mpasivos_margen)]
dataset[ ,mtarjeta_visa_consumo:=ifelse(foto_mes==202103,ifelse((mtarjeta_visa_consumo<0),mtarjeta_visa_consumo*1,mtarjeta_visa_consumo*1.0937),mtarjeta_visa_consumo)]
dataset[ ,Visa_msaldototal:=ifelse(foto_mes==202103,ifelse((Visa_msaldototal<0),Visa_msaldototal*0.958,Visa_msaldototal*1.0718),Visa_msaldototal)]
dataset[ ,Visa_msaldopesos:=ifelse(foto_mes==202103,ifelse((Visa_msaldopesos<0),Visa_msaldopesos*0.9155,Visa_msaldopesos*1.07),Visa_msaldopesos)]
dataset[ ,ctarjeta_visa_transacciones:=ifelse(foto_mes==202103,ifelse((ctarjeta_visa_transacciones<0),ctarjeta_visa_transacciones*1,ctarjeta_visa_transacciones*1.0029),ctarjeta_visa_transacciones)]
dataset[ ,mcomisiones:=ifelse(foto_mes==202103,ifelse((mcomisiones<0),mcomisiones*0.6337,mcomisiones*0.8264),mcomisiones)]
dataset[ ,cliente_antiguedad:=ifelse(foto_mes==202103,ifelse((cliente_antiguedad<0),cliente_antiguedad*1,cliente_antiguedad*1.0069),cliente_antiguedad)]
dataset[ ,mcomisiones_otras:=ifelse(foto_mes==202103,ifelse((mcomisiones_otras<0),mcomisiones_otras*0.6334,mcomisiones_otras*0.8274),mcomisiones_otras)]
dataset[ ,mpayroll:=ifelse(foto_mes==202103,ifelse((mpayroll<0),mpayroll*1,mpayroll*0.9526),mpayroll)]


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )




#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla
                                  )
                    )

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- c( 7428,
             7488,
             7548,
             7608,
             7668,
             7728,#Elegido
             7788,
             7848,
             7908,
             7968,
             8028,
             8088,
             8148,
             8208,
             8268,
             8328,
             8388,
             8448,
             8508,
             8568,
             8628,
             8688,
             8748,
             8808,
             8868,
             8928,
             8988,
             9048,
             9108,
             9168,
             9228
)
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]

  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  PARAM$experimento, "_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------

#quit( save= "no" )

