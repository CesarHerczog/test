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
PARAM$experimento  <- "KAClase"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$finalmodel$max_bin           <-    31
PARAM$finalmodel$learning_rate     <-    	0.005015327	#0.021330642	0.007112731	0.005121231	0.006152836	0.005373856	0.020615143	0.005084135	0.005089125	0.00501903	0.005946621	0.020967276	0.005333749	0.007098504	0.007033951	0.005005274	0.005092466	0.020567149	0.008016778	0.007095026	0.021438255	0.005005634
PARAM$finalmodel$num_iterations    <-    	751#	370	597	1785	646	1675	170	1035	1193	801	719	181	1178	824	785	1057	538	203	515	592	195	862
PARAM$finalmodel$num_leaves        <-    	234#	339	16	30	55	17	17	338	241	243	157	408	34	256	18	252	190	391	80	289	469	392
PARAM$finalmodel$min_data_in_leaf  <-    	911#	429	201	30	651	9	1186	788	2523	621	1004	1347	2042	3210	2156	2416	725	1103	1791	706	1548	2239
PARAM$finalmodel$feature_fraction  <-    	0.37896344#	0.380718954	0.394186302	0.373105425	0.36578093	0.398241597	0.397929819	0.398603314	0.38891343	0.395498819	0.376912223	0.397109484	0.38250846	0.378436543	0.225030725	0.369789109	0.368892489	0.365311167	0.378358382	0.381423653	0.398290003	0.379513657

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
        file= "prediccion4.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 9000,9004 )
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

