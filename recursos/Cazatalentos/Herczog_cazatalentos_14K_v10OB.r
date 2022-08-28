#Solucion del desafio  14k
#Se logró un resultado de un XX% (con la semilla 113111) sin superar los en ningun intento los 13K


#limpio la memoria
rm( list=ls() )
gc()

require("DiceKriging")
require("mlrMBO")
require("data.table")

ftirar  <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}


GLOBAL_jugadores  <- c()

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 ) / 1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )
  
  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------


set.seed( 113111 )

#inicializo el juego
gimnasio_init()

#Esta el la planilla del cazatalentos
planilla_cazatalentos  <- data.table( "id" = 1:100 )

x <- c(15,1,10,1,5,2,2,10,2,2,2)

x[1]

exito <- function(x) #r1,min1,max1,v1,s1,p1,min2,max2,v2,s2,p2
    {         for(minimo in c(x[2])){
                for(maximo in c(x[3])){
                  for(valor in c(x[4])){
                    for(suma in c(x[5])){
                      for(potencia in c(x[6])){
                        for(minimo_r in c(x[7])){
                          for(maximo_r in c(x[8])){
                            for(valor_r in c(x[9])){
                              for(suma_r in c(x[10])){
                                for(potencia_r in c(x[11])){
                                  tasa_de_aciertos  <- 0 # % de veces que salió elegido el mejor
                                  MAX_GLOBAL_tiros_total  <- 0
                                  for(i in 1:1000)#diez mil experimentos {
                                    n = 1
                                    acumulado_ratio_de_acierto <- 0
                                    acumulado_aciertos <- 0
                                    acumulado_tiros <- 0
                                    acumulado_potencia <- 1
                                    GLOBAL_tiros_total  <- 0
                                    cant_tiros = minimo #Cantidad de tiros con la que empieza la primera ronda
                                    ratio = minimo_r

                                    planilla_cazatalentos  <- data.table( "id" = 1:100 ) #Creación de la planilla del cazatalentos
                                    idjuegan <- 1:100   #los jugadores que participan la primera ronda
                                    
                                    for( rondas in c(x[1])){
                                    
                                      rondas_pasadas <- n-1

                                      #Juegan
                                      planilla_cazatalentos[ idjuegan,  paste("tiros",n, sep="") := cant_tiros ]  #registro en la planilla la cantidad de tiros
                                      resultado <- gimnasio_tirar( idjuegan, cant_tiros)
                                      planilla_cazatalentos[ idjuegan,  paste("aciertos",n, sep="") := resultado ]  #registro en la planilla lo resultados de la ronda
                                      ratio_de_acierto <- resultado/(cant_tiros+0.01) #defino un ratio de aciertos

                                      #Valores acumulados
                                      planilla_cazatalentos[ idjuegan,  acumulado_tiros := (acumulado_tiros+cant_tiros) ]
                                      planilla_cazatalentos[ idjuegan,  acumulado_aciertos := (acumulado_aciertos+resultado)]
                                      planilla_cazatalentos[ idjuegan,  acumulado_ratio_de_acierto := (((acumulado_ratio_de_acierto*rondas_pasadas)+ratio_de_acierto)/n) ]

                                      #Valores de la ronda
                                      planilla_cazatalentos[ idjuegan,  paste("acumulado_aciertos",n, sep="") := (acumulado_aciertos+resultado)]
                                      planilla_cazatalentos[ idjuegan,  paste("acumulado_tiros",n, sep="") := (acumulado_tiros+cant_tiros) ]
                                      planilla_cazatalentos[ idjuegan,  paste("ratio_acierto",n, sep="") := (((acumulado_ratio_de_acierto*rondas_pasadas)+ratio_de_acierto)/n) ]

                                      #Selección de quienes no pasan de ronda

                                      idjuegan  <- planilla_cazatalentos[ idjuegan ][acumulado_ratio_de_acierto > ratio/100 , id ]

                                      #Aumento de ratio para la proxima ronda

                                      acumulado_potencia <- acumulado_potencia+potencia_r
                                      suma_de_ratio = ((valor_r+suma_r)^acumulado_potencia)+ratio
                                      if (suma_de_ratio > maximo_r) ratio <- maximo_r
                                      if (suma_de_ratio <= maximo_r) ratio <- suma_de_ratio 
                                      if ((max(planilla_cazatalentos$acumulado_ratio_de_acierto)*100)+2 <= ratio) ratio <- (max(planilla_cazatalentos$acumulado_ratio_de_acierto)*100)-2.01 


                                      #Cantidad de tiros para la siguiente ronda

                                      suma_de_tiros = ((valor+suma)^potencia)+cant_tiros
                                      if (suma_de_tiros > maximo) cant_tiros <- maximo
                                      if (suma_de_tiros <= maximo) cant_tiros <- suma_de_tiros 

                                      #Para quienes pasan de ronda
                                      n = n + 1
                                      #idjuegan <- paste("ids_juegan",n, sep = "")
                                      tiros <- paste("tiros",n, sep = "")
                                      resultado <- paste("resultado",n, sep = "")
                                    }  

                                    #Ultima ronda

                                    #Juegan
                                    planilla_cazatalentos[ idjuegan,  paste("tiros",n, sep="") := cant_tiros ]  #registro en la planilla la cantidad de tiros
                                    resultado <- gimnasio_tirar( idjuegan, (450-acumulado_tiros))
                                    planilla_cazatalentos[ idjuegan,  paste("aciertos",n, sep="") := resultado ]  #registro en la planilla lo resultados de la ronda
                                    ratio_de_acierto <- resultado/(450-acumulado_tiros) #defino un ratio de aciertos

                                    #Valores acumulados
                                    planilla_cazatalentos[ idjuegan,  acumulado_tiros := (acumulado_tiros+cant_tiros) ]
                                    planilla_cazatalentos[ idjuegan,  acumulado_aciertos := (acumulado_aciertos+resultado)]
                                    planilla_cazatalentos[ idjuegan,  acumulado_ratio_de_acierto := (((acumulado_ratio_de_acierto*rondas_pasadas)+ratio_de_acierto)/n) ]


                                    #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
                                    pos_mejor <-  planilla_cazatalentos[ , which.max(acumulado_ratio_de_acierto*acumulado_aciertos^2) ]#aca está la clave de todo
                                    id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
                                    #Finalmente, la hora de la verdadero_mejor
                                    #Termino el juego
                                    veredicto  <- gimnasio_veredicto( id_mejor )
                                    veredicto[2]
                                    #El veredicto da que la estrategia seguida por el cazatalentos fue exitosa para este caso
                                    #Le acerto al verdadero_mejor
                                    #En el siguiente script veremos de hacer una Estimacion Montecarlo
                                    #De 10000 veces que el entrenador sigue esta estrategia, cuantas realmente le acierta
                                    if(GLOBAL_tiros_total >= MAX_GLOBAL_tiros_total) MAX_GLOBAL_tiros_total <- GLOBAL_tiros_total
                                    if( veredicto[2] == 1 )  tasa_de_aciertos  <- tasa_de_aciertos + 1
                                  }
                                  #Tiros por ronda.
                                  #if (MAX_GLOBAL_tiros_total > 180000) resultado_tiros <- 0
                                  #if (MAX_GLOBAL_tiros_total < 180000) resultado_tiros <- MAX_GLOBAL_tiros_total
                                  #if (tasa_de_aciertos/10 > 90) resultado_aciertos <- tasa_de_aciertos/10
                                  #if (tasa_de_aciertos/10 < 90) resultado_aciertos <- 0
                                  cat(rondas,minimo, maximo, valor, suma, potencia, MAX_GLOBAL_tiros_total, tasa_de_aciertos/10, "\n" )
                                  #Tiros por ronda.
                                  cat(rondas,minimo_r, maximo_r, valor_r, suma_r, potencia_r, MAX_GLOBAL_tiros_total, tasa_de_aciertos/10, "\n" )
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
    return(MAX_GLOBAL_tiros_total*(100-(tasa_de_aciertos/10)))
    }




#-----------------------------------------------------------------------------------------------------
#Optimización


configureMlr( show.learner.output = FALSE)

obj.fun  <- makeSingleObjectiveFunction(
              fn= exito,
              minimize= TRUE,   #estoy Maximizando la ganancia
              has.simple.signature = TRUE,  #porque tengo DOS dimensiones
              par.set=  makeParamSet(  makeIntegerParam( "r1", lower= 8, upper=  8),
                                       makeIntegerParam( "min1", lower= 1, upper=  10),
                                       makeIntegerParam( "max1", lower= 5, upper=  15),
                                       makeIntegerParam( "v1", lower= 1, upper=  10),
                                       makeIntegerParam( "s1", lower= 1, upper=  10),
                                       makeIntegerParam( "p1", lower= 1, upper=  2),
                                       makeIntegerParam( "min2", lower= 1, upper=  10),
                                       makeIntegerParam( "max2", lower= 1, upper=  30), 
                                       makeIntegerParam( "v2", lower= 1, upper=  3), 
                                       makeIntegerParam( "s2", lower= 1, upper=  2), 
                                       makeIntegerParam( "p2", lower= 1, upper=  2) 
                                    ),
             )

fproxy  <- makeLearner( cl= "regr.km",
                        predict.type= "se", 
                        covtype= "matern3_2" )


ctrl  <- makeMBOControl()
ctrl  <- setMBOControlInfill( ctrl, crit= makeMBOInfillCritEI())
ctrl  <- setMBOControlTermination( ctrl, iters= 200 )


run  <- mbo( fun=     obj.fun, 
             learner= fproxy, 
             control= ctrl )

tb_resultados  <- as.data.table( run$opt.path )


tb_resultados

