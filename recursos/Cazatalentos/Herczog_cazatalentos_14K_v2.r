#Solucion del desafio  14k


#Se logró un resultado de un XX% (con la semilla 102191) sin superar los en ningun intento los 13K


#limpio la memoria
rm( list=ls() )
gc()

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

set.seed( 102191 )

#inicializo el juego
gimnasio_init()

#Esta el la planilla del cazatalentos
planilla_cazatalentos  <- data.table( "id" = 1:100 )


#variables globales que usan las funciones gimnasio_xxxx
tasa_de_aciertos  <- 0
#GLOBAL_tiros_total  <- 0
MAX_GLOBAL_tiros_total  <- 0


for( i in 1:10000 ) { #diez mil experimentos
  
  GLOBAL_tiros_total  <- 0
  tiros_r1 <- 10 
  tiros_r2 <- 10 
  tiros_r3 <- 10
  tiros_r4 <- 10 
  tiros_r5 <- 10 
  tiros_r6 <- 10 
  tiros_r7 <- 10 
  tiros_r8 <- 10 
  tiros_r9 <- 10 
  tiros_r10 <- 10
  tiros_r11 <- 10 
  tiros_r12 <- 10 
  tiros_r13 <- 10 
  tiros_r14 <- 300
  
  poda<- 1    #Percentil de eliminación para todas las rondas menos la final
  poda_final <- 25 #Percentil de eliminación para la ronda final 

  poda_mediana_magica1 <- 0.50 #Valor de 0 a 1 en que se selecciona el limite de la mediana de tasa de aciertos
                          #que pasa a la última ronda
  
  poda_mediana_magica2 <- 0.60 #Valor de 0 a 1 en que se selecciona el limite de la mediana de tasa de aciertos
  #que pasa a la última ronda
  
  planilla_cazatalentos  <- data.table( "id" = 1:100 )
  
  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100 50 tiros libres cada uno
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
  
  planilla_cazatalentos[ ids_juegan1,  tiros1 := tiros_r1 ]  #registro en la planilla que tiran 90 tiros
  
  #Hago que tiren
  resultado1  <- gimnasio_tirar( ids_juegan1, tiros_r1)
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
  
  
  #Ronda 2 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  minmed  <- planilla_cazatalentos[ ids_juegan1, quantile(aciertos1, prob=seq(0, 1, length = 101))[poda]]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][aciertos1 > minmed, id ]
  
  planilla_cazatalentos[ ids_juegan2,  tiros2 := tiros_r2 ]  #registro en la planilla que tiran x tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, tiros_r2)
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla lo resultados de la ronda 2
  planilla_cazatalentos[ ids_juegan2,  acumulado2 := (aciertos1+aciertos2) ] #acumulo los resultados
  
  #Ronda 3 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  minmed  <- planilla_cazatalentos[ ids_juegan2, quantile(acumulado2, prob=seq(0, 1, length = 101))[poda]]
  ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][acumulado2 > minmed, id ]
  
  
  planilla_cazatalentos[ ids_juegan3,  tiros3 := tiros_r3 ]  #registro en la planilla que tiran x tiros
  resultado3  <- gimnasio_tirar( ids_juegan3, tiros_r3)
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan3,  acumulado3 := (acumulado2+aciertos3) ]
  
  #Ronda 4 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  
  minmed  <- planilla_cazatalentos[ ids_juegan3, quantile(acumulado3, prob=seq(0, 1, length = 101))[poda] ]
  ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][acumulado3 >= minmed, id ]
  
  planilla_cazatalentos[ ids_juegan4,  tiros4 := tiros_r4 ]  #registro en la planilla que tiran x tiros
  resultado4  <- gimnasio_tirar( ids_juegan4, tiros_r4)
  planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado4 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan4,  acumulado4 := (acumulado3+aciertos4) ]
  
  #Ronda 5 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  
  minmed  <- planilla_cazatalentos[ ids_juegan4, quantile(acumulado4, prob=seq(0, 1, length = 101))[poda] ]
  ids_juegan5  <- planilla_cazatalentos[ ids_juegan4 ][acumulado4 >= minmed, id ]
  
  planilla_cazatalentos[ ids_juegan5,  tiros5 := tiros_r5 ]  #registro en la planilla que tiran x tiros
  resultado5  <- gimnasio_tirar( ids_juegan5, tiros_r5)
  planilla_cazatalentos[ ids_juegan5,  aciertos5 := resultado5 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan5,  acumulado5 := (acumulado4+aciertos5) ]  
  
  #Ronda 6 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  
  minmed  <- planilla_cazatalentos[ ids_juegan5, quantile(acumulado5, prob=seq(0, 1, length = 101))[poda] ]
  ids_juegan6  <- planilla_cazatalentos[ ids_juegan5 ][acumulado5 >= minmed, id ]
  
  planilla_cazatalentos[ ids_juegan6,  tiros6 := tiros_r6 ]  #registro en la planilla que tiran x tiros
  resultado6  <- gimnasio_tirar( ids_juegan6, tiros_r6)
  planilla_cazatalentos[ ids_juegan6,  aciertos6 := resultado6 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan6,  acumulado6 := (acumulado5+aciertos6) ]
  
  
  #Ronda 7 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  
  minmed  <- planilla_cazatalentos[ ids_juegan6, quantile(acumulado6, prob=seq(0, 1, length = 101))[poda] ]
  ids_juegan7  <- planilla_cazatalentos[ ids_juegan6 ][acumulado6 >= minmed, id ]

  planilla_cazatalentos[ ids_juegan7,  tiros7 := tiros_r7 ]  #registro en la planilla que tiran x tiros
  resultado7  <- gimnasio_tirar( ids_juegan7, tiros_r7)
  planilla_cazatalentos[ ids_juegan7,  aciertos7 := resultado7 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan7,  acumulado7 := (acumulado6+aciertos7) ] 
 
  #---------------------------------------------------------------
  
  #Creo márgenes de aciertos por ronda en la planilla
  planilla_cazatalentos$margen1 <- planilla_cazatalentos$aciertos1/planilla_cazatalentos$tiros1
  planilla_cazatalentos$margen2 <- planilla_cazatalentos$aciertos2/planilla_cazatalentos$tiros2
  planilla_cazatalentos$margen3 <- planilla_cazatalentos$aciertos3/planilla_cazatalentos$tiros3
  planilla_cazatalentos$margen4 <- planilla_cazatalentos$aciertos4/planilla_cazatalentos$tiros4
  planilla_cazatalentos$margen5 <- planilla_cazatalentos$aciertos5/planilla_cazatalentos$tiros5
  planilla_cazatalentos$margen6 <- planilla_cazatalentos$aciertos6/planilla_cazatalentos$tiros6
  planilla_cazatalentos$margen7 <- planilla_cazatalentos$aciertos7/planilla_cazatalentos$tiros7
  
  #Calculo la mediana de la las rondas anteriores
  planilla_cazatalentos$mediana_magica1 <- apply(planilla_cazatalentos[, c("margen1" ,"margen2" ,"margen3" ,"margen4" ,"margen5" ,"margen6" ,"margen7")], 1, median) 
  
  
  
  #Ronda 8 -------------------------------------------------------
  #los mejores jugadores tiran x tiros cada uno
  minmed  <- planilla_cazatalentos[ ids_juegan7, quantile(acumulado7, prob=seq(0, 1, length = 101))[poda] ]
  ids_juegan8  <- planilla_cazatalentos[ ids_juegan7 ][acumulado7 >= minmed, id ]
  ids_juegan8  <- planilla_cazatalentos[ ids_juegan7 ][mediana_magica1 > poda_mediana_magica1, id ]
  
  planilla_cazatalentos[ ids_juegan8,  tiros8 := tiros_r8 ]  #registro en la planilla que tiran x tiros
  resultado8  <- gimnasio_tirar( ids_juegan8, tiros_r8)
  planilla_cazatalentos[ ids_juegan8,  aciertos8 := resultado8 ]  #registro en la planilla lo resultados de la ronda 2
  planilla_cazatalentos[ ids_juegan8,  acumulado8 := (acumulado7+aciertos8) ] #acumulo los resultados
  
  #Ronda 9 -------------------------------------------------------
  #los mejores jugadores tiran x tiros cada uno
  minmed  <- planilla_cazatalentos[ ids_juegan8, quantile(acumulado8, prob=seq(0, 1, length = 101))[poda] ]
  ids_juegan9  <- planilla_cazatalentos[ ids_juegan8 ][acumulado8 >= minmed, id ]

  planilla_cazatalentos[ ids_juegan9,  tiros9 := tiros_r9 ]  #registro en la planilla que tiran x tiros
  resultado9  <- gimnasio_tirar( ids_juegan8, tiros_r9)
  planilla_cazatalentos[ ids_juegan9,  aciertos9 := resultado9 ]  #registro en la planilla lo resultados de la ronda 2
  planilla_cazatalentos[ ids_juegan9,  acumulado9 := (acumulado8+aciertos9) ] #acumulo los resultados
  
  #Ronda 10 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  
  minmed  <- planilla_cazatalentos[ ids_juegan9, quantile(acumulado9, prob=seq(0, 1, length = 101))[poda] ]
  ids_juegan10  <- planilla_cazatalentos[ ids_juegan9 ][acumulado9 >= minmed, id ]
  
  planilla_cazatalentos[ ids_juegan10,  tiros10 := tiros_r10 ]  #registro en la planilla que tiran x tiros
  resultado10  <- gimnasio_tirar( ids_juegan10, tiros_r10)
  planilla_cazatalentos[ ids_juegan10,  aciertos10 := resultado10 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan10,  acumulado10 := (acumulado9+aciertos10) ]
  
  #Ronda 11 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  
  minmed  <- planilla_cazatalentos[ ids_juegan10, quantile(acumulado10, prob=seq(0, 1, length = 101))[poda] ]
  ids_juegan11  <- planilla_cazatalentos[ ids_juegan10 ][acumulado10 >= minmed, id ]
  
  planilla_cazatalentos[ ids_juegan11,  tiros11 := tiros_r11 ]  #registro en la planilla que tiran x tiros
  resultado11  <- gimnasio_tirar( ids_juegan11, tiros_r11)
  planilla_cazatalentos[ ids_juegan11,  aciertos11 := resultado11 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan11,  acumulado11 := (acumulado10+aciertos11) ]  
  
  #Ronda 12 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  
  minmed  <- planilla_cazatalentos[ ids_juegan11, quantile(acumulado11, prob=seq(0, 1, length = 101))[poda] ]
  ids_juegan12 <- planilla_cazatalentos[ ids_juegan11 ][acumulado11 >= minmed, id ]
  
  planilla_cazatalentos[ ids_juegan12,  tiros12 := tiros_r12 ]  #registro en la planilla que tiran x tiros
  resultado6  <- gimnasio_tirar( ids_juegan12, tiros_r12)
  planilla_cazatalentos[ ids_juegan12,  aciertos12 := resultado6 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan12,  acumulado12 := (acumulado11+aciertos12) ]
  
  #Creo márgenes de aciertos por ronda en la planilla
  planilla_cazatalentos$margen8 <- planilla_cazatalentos$aciertos8/planilla_cazatalentos$tiros8
  planilla_cazatalentos$margen9 <- planilla_cazatalentos$aciertos9/planilla_cazatalentos$tiros9
  planilla_cazatalentos$margen10 <- planilla_cazatalentos$aciertos10/planilla_cazatalentos$tiros10
  planilla_cazatalentos$margen11 <- planilla_cazatalentos$aciertos11/planilla_cazatalentos$tiros11
  planilla_cazatalentos$margen12 <- planilla_cazatalentos$aciertos12/planilla_cazatalentos$tiros12

  
  #Calculo la mediana de la las rondas anteriores
  planilla_cazatalentos$mediana_magica2 <- apply(planilla_cazatalentos[, c("margen1" ,"margen2" ,"margen3" ,"margen4" ,"margen5" ,"margen6", "margen7", "margen8", "margen9", "margen10", "margen11", "margen12")], 1, median)
  
  
  #Ultima Ronda -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  
  minmed  <- planilla_cazatalentos[ ids_juegan12, quantile(acumulado12, prob=seq(0, 1, length = 101))[poda_final] ]
  ids_juegan13  <- planilla_cazatalentos[ ids_juegan12 ][acumulado12 >= minmed, id ]
  ids_juegan13  <- planilla_cazatalentos[ ids_juegan12 ][mediana_magica2 > poda_mediana_magica2, id ]
  
  planilla_cazatalentos[ ids_juegan13,  tiros13 := tiros_r13 ]  #registro en la planilla que tiran x tiros
  resultado13  <- gimnasio_tirar( ids_juegan13, tiros_r13)
  planilla_cazatalentos[ ids_juegan13,  aciertos13 := resultado13 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan13,  acumulado13 := (acumulado12+aciertos13) ] 
  
  
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(acumulado13) ]
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
cat(tiros_r1 , tiros_r2, tiros_r3, tiros_r4, tiros_r5, tiros_r6, tiros_r7, MAX_GLOBAL_tiros_total, tasa_de_aciertos/100, "\n" )
#Eliminación a quienes estan debajo de X percentil antes de cada ronda.
cat(poda, poda_final, poda_mediana_magica1, poda_mediana_magica2, MAX_GLOBAL_tiros_total, tasa_de_aciertos/100, "\n" )

