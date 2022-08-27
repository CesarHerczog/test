#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar

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


#Nueva Estrategia
#En la primera ronda se tiran x(20) tiros, se elimina a los que están por debajo del promedio entre la media y el mínimo.
#En la segunda ronda se tiran x(40) tiros más, se elimina a los que están por debajo del promedio entre la media y el mínimo.
#En la segunda ronda se tiran x(80) tiros más, se elimina a los que están por debajo del promedio entre la media y el mínimo.





#En la primer ronda se hace tirar 90 tiros libres a cada uno de los 100 jugadores ( se gastan 90000 tiros )
#Se eligen a la mejor mitad  ( se descarta a la peor mitad )
#En la segunda ronda, a la mejor mitad de la anterior ronda se los hace tirar 400 tiros a cada uno
#Se elige el mejor jugador de la segunda ronda

set.seed( 102191 )

#inicializo el juego
gimnasio_init()

#Esta el la planilla del cazatalentos
planilla_cazatalentos  <- data.table( "id" = 1:100 )

for( base_tiros in c(50,60) ){
  for( coeficiente in c(1, 3, 5) ){

    #variables globales que usan las funciones gimnasio_xxxx
    tasa_de_aciertos  <- 0
    #GLOBAL_tiros_total  <- 0
    MAX_GLOBAL_tiros_total  <- 0
    
    
    for( i in 1:10000 ) { #diez mil experimentos
      
      mas_tiros <- -5
      
      GLOBAL_tiros_total  <- 0
      tiros_r1 <- 50 #base_tiros 
      tiros_r2 <- 30 #(tiros_r1 + mas_tiros)
      tiros_r3 <- 15 #(tiros_r2 + mas_tiros)
      tiros_r4 <- 15 #(tiros_r3 + mas_tiros)
      tiros_r5 <- 15 #(tiros_r4 + mas_tiros)
      tiros_r6 <- 20 #(tiros_r5 + mas_tiros)
      tiros_r7 <- 300 #(tiros_r6 + mas_tiros)
        
      #vaciertos  <- mapply( ftirar, jugadores, tiros_libres )
      #mejor  <- which.max( vaciertos )
      
      #Ronda 1  ------------------------------------------------------
      #tiran los 100 jugadores es decir 1:100  90 tiros libres cada uno
      ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
      
      planilla_cazatalentos[ ids_juegan1,  tiros1 := tiros_r1 ]  #registro en la planilla que tiran 90 tiros
      
      #Hago que tiren
      resultado1  <- gimnasio_tirar( ids_juegan1, tiros_r1)
      planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
      
      
      #Ronda 2 -------------------------------------------------------
      #los mejores 40 jugadores tiran x tiros cada uno
      minmed  <- planilla_cazatalentos[ ids_juegan1, quantile(aciertos1, prob=seq(0, 1, length = 101))[18]]
      ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][aciertos1 > minmed, id ]
      
      planilla_cazatalentos[ ids_juegan2,  tiros2 := tiros_r2 ]  #registro en la planilla que tiran x tiros
      resultado2  <- gimnasio_tirar( ids_juegan2, tiros_r2)
      planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla lo resultados de la ronda 2
      planilla_cazatalentos[ ids_juegan2,  acumulado2 := (aciertos1+aciertos2) ] #acumulo los resultados
      
      #Ronda 3 -------------------------------------------------------
      #los mejores 40 jugadores tiran x tiros cada uno
      
      minmed  <- planilla_cazatalentos[ ids_juegan2, quantile(acumulado2, prob=seq(0, 1, length = 101))[21]]
      ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][acumulado2 > minmed, id ]
      
      
      planilla_cazatalentos[ ids_juegan3,  tiros3 := tiros_r3 ]  #registro en la planilla que tiran x tiros
      resultado3  <- gimnasio_tirar( ids_juegan3, tiros_r3)
      planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla la suma de la ronda uno y la dos
      planilla_cazatalentos[ ids_juegan3,  acumulado3 := (acumulado2+aciertos3) ]
      
      #Ronda 4 -------------------------------------------------------
      #los mejores 40 jugadores tiran x tiros cada uno
      
      
      minmed  <- planilla_cazatalentos[ ids_juegan3, quantile(acumulado3, prob=seq(0, 1, length = 101))[26] ]
      ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][acumulado3 > minmed, id ]
      
      planilla_cazatalentos[ ids_juegan4,  tiros4 := tiros_r4 ]  #registro en la planilla que tiran x tiros
      resultado4  <- gimnasio_tirar( ids_juegan4, tiros_r4)
      planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado4 ]  #registro en la planilla la suma de la ronda uno y la dos
      planilla_cazatalentos[ ids_juegan4,  acumulado4 := (acumulado3+aciertos4) ]
      
      #Ronda 5 -------------------------------------------------------
      #los mejores 40 jugadores tiran x tiros cada uno
      
      
      minmed  <- planilla_cazatalentos[ ids_juegan4, quantile(acumulado4, prob=seq(0, 1, length = 101))[36] ]
      ids_juegan5  <- planilla_cazatalentos[ ids_juegan4 ][acumulado4 > minmed, id ]
      
      planilla_cazatalentos[ ids_juegan5,  tiros5 := tiros_r5 ]  #registro en la planilla que tiran x tiros
      resultado5  <- gimnasio_tirar( ids_juegan5, tiros_r5)
      planilla_cazatalentos[ ids_juegan5,  aciertos5 := resultado5 ]  #registro en la planilla la suma de la ronda uno y la dos
      planilla_cazatalentos[ ids_juegan5,  acumulado5 := (acumulado4+aciertos5) ]  
      
      #Ronda 6 -------------------------------------------------------
      #los mejores 40 jugadores tiran x tiros cada uno
      
      
      minmed  <- planilla_cazatalentos[ ids_juegan5, quantile(acumulado5, prob=seq(0, 1, length = 101))[41] ]
      ids_juegan6  <- planilla_cazatalentos[ ids_juegan5 ][acumulado5 > minmed, id ]
      
      planilla_cazatalentos[ ids_juegan6,  tiros6 := tiros_r6 ]  #registro en la planilla que tiran x tiros
      resultado6  <- gimnasio_tirar( ids_juegan6, tiros_r6)
      planilla_cazatalentos[ ids_juegan6,  aciertos6 := resultado6 ]  #registro en la planilla la suma de la ronda uno y la dos
      planilla_cazatalentos[ ids_juegan6,  acumulado6 := (acumulado5+aciertos6) ]
      
      #Ronda 7 -------------------------------------------------------
      #los mejores 40 jugadores tiran x tiros cada uno
      
      
      minmed  <- planilla_cazatalentos[ ids_juegan6, quantile(acumulado6, prob=seq(0, 1, length = 101))[51] ]
      ids_juegan7  <- planilla_cazatalentos[ ids_juegan6 ][acumulado6 > minmed, id ]
      
      planilla_cazatalentos[ ids_juegan6,  tiros7 := tiros_r7 ]  #registro en la planilla que tiran x tiros
      resultado7  <- gimnasio_tirar( ids_juegan7, tiros_r7)
      planilla_cazatalentos[ ids_juegan7,  aciertos7 := resultado7 ]  #registro en la planilla la suma de la ronda uno y la dos
      planilla_cazatalentos[ ids_juegan7,  acumulado7 := (acumulado6+aciertos7) ]
      
      #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
      pos_mejor <-  planilla_cazatalentos[ , which.max(acumulado7) ]
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
      
    cat( tiros_r1 , tiros_r2, tiros_r3, tiros_r4, tiros_r5, tiros_r6, tiros_r7, MAX_GLOBAL_tiros_total, tasa_de_aciertos/100, "\n" )

  }
  #cat( ultima_ronda, "\t", tasa_de_aciertos/100, "\n" )
}


