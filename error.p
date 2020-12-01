set terminal pdf            #Hace que salga por png
set output 'Error.pdf'      #Establece el nombre del archivo de salida

set autoscale               # escala los ejes automaticamente
unset log                   # quita la escala logaritmica (si hubiera)
unset label                 # quita los titulos anteriores

set xtic auto               # establece las divisiones del eje x automaticamente
set ytic auto               # establece las divisiones del eje y automaticamente
set xrange[0:10]             # establece el rango a mostrar del eje x
#set yrange[0:0.1]             # establece el rango a mostrar del eje y
set grid

set title 'Error en EDO-PVI'
set xlabel "t"
set ylabel "y"

#LAS POSICIONES DE LOS ERRORES Y LA TOLERANCIA
#VARIAN SEGUN LA CANTIDAD DE ECUACIONES
#tal vez deberian ir adelante pero capaz seria confuso de ver el resto
plot    "EDOPVI_TOL.txt" using 1:4 title 'Error' with lines lw 2 lc 4,\
        "EDOPVI_TOL.txt" using 1:5 title 'Tolerancia' with lines lw 2 lc 5,\
        "EDOPVI_TOL.txt" using 1:6 title 'Error acumulado' with lines lw 2 lc 6
        
#using 1:2 son las columnas de las que se van a tomar valores para x e y creo.
#lw es line width o ancho de linea
#lc es line color o color de linea
#title es el nombre que va a tener la funcion en la leyenda
#con ",\" se separan las distintas lineas del plot (las distintas funciones a plotear)
