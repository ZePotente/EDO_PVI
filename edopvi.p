set terminal pdf            #Hace que salga por png
set output 'edopvi.pdf'       #Establece el nombre del archivo de salida

set autoscale               # escala los ejes automaticamente
unset log                   # quita la escala logaritmica (si hubiera)
unset label                 # quita los titulos anteriores

set xtic auto               # establece las divisiones del eje x automaticamente
set ytic auto               # establece las divisiones del eje y automaticamente
#set xrange[0:500]             # establece el rango a mostrar del eje x
#set yrange[0:1000]             # establece el rango a mostrar del eje y
set grid

set title 'RK4 tol=0.005 hi=0.0125'
set xlabel "t"
set ylabel "y"

plot    "EDOPVI_TOL.txt" using 1:3 title "v(t)" with lines lw 2 lc 1,\
        #"EDOPVI_TOL.txt" using 1:3 title "v1" with lines lw 2 lc 2,\
        #"EDOPVI_TOL.txt" using 1:4 title "y2" with lines lw 2 lc 3,\
        #"EDOPVI_TOL.txt" using 1:5 title "v2" with lines lw 2 lc 4,\
        #"EDOPVI_TOL.txt" using 1:6 title "y3" with lines lw 2 lc 5,\
        #"EDOPVI_TOL.txt" using 1:7 title "v3" with lines lw 2 lc 6,\
        
#using 1:2 son las columnas de las que se van a tomar valores para x e y creo.
#lw es line width o ancho de linea
#lc es line color o color de linea
#title es el nombre que va a tener la funcion en la leyenda
#con ",\" se separan las distintas lineas del plot (las distintas funciones a plotear)
