MODULE EDO_SOLVER    
    USE EDO_SETUP
    IMPLICIT NONE
    CHARACTER(*), PARAMETER :: FORMATO = 'F25.8)'
    !Al Formato le falta el parentesis adelate porque se lo agrega despues junto con la cantidad de floats
CONTAINS
    !Subrutina que escribe en un archivo los valores correspondientes a X y las correspondientes f(x,y,y'...)
    !Usa un paso fijo H que se repite MAXITER veces
    !VI es el vector de valores iniciales
    !H es el paso en X fijo que se usa en cada iteracion
    !ARCHIVO es un nombre opcional para el archivo donde se guardan los resultados.
    SUBROUTINE EDOPVI_ITER(VI, H, MAXITER, ARCHIVO)
        REAL(8), DIMENSION(0:CANT_EC), INTENT(IN) :: VI
        REAL(8), INTENT(IN) :: H
        INTEGER, INTENT(IN) :: MAXITER
        CHARACTER(*), INTENT(IN), OPTIONAL :: ARCHIVO
        !Local
        CHARACTER(*), PARAMETER :: ARCH_DEF = 'EDOPVI_ITER.txt'
        REAL(8), DIMENSION(0:CANT_EC) :: V
        REAL(8), DIMENSION(0:CANT_EC) :: ERROR !RKF
        INTEGER :: I
        !Preparacion
        CHARACTER(LEN = 50) :: ARCH, F !por lo visto character(:), allocatable es de fortran2003
        ARCH = ARCH_DEF
        IF (PRESENT(ARCHIVO)) ARCH = ARCHIVO
        WRITE(F, '(A1, I0, A)') '(', CANT_EC+1, FORMATO !FORMATO es una variable del modulo I0 agarra los digitos necesarios.
        OPEN(UNIT = 1, FILE = ARCH, ACTION = 'WRITE')
        
        !Principal
        V = VI
        WRITE(1,F) V !escribir los valores iniciales tambien
        DO I = 1, MAXITER
            V = H_EULERMODIFICADO(V, H)
            !V = H_RKF(V, H, ERROR)
            WRITE(1,F) V
        END DO
        !
        CLOSE(1)
    END SUBROUTINE
    
    SUBROUTINE EDOPVI_COND(VI, H, ARCHIVO)
        REAL(8), DIMENSION(0:CANT_EC), INTENT(IN) :: VI
        REAL(8), INTENT(IN) :: H
        CHARACTER(*), INTENT(IN), OPTIONAL :: ARCHIVO
        !Local
        CHARACTER(*), PARAMETER :: ARCH_DEF = 'EDOPVI_COND.txt'
        REAL(8), DIMENSION(0:CANT_EC) :: V
        REAL(8), DIMENSION(0:CANT_EC) :: ERROR !RKF
        INTEGER :: I, LIM
        !Preparacion
        CHARACTER(LEN = 50) :: ARCH, F !por lo visto character(:), allocatable es de fortran2003
        ARCH = ARCH_DEF
        IF (PRESENT(ARCHIVO)) ARCH = ARCHIVO
        WRITE(F, '(A1, I0, A)') '(', CANT_EC+1, FORMATO !FORMATO es una variable del modulo I0 agarra los digitos necesarios.
        OPEN(UNIT = 1, FILE = ARCH, ACTION = 'WRITE')
        
        I = 0; LIM = 100000
        !Principal
        V = VI
        WRITE(1,F) V !escribir los valores iniciales tambien
        !Ese sum se fija si la suma de E, A e I son básicamente 0.
        DO WHILE((SUM(V(2:4)) > 0.00001) .AND. I < LIM)
            V = H_RK4(V, H)
            !V = H_RKF(V, H, ERROR)
            WRITE(1,F) V
            I = I + 1
        END DO
        !
        IF (I >= LIM) THEN
            PRINT *, 'Te salve la MV, no terminaba mas sino.'
            WRITE(*,*) 'Se hicieron ', I, ' iteraciones.'
        END IF
        CLOSE(1)
    END SUBROUTINE
    
    !Subrutina que escribe en un archivo los valores correspondientes a X y las correspondientes f(x,y,y'...)
    !Que ajusta el tamaño del paso segun una tolerancia pasada por parametro.
    !VI es el vector de valores iniciales
    !HI es el paso inicial sobre el cual se basa el H que se va a modificar a cada paso
    !XN es el valor de X al que se quiere llegar, se usa para saber cuando se llega a destino
    !ARCHIVO es un nombre opcional para el archivo donde se guardan los resultados.
    SUBROUTINE EDOPVI_TOL(VI, HI, XN, TOL, ARCHIVO)
        REAL(8), DIMENSION(0:CANT_EC), INTENT(IN) :: VI
        REAL(8), INTENT(IN) :: HI, TOL, XN
        CHARACTER(*), INTENT(IN), OPTIONAL :: ARCHIVO
        !Local
        CHARACTER(*), PARAMETER :: ARCH_DEF = 'EDOPVI_TOL.txt'
        REAL(8), DIMENSION(0:CANT_EC) :: V
        REAL(8) :: H, ER, ERRORMAX, ERRORACUM
        INTEGER :: I, LIM
        !Preparacion
        CHARACTER(LEN = 50) :: ARCH, FV, FE !por lo visto character(:), allocatable es de fortran2003
        ARCH = ARCH_DEF
        IF (PRESENT(ARCHIVO)) ARCH = ARCHIVO
        WRITE(FV, '(A1, I0, A)') '(', CANT_EC+1, FORMATO !FORMATO es una variable del modulo I0 agarra los digitos necesarios.
        WRITE(FE, '(A1, I0, A)') '(', 3, FORMATO
        OPEN(UNIT = 1, FILE = ARCH, ACTION = 'WRITE')
        
        I = 0; LIM = 100000; ER = 0.000001!Error de representacion
        !Principal
        V = VI; H = HI; ERRORMAX = 0 !porque VI es exacto.
        ERRORACUM = 0
        WRITE(1,FV, ADVANCE = 'NO') V
        WRITE(1,FE) ERRORMAX, TOL, ERRORACUM
        DO WHILE(V(0)+ER < XN .AND. I < LIM)
            !Modifican V y H y también devuelven ERROR
            CALL ESTRAT1(V, H, TOL, ERRORMAX) 
            !CALL ESTRAT2(V, H, TOL, ERRORMAX) !Se modifica el tamaño del siguiente paso segun el error y la tolerancia.
            WRITE(1,FV, ADVANCE = 'NO') V
            
            ERRORACUM = ERRORACUM + ERRORMAX
            WRITE(1,FE) ERRORMAX, TOL, ERRORACUM
            
            I = I + 1 !Solo existe para que no me explote el disco
        END DO
        
        IF (I >= LIM) THEN
            PRINT *, 'Te salve la MV, no terminaba mas sino.'
            WRITE(*,*) 'Se hicieron ', I, ' iteraciones.'
        END IF
        
        CLOSE(1)
    END SUBROUTINE
    
    !Subrutina que toma un vector de variable, funcion y sus derivadas y devuelve su valor en el proximo paso.
    !Este paso es ajutado a la tolerancia pasada por parametro.
    !---Sobreescribe los .valores de V y H.---
    !V: vector de valores de la variable, funcion y sus derivadas en un paso determinado.
    !H: valor del paso de X de la iteracion anterior. Este valor se ajusta segun una estimacion del error y la tolerancia.
    !TOL: valor de la tolerancia máxima al error que se desea.
    SUBROUTINE ESTRAT1(V, H, TOL, ERRORMAX)
        REAL(8), DIMENSION(0:CANT_EC), INTENT(INOUT) :: V
        REAL(8), INTENT(INOUT) :: H
        REAL(8), INTENT(IN) :: TOL
        REAL(8), INTENT(OUT) :: ERRORMAX !Si bien el ERROR es un vector, toma directamente el maxval
        !Local
        REAL(8), DIMENSION(0:CANT_EC) :: V1, V2, ERROR
        
        ERRORMAX = 2*TOL !Valor imposible porque no hay do{}while(); salvo con GOTOs.
        DO WHILE(ERRORMAX > TOL)
            V1 = H_EULERMODIFICADO(V, H)
            V2 = H_EULERMODIFICADO(V,  H/2.)
            V2 = H_EULERMODIFICADO(V2, H/2.)
            !V1 = H_RKF(V, H, ERROR)
            ERROR = V1-V2 !Comentar si H_RKF.
            ERRORMAX = MAXVAL(ABS(ERROR))
            IF (ERRORMAX > TOL) THEN
                H = H / 2.
                PRINT *, 'Se achico el paso.'
            END IF
        END DO
        V = V1
    END SUBROUTINE
    
    !A diferencia de la Estrategia 1, esta directamente realiza el paso
    !y devuelve el error estimado de RKF, y compara ese error con la tolerancia
    !para ajustarlo en el proximo paso.
    !Por eso a veces se pasa de la tolerancia, porque ajusta el paso
    !pero para el siguiente, no recalcula el actual (no se por que)
    SUBROUTINE ESTRAT2(V, H, TOL, ERRORMAX)
        REAL(8), DIMENSION(0:CANT_EC), INTENT(INOUT) :: V
        REAL(8), INTENT(INOUT) :: H
        REAL(8), INTENT(IN) :: TOL
        REAL(8), INTENT(OUT) :: ERRORMAX
        !
        REAL(8), PARAMETER :: ALFAMENOR = 0.2, ALFAMAYOR = 0.22
        REAL(8) :: ALFA, ERROR(0:CANT_EC)
        
        V = H_RKF(V, H, ERROR) !Se hace un paso para saber el error de RKF.
        ERRORMAX = MAXVAL(ABS(ERROR))!Uso MAXVAL(ERROR) porque RKF devuelve un vector de errores.
        PRINT *, ERROR
        PRINT *, ERRORMAX
        IF (ERRORMAX > TOL) THEN
            ALFA = ALFAMENOR
        ELSE
            ALFA = ALFAMAYOR
        END IF
        H = H*(ABS(TOL/ERRORMAX)**ALFA) !comentar para un rkf que imprime errores pero no ajusta.
    END SUBROUTINE
    
    !Los H_Metodos realizan el calculo de un metodo en un paso H
    
    FUNCTION H_EULERSIMPLE(V, H)
        REAL(8) :: H_EULERSIMPLE(0:CANT_EC)
        REAL(8), INTENT(IN) :: V(:), H
        
         H_EULERSIMPLE = V + H*V_PRIMA(V)
    END FUNCTION
    
    FUNCTION H_EULERMODIFICADO(V, H)
        REAL(8) :: H_EULERMODIFICADO(0:CANT_EC)
        REAL(8), INTENT(IN) :: V(:), H
        
        REAL(8), DIMENSION(0:CANT_EC) :: VP
        
        VP = V_PRIMA(V)
        H_EULERMODIFICADO = V + H*(VP + V_PRIMA(V + H*VP))/2. !ese V_PRIMA(V + H*VP) es un euler simple
    END FUNCTION
    
    FUNCTION H_EULERMEJORADO(V, H)
        REAL(8) :: H_EULERMEJORADO(0:CANT_EC)
        REAL(8), INTENT(IN) :: V(:), H
        
        REAL(8), DIMENSION(0:CANT_EC) :: VINT
        REAL(8) :: HINT
        HINT = H/2.
        
        VINT = H_EULERSIMPLE(V, HINT) !Se aproxima un valor intermedio con euler simple.
        H_EULERMEJORADO = V + H*V_PRIMA(VINT)
    END FUNCTION
    
    FUNCTION H_RK4(V, H)
        REAL(8) :: H_RK4(0:CANT_EC)
        REAL(8), INTENT(IN) :: V(:), H
        
        REAL(8), DIMENSION(0:CANT_EC) :: K1, K2, K3, K4
        
        K1 = H*V_PRIMA(V)
        K2 = H*V_PRIMA(V + K1/2.)
        K3 = H*V_PRIMA(V + K2/2.)
        K4 = H*V_PRIMA(V + K3)
        
        H_RK4 = V + (K1 + 2.*K2 + 2.*K3 + K4)/6.
    END FUNCTION
    
    FUNCTION H_RKF(V, H, ERROR)
        REAL(8) :: H_RKF(0:CANT_EC)
        REAL(8), INTENT(IN) :: V(0:CANT_EC), H
        REAL(8), INTENT(OUT) :: ERROR(0:CANT_EC)
        
        REAL(8), DIMENSION(0:CANT_EC) :: K1, K2, K3, K4, K5, K6
        
        K1 = H*V_PRIMA(V)
        K2 = H*V_PRIMA(V + K1/4.0)
        K3 = H*V_PRIMA(V + (3.0*K1 + 9.0*K2)/32.0)
        K4 = H*V_PRIMA(V + (1932.0*K1 - 7200.0*K2 + 7296.0*K3)/2197.0)
        K5 = H*V_PRIMA(V + 439.0*K1/216.0 - 8.0*K2 + 3680.0*K3/513.0 - 845.0*K4/4104.0)
        K6 = H*V_PRIMA(V -8.0*K1/27.0 + 2.0*K2 - 3544.0*K3/2565.0 + 1859.0*K4/4104.0 - 11.0*K5/40.0)
        ERROR = K1/360.0 - 128.0*K3/4275.0 - 2197.0*K4/75240.0 + K5/50.0 + 2.0*K6/55.0 
        
        H_RKF = V + (25.0*K1/216.0 + 1408.0*K3/2565.0 + 2197.0*K4/4104.0 - K5/5.0)
    END FUNCTION
END MODULE
