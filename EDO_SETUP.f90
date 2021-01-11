MODULE EDO_SETUP
    IMPLICIT NONE
!---------------CAMBIAR LA CANTIDAD DE ECUACIONES---------------!    
    INTEGER, PARAMETER :: CANT_EC = 2 !Cantidad de Ecuaciones (o sea sin contar X)
!---------------CAMBIAR LA CANTIDAD DE ECUACIONES---------------!    
CONTAINS
    FUNCTION V_PRIMA(V)
    ! Definicion de la Ecuacion Diferencial
        REAL(8), DIMENSION(0:CANT_EC) :: V_PRIMA
        REAL(8), DIMENSION(0:CANT_EC), INTENT(IN) :: V
        !Constantes:
        REAL(8) :: M, G, R
        M = 100.
        R = 1.
        G = 9.8
        
        !Derivadas:
        !la derivada de t (o sea V(0))
        V_PRIMA(0) = 1. ! dx/dx = 1.
        !el resto de las derivadas (se puede hacer en cualquier orden siempre que esten en el mismo orden que ARMARVI()
        !y1 y sus derivadas
        V_PRIMA(1) = V(2) !y' = z
        V_PRIMA(2) = -G + (R*V(2)**2)/M !y'' = z'
        !y2 y sus derivadas
        
        !y3 y sus derivadas
        
    END FUNCTION
    
    FUNCTION ARMARVI()
        REAL(8), DIMENSION(0:CANT_EC) :: ARMARVI
        !Variable independiente
        ARMARVI(0) = 0. !t

        !y1(t) y sus derivadas en orden (sus valores iniciales)
        ARMARVI(1) = 0. !y
        ARMARVI(2) = 20. !y' = z
        !y2(t) y sus derivadas en orden (sus valores iniciales)
        
        !y3(t) y sus derivadas en orden (sus valores iniciales)
        
    END FUNCTION
END MODULE
