PROGRAM EDO_PVI
    !Modulo
    USE EDO_SETUP
    USE EDO_SOLVER
    
    IMPLICIT NONE
    !CHARACTER(*), PARAMETER ::  ARCHES = 'EulerSimple.txt', ARCHEMOD = 'EulerModificado.txt', ARCHEMEJ = 'EulerMejorado.txt'
    !CHARACTER(*), PARAMETER ::  ARCHRK4 = 'RK4.txt', ARCHRKF = 'RKF.txt'
    
    REAL(8), DIMENSION(0:CANT_EC) :: VI
    REAL(8) :: H
    !Inicializado
    H = 0.0125;
    VI = ARMARVI()
    !
!    CALL EJECUTAR_ITER(VI, H)
!    CALL EJECUTAR_COND(VI, H)
    CALL EJECUTAR_TOL(VI, H)
    CALL SYSTEM("gnuplot edopvi.p")

CONTAINS
    !Se separaron las dos formas de resolver para aislarlas y no mezclar variables
    SUBROUTINE EJECUTAR_ITER(VI, H)
        REAL(8), DIMENSION(0:CANT_EC), INTENT(IN) :: VI
        REAL(8), INTENT(IN) :: H
        !
        INTEGER :: MAXITER
        MAXITER = 80000
        CALL EDOPVI_ITER(VI, H, MAXITER)
        
    END SUBROUTINE
    
    SUBROUTINE EJECUTAR_COND(VI, H)
        REAL(8), DIMENSION(0:CANT_EC), INTENT(IN) :: VI
        REAL(8), INTENT(IN) :: H
        
        CALL EDOPVI_COND(VI, H)
    END SUBROUTINE
    
    SUBROUTINE EJECUTAR_TOL(VI, HI)
        REAL(8), DIMENSION(0:CANT_EC), INTENT(IN) :: VI
        REAL(8), INTENT(IN) :: HI
        !
        REAL(8) :: XN, TOL
        
        !Inicializado
        XN = 100
        TOL = 0.005
        !
        CALL EDOPVI_TOL(VI, HI, XN, TOL)
        CALL SYSTEM("gnuplot error.p")
    END SUBROUTINE
END PROGRAM
