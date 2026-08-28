      SUBROUTINE DUMP_IMPLICIT_DATA(NDDL, NNZK, IADK, JDIK, DIAG_K,
     .                              LT_K, NDDLI, NNZI, IADI, JDII,
     .                              ITOK, DIAG_I, LT_I, LB, NUMNOD,
     .                              IDDL, NDOF, MS, IN)
#include "implicit_f.inc"

        INTEGER NDDL, NNZK, NDDLI, NNZI, NUMNOD
        INTEGER IADK(*), JDIK(*), ITOK(*)
        INTEGER IADI(*), JDII(*)
        INTEGER IDDL(*), NDOF(*)
        my_real DIAG_K(*), LT_K(*), DIAG_I(*), LT_I(*)
        my_real LB(*), MS(*), IN(*)

        INTEGER UNIT_K, UNIT_M, IERR, I, J, P, NODE, START_DOF, NUM_DOF
        my_real VAL

        WRITE(6,*) '*** EXPORTING STIFFNESS AND MASS MATRICES ***'
        CALL FLUSH(6)
        
        ! Prevent unused dummy argument warnings
        IF (NNZK > 0 .AND. NDDLI > 0 .AND. NNZI > 0) THEN
           VAL = IADI(1) + JDII(1) + ITOK(1) + DIAG_I(1) + LT_I(1) + LB(1)
        END IF

        ! ----------------------------------------------------
        ! 1. Export Stiffness Matrix K (Coordinate Format)
        ! ----------------------------------------------------
        OPEN(NEWUNIT=UNIT_K, FILE='K_matrix.txt', STATUS='UNKNOWN')
        
        ! Write Diagonals
        DO I = 1, NDDL
           WRITE(UNIT_K, *) I, I, DIAG_K(I)
        END DO
        
        ! Write Off-Diagonals (Symmetric)
        DO I = 1, NDDL
           DO P = IADK(I), IADK(I+1)-1
              J = JDIK(P)
              WRITE(UNIT_K, *) I, J, LT_K(P)
              WRITE(UNIT_K, *) J, I, LT_K(P)
           END DO
        END DO
        CLOSE(UNIT_K)

        ! ----------------------------------------------------
        ! 2. Export Mass Matrix M (Coordinate Format)
        ! ----------------------------------------------------
        OPEN(NEWUNIT=UNIT_M, FILE='M_matrix.txt', STATUS='UNKNOWN')
        
        DO NODE = 1, NUMNOD
           START_DOF = IDDL(NODE) - 1
           NUM_DOF = NDOF(NODE)
           DO J = 1, NUM_DOF
              I = START_DOF + J
              IF (I >= 1 .AND. I <= NDDL) THEN
                 IF (J <= 3) THEN
                    VAL = MS(NODE)  ! m1, m2, m3
                 ELSE
                    VAL = IN(NODE)  ! ix, iy, iz
                 END IF
                 WRITE(UNIT_M, *) I, I, VAL
              END IF
           END DO
        END DO
        CLOSE(UNIT_M)

        WRITE(6,*) '*** MATRICES EXPORTED TO K_matrix.txt & M_matrix.txt ***'
        CALL FLUSH(6)
      END
