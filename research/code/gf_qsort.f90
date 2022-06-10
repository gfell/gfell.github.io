! Recursive Fortran 95 quicksort routine
! sorts real numbers into ascending numerical order
! Author: Juli Rew, SCD Consulting (juliana@ucar.edu), 9/03
! Based on algorithm from Cormen et al., Introduction to Algorithms,
! 1997 printing

! Made F conformant by Walt Brainerd

! Modified by Giulio Fella (02/2013) to sort the array index accordingly 

! Contains:
! 1. Test program: 	sortdriver
! 2. Module: 		qsort_c_module declaring kind of real array and
!  containing the component subroutines: Partition, QsortC

MODULE qsort_c_module
  IMPLICIT NONE
  PUBLIC :: QsortC
  PRIVATE :: Partition

  ! Set kind of real arrays
  INTEGER, PARAMETER :: ki=SELECTED_REAL_KIND(p = 15, r = 30)

CONTAINS

  RECURSIVE SUBROUTINE QsortC(A, index_A)
    ! Input/output variables
    ! A    	: real(precision) array to sort
    ! index_A   : integer array indexing elements of A
    REAL(ki), INTENT(in out), DIMENSION(:) :: A
    INTEGER, INTENT(in out), DIMENSION(SIZE(A)) :: index_A
    INTEGER :: iq

    IF(SIZE(A) > 1) THEN
      CALL Partition(A,index_A, iq)
      CALL QsortC(A(:iq-1),index_A(:iq-1))
      CALL QsortC(A(iq:),index_A(iq:))
    ENDIF
  END SUBROUTINE QsortC

  SUBROUTINE Partition(A, index_A, marker)
    REAL(ki), INTENT(in out), DIMENSION(:) :: A
    INTEGER, INTENT(in out), DIMENSION(SIZE(A)) :: index_A
    INTEGER, INTENT(out) :: marker
    INTEGER :: i, j
    INTEGER :: index_temp
    REAL(ki) :: temp
    REAL(ki) :: x      ! pivot point
    x = A(1)
    i= 0
    j= SIZE(A) + 1

    DO
      j = j-1
      DO
        IF (A(j) <= x) EXIT
        j = j-1
      END DO
      i = i+1
      DO
        IF (A(i) >= x) EXIT
        i = i+1
      END DO
      IF (i < j) THEN
        ! exchange A(i) and A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp

        index_temp = index_A(i)
        index_A(i) = index_A(j)
        index_A(j) = index_temp

      ELSEIF (i == j) THEN
        marker = i+1
        RETURN
      ELSE
        marker = i
        RETURN
      ENDIF
    END DO

  END SUBROUTINE Partition

END MODULE qsort_c_module

PROGRAM sortdriver
  ! Test program
  USE qsort_c_module
  IMPLICIT NONE
  INTEGER, PARAMETER :: r = 10
  REAL(ki), DIMENSION(1:r) :: myarray = &        ! (1:r)
      (/0, 50, 20, 25, 90, 10, 5, 99, 99, 99/)
  INTEGER, DIMENSION(1:r) :: myindex = &
      (/1:r/)
  PRINT *, "Original array is ", myarray
  CALL QsortC(myarray,myindex)
  PRINT *, "Sorted array is ", myarray
  PRINT *, "Sorted index is ", myindex
END PROGRAM sortdriver
