! *********************************COPYRIGHT************************************
! (C) Crown copyright Met Office. All rights reserved.                       
! For further details please refer to the file LICENCE.txt                   
! which you should have received as part of this distribution.               
! *********************************COPYRIGHT************************************
!                                                                            
! This file is part of the UM Shared Library project.                        
!                                                                            
! The UM Shared Library is free software: you can redistribute it            
! and/or modify it under the terms of the Modified BSD License, as           
! published by the Open Source Initiative.                                   
!                                                                            
! The UM Shared Library is distributed in the hope that it will be           
! useful, but WITHOUT ANY WARRANTY; without even the implied warranty        
! of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           
! Modified BSD License for more details.                                     
!                                                                            
! You should have received a copy of the Modified BSD License                
! along with the UM Shared Library.                                          
! If not, see <http://opensource.org/licenses/BSD-3-Clause>.                 
!*******************************************************************************
PROGRAM test_byteswap

USE ISO_C_BINDING, ONLY: C_INT64_T

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap, f_shum_get_machine_endianism, &
                               f_shum_littleendian, f_shum_bigendian

IMPLICIT NONE 

! -----------------------------------------------------------------------------!
! 64 and 32-bit real types; since the ISO_C_BINDING module does not yet provide
! these (for integers it does)

! Precision and range for 64 bit real
INTEGER, PARAMETER :: prec64  = 15
INTEGER, PARAMETER :: range64 = 307

! Precision and range for 32 bit real
INTEGER, PARAMETER :: prec32  = 6
INTEGER, PARAMETER :: range32 = 37

! Kind for 64 bit real
INTEGER, PARAMETER :: real64  = SELECTED_REAL_KIND(prec64,range64)
! Kind for 32 bit real
INTEGER, PARAMETER :: real32  = SELECTED_REAL_KIND(prec32,range32)
! -----------------------------------------------------------------------------!

INTEGER(C_INT64_T) :: len
REAL(real64), ALLOCATABLE :: data_in(:)
INTEGER(C_INT64_T) :: status
INTEGER(C_INT64_T) :: word_size = 8
INTEGER(C_INT64_T) :: i

PRINT*, REPEAT("*", 70)
PRINT*, "Testing byte-swapping library"
PRINT*, REPEAT("*", 70)

len = 10
ALLOCATE(data_in(len))

DO i = 1, len
  data_in(i) = REAL(i)
END DO

SELECT CASE (f_shum_get_machine_endianism())
  CASE(f_shum_littleendian)
    PRINT*, "MACHINE IS LITTLE ENDIAN"
  CASE(f_shum_bigendian)
    PRINT*, "MACHINE IS BIG ENDIAN"
  CASE DEFAULT
    PRINT*, "ERROR #1: Machine endianism check failed"
END SELECT

PRINT*, "ORIGINAL DATA:"
PRINT*, data_in

status = f_shum_byteswap(data_in, len, word_size)
IF (status /= 0) THEN
  PRINT*, "ERROR #2: Failed to byteswap first time"
END IF

PRINT*, "BYTE SWAPPED:"
PRINT*, data_in

status = f_shum_byteswap(data_in, len, word_size)
IF (status /= 0) THEN
  PRINT*, "ERROR #3: Failed to byteswap second time"
END IF

PRINT*, "BYTE SWAPPED AGAIN:"
PRINT*, data_in

DEALLOCATE(data_in)

PRINT*, REPEAT("*", 70)

END PROGRAM test_byteswap
