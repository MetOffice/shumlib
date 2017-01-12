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
PROGRAM test_packing

USE f_shum_read_wgdos_header_mod, ONLY: f_shum_read_wgdos_header
USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack, f_shum_wgdos_unpack

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T

IMPLICIT NONE 

! -----------------------------------------------------------------------------!
! 64 and 32-bit real types; since the ISO_C_BINDING module does not yet provide
! these (for integers it does)
!
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

INTEGER(C_INT64_T) :: len1, len2
REAL(real64), ALLOCATABLE :: data_in(:,:)
REAL(real64), ALLOCATABLE :: data_check(:,:)
INTEGER(C_INT64_T) :: status, num_words, accuracy
INTEGER(C_INT64_T) :: acc, cols, rows
REAL(real64) :: mdi
INTEGER(C_INT64_T) :: i, j, k
INTEGER(C_INT64_T), ALLOCATABLE :: packed(:)

CHARACTER(LEN=500) :: message

PRINT*, REPEAT("*", 70)
PRINT*, "Testing packing library"
PRINT*, REPEAT("*", 70)

accuracy = 1
mdi = -99.0
len1 = 10
len2 = 20
ALLOCATE(data_in(len1, len2))
ALLOCATE(data_check(len1, len2))

k = 0
DO j = 1, len2
  DO i = 1, len1
    k = k + 1
    data_in(i, j) = REAL(k, KIND=real64)
  END DO
END DO

data_check(:,:) = 0.0

PRINT*, "ORIGINAL DATA:"
PRINT*, data_in

ALLOCATE(packed(len1*len2))
packed(:) = 0

status = f_shum_wgdos_pack(data_in, packed, len1*len2, len1, len2, num_words, &
                           accuracy, mdi, message)
IF (status /= 0) THEN
  PRINT*, "ERROR packing data:"
  PRINT*, TRIM(message)
  STOP
END IF

PRINT*, "PACKED:"
PRINT*, packed(1:(num_words+1)/2)

status = f_shum_read_wgdos_header(packed, acc, cols, rows, message)

PRINT*, "READ HEADER:"
PRINT*, acc, cols, rows

status = f_shum_wgdos_unpack(data_check, packed, len1*len2, len1, len2, &
                             accuracy, mdi, message)
IF (status /= 0) THEN
  PRINT*, "ERROR unpacking data:"
  PRINT*, TRIM(message)
  STOP
END IF

PRINT*, "UNPACKED:"
PRINT*, data_check

status = f_shum_wgdos_pack(data_check, packed, len1*len2, len1, len2, &
                           num_words, accuracy, mdi, message)
IF (status /= 0) THEN
  PRINT*, "ERROR packing data:"
  PRINT*, TRIM(message)
  STOP
END IF

PRINT*, "PACKED (AGAIN):"
PRINT*, packed(1:(num_words+1)/2)

status = f_shum_wgdos_unpack(data_check, packed, len1*len2, len1, len2, &
                             accuracy, mdi, message)
IF (status /= 0) THEN
  PRINT*, "ERROR unpacking data:"
  PRINT*, TRIM(message)
  STOP
END IF

PRINT*, "UNPACKED:"
PRINT*, data_check

DEALLOCATE(packed)
DEALLOCATE(data_check)
DEALLOCATE(data_in)


PRINT*, REPEAT("*", 70)

END PROGRAM test_packing
