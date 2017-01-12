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
! This module contains the interfaces to call c code within fortran.
!
MODULE f_shum_read_wgdos_header_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_LOC, C_PTR, C_INT64_T, C_INT32_T, C_CHAR

USE f_shum_string_conv_mod, ONLY: f_shum_c2f_string

IMPLICIT NONE

PRIVATE

PUBLIC ::                                                                      &
  f_shum_read_wgdos_header

!! -----------------------------------------------------------------------------!
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
INTEGER, PARAMETER :: real64 = SELECTED_REAL_KIND(prec64,range64)
! Kind for 32 bit real
INTEGER, PARAMETER :: real32 = SELECTED_REAL_KIND(prec32,range32)
! -----------------------------------------------------------------------------!
! Interfaces

! C Interfaces

INTERFACE
FUNCTION c_shum_read_wgdos_header (bytes_in, accuracy, cols, rows, message)    &
                                   BIND(c, NAME="c_shum_read_wgdos_header")

IMPORT :: C_INT64_T, C_PTR, C_CHAR

IMPLICIT NONE

INTEGER(KIND=C_INT64_T) ::                                                     &
    c_shum_read_wgdos_header

TYPE(C_PTR), INTENT(IN), VALUE ::                                              &
    bytes_in

INTEGER(KIND=C_INT64_T), INTENT(INOUT) ::                                      &
    accuracy,                                                                  &
    cols,                                                                      &
    rows

CHARACTER(KIND=C_CHAR, LEN=1), INTENT(INOUT) :: message(*)

END FUNCTION c_shum_read_wgdos_header
END INTERFACE

INTERFACE f_shum_read_wgdos_header
MODULE PROCEDURE                                                               &
  shum_read_wgdos_header_real64,                                               &
  shum_read_wgdos_header_integer64,                                            &
  shum_read_wgdos_header_real32,                                               &
  shum_read_wgdos_header_integer32
END INTERFACE

CONTAINS

! -----------------------------------------------------------------------------!

FUNCTION shum_read_wgdos_header_real64(data_in, accuracy, cols, rows, message)
                          
IMPLICIT NONE

INTEGER(KIND=C_INT64_T) ::                                                     &
  shum_read_wgdos_header_real64

REAL(KIND=real64), TARGET, INTENT(IN) ::                                       &
  data_in(*)

INTEGER(KIND=C_INT64_T), INTENT(OUT) ::                                        &
  accuracy, cols, rows

CHARACTER(LEN=*), INTENT(INOUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)))

shum_read_wgdos_header_real64 = c_shum_read_wgdos_header(                      &
        C_LOC(data_in(1)), accuracy, cols, rows, cmessage)

message = f_shum_c2f_string(cmessage)

END FUNCTION shum_read_wgdos_header_real64

!------------------------------------------------------------------------------!

FUNCTION shum_read_wgdos_header_integer64(data_in, accuracy, cols, rows,       &
                                          message)
                          
IMPLICIT NONE

INTEGER(KIND=C_INT64_T) ::                                                     &
  shum_read_wgdos_header_integer64

INTEGER(KIND=C_INT64_T), TARGET, INTENT(IN) ::                                 &
  data_in(*)

INTEGER(KIND=C_INT64_T), INTENT(INOUT) ::                                      &
  accuracy, cols, rows

CHARACTER(LEN=*), INTENT(INOUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)))

shum_read_wgdos_header_integer64 = c_shum_read_wgdos_header(                   &
        C_LOC(data_in(1)), accuracy, cols, rows, cmessage)

message = f_shum_c2f_string(cmessage)

END FUNCTION shum_read_wgdos_header_integer64

!------------------------------------------------------------------------------!

FUNCTION shum_read_wgdos_header_real32(data_in, accuracy, cols, rows, message)
                          
IMPLICIT NONE

INTEGER(KIND=C_INT64_T) ::                                                     &
  shum_read_wgdos_header_real32

REAL(KIND=real32), TARGET, INTENT(IN) ::                                       &
  data_in(*)

INTEGER(KIND=C_INT64_T), INTENT(INOUT) ::                                      &
  accuracy, cols, rows

CHARACTER(LEN=*), INTENT(INOUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)))

shum_read_wgdos_header_real32 = c_shum_read_wgdos_header(                      &
        C_LOC(data_in(1)), accuracy, cols, rows, cmessage)

message = f_shum_c2f_string(cmessage)

END FUNCTION shum_read_wgdos_header_real32

!------------------------------------------------------------------------------!

FUNCTION shum_read_wgdos_header_integer32(data_in, accuracy, cols, rows,       &
                                          message)
                          
IMPLICIT NONE

INTEGER(KIND=C_INT64_T) ::                                                     &
  shum_read_wgdos_header_integer32

INTEGER(KIND=C_INT32_T), TARGET, INTENT(IN) ::                                 &
  data_in(*)

INTEGER(KIND=C_INT64_T), INTENT(INOUT) ::                                      &
  accuracy, cols, rows

CHARACTER(LEN=*), INTENT(INOUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)))

shum_read_wgdos_header_integer32 = c_shum_read_wgdos_header(                   &
        C_LOC(data_in(1)), accuracy, cols, rows, cmessage)

message = f_shum_c2f_string(cmessage)

END FUNCTION shum_read_wgdos_header_integer32

END MODULE f_shum_read_wgdos_header_mod

