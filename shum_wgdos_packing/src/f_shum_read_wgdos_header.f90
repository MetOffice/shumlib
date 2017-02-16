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
  C_LOC, C_PTR, C_INT64_T, C_INT32_T, C_CHAR, C_FLOAT, C_DOUBLE

USE f_shum_string_conv_mod, ONLY: f_shum_c2f_string

IMPLICIT NONE

PRIVATE

PUBLIC ::                                                                      &
  f_shum_read_wgdos_header

!------------------------------------------------------------------------------!
! We're going to use the types from the ISO_C_BINDING module, since although   !
! the REALs aren't 100% guaranteed to correspond to the sizes we want to       !
! enforce, they should be good enough on the majority of systems.              !
!                                                                              !
! Additional protection for the case that FLOAT/DOUBLE do not conform to the   !
! sizes we expect is provided via the "precision_bomb" macro-file              !
!------------------------------------------------------------------------------!
  INTEGER, PARAMETER :: int64  = C_INT64_T
  INTEGER, PARAMETER :: int32  = C_INT32_T
  INTEGER, PARAMETER :: real64 = C_DOUBLE
  INTEGER, PARAMETER :: real32 = C_FLOAT                                       
!------------------------------------------------------------------------------!
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

!------------------------------------------------------------------------------!

INTERFACE f_shum_read_wgdos_header
MODULE PROCEDURE                                                               &
  shum_read_wgdos_header_real64,                                               &
  shum_read_wgdos_header_integer64,                                            &
  shum_read_wgdos_header_real32,                                               &
  shum_read_wgdos_header_integer32
END INTERFACE

CONTAINS

!------------------------------------------------------------------------------!

FUNCTION shum_read_wgdos_header_real64(data_in, accuracy, cols, rows, message)
                          
IMPLICIT NONE

INTEGER(KIND=int64) ::                                                         &
  shum_read_wgdos_header_real64

REAL(KIND=real64), TARGET, INTENT(IN) ::                                       &
  data_in(*)

INTEGER(KIND=int64), INTENT(OUT) ::                                            &
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

INTEGER(KIND=int64) ::                                                         &
  shum_read_wgdos_header_integer64

INTEGER(KIND=int64), TARGET, INTENT(IN) ::                                     &
  data_in(*)

INTEGER(KIND=int64), INTENT(INOUT) ::                                          &
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

INTEGER(KIND=int64) ::                                                         &
  shum_read_wgdos_header_real32

REAL(KIND=real32), TARGET, INTENT(IN) ::                                       &
  data_in(*)

INTEGER(KIND=int64), INTENT(INOUT) ::                                          &
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

INTEGER(KIND=int64) ::                                                         &
  shum_read_wgdos_header_integer32

INTEGER(KIND=int32), TARGET, INTENT(IN) ::                                     &
  data_in(*)

INTEGER(KIND=int64), INTENT(INOUT) ::                                          &
  accuracy, cols, rows

CHARACTER(LEN=*), INTENT(INOUT) :: message
CHARACTER(KIND=C_CHAR,LEN=1), ALLOCATABLE  :: cmessage(:)

ALLOCATE(cmessage(LEN(message)))

shum_read_wgdos_header_integer32 = c_shum_read_wgdos_header(                   &
        C_LOC(data_in(1)), accuracy, cols, rows, cmessage)

message = f_shum_c2f_string(cmessage)

END FUNCTION shum_read_wgdos_header_integer32

!------------------------------------------------------------------------------!

END MODULE f_shum_read_wgdos_header_mod

