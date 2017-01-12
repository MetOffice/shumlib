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
MODULE f_shum_byteswap_mod

! DEPENDS ON: c_shum_byteswap.o

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_LOC, C_PTR, C_INT64_T

IMPLICIT NONE

PRIVATE

PUBLIC ::                                                                      &
  f_shum_byteswap,                                                             &
  f_shum_get_machine_endianism,                                                &
  f_shum_endianness,                                                           &
  f_shum_bigendian,                                                            &
  f_shum_littleendian,                                                         &
  f_shum_numendians

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
INTEGER, PARAMETER :: real64 = SELECTED_REAL_KIND(prec64,range64)
! Kind for 32 bit real
INTEGER, PARAMETER :: real32  = SELECTED_REAL_KIND(prec32,range32)

! -----------------------------------------------------------------------------!

ENUM, BIND(c)
ENUMERATOR ::                                                                  &
  f_shum_bigendian,                                                            &
  f_shum_littleendian,                                                         &
  f_shum_numendians
END ENUM

INTEGER, PARAMETER :: f_shum_endianness = KIND(f_shum_bigendian)

! -----------------------------------------------------------------------------!
! Interfaces

! C Interfaces

INTERFACE
FUNCTION c_shum_byteswap (bytes, len, word_len) BIND(c, NAME="c_shum_byteswap")

IMPORT :: C_INT64_T, C_PTR

IMPLICIT NONE

TYPE(C_PTR), INTENT(IN), VALUE ::                                              &
    bytes

INTEGER(KIND=C_INT64_T) ::                                                     &
    c_shum_byteswap

INTEGER(KIND=C_INT64_T), INTENT(IN) , VALUE ::                                 &
    len,                                                                       &
    word_len

END FUNCTION c_shum_byteswap
END INTERFACE

! -----------------------------------------------------------------------------!

INTERFACE
FUNCTION f_shum_get_machine_endianism ()                                       &
    BIND(c,NAME="c_shum_get_machine_endianism")

IMPORT :: f_shum_endianness

IMPLICIT NONE

INTEGER(KIND=f_shum_endianness) ::                                             &
  f_shum_get_machine_endianism

END FUNCTION f_shum_get_machine_endianism
END INTERFACE

! Generic (Fortran) Interfaces

INTERFACE f_shum_byteswap
MODULE PROCEDURE                                                               &
  shum_byteswap_64
END INTERFACE

CONTAINS

! -----------------------------------------------------------------------------!

FUNCTION shum_byteswap_64(bytes, len, word_len)
                          
IMPLICIT NONE

INTEGER(KIND=C_INT64_T) ::                                                     &
  shum_byteswap_64

INTEGER(KIND=C_INT64_T), INTENT(IN) ::                                         &
  len,                                                                         &
  word_len

REAL(KIND=real64), INTENT(INOUT), TARGET ::                                    &
  bytes(len)

shum_byteswap_64 = c_shum_byteswap(C_LOC(bytes), len, word_len)

END FUNCTION shum_byteswap_64

!------------------------------------------------------------------------------!

END MODULE f_shum_byteswap_mod

