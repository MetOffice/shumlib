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

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_LOC, C_PTR, C_INT64_T, C_INT32_T, C_CHAR

USE f_shum_string_conv_mod, ONLY: f_shum_c2f_string

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
FUNCTION c_shum_byteswap (bytes, swap_words, word_len, message)                &
                          BIND(c, NAME="c_shum_byteswap")

IMPORT :: C_INT64_T, C_PTR, C_CHAR

IMPLICIT NONE

TYPE(C_PTR), INTENT(IN), VALUE ::                                              &
    bytes

INTEGER(KIND=C_INT64_T) ::                                                     &
    c_shum_byteswap

INTEGER(KIND=C_INT64_T), INTENT(IN) , VALUE ::                                 &
    swap_words,                                                                &
    word_len

CHARACTER(KIND=C_CHAR, LEN=1), INTENT(INOUT) :: message(*)

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

! Interfaces; the C code which performs the byte-swapping treats the input data
! as a stream of contiguous bytes; the type is irrelevant. So for Fortran we 
! can overload the interface as many times as is needed to cope with any type
! a user might want to pass it.  We also provide interfaces with both 32-bit 
! and 64-bit versions of the other arguments, for easy integration.
! -----------------------------------------------------------------------------!
INTERFACE f_shum_byteswap
MODULE PROCEDURE                                                               &
  shum_byteswap_real64_int64args,                                              &
  shum_byteswap_real64_int32args,                                              & 
  shum_byteswap_real32_int64args,                                              &
  shum_byteswap_real32_int32args
END INTERFACE

CONTAINS

! -----------------------------------------------------------------------------!

FUNCTION shum_byteswap_real64_int64args(bytes, swap_words, word_len, message)
                          
IMPLICIT NONE

INTEGER(KIND=C_INT64_T) ::                                                     &
  shum_byteswap_real64_int64args

INTEGER(KIND=C_INT64_T), INTENT(IN) ::                                         &
  swap_words,                                                                  &
  word_len

REAL(KIND=real64), INTENT(INOUT), TARGET ::                                    &
  bytes(:)

CHARACTER(LEN=*), INTENT(INOUT) :: message
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*8) THEN
  shum_byteswap_real64_int64args = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*8, " byte input array"
ELSE
  ALLOCATE(cmessage(LEN(message)))
  shum_byteswap_real64_int64args = c_shum_byteswap(                            &
                                                C_LOC(bytes(1)), swap_words,   &
                                                word_len, cmessage)
  message = f_shum_c2f_string(cmessage)
END IF

END FUNCTION shum_byteswap_real64_int64args

! -----------------------------------------------------------------------------!

FUNCTION shum_byteswap_real64_int32args(bytes, swap_words, word_len, message)
                          
IMPLICIT NONE

INTEGER(KIND=C_INT32_T) ::                                                     &
  shum_byteswap_real64_int32args

INTEGER(KIND=C_INT32_T), INTENT(IN) ::                                         &
  swap_words,                                                                  &
  word_len

REAL(KIND=real64), INTENT(INOUT), TARGET ::                                    &
  bytes(:)

CHARACTER(LEN=*), INTENT(INOUT) :: message
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*8) THEN
  shum_byteswap_real64_int32args = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*8, " byte input array"
ELSE
  ALLOCATE(cmessage(LEN(message)))
  shum_byteswap_real64_int32args = INT(c_shum_byteswap(                        &
                                        C_LOC(bytes(1)),                       &
                                        INT(swap_words, KIND=C_INT64_T),       &
                                        INT(word_len, KIND=C_INT64_T),         &
                                        cmessage),                             &                     
                                       KIND=C_INT32_T)
  message = f_shum_c2f_string(cmessage)
END IF

END FUNCTION shum_byteswap_real64_int32args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_real32_int64args(bytes, swap_words, word_len, message)
                          
IMPLICIT NONE

INTEGER(KIND=C_INT64_T) ::                                                     &
  shum_byteswap_real32_int64args

INTEGER(KIND=C_INT64_T), INTENT(IN) ::                                         &
  swap_words,                                                                  &
  word_len

REAL(KIND=real32), INTENT(INOUT), TARGET ::                                    &
  bytes(:)

CHARACTER(LEN=*), INTENT(INOUT) :: message
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*4) THEN
  shum_byteswap_real32_int64args = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*4, " byte input array"
ELSE
  ALLOCATE(cmessage(LEN(message)))
  shum_byteswap_real32_int64args = c_shum_byteswap(                            &
                                                 C_LOC(bytes(1)), swap_words,  &
                                                 word_len, cmessage)
  message = f_shum_c2f_string(cmessage)
END IF

END FUNCTION shum_byteswap_real32_int64args

!------------------------------------------------------------------------------!

FUNCTION shum_byteswap_real32_int32args(bytes, swap_words, word_len, message)
                          
IMPLICIT NONE

INTEGER(KIND=C_INT32_T) ::                                                     &
  shum_byteswap_real32_int32args

INTEGER(KIND=C_INT32_T), INTENT(IN) ::                                         &
  swap_words,                                                                  &
  word_len

REAL(KIND=real32), INTENT(INOUT), TARGET ::                                    &
  bytes(:)

CHARACTER(LEN=*), INTENT(INOUT) :: message
CHARACTER(KIND=C_CHAR, LEN=1), ALLOCATABLE :: cmessage(:)

IF (word_len*swap_words > SIZE(bytes)*4) THEN
  shum_byteswap_real32_int32args = 1
  WRITE(message, "(A,I0,1X,I0,A,I0,A)")                                        &
    "f_shum_byteswap: Request to swap ", swap_words, word_len,                 &
    "-byte words overflows ", SIZE(bytes)*4, " byte input array"
ELSE
  ALLOCATE(cmessage(LEN(message)))
  shum_byteswap_real32_int32args = INT(c_shum_byteswap(                        &
                                        C_LOC(bytes(1)),                       &
                                        INT(swap_words, KIND=C_INT64_T),       &
                                        INT(word_len, KIND=C_INT64_T),         &
                                        cmessage),                             &
                                       KIND=C_INT32_T)
  message = f_shum_c2f_string(cmessage)
END IF

END FUNCTION shum_byteswap_real32_int32args

!------------------------------------------------------------------------------!

END MODULE f_shum_byteswap_mod

