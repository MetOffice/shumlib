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
MODULE fruit_test_shum_byteswap_mod

USE fruit
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE

IMPLICIT NONE 

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

CONTAINS

SUBROUTINE fruit_test_shum_byteswap

IMPLICIT NONE 

CALL run_test_case(test_returns_valid_endian, "returns_valid_endian")
CALL run_test_case(test_64bit_data_8_word_size, "64bit_data_8_word_size")
CALL run_test_case(test_64bit_data_4_word_size, "64bit_data_4_word_size")
CALL run_test_case(test_32bit_data_8_word_size, "32bit_data_8_word_size")
CALL run_test_case(test_32bit_data_4_word_size, "32bit_data_4_word_size")

END SUBROUTINE fruit_test_shum_byteswap

!------------------------------------------------------------------------------!

SUBROUTINE test_returns_valid_endian

USE f_shum_byteswap_mod, ONLY: f_shum_get_machine_endianism,                   &
                               f_shum_littleendian, f_shum_bigendian
IMPLICIT NONE 

INTEGER :: endian
LOGICAL :: check
CALL set_case_name("test_byteswap_returns_valid_endian")
endian = f_shum_get_machine_endianism()
check = ((endian == f_shum_littleendian) .OR. (endian == f_shum_bigendian))
CALL assert_true(check, "Returned value is not a valid endian enum value")

END SUBROUTINE test_returns_valid_endian

!------------------------------------------------------------------------------!

SUBROUTINE test_64bit_data_8_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE 

INTEGER, PARAMETER :: len = 10
INTEGER, PARAMETER :: word_size = 8

REAL(KIND=real64) :: data_in(len)
REAL(KIND=real64) :: data_swapped_expected(len)
REAL(KIND=real64) :: data_in_copy(len)

INTEGER :: status
INTEGER :: i
REAL(KIND=real64), PARAMETER :: o64 = 0
CHARACTER(LEN=500) :: message

DO i = 1, len
  data_in(i) = REAL(i, KIND=real64)
  data_in_copy(i) = REAL(i, KIND=real64)
END DO

data_swapped_expected(1)  = TRANSFER(INT(z"0000F03F", KIND=int64), o64)
data_swapped_expected(2)  = TRANSFER(INT(z"00000040", KIND=int64), o64)
data_swapped_expected(3)  = TRANSFER(INT(z"00000840", KIND=int64), o64)
data_swapped_expected(4)  = TRANSFER(INT(z"00001040", KIND=int64), o64)
data_swapped_expected(5)  = TRANSFER(INT(z"00001440", KIND=int64), o64)
data_swapped_expected(6)  = TRANSFER(INT(z"00001840", KIND=int64), o64)
data_swapped_expected(7)  = TRANSFER(INT(z"00001C40", KIND=int64), o64)
data_swapped_expected(8)  = TRANSFER(INT(z"00002040", KIND=int64), o64)
data_swapped_expected(9)  = TRANSFER(INT(z"00002240", KIND=int64), o64)
data_swapped_expected(10) = TRANSFER(INT(z"00002440", KIND=int64), o64)

status = f_shum_byteswap(data_in, len, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len,                        &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len,                   &
    "Double byteswapped array does not agree with intiial array")


END SUBROUTINE test_64bit_data_8_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_64bit_data_4_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE 

INTEGER, PARAMETER :: len = 10
INTEGER, PARAMETER :: word_size = 4

REAL(KIND=real64) :: data_in(len)
REAL(KIND=real64) :: data_swapped_expected(len)
REAL(KIND=real64) :: data_in_copy(len)

INTEGER :: status
INTEGER :: i
REAL(KIND=real64), PARAMETER :: o64 = 0
CHARACTER(LEN=500) :: message

DO i = 1, len
  data_in(i) = REAL(i, KIND=real64)
  data_in_copy(i) = REAL(i, KIND=real64)
END DO

data_swapped_expected(1)  = TRANSFER(INT(z"F03F00000000", KIND=int64), o64)
data_swapped_expected(2)  = TRANSFER(INT(z"004000000000", KIND=int64), o64)
data_swapped_expected(3)  = TRANSFER(INT(z"084000000000", KIND=int64), o64)
data_swapped_expected(4)  = TRANSFER(INT(z"104000000000", KIND=int64), o64)
data_swapped_expected(5)  = TRANSFER(INT(z"144000000000", KIND=int64), o64)
data_swapped_expected(6)  = TRANSFER(INT(z"184000000000", KIND=int64), o64)
data_swapped_expected(7)  = TRANSFER(INT(z"1C4000000000", KIND=int64), o64)
data_swapped_expected(8)  = TRANSFER(INT(z"204000000000", KIND=int64), o64)
data_swapped_expected(9)  = TRANSFER(INT(z"224000000000", KIND=int64), o64)
data_swapped_expected(10) = TRANSFER(INT(z"244000000000", KIND=int64), o64)

status = f_shum_byteswap(data_in, len*2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len,                        &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len*2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len,                   &
    "Double byteswapped array does not agree with intiial array")


END SUBROUTINE test_64bit_data_4_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_32bit_data_8_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE 

INTEGER, PARAMETER :: len = 10
INTEGER, PARAMETER :: word_size = 8

REAL(KIND=real32) :: data_in(len)
REAL(KIND=real32) :: data_swapped_expected(len)
REAL(KIND=real32) :: data_in_copy(len)

INTEGER :: status
INTEGER :: i
REAL(KIND=real32), PARAMETER :: o32 = 0
CHARACTER(LEN=500) :: message

DO i = 1, len
  data_in(i) = REAL(i, KIND=real32)
  data_in_copy(i) = REAL(i, KIND=real32)
END DO

data_swapped_expected(1)  = TRANSFER(INT(z"0040", KIND=int32), o32)
data_swapped_expected(2)  = TRANSFER(INT(z"803F", KIND=int32), o32)
data_swapped_expected(3)  = TRANSFER(INT(z"8040", KIND=int32), o32)
data_swapped_expected(4)  = TRANSFER(INT(z"4040", KIND=int32), o32)
data_swapped_expected(5)  = TRANSFER(INT(z"C040", KIND=int32), o32)
data_swapped_expected(6)  = TRANSFER(INT(z"A040", KIND=int32), o32)
data_swapped_expected(7)  = TRANSFER(INT(z"0041", KIND=int32), o32)
data_swapped_expected(8)  = TRANSFER(INT(z"E040", KIND=int32), o32)
data_swapped_expected(9)  = TRANSFER(INT(z"2041", KIND=int32), o32)
data_swapped_expected(10) = TRANSFER(INT(z"1041", KIND=int32), o32)

status = f_shum_byteswap(data_in, len/2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in, len,                        &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len/2, word_size, message)
CALL assert_equals(0, status,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_in_copy, data_swapped_expected, len,                   &
    "Double byteswapped array does not agree with intiial array")


END SUBROUTINE test_32bit_data_8_word_size

!------------------------------------------------------------------------------!

SUBROUTINE test_32bit_data_4_word_size

USE f_shum_byteswap_mod, ONLY: f_shum_byteswap

IMPLICIT NONE 

INTEGER, PARAMETER :: len = 10
INTEGER, PARAMETER :: word_size = 4

REAL(KIND=real32) :: data_in(len)
REAL(KIND=real32) :: data_swapped_expected(len)
REAL(KIND=real32) :: data_in_copy(len)

INTEGER :: status
INTEGER :: i
REAL(KIND=real32), PARAMETER :: o32 = 0
CHARACTER(LEN=500) :: message

CALL set_case_name("test_byteswap_32bit_data_4_word_size")

DO i = 1, len
  data_in(i) = REAL(i, KIND=real32)
  data_in_copy(i) = REAL(i, KIND=real32)
END DO

data_swapped_expected(1)  = TRANSFER(INT(z"803F", KIND=int32), o32)
data_swapped_expected(2)  = TRANSFER(INT(z"0040", KIND=int32), o32)
data_swapped_expected(3)  = TRANSFER(INT(z"4040", KIND=int32), o32)
data_swapped_expected(4)  = TRANSFER(INT(z"8040", KIND=int32), o32)
data_swapped_expected(5)  = TRANSFER(INT(z"A040", KIND=int32), o32)
data_swapped_expected(6)  = TRANSFER(INT(z"C040", KIND=int32), o32)
data_swapped_expected(7)  = TRANSFER(INT(z"E040", KIND=int32), o32)
data_swapped_expected(8)  = TRANSFER(INT(z"0041", KIND=int32), o32)
data_swapped_expected(9)  = TRANSFER(INT(z"1041", KIND=int32), o32)
data_swapped_expected(10) = TRANSFER(INT(z"2041", KIND=int32), o32)

status = f_shum_byteswap(data_in, len, word_size, message)
CALL assert_equals(status, 0,                                                  &
    "Byteswap of initial array returned non-zero exit status")

CALL assert_equals(data_in, data_swapped_expected, len,                        &
    "Byteswapped array is not as expected")

status = f_shum_byteswap(data_swapped_expected, len, word_size, message)
CALL assert_equals(status, 0,                                                  &
    "Byteswap of byteswapped array returned non-zero exit status")

CALL assert_equals(data_swapped_expected, data_in_copy, len,                   &
    "Double byteswapped array does not agree with intiial array")

END SUBROUTINE test_32bit_data_4_word_size

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_byteswap_mod
