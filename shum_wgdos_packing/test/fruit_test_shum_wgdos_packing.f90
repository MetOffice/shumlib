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
MODULE fruit_test_shum_wgdos_packing_mod

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

SUBROUTINE fruit_test_shum_wgdos_packing

IMPLICIT NONE 

CALL run_test_case(test_pack_simple_field, "pack_simple_field")
CALL run_test_case(test_unpack_simple_field, "unpack_simple_field")
CALL run_test_case(test_packing_field_with_zeros, "packing_field_with_zeros")
CALL run_test_case(test_packing_field_with_mdi, "packing_field_with_mdi")
CALL run_test_case(test_fail_packing_accuracy, "fail_packing_accuracy")

END SUBROUTINE fruit_test_shum_wgdos_packing

!------------------------------------------------------------------------------!

SUBROUTINE test_pack_simple_field

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 11

REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(KIND=int64) :: packed_data(len1_unpacked*len2_unpacked)
INTEGER(KIND=int64) :: expected_data(len_packed)

INTEGER(KIND=int64) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

unpacked_data(:,1) = [  1.0,  2.0,  3.0,  4.0,  5.0 ]
unpacked_data(:,2) = [  6.0,  7.0,  8.0,  9.0, 10.0 ]
unpacked_data(:,3) = [ 11.0, 12.0, 13.0, 14.0, 15.0 ]
unpacked_data(:,4) = [ 16.0, 17.0, 18.0, 19.0, 20.0 ]
unpacked_data(:,5) = [ 21.0, 22.0, 23.0, 24.0, 25.0 ]
unpacked_data(:,6) = [ 26.0, 27.0, 28.0, 29.0, 30.0 ]
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(                                                    &
                unpacked_data, packed_data, len1_unpacked*len2_unpacked,       &
                len1_unpacked, len2_unpacked, num_words, accuracy, mdi, message)

CALL assert_equals(status, 0, &
    "Packing of array returned non-zero exit status")


expected_data = [         90194313217_int64,                                   &
                     1407401745973248_int64,                                   &
                      562954340663296_int64,                                   &
                  4710765210229669889_int64,                                   &
                  1621295866956480512_int64,                                   &
                      562954340663296_int64,                                   &
                  4760304806130745345_int64,                                   &
                  1621295866962116608_int64,                                   &
                      562954340663296_int64,                                   &
                  4763119555897851905_int64,                                   &
                  1621295865853378560_int64    ]

CALL assert_equals((num_words+1)/2, len_packed,                                &
    "Number of packed words is incorrect")

CALL assert_equals(packed_data(1:(num_words+1)/2), expected_data,              &
    INT(len_packed, KIND=int32),                                               &
    "Packed array does not agree with expected result")

END SUBROUTINE test_pack_simple_field

!------------------------------------------------------------------------------!

SUBROUTINE test_unpack_simple_field

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 11

INTEGER(KIND=int64) :: packed_data(len_packed)
REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
REAL(KIND=real64)   :: expected_data(len1_unpacked, len2_unpacked)

INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

packed_data = [         90194313217_int64,                                     &
                   1407401745973248_int64,                                     &
                    562954340663296_int64,                                     &
                4710765210229669889_int64,                                     &
                1621295866956480512_int64,                                     &
                    562954340663296_int64,                                     &
                4760304806130745345_int64,                                     &
                1621295866962116608_int64,                                     &
                    562954340663296_int64,                                     &
                4763119555897851905_int64,                                     &
                1621295865853378560_int64    ]

accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_unpack(                                                  &
                unpacked_data, packed_data, len1_unpacked*len2_unpacked,       &
                len1_unpacked, len2_unpacked, accuracy, mdi, message)

CALL assert_equals(status, 0, &
    "Unpacking of array returned non-zero exit status")

expected_data(:,1) = [  2.0,  2.0,  4.0,  4.0,  6.0 ]
expected_data(:,2) = [  6.0,  8.0,  8.0, 10.0, 10.0 ]
expected_data(:,3) = [ 12.0, 12.0, 14.0, 14.0, 16.0 ]
expected_data(:,4) = [ 16.0, 18.0, 18.0, 20.0, 20.0 ]
expected_data(:,5) = [ 22.0, 22.0, 24.0, 24.0, 26.0 ]
expected_data(:,6) = [ 26.0, 28.0, 28.0, 30.0, 30.0 ]

CALL assert_equals(unpacked_data, expected_data,                               &
    INT(len1_unpacked, KIND=int32), INT(len2_unpacked, KIND=int32),            &
    "Packed array does not agree with expected result")

END SUBROUTINE test_unpack_simple_field

!------------------------------------------------------------------------------!

SUBROUTINE test_packing_field_with_zeros

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack, f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 12

REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(KIND=int64) :: packed_data(len1_unpacked*len2_unpacked)

INTEGER(KIND=int64) :: expected_packed_data(len_packed)
REAL(KIND=real64)   :: expected_unpacked_data(len1_unpacked, len2_unpacked)

INTEGER(KIND=int64) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

! Row starting with zeros
unpacked_data(:,1) = [  0.0,  0.0,  3.0,  4.0,  5.0 ]
! Row ending with zeros
unpacked_data(:,2) = [  6.0,  7.0,  8.0,  0.0,  0.0 ]
! Row with middle group of zeros
unpacked_data(:,3) = [ 11.0, 12.0,  0.0,  0.0, 15.0 ]
! Row of all zeros
unpacked_data(:,4) = [  0.0,  0.0,  0.0,  0.0,  0.0 ]
! Rows with random grouped zeros
unpacked_data(:,5) = [  0.0, 22.0,  0.0,  0.0, 25.0 ]
unpacked_data(:,6) = [ 26.0,  0.0, 28.0, 29.0,  0.0 ]
  
accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(                                                    &
              unpacked_data, packed_data, len1_unpacked*len2_unpacked,         &
              len1_unpacked, len2_unpacked, num_words, accuracy, mdi, message)

CALL assert_equals(status, 0, "Packing of array returned non-zero exit status")

expected_packed_data = [        103079215105_int64,                            &
                            1407400653357056_int64,                            &
                             562954428743680_int64,                            &
                                     8585218_int64,                            &
                        -1729382259292635136_int64,                            &
                                     8650754_int64,                            &
                        -3458764516395843584_int64,                            &
                                           0_int64,                            &
                                     8650754_int64,                            &
                         5764607521910161408_int64,                            &
                                     8650754_int64,                            &
                        -5188146771285508096_int64 ] 

CALL assert_equals(len_packed, (num_words+1)/2,                                &
    "Number of packed words is incorrect")

CALL assert_equals(expected_packed_data, packed_data(1:(num_words+1)/2),       &
    INT(len_packed, KIND=int32),                                               &
    "Packed array does not agree with expected result")

status = f_shum_wgdos_unpack(                                                  &
                unpacked_data, packed_data, len1_unpacked*len2_unpacked,       &
                len1_unpacked, len2_unpacked, accuracy, mdi, message)

CALL assert_equals(status, 0,                                                  &
    "Unpacking of array returned non-zero exit status")

expected_unpacked_data(:,1) = [  0.0,  0.0,  4.0,  4.0,  6.0 ]
expected_unpacked_data(:,2) = [  6.0,  8.0,  8.0,  0.0,  0.0 ]
expected_unpacked_data(:,3) = [ 12.0, 12.0,  0.0,  0.0, 16.0 ]
expected_unpacked_data(:,4) = [  0.0,  0.0,  0.0,  0.0,  0.0 ]
expected_unpacked_data(:,5) = [  0.0, 22.0,  0.0,  0.0, 26.0 ]
expected_unpacked_data(:,6) = [ 26.0,  0.0, 28.0, 30.0,  0.0 ]

CALL assert_equals(expected_unpacked_data, unpacked_data,                      &
    INT(len1_unpacked, KIND=int32), INT(len2_unpacked, KIND=int32),            &
    "Packed array does not agree with expected result")

END SUBROUTINE test_packing_field_with_zeros

!------------------------------------------------------------------------------!

SUBROUTINE test_packing_field_with_mdi

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack, f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6
INTEGER(KIND=int64), PARAMETER :: len_packed = 13

REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(KIND=int64) :: packed_data(len1_unpacked*len2_unpacked)

INTEGER(KIND=int64) :: expected_packed_data(len_packed)
REAL(KIND=real64)   :: expected_unpacked_data(len1_unpacked, len2_unpacked)

INTEGER(KIND=int64) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

accuracy = 1
mdi      = -99.0

! Row starting with mdi
unpacked_data(:,1) = [ -99.0, -99.0,   3.0,   4.0,   5.0 ]
! Row ending with mdi
unpacked_data(:,2) = [   6.0,   7.0,   8.0, -99.0, -99.0 ]
! Row with middle group of mdi
unpacked_data(:,3) = [  11.0,  12.0, -99.0, -99.0,  15.0 ]
! Row of all mdi
unpacked_data(:,4) = [ -99.0, -99.0, -99.0, -99.0, -99.0 ]
! Rows with random grouped mdi
unpacked_data(:,5) = [ -99.0,  22.0, -99.0, -99.0,  25.0 ]
unpacked_data(:,6) = [  26.0, -99.0,  28.0,  29.0, -99.0 ]
  
status = f_shum_wgdos_pack(                                                    &
              unpacked_data, packed_data, len1_unpacked*len2_unpacked,         &
              len1_unpacked, len2_unpacked, num_words, accuracy, mdi, message)

CALL assert_equals(status, 0, "Packing of array returned non-zero exit status")

expected_packed_data = [         111669149697_int64,                           &
                             1407401748070400_int64,                           &
                             9288686176829439_int64,                           &
                          2305843010310504448_int64,                           &
                             9288683358257151_int64,                           &
                          6917529028744183808_int64,                           &
                             9570158737620991_int64,                           &
                           576460755542474752_int64,                           &
                             9007207844675583_int64,                           &
                          4761993655993106434_int64,                           &
                         -5188146774488907776_int64,                           &
                          4763119555899949058_int64,                           &
                          5764607519141920768_int64 ]

CALL assert_equals(len_packed, (num_words+1)/2,                                &
    "Number of packed words is incorrect")

CALL assert_equals(expected_packed_data, packed_data(1:(num_words+1)/2),       &
    INT(len_packed, KIND=int32),                                               &
    "Packed array does not agree with expected result")

status = f_shum_wgdos_unpack(                                                  &
                unpacked_data, packed_data, len1_unpacked*len2_unpacked,       &
                len1_unpacked, len2_unpacked, accuracy, mdi, message)

CALL assert_equals(status, 0,                                                  &
    "Unpacking of array returned non-zero exit status")

expected_unpacked_data(:,1) = [ -99.0, -99.0,  4.0,    4.0,   6.0 ]
expected_unpacked_data(:,2) = [   6.0,   8.0,  8.0,  -99.0, -99.0 ]
expected_unpacked_data(:,3) = [  12.0,  12.0, -99.0, -99.0,  16.0 ]
expected_unpacked_data(:,4) = [ -99.0, -99.0, -99.0, -99.0, -99.0 ]
expected_unpacked_data(:,5) = [ -99.0,  22.0, -99.0, -99.0,  26.0 ]
expected_unpacked_data(:,6) = [  26.0, -99.0,  28.0,  30.0, -99.0 ]

CALL assert_equals(expected_unpacked_data, unpacked_data,                      &
    INT(len1_unpacked, KIND=int32), INT(len2_unpacked, KIND=int32),            &
    "Packed array does not agree with expected result")

END SUBROUTINE test_packing_field_with_mdi

!------------------------------------------------------------------------------!

SUBROUTINE test_fail_packing_accuracy

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: len1_unpacked = 5
INTEGER(KIND=int64), PARAMETER :: len2_unpacked = 6

REAL(KIND=real64)   :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(KIND=int64) :: packed_data(len1_unpacked*len2_unpacked)

INTEGER(KIND=int64) :: num_words
INTEGER(KIND=int32) :: status
INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: mdi
CHARACTER(LEN=500)  :: message

unpacked_data(:,1) = [  1.0,  2.0,  3.0,  4.0,  5.0 ]
unpacked_data(:,2) = [  6.0,  7.0,  8.0,  9.0, 10.0 ]
unpacked_data(:,3) = [ 11.0, 12.0, 13.0, 14.0, 15.0 ]
unpacked_data(:,4) = [ 16.0, 17.0, 18.0, 19.0, 20.0 ]
unpacked_data(:,5) = [ 21.0, 22.0, 23.0, 24.0, 25.0 ]
unpacked_data(:,6) = [ 26.0, 27.0, 28.0, 29.0, 30.0 ]
  
unpacked_data(3,3) = 999999999999999.9_real64

accuracy = 1
mdi      = -99.0

status = f_shum_wgdos_pack(                                                    &
              unpacked_data, packed_data, len1_unpacked*len2_unpacked,         &
              len1_unpacked, len2_unpacked, num_words, accuracy, mdi, message)

CALL assert_equals(2, status,                                                  &
    "Packing of array with unpackable value returned successful exit status")


CALL assert_equals("Unable to WGDOS pack to this accuracy", TRIM(message),     &                   
    "Error message issued different than expected")

END SUBROUTINE test_fail_packing_accuracy

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_wgdos_packing_mod
