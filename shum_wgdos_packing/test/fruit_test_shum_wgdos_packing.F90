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
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T, C_INT32_T

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

CONTAINS

SUBROUTINE fruit_test_shum_wgdos_packing

IMPLICIT NONE 

CALL run_test_case(test_pack_simple_field, "pack_simple_field")
CALL run_test_case(test_unpack_simple_field, "unpack_simple_field")
CALL run_test_case(test_packing_field_with_zeros, "packing_field_with_zeros")
CALL run_test_case(test_packing_field_with_mdi, "packing_field_with_mdi")
CALL run_test_case(test_fail_packing_accuracy, "fail_packing_accuracy")

END SUBROUTINE fruit_test_shum_wgdos_packing

! -----------------------------------------------------------------------------!

SUBROUTINE test_pack_simple_field

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(C_INT64_T), PARAMETER :: len1_unpacked = 5
INTEGER(C_INT64_T), PARAMETER :: len2_unpacked = 6
INTEGER(C_INT64_T), PARAMETER :: len_packed = 11

REAL(real64)       :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(C_INT64_T) :: packed_data(len1_unpacked*len2_unpacked)
INTEGER(C_INT64_T) :: expected_data(len_packed)

INTEGER(C_INT64_T) :: num_words
INTEGER(C_INT32_T) :: status
INTEGER(C_INT64_T) :: accuracy
REAL(real64)       :: mdi
CHARACTER(LEN=500) :: message

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


expected_data = [         90194313217_C_INT64_T,                               &
                     1407401745973248_C_INT64_T,                               &
                      562954340663296_C_INT64_T,                               &
                  4710765210229669889_C_INT64_T,                               &
                  1621295866956480512_C_INT64_T,                               &
                      562954340663296_C_INT64_T,                               &
                  4760304806130745345_C_INT64_T,                               &
                  1621295866962116608_C_INT64_T,                               &
                      562954340663296_C_INT64_T,                               &
                  4763119555897851905_C_INT64_T,                               &
                  1621295865853378560_C_INT64_T    ]

CALL assert_equals((num_words+1)/2, len_packed,                                &
    "Number of packed words is incorrect")

CALL assert_equals(packed_data(1:(num_words+1)/2), expected_data,              &
    INT(len_packed, KIND=C_INT32_T),                                           &
    "Packed array does not agree with expected result")

END SUBROUTINE test_pack_simple_field

! -----------------------------------------------------------------------------!

SUBROUTINE test_unpack_simple_field

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(C_INT64_T), PARAMETER :: len1_unpacked = 5
INTEGER(C_INT64_T), PARAMETER :: len2_unpacked = 6
INTEGER(C_INT64_T), PARAMETER :: len_packed = 11

INTEGER(C_INT64_T) :: packed_data(len_packed)
REAL(real64)       :: unpacked_data(len1_unpacked, len2_unpacked)
REAL(real64)       :: expected_data(len1_unpacked, len2_unpacked)

INTEGER(C_INT32_T) :: status
INTEGER(C_INT64_T) :: accuracy
REAL(real64)       :: mdi
CHARACTER(LEN=500) :: message

packed_data = [         90194313217_C_INT64_T,                                 &
                   1407401745973248_C_INT64_T,                                 &
                    562954340663296_C_INT64_T,                                 &
                4710765210229669889_C_INT64_T,                                 &
                1621295866956480512_C_INT64_T,                                 &
                    562954340663296_C_INT64_T,                                 &
                4760304806130745345_C_INT64_T,                                 &
                1621295866962116608_C_INT64_T,                                 &
                    562954340663296_C_INT64_T,                                 &
                4763119555897851905_C_INT64_T,                                 &
                1621295865853378560_C_INT64_T    ]

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
    INT(len1_unpacked, KIND=C_INT32_T), INT(len2_unpacked, KIND=C_INT32_T),    &
    "Packed array does not agree with expected result")

END SUBROUTINE test_unpack_simple_field

! -----------------------------------------------------------------------------!

SUBROUTINE test_packing_field_with_zeros

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack, f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(C_INT64_T), PARAMETER :: len1_unpacked = 5
INTEGER(C_INT64_T), PARAMETER :: len2_unpacked = 6
INTEGER(C_INT64_T), PARAMETER :: len_packed = 12

REAL(real64)       :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(C_INT64_T) :: packed_data(len1_unpacked*len2_unpacked)

INTEGER(C_INT64_T) :: expected_packed_data(len_packed)
REAL(real64)       :: expected_unpacked_data(len1_unpacked, len2_unpacked)

INTEGER(C_INT64_T) :: num_words
INTEGER(C_INT32_T) :: status
INTEGER(C_INT64_T) :: accuracy
REAL(real64)       :: mdi
CHARACTER(LEN=500) :: message

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
                unpacked_data, packed_data, len1_unpacked*len2_unpacked,       &
                len1_unpacked, len2_unpacked, num_words, accuracy, mdi, message)

CALL assert_equals(status, 0, "Packing of array returned non-zero exit status")

expected_packed_data = [        103079215105_C_INT64_T,                        &
                            1407400653357056_C_INT64_T,                        &
                             562954428743680_C_INT64_T,                        &
                                     8585218_C_INT64_T,                        &
                        -1729382259292635136_C_INT64_T,                        &
                                     8650754_C_INT64_T,                        &
                        -3458764516395843584_C_INT64_T,                        &
                                           0_C_INT64_T,                        &
                                     8650754_C_INT64_T,                        &
                         5764607521910161408_C_INT64_T,                        &
                                     8650754_C_INT64_T,                        &
                        -5188146771285508096_C_INT64_T ] 

CALL assert_equals(len_packed, (num_words+1)/2,                                &
    "Number of packed words is incorrect")

CALL assert_equals(expected_packed_data, packed_data(1:(num_words+1)/2),       &
    INT(len_packed, KIND=C_INT32_T),                                           &
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
    INT(len1_unpacked, KIND=C_INT32_T), INT(len2_unpacked, KIND=C_INT32_T),    &
    "Packed array does not agree with expected result")

END SUBROUTINE test_packing_field_with_zeros

! -----------------------------------------------------------------------------!

SUBROUTINE test_packing_field_with_mdi

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack, f_shum_wgdos_unpack

IMPLICIT NONE 

INTEGER(C_INT64_T), PARAMETER :: len1_unpacked = 5
INTEGER(C_INT64_T), PARAMETER :: len2_unpacked = 6
INTEGER(C_INT64_T), PARAMETER :: len_packed = 13
REAL(real64)       :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(C_INT64_T) :: packed_data(len1_unpacked*len2_unpacked)
INTEGER(C_INT64_T) :: expected_packed_data(len_packed)
REAL(real64)       :: expected_unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(C_INT64_T) :: num_words
INTEGER(C_INT32_T) :: status
INTEGER(C_INT64_T) :: accuracy
REAL(real64)       :: mdi
CHARACTER(LEN=500) :: message

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
                unpacked_data, packed_data, len1_unpacked*len2_unpacked,       &
                len1_unpacked, len2_unpacked, num_words, accuracy, mdi, message)

CALL assert_equals(status, 0, "Packing of array returned non-zero exit status")

expected_packed_data = [         111669149697_C_INT64_T,                       &
                             1407401748070400_C_INT64_T,                       &
                             9288686176829439_C_INT64_T,                       &
                          2305843010310504448_C_INT64_T,                       &
                             9288683358257151_C_INT64_T,                       &
                          6917529028744183808_C_INT64_T,                       &
                             9570158737620991_C_INT64_T,                       &
                           576460755542474752_C_INT64_T,                       &
                             9007207844675583_C_INT64_T,                       &
                          4761993655993106434_C_INT64_T,                       &
                         -5188146774488907776_C_INT64_T,                       &
                          4763119555899949058_C_INT64_T,                       &
                          5764607519141920768_C_INT64_T ]

CALL assert_equals(len_packed, (num_words+1)/2,                                &
    "Number of packed words is incorrect")

CALL assert_equals(expected_packed_data, packed_data(1:(num_words+1)/2),       &
    INT(len_packed, KIND=C_INT32_T),                                           &
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
    INT(len1_unpacked, KIND=C_INT32_T), INT(len2_unpacked, KIND=C_INT32_T),    &
    "Packed array does not agree with expected result")

END SUBROUTINE test_packing_field_with_mdi

! -----------------------------------------------------------------------------!

SUBROUTINE test_fail_packing_accuracy

USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack

IMPLICIT NONE 

INTEGER(C_INT64_T), PARAMETER :: len1_unpacked = 5
INTEGER(C_INT64_T), PARAMETER :: len2_unpacked = 6

REAL(real64)       :: unpacked_data(len1_unpacked, len2_unpacked)
INTEGER(C_INT64_T) :: packed_data(len1_unpacked*len2_unpacked)

INTEGER(C_INT64_T) :: num_words
INTEGER(C_INT32_T) :: status
INTEGER(C_INT64_T) :: accuracy
REAL(real64)       :: mdi
CHARACTER(LEN=500) :: message

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
                unpacked_data, packed_data, len1_unpacked*len2_unpacked,       &
                len1_unpacked, len2_unpacked, num_words, accuracy, mdi, message)

CALL assert_equals(2, status,                                                  &
    "Packing of array with unpackable value returned successful exit status")


CALL assert_equals("Unable to WGDOS pack to this accuracy", TRIM(message),     &                   
    "Error message issued different than expected")

END SUBROUTINE test_fail_packing_accuracy

! -----------------------------------------------------------------------------!

END MODULE fruit_test_shum_wgdos_packing_mod
