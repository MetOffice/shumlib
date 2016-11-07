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

MODULE c_shum_wgdos_packing_mod

USE f_shum_string_conv_mod,   ONLY: f_shum_f2c_string
USE f_shum_wgdos_packing_mod, ONLY: f_shum_wgdos_pack, f_shum_wgdos_unpack

USE, INTRINSIC :: iso_c_binding, ONLY: &
    C_F_POINTER, C_LOC, C_INT64_T, C_DOUBLE, C_CHAR

IMPLICIT NONE 

CONTAINS

FUNCTION c_shum_wgdos_pack(field, comp_field, len_comp, cols, rows, acc,       &
                           rmdi, num_words, message_len, cmessage)             &
                           RESULT(status)                                      &
                           BIND(c, NAME="c_shum_wgdos_pack")
IMPLICIT NONE

INTEGER(KIND=C_INT64_T),       INTENT(IN)         :: len_comp
INTEGER(KIND=C_INT64_T),       INTENT(OUT)        :: comp_field(len_comp)
INTEGER(KIND=C_INT64_T),       INTENT(IN)         :: cols
INTEGER(KIND=C_INT64_T),       INTENT(IN)         :: rows
REAL(KIND=C_DOUBLE),           INTENT(IN), TARGET :: field(cols*rows)
INTEGER(KIND=C_INT64_T),       INTENT(IN)         :: acc
REAL(KIND=C_DOUBLE),           INTENT(IN)         :: rmdi
INTEGER(KIND=C_INT64_T),       INTENT(OUT)        :: num_words
INTEGER(KIND=C_INT64_T),       INTENT(IN)         :: message_len
CHARACTER(KIND=C_CHAR, LEN=1), INTENT(OUT)        :: cmessage(message_len +1)

INTEGER(KIND=C_INT64_T) :: status

CHARACTER(LEN=message_len) :: message

REAL(KIND=C_DOUBLE), POINTER :: field2d(:,:)

! Need to associate the fortan pointer to the field ("field2d") with the
! target of the C pointer ("field") which was passed in, since cmps_all is
! going to need to be able to read from that address
CALL C_F_POINTER (C_LOC(field(1)), field2d, [cols,rows])

status = f_shum_wgdos_pack(field2d, comp_field, len_comp, cols, rows,          &
                           num_words, acc, rmdi, message)
NULLIFY(field2d)

! If something went wrong allow the calling program to catch the non-zero 
! exit code and error message then act accordingly
IF (status /= 0) THEN
  cmessage = f_shum_f2c_string(message)
END IF

END FUNCTION c_shum_wgdos_pack

!-------------------------------------------------------------------------------

FUNCTION c_shum_wgdos_unpack(field, comp_field, len_comp, cols, rows,          &
                               acc, rmdi, message_len, cmessage)               &
                               RESULT(status)                                  &
                               BIND(c, NAME="c_shum_wgdos_unpack")
IMPLICIT NONE

INTEGER(KIND=C_INT64_T),       INTENT(IN)          :: len_comp
INTEGER(KIND=C_INT64_T),       INTENT(IN)          :: comp_field(len_comp)
INTEGER(KIND=C_INT64_T),       INTENT(IN)          :: cols
INTEGER(KIND=C_INT64_T),       INTENT(IN)          :: rows
REAL(KIND=C_DOUBLE),           INTENT(OUT), TARGET :: field(cols*rows)
INTEGER(KIND=C_INT64_T),       INTENT(IN)          :: acc
REAL(KIND=C_DOUBLE),           INTENT(IN)          :: rmdi
INTEGER(KIND=C_INT64_T),       INTENT(IN)          :: message_len
CHARACTER(KIND=C_CHAR, LEN=1), INTENT(OUT)         :: cmessage(message_len +1)

INTEGER(KIND=C_INT64_T) :: status

CHARACTER(LEN=message_len) :: message

REAL(KIND=C_DOUBLE), POINTER :: field2d(:,:)

! Need to associate the fortan pointer to the field ("field2d") with the
! target of the C pointer ("field") which was passed in, since wgdos_unpack
! is going to need to be able to write to that address
CALL C_F_POINTER (C_LOC(field(1)), field2d, [cols,rows])

status = f_shum_wgdos_unpack(field2d, comp_field, len_comp, cols, rows, acc,   &
                             rmdi, message)
NULLIFY(field2d)

! If something went wrong allow the calling program to catch the non-zero 
! exit code and error message then act accordingly
IF (status /= 0) THEN
  cmessage = f_shum_f2c_string(message)
END IF

END FUNCTION c_shum_wgdos_unpack

END MODULE c_shum_wgdos_packing_mod
