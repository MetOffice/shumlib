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
MODULE fruit_test_shum_horizontal_field_interp_mod

USE fruit
USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE

IMPLICIT NONE 

PRIVATE

PUBLIC :: fruit_test_shum_horizontal_field_interp

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

! Set a small tolerance level for real to real comparisons
REAL(KIND=real64), PARAMETER :: tolerance = EPSILON(1.0_real64)*100.0_real64

CONTAINS

SUBROUTINE fruit_test_shum_horizontal_field_interp

USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
USE f_shum_horizontal_field_interp_version_mod, ONLY:                          &
                                     get_shum_horizontal_field_interp_version

IMPLICIT NONE 

INTEGER(KIND=int64) :: version

!------------------------------------------------------------------------------!
! Note: we don't have a test case for the version checking because we don't
! want the testing to include further hardcoded version numbers to test
! against.  Since the version module is simple and hardcoded anyway it's 
! sufficient to make sure it is callable; but let's print the version for info.
!------------------------------------------------------------------------------!
version = get_shum_horizontal_field_interp_version()

WRITE(OUTPUT_UNIT, "()")
WRITE(OUTPUT_UNIT, "(A,I0)")                                                   &
    "Testing shum_horizontal_field_interp at Shumlib version: ", version

CALL run_test_case(test_get_simple_indices, "interp_simple_2d_global_field")
CALL run_test_case(                                                            &
             test_get_corner_indices, "interp_corner_set_2d_global_field")
CALL run_test_case(test_get_wrapped_indices, "interp_wrapped_2d_global_field")
CALL run_test_case(test_get_simple_weights, "test_get_simple_weights")
CALL run_test_case(test_apply_weights, "test_apply_weights")

END SUBROUTINE fruit_test_shum_horizontal_field_interp

!------------------------------------------------------------------------------!

SUBROUTINE write_1D_int_array(title, array) 
! Subroutine to write out a 1D int array used when createing new tests
IMPLICIT NONE 
INTEGER(KIND=int64), INTENT(IN) :: array(:)
CHARACTER(LEN=*), INTENT(IN)    :: title

write(6,'(A,A)') title," =  [ &"
write(6,'(10(1X,I5,","),"   &")') array
write(6,'(A)') " ]"
END SUBROUTINE write_1D_int_array

!------------------------------------------------------------------------------!

SUBROUTINE write_1D_real_array(title, array) 
! Subroutine to write out a 1D real array used when createing new tests
IMPLICIT NONE 
REAL(KIND=real64), INTENT(IN) :: array(:)
CHARACTER(LEN=*), INTENT(IN)  :: title

write(6,'(A,A)') title," =    [ &"
write(6,'(3(1X,F10.4,"_real64,",1X),"   &")') array
write(6,'(A)') " ]"
END SUBROUTINE write_1D_real_array

!------------------------------------------------------------------------------!

SUBROUTINE generate_1d_reg_grid_coords (start_coord, end_coord,                &
                                        no_of_points, coord_array,             &
                                        last_point_present_arg)
! Subroutine to take a start lat/lon and an end lat/lon and create an array of
! evenly spaced coordinates with a given number of points. The optional logical
! argument dictates whether the  end lat/lon coordinate given is a member of
! the array or not.
IMPLICIT NONE 
REAL(KIND=real64),   INTENT(IN)           :: start_coord
REAL(KIND=real64),   INTENT(IN)           :: end_coord
INTEGER(KIND=int64), INTENT(IN)           :: no_of_points
REAL(KIND=real64),   INTENT(OUT)          :: coord_array(:)
LOGICAL,             INTENT(IN), OPTIONAL :: last_point_present_arg

! Local Variables
REAL(KIND=real64)              :: grid_length
INTEGER(KIND=int64)            :: no_of_grid_lengths
INTEGER(KIND=int64)            :: i
LOGICAL                        :: last_point_present

IF ( PRESENT( last_point_present_arg ) ) THEN
  last_point_present = last_point_present_arg
ELSE
  last_point_present = .TRUE.
END IF

IF ( last_point_present ) THEN
  no_of_grid_lengths = no_of_points - 1
ELSE
  no_of_grid_lengths = no_of_points
END IF

grid_length = (end_coord - start_coord) / no_of_grid_lengths

DO i = 1, no_of_grid_lengths
  coord_array(i) = start_coord + (grid_length * (i-1))
END DO

END SUBROUTINE generate_1d_reg_grid_coords

!------------------------------------------------------------------------------!

SUBROUTINE generate_2d_reg_grid_coords(start_lat_coord, end_lat_coord,         &
                                       start_lon_coord, end_lon_coord,         &
                                       no_of_lat_points, no_of_lon_points,     &
                                       lat_coord_array, lon_coord_array,       &
                                       last_point_present_arg)
! Subroutine to take the start lat & lon coordinates and the end lat & lon
! coordinates and create 2 arrays of evenly spaced coordinates with a given
! number of points in each direction. The 2 arrays each contain a full set of
! points for a 2D array of that directional coordinate to describe a 2D grid.
! The optional logical argument dictates whether the  end lat/lon coordinates
! given are members of the arrays or not.

IMPLICIT NONE 

REAL(KIND=real64),   INTENT(IN)           :: start_lat_coord
REAL(KIND=real64),   INTENT(IN)           :: end_lat_coord
REAL(KIND=real64),   INTENT(IN)           :: start_lon_coord
REAL(KIND=real64),   INTENT(IN)           :: end_lon_coord
INTEGER(KIND=int64), INTENT(IN)           :: no_of_lat_points
INTEGER(KIND=int64), INTENT(IN)           :: no_of_lon_points
REAL(KIND=real64),   INTENT(OUT)          :: lat_coord_array(:)
REAL(KIND=real64),   INTENT(OUT)          :: lon_coord_array(:)
LOGICAL,             INTENT(IN), OPTIONAL :: last_point_present_arg

! Local Variables
REAL(KIND=real64)              :: lat_coords(no_of_lat_points)
REAL(KIND=real64)              :: lon_coords(no_of_lon_points)
INTEGER(KIND=int64)            :: no_of_lat_grid_lengths
INTEGER(KIND=int64)            :: no_of_lon_grid_lengths
INTEGER(KIND=int64)            :: i,j
LOGICAL                        :: last_point_present

IF ( PRESENT( last_point_present_arg ) ) THEN
  last_point_present = last_point_present_arg
ELSE
  last_point_present = .TRUE.
END IF

IF ( last_point_present ) THEN
  no_of_lat_grid_lengths = no_of_lat_points - 1
  no_of_lon_grid_lengths = no_of_lon_points - 1
ELSE
  no_of_lat_grid_lengths = no_of_lat_points
  no_of_lon_grid_lengths = no_of_lon_points
END IF

CALL generate_1d_reg_grid_coords(start_lat_coord, end_lat_coord,               &
                                 no_of_lat_points, lat_coords,                 &
                                 last_point_present)

CALL generate_1d_reg_grid_coords(start_lon_coord, end_lon_coord,               &
                                 no_of_lon_points, lon_coords,                 &
                                 last_point_present)

DO i=1,no_of_lat_grid_lengths
  DO j=1,no_of_lon_grid_lengths
    lon_coord_array(((i-1) * no_of_lon_grid_lengths) +j) = lon_coords(j)
    lat_coord_array(((i-1) * no_of_lon_grid_lengths) +j) = lat_coords(i)
  END DO
END DO

END SUBROUTINE generate_2d_reg_grid_coords

!------------------------------------------------------------------------------!

SUBROUTINE create_four_value_source_grid( grid_values, x_dim, y_dim,           &
                                          x_dim_change, y_dim_change,          &
                                          value_1, value_2, value_3)

! Create source grid 'data' values as 4 blocks of values: 
!                            x_dim_change
!              value_1 + value_3  |   value_1 + value_2 + value_3
! y_dim_change -------------------------------------------------- y_dim_change
!              value_1            |   value_1 + value_2
!                            x_dim_change
! 
IMPLICIT NONE 

REAL(KIND=real64),   INTENT(OUT)          :: grid_values(:)
REAL(KIND=real64),   INTENT(IN)           :: value_1, value_2, value_3
INTEGER(KIND=int64), INTENT(IN)           :: x_dim, y_dim
INTEGER(KIND=int64), INTENT(IN)           :: x_dim_change, y_dim_change
INTEGER(KIND=int32)                       :: i, j, ij

DO j = 1, y_dim
  DO i = 1, x_dim
    ij = (j-1)* x_dim + i 
    IF (i <= x_dim_change) THEN
      grid_values(ij) = value_1
    ELSE
      grid_values(ij) = value_1 + value_2
    END IF
    IF (j > y_dim_change) THEN
      grid_values(ij) = grid_values(ij) + value_3
    END IF
  END DO
END DO

END SUBROUTINE create_four_value_source_grid

!------------------------------------------------------------------------------!

SUBROUTINE return_three_grid_indices( x_dir_indices, y_dir_indices,            &
                                      x_dir_plus_1_indices )
! returns 3 arrays of indices used by multiple tests.
IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(OUT)          :: x_dir_indices(:),                 &
                                             y_dir_indices(:),                 &
                                             x_dir_plus_1_indices(:)

x_dir_indices =  [                                                             &
     2,     3,     3,     3,     4,     4,     5,     5,     5,     6,         &
     6,     7,     7,     7,     8,     2,     3,     3,     3,     4,         &
     4,     5,     5,     5,     6,     6,     7,     7,     7,     8,         &
     2,     3,     3,     3,     4,     4,     5,     5,     5,     6,         &
     6,     7,     7,     7,     8,     2,     3,     3,     3,     4,         &
     4,     5,     5,     5,     6,     6,     7,     7,     7,     8,         &
     2,     3,     3,     3,     4,     4,     5,     5,     5,     6,         &
     6,     7,     7,     7,     8,     2,     3,     3,     3,     4,         &
     4,     5,     5,     5,     6,     6,     7,     7,     7,     8,         &
     2,     3,     3,     3,     4,     4,     5,     5,     5,     6,         &
     6,     7,     7,     7,     8,     2,     3,     3,     3,     4,         &
     4,     5,     5,     5,     6,     6,     7,     7,     7,     8,         &
     2,     3,     3,     3,     4,     4,     5,     5,     5,     6,         &
     6,     7,     7,     7,     8,     2,     3,     3,     3,     4,         &
     4,     5,     5,     5,     6,     6,     7,     7,     7,     8          &
 ]
y_dir_indices =  [                                                             &
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,         &
     2,     2,     2,     2,     2,     3,     3,     3,     3,     3,         &
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,         &
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,         &
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,         &
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,         &
     4,     4,     4,     4,     4,     4,     4,     4,     4,     4,         &
     4,     4,     4,     4,     4,     4,     4,     4,     4,     4,         &
     4,     4,     4,     4,     4,     4,     4,     4,     4,     4,         &
     5,     5,     5,     5,     5,     5,     5,     5,     5,     5,         &
     5,     5,     5,     5,     5,     5,     5,     5,     5,     5,         &
     5,     5,     5,     5,     5,     5,     5,     5,     5,     5,         &
     5,     5,     5,     5,     5,     5,     5,     5,     5,     5,         &
     5,     5,     5,     5,     5,     6,     6,     6,     6,     6,         &
     6,     6,     6,     6,     6,     6,     6,     6,     6,     6          &
 ]
x_dir_plus_1_indices =  [                                                      &
     3,     4,     4,     4,     5,     5,     6,     6,     6,     7,         &
     7,     8,     8,     8,     9,     3,     4,     4,     4,     5,         &
     5,     6,     6,     6,     7,     7,     8,     8,     8,     9,         &
     3,     4,     4,     4,     5,     5,     6,     6,     6,     7,         &
     7,     8,     8,     8,     9,     3,     4,     4,     4,     5,         &
     5,     6,     6,     6,     7,     7,     8,     8,     8,     9,         &
     3,     4,     4,     4,     5,     5,     6,     6,     6,     7,         &
     7,     8,     8,     8,     9,     3,     4,     4,     4,     5,         &
     5,     6,     6,     6,     7,     7,     8,     8,     8,     9,         &
     3,     4,     4,     4,     5,     5,     6,     6,     6,     7,         &
     7,     8,     8,     8,     9,     3,     4,     4,     4,     5,         &
     5,     6,     6,     6,     7,     7,     8,     8,     8,     9,         &
     3,     4,     4,     4,     5,     5,     6,     6,     6,     7,         &
     7,     8,     8,     8,     9,     3,     4,     4,     4,     5,         &
     5,     6,     6,     6,     7,     7,     8,     8,     8,     9          &
 ]

END SUBROUTINE return_three_grid_indices

!------------------------------------------------------------------------------!

SUBROUTINE return_two_grid_indices( index_bottom_left, index_bottom_right)
! returns 2 arrays of indices used by multiple tests.

IMPLICIT NONE 

INTEGER(KIND=int64), INTENT(OUT)          :: index_bottom_left(:),             &
                                             index_bottom_right(:)

index_bottom_left =  [                                                         &
    20,    21,    21,    21,    22,    22,    23,    23,    23,    24,         &
    24,    25,    25,    25,    26,    38,    39,    39,    39,    40,         &
    40,    41,    41,    41,    42,    42,    43,    43,    43,    44,         &
    38,    39,    39,    39,    40,    40,    41,    41,    41,    42,         &
    42,    43,    43,    43,    44,    38,    39,    39,    39,    40,         &
    40,    41,    41,    41,    42,    42,    43,    43,    43,    44,         &
    56,    57,    57,    57,    58,    58,    59,    59,    59,    60,         &
    60,    61,    61,    61,    62,    56,    57,    57,    57,    58,         &
    58,    59,    59,    59,    60,    60,    61,    61,    61,    62,         &
    74,    75,    75,    75,    76,    76,    77,    77,    77,    78,         &
    78,    79,    79,    79,    80,    74,    75,    75,    75,    76,         &
    76,    77,    77,    77,    78,    78,    79,    79,    79,    80,         &
    74,    75,    75,    75,    76,    76,    77,    77,    77,    78,         &
    78,    79,    79,    79,    80,    92,    93,    93,    93,    94,         &
    94,    95,    95,    95,    96,    96,    97,    97,    97,    98          &
 ]
index_bottom_right =  [                                                        &
    21,    22,    22,    22,    23,    23,    24,    24,    24,    25,         &
    25,    26,    26,    26,    27,    39,    40,    40,    40,    41,         &
    41,    42,    42,    42,    43,    43,    44,    44,    44,    45,         &
    39,    40,    40,    40,    41,    41,    42,    42,    42,    43,         &
    43,    44,    44,    44,    45,    39,    40,    40,    40,    41,         &
    41,    42,    42,    42,    43,    43,    44,    44,    44,    45,         &
    57,    58,    58,    58,    59,    59,    60,    60,    60,    61,         &
    61,    62,    62,    62,    63,    57,    58,    58,    58,    59,         &
    59,    60,    60,    60,    61,    61,    62,    62,    62,    63,         &
    75,    76,    76,    76,    77,    77,    78,    78,    78,    79,         &
    79,    80,    80,    80,    81,    75,    76,    76,    76,    77,         &
    77,    78,    78,    78,    79,    79,    80,    80,    80,    81,         &
    75,    76,    76,    76,    77,    77,    78,    78,    78,    79,         &
    79,    80,    80,    80,    81,    93,    94,    94,    94,    95,         &
    95,    96,    96,    96,    97,    97,    98,    98,    98,    99          &
 ]

END SUBROUTINE return_two_grid_indices

!------------------------------------------------------------------------------!

SUBROUTINE return_4_sets_of_weights( weight_t_r, weight_b_r,                   &
                                     weight_t_l, weight_b_l )
! returns 4 arrays of weights used by multiple tests.

IMPLICIT NONE 

REAL(KIND=real64), INTENT(OUT)          :: weight_t_r(:),                      &
                                           weight_b_r(:),                      &
                                           weight_t_l(:),                      &
                                           weight_b_l(:)

weight_t_r =    [ &
     0.3600_real64,      0.0000_real64,      0.2400_real64,                    &
     0.4800_real64,      0.1200_real64,      0.3600_real64,                    &
     0.0000_real64,      0.2400_real64,      0.4800_real64,                    &
     0.1200_real64,      0.3600_real64,      0.0000_real64,                    &
     0.2400_real64,      0.4800_real64,      0.1200_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.2400_real64,      0.0000_real64,      0.1600_real64,                    &
     0.3200_real64,      0.0800_real64,      0.2400_real64,                    &
     0.0000_real64,      0.1600_real64,      0.3200_real64,                    &
     0.0800_real64,      0.2400_real64,      0.0000_real64,                    &
     0.1600_real64,      0.3200_real64,      0.0800_real64,                    &
     0.4800_real64,      0.0000_real64,      0.3200_real64,                    &
     0.6400_real64,      0.1600_real64,      0.4800_real64,                    &
     0.0000_real64,      0.3200_real64,      0.6400_real64,                    &
     0.1600_real64,      0.4800_real64,      0.0000_real64,                    &
     0.3200_real64,      0.6400_real64,      0.1600_real64,                    &
     0.1200_real64,      0.0000_real64,      0.0800_real64,                    &
     0.1600_real64,      0.0400_real64,      0.1200_real64,                    &
     0.0000_real64,      0.0800_real64,      0.1600_real64,                    &
     0.0400_real64,      0.1200_real64,      0.0000_real64,                    &
     0.0800_real64,      0.1600_real64,      0.0400_real64,                    &
     0.3600_real64,      0.0000_real64,      0.2400_real64,                    &
     0.4800_real64,      0.1200_real64,      0.3600_real64,                    &
     0.0000_real64,      0.2400_real64,      0.4800_real64,                    &
     0.1200_real64,      0.3600_real64,      0.0000_real64,                    &
     0.2400_real64,      0.4800_real64,      0.1200_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.2400_real64,      0.0000_real64,      0.1600_real64,                    &
     0.3200_real64,      0.0800_real64,      0.2400_real64,                    &
     0.0000_real64,      0.1600_real64,      0.3200_real64,                    &
     0.0800_real64,      0.2400_real64,      0.0000_real64,                    &
     0.1600_real64,      0.3200_real64,      0.0800_real64,                    &
     0.4800_real64,      0.0000_real64,      0.3200_real64,                    &
     0.6400_real64,      0.1600_real64,      0.4800_real64,                    &
     0.0000_real64,      0.3200_real64,      0.6400_real64,                    &
     0.1600_real64,      0.4800_real64,      0.0000_real64,                    &
     0.3200_real64,      0.6400_real64,      0.1600_real64,                    &
     0.1200_real64,      0.0000_real64,      0.0800_real64,                    &
     0.1600_real64,      0.0400_real64,      0.1200_real64,                    &
     0.0000_real64,      0.0800_real64,      0.1600_real64,                    &
     0.0400_real64,      0.1200_real64,      0.0000_real64,                    &
     0.0800_real64,      0.1600_real64,      0.0400_real64                     &
 ]
weight_b_r =    [ &
     0.2400_real64,      0.0000_real64,      0.1600_real64,                    &
     0.3200_real64,      0.0800_real64,      0.2400_real64,                    &
     0.0000_real64,      0.1600_real64,      0.3200_real64,                    &
     0.0800_real64,      0.2400_real64,      0.0000_real64,                    &
     0.1600_real64,      0.3200_real64,      0.0800_real64,                    &
     0.6000_real64,      0.0000_real64,      0.4000_real64,                    &
     0.8000_real64,      0.2000_real64,      0.6000_real64,                    &
     0.0000_real64,      0.4000_real64,      0.8000_real64,                    &
     0.2000_real64,      0.6000_real64,      0.0000_real64,                    &
     0.4000_real64,      0.8000_real64,      0.2000_real64,                    &
     0.3600_real64,      0.0000_real64,      0.2400_real64,                    &
     0.4800_real64,      0.1200_real64,      0.3600_real64,                    &
     0.0000_real64,      0.2400_real64,      0.4800_real64,                    &
     0.1200_real64,      0.3600_real64,      0.0000_real64,                    &
     0.2400_real64,      0.4800_real64,      0.1200_real64,                    &
     0.1200_real64,      0.0000_real64,      0.0800_real64,                    &
     0.1600_real64,      0.0400_real64,      0.1200_real64,                    &
     0.0000_real64,      0.0800_real64,      0.1600_real64,                    &
     0.0400_real64,      0.1200_real64,      0.0000_real64,                    &
     0.0800_real64,      0.1600_real64,      0.0400_real64,                    &
     0.4800_real64,      0.0000_real64,      0.3200_real64,                    &
     0.6400_real64,      0.1600_real64,      0.4800_real64,                    &
     0.0000_real64,      0.3200_real64,      0.6400_real64,                    &
     0.1600_real64,      0.4800_real64,      0.0000_real64,                    &
     0.3200_real64,      0.6400_real64,      0.1600_real64,                    &
     0.2400_real64,      0.0000_real64,      0.1600_real64,                    &
     0.3200_real64,      0.0800_real64,      0.2400_real64,                    &
     0.0000_real64,      0.1600_real64,      0.3200_real64,                    &
     0.0800_real64,      0.2400_real64,      0.0000_real64,                    &
     0.1600_real64,      0.3200_real64,      0.0800_real64,                    &
     0.6000_real64,      0.0000_real64,      0.4000_real64,                    &
     0.8000_real64,      0.2000_real64,      0.6000_real64,                    &
     0.0000_real64,      0.4000_real64,      0.8000_real64,                    &
     0.2000_real64,      0.6000_real64,      0.0000_real64,                    &
     0.4000_real64,      0.8000_real64,      0.2000_real64,                    &
     0.3600_real64,      0.0000_real64,      0.2400_real64,                    &
     0.4800_real64,      0.1200_real64,      0.3600_real64,                    &
     0.0000_real64,      0.2400_real64,      0.4800_real64,                    &
     0.1200_real64,      0.3600_real64,      0.0000_real64,                    &
     0.2400_real64,      0.4800_real64,      0.1200_real64,                    &
     0.1200_real64,      0.0000_real64,      0.0800_real64,                    &
     0.1600_real64,      0.0400_real64,      0.1200_real64,                    &
     0.0000_real64,      0.0800_real64,      0.1600_real64,                    &
     0.0400_real64,      0.1200_real64,      0.0000_real64,                    &
     0.0800_real64,      0.1600_real64,      0.0400_real64,                    &
     0.4800_real64,      0.0000_real64,      0.3200_real64,                    &
     0.6400_real64,      0.1600_real64,      0.4800_real64,                    &
     0.0000_real64,      0.3200_real64,      0.6400_real64,                    &
     0.1600_real64,      0.4800_real64,      0.0000_real64,                    &
     0.3200_real64,      0.6400_real64,      0.1600_real64                     &
 ]
weight_t_l =    [ &
     0.2400_real64,      0.6000_real64,      0.3600_real64,                    &
     0.1200_real64,      0.4800_real64,      0.2400_real64,                    &
     0.6000_real64,      0.3600_real64,      0.1200_real64,                    &
     0.4800_real64,      0.2400_real64,      0.6000_real64,                    &
     0.3600_real64,      0.1200_real64,      0.4800_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.1600_real64,      0.4000_real64,      0.2400_real64,                    &
     0.0800_real64,      0.3200_real64,      0.1600_real64,                    &
     0.4000_real64,      0.2400_real64,      0.0800_real64,                    &
     0.3200_real64,      0.1600_real64,      0.4000_real64,                    &
     0.2400_real64,      0.0800_real64,      0.3200_real64,                    &
     0.3200_real64,      0.8000_real64,      0.4800_real64,                    &
     0.1600_real64,      0.6400_real64,      0.3200_real64,                    &
     0.8000_real64,      0.4800_real64,      0.1600_real64,                    &
     0.6400_real64,      0.3200_real64,      0.8000_real64,                    &
     0.4800_real64,      0.1600_real64,      0.6400_real64,                    &
     0.0800_real64,      0.2000_real64,      0.1200_real64,                    &
     0.0400_real64,      0.1600_real64,      0.0800_real64,                    &
     0.2000_real64,      0.1200_real64,      0.0400_real64,                    &
     0.1600_real64,      0.0800_real64,      0.2000_real64,                    &
     0.1200_real64,      0.0400_real64,      0.1600_real64,                    &
     0.2400_real64,      0.6000_real64,      0.3600_real64,                    &
     0.1200_real64,      0.4800_real64,      0.2400_real64,                    &
     0.6000_real64,      0.3600_real64,      0.1200_real64,                    &
     0.4800_real64,      0.2400_real64,      0.6000_real64,                    &
     0.3600_real64,      0.1200_real64,      0.4800_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.0000_real64,      0.0000_real64,      0.0000_real64,                    &
     0.1600_real64,      0.4000_real64,      0.2400_real64,                    &
     0.0800_real64,      0.3200_real64,      0.1600_real64,                    &
     0.4000_real64,      0.2400_real64,      0.0800_real64,                    &
     0.3200_real64,      0.1600_real64,      0.4000_real64,                    &
     0.2400_real64,      0.0800_real64,      0.3200_real64,                    &
     0.3200_real64,      0.8000_real64,      0.4800_real64,                    &
     0.1600_real64,      0.6400_real64,      0.3200_real64,                    &
     0.8000_real64,      0.4800_real64,      0.1600_real64,                    &
     0.6400_real64,      0.3200_real64,      0.8000_real64,                    &
     0.4800_real64,      0.1600_real64,      0.6400_real64,                    &
     0.0800_real64,      0.2000_real64,      0.1200_real64,                    &
     0.0400_real64,      0.1600_real64,      0.0800_real64,                    &
     0.2000_real64,      0.1200_real64,      0.0400_real64,                    &
     0.1600_real64,      0.0800_real64,      0.2000_real64,                    &
     0.1200_real64,      0.0400_real64,      0.1600_real64                     &
 ]
weight_b_l =    [ &
     0.1600_real64,      0.4000_real64,      0.2400_real64,                    &
     0.0800_real64,      0.3200_real64,      0.1600_real64,                    &
     0.4000_real64,      0.2400_real64,      0.0800_real64,                    &
     0.3200_real64,      0.1600_real64,      0.4000_real64,                    &
     0.2400_real64,      0.0800_real64,      0.3200_real64,                    &
     0.4000_real64,      1.0000_real64,      0.6000_real64,                    &
     0.2000_real64,      0.8000_real64,      0.4000_real64,                    &
     1.0000_real64,      0.6000_real64,      0.2000_real64,                    &
     0.8000_real64,      0.4000_real64,      1.0000_real64,                    &
     0.6000_real64,      0.2000_real64,      0.8000_real64,                    &
     0.2400_real64,      0.6000_real64,      0.3600_real64,                    &
     0.1200_real64,      0.4800_real64,      0.2400_real64,                    &
     0.6000_real64,      0.3600_real64,      0.1200_real64,                    &
     0.4800_real64,      0.2400_real64,      0.6000_real64,                    &
     0.3600_real64,      0.1200_real64,      0.4800_real64,                    &
     0.0800_real64,      0.2000_real64,      0.1200_real64,                    &
     0.0400_real64,      0.1600_real64,      0.0800_real64,                    &
     0.2000_real64,      0.1200_real64,      0.0400_real64,                    &
     0.1600_real64,      0.0800_real64,      0.2000_real64,                    &
     0.1200_real64,      0.0400_real64,      0.1600_real64,                    &
     0.3200_real64,      0.8000_real64,      0.4800_real64,                    &
     0.1600_real64,      0.6400_real64,      0.3200_real64,                    &
     0.8000_real64,      0.4800_real64,      0.1600_real64,                    &
     0.6400_real64,      0.3200_real64,      0.8000_real64,                    &
     0.4800_real64,      0.1600_real64,      0.6400_real64,                    &
     0.1600_real64,      0.4000_real64,      0.2400_real64,                    &
     0.0800_real64,      0.3200_real64,      0.1600_real64,                    &
     0.4000_real64,      0.2400_real64,      0.0800_real64,                    &
     0.3200_real64,      0.1600_real64,      0.4000_real64,                    &
     0.2400_real64,      0.0800_real64,      0.3200_real64,                    &
     0.4000_real64,      1.0000_real64,      0.6000_real64,                    &
     0.2000_real64,      0.8000_real64,      0.4000_real64,                    &
     1.0000_real64,      0.6000_real64,      0.2000_real64,                    &
     0.8000_real64,      0.4000_real64,      1.0000_real64,                    &
     0.6000_real64,      0.2000_real64,      0.8000_real64,                    &
     0.2400_real64,      0.6000_real64,      0.3600_real64,                    &
     0.1200_real64,      0.4800_real64,      0.2400_real64,                    &
     0.6000_real64,      0.3600_real64,      0.1200_real64,                    &
     0.4800_real64,      0.2400_real64,      0.6000_real64,                    &
     0.3600_real64,      0.1200_real64,      0.4800_real64,                    &
     0.0800_real64,      0.2000_real64,      0.1200_real64,                    &
     0.0400_real64,      0.1600_real64,      0.0800_real64,                    &
     0.2000_real64,      0.1200_real64,      0.0400_real64,                    &
     0.1600_real64,      0.0800_real64,      0.2000_real64,                    &
     0.1200_real64,      0.0400_real64,      0.1600_real64,                    &
     0.3200_real64,      0.8000_real64,      0.4800_real64,                    &
     0.1600_real64,      0.6400_real64,      0.3200_real64,                    &
     0.8000_real64,      0.4800_real64,      0.1600_real64,                    &
     0.6400_real64,      0.3200_real64,      0.8000_real64,                    &
     0.4800_real64,      0.1600_real64,      0.6400_real64                     &
 ]
END SUBROUTINE return_4_sets_of_weights

!------------------------------------------------------------------------------!

SUBROUTINE test_get_corner_indices
! Subroutine used to return sample dataset of indices in the top/North
! right/East corner of the source grid - the goal here 
! isn't for a numerical workout but rather easy to identify and work with
! numbers
USE f_shum_horizontal_field_interp_mod, ONLY : f_shum_find_source_box_indices

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: points = 35
INTEGER(KIND=int64), PARAMETER :: points_lambda_srce = 18_int64
INTEGER(KIND=int64), PARAMETER :: points_phi_srce = 9_int64

INTEGER(KIND=int64)            :: index_b_l(points)
INTEGER(KIND=int64)            :: index_b_r(points)
INTEGER(KIND=int64)            :: ix(points)
INTEGER(KIND=int64)            :: ixp1(points)
INTEGER(KIND=int64)            :: iy(points)

INTEGER(KIND=int64)            :: index_b_l_out(points)
INTEGER(KIND=int64)            :: index_b_r_out(points)
INTEGER(KIND=int64)            :: ix_out(points)
INTEGER(KIND=int64)            :: ixp1_out(points)
INTEGER(KIND=int64)            :: iy_out(points)

REAL(KIND=real64)   :: lambda_srce(points_lambda_srce)
REAL(KIND=real64)   :: phi_srce(points_phi_srce)
REAL(KIND=real64)   :: lambda_targ(points)
REAL(KIND=real64)   :: phi_targ(points)
REAL(KIND=real64)   :: t_lambda(points)
REAL(KIND=real64)   :: t_lambda_out(points)

LOGICAL(KIND=int64)             :: cyclic
LOGICAL                         :: create_last_point

INTEGER(KIND=int32)            :: i
INTEGER(KIND=int32) :: status
CHARACTER(LEN=500)  :: message

create_last_point = .FALSE.

CALL generate_1d_reg_grid_coords(0.000_real64, 360.000_real64,                 &
                                 points_lambda_srce, lambda_srce,              &
                                 .FALSE.)

CALL generate_1d_reg_grid_coords(-90.000_real64, 90.000_real64,                &
                                 points_phi_srce, phi_srce,                    &
                                 create_last_point)


CALL generate_2d_reg_grid_coords(45.000_real64, 90.000_real64,                 &
                                 297.000_real64, 360.000_real64,               &
                                 5_int64, 7_int64,                             &
                                 phi_targ, lambda_targ,                        &
                                 create_last_point)

ix_out   = [  15,    16,    16,    17,    17,    18,    18,                    &
              15,    16,    16,    17,    17,    18,    18,                    &
              15,    16,    16,    17,    17,    18,    18,                    &
              15,    16,    16,    17,    17,    18,    18,                    &
              15,    16,    16,    17,    17,    18,    18]
ixp1_out = [  16,    17,    17,    18,    18,     1,     1,                    &
              16,    17,    17,    18,    18,     1,     1,                    &
              16,    17,    17,    18,    18,     1,     1,                    &
              16,    17,    17,    18,    18,     1,     1,                    &
              16,    17,    17,    18,    18,     1,     1]
iy_out   = [   7,     7,     7,     7,     7,     7,     7,                    &
               8,     8,     8,     8,     8,     8,     8,                    &
               8,     8,     8,     8,     8,     8,     8,                    &
               8,     8,     8,     8,     8,     8,     8,                    &
               8,     8,     8,     8,     8,     8,     8]
index_b_l_out = [ 123,   124,   124,   125,   125,   126,   126,               &
                  141,   142,   142,   143,   143,   144,   144,               &
                  141,   142,   142,   143,   143,   144,   144,               &
                  141,   142,   142,   143,   143,   144,   144,               &
                  141,   142,   142,   143,   143,   144,   144]
index_b_r_out = [ 124,   125,   125,   126,   126,   109,   109,               &
                  142,   143,   143,   144,   144,   127,   127,               &
                  142,   143,   143,   144,   144,   127,   127,               &
                  142,   143,   143,   144,   144,   127,   127,               &
                  142,   143,   143,   144,   144,   127,   127]
! t_lambda_out is lambda_targ mapped to between 0 and 360 degrees East of
! the West most point of lambda_srce.
! (which has to be the 1st point of lambda_srce)
DO i=1, points
  t_lambda_out(i) = MOD(((lambda_targ(i)-lambda_srce(1))+1440.0_real64),       &
                          360.0_real64) + lambda_srce(1)
END DO

cyclic = .TRUE.

CALL f_shum_find_source_box_indices                                            &
                  ( index_b_l, index_b_r                                       &
                  , lambda_srce, phi_srce, lambda_targ, phi_targ               &
                  , points_lambda_srce, points_phi_srce, cyclic                &
                  , t_lambda, ixp1, ix, iy )

CALL assert_equals(ix_out, ix, INT(points), "array of x indicies is incorrect")

CALL assert_equals(ixp1_out, ixp1, INT(points),                                &
                 "array of x_plus_1 indicies is incorrect")

CALL assert_equals(iy_out, iy, INT(points), "array of y indicies is incorrect")

CALL assert_equals(index_b_l_out, index_b_l, INT(points),                      &
                   "array of bottom left indicies is incorrect")

CALL assert_equals(index_b_r_out, index_b_r, INT(points),                      &
                   "array of bottom right indicies is incorrect")

CALL assert_equals(t_lambda_out, t_lambda, INT(points), tolerance,             &
                   "array of target lambda points is incorrect")

END SUBROUTINE test_get_corner_indices

!------------------------------------------------------------------------------!

SUBROUTINE test_get_simple_indices
! Subroutine used to return sample dataset of indices generically well within
! the source grid - the goal here 
! isn't for a numerical workout but rather easy to identify and work with
! numbers

USE f_shum_horizontal_field_interp_mod, ONLY : f_shum_find_source_box_indices

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: points = 150_int64
INTEGER(KIND=int64), PARAMETER :: points_lambda_srce = 18_int64
INTEGER(KIND=int64), PARAMETER :: points_phi_srce = 9_int64

INTEGER(KIND=int64)            :: index_b_l(points)
INTEGER(KIND=int64)            :: index_b_r(points)
INTEGER(KIND=int64)            :: ix(points)
INTEGER(KIND=int64)            :: ixp1(points)
INTEGER(KIND=int64)            :: iy(points)

INTEGER(KIND=int64)            :: index_b_l_out(points)
INTEGER(KIND=int64)            :: index_b_r_out(points)
INTEGER(KIND=int64)            :: ix_out(points)
INTEGER(KIND=int64)            :: ixp1_out(points)
INTEGER(KIND=int64)            :: iy_out(points)

REAL(KIND=real64)   :: lambda_srce(points_lambda_srce)
REAL(KIND=real64)   :: phi_srce(points_phi_srce)
REAL(KIND=real64)   :: lambda_targ(points)
REAL(KIND=real64)   :: phi_targ(points)
REAL(KIND=real64)   :: t_lambda(points)
REAL(KIND=real64)   :: t_lambda_out(points)

LOGICAL(KIND=int64)             :: cyclic
LOGICAL                         :: create_last_point

INTEGER(KIND=int32)            :: i
INTEGER(KIND=int32) :: status
CHARACTER(LEN=500)  :: message

create_last_point = .FALSE.

CALL generate_1d_reg_grid_coords(0.000_real64, 360.000_real64,                 &
                                 points_lambda_srce, lambda_srce,              &
                                 .FALSE.)

CALL generate_1d_reg_grid_coords(-90.000_real64, 90.000_real64,                &
                                 points_phi_srce, phi_srce,                    &
                                 create_last_point)


CALL generate_2d_reg_grid_coords(-58.000_real64, 22.000_real64,                &
                                 32.000_real64, 152.000_real64,                &
                                 10_int64, 15_int64,                           &
                                 phi_targ, lambda_targ,                        &
                                 create_last_point)

CALL return_three_grid_indices( ix_out, iy_out, ixp1_out )

CALL return_two_grid_indices( index_b_l_out, index_b_r_out )

! t_lambda_out is lambda_targ mapped to between 0 and 360 degrees East of
! the West most point of lambda_srce.
! (which has to be the 1st point of lambda_srce)
DO i=1, points
  t_lambda_out(i) = MOD(((lambda_targ(i)-lambda_srce(1))+1440.0_real64),       &
                          360.0_real64) + lambda_srce(1)
END DO


cyclic = .TRUE.

CALL f_shum_find_source_box_indices                                            &
                  ( index_b_l, index_b_r                                       &
                  , lambda_srce, phi_srce, lambda_targ, phi_targ               &
                  , points_lambda_srce, points_phi_srce, cyclic                &
                  , t_lambda, ixp1, ix, iy )

CALL assert_equals(ix_out, ix, INT(points), "array of x indicies is incorrect")

CALL assert_equals(ixp1_out, ixp1, INT(points),                                &
                 "array of x_plus_1 indicies is incorrect")

CALL assert_equals(iy_out, iy, INT(points), "array of y indicies is incorrect")

CALL assert_equals(index_b_l_out, index_b_l, INT(points),                      &
                   "array of bottom left indicies is incorrect")

CALL assert_equals(index_b_r_out, index_b_r, INT(points),                      &
                   "array of bottom right indicies is incorrect")

CALL assert_equals(t_lambda_out, t_lambda, INT(points), tolerance,             &
                   "array of target lambda points is incorrect")

END SUBROUTINE test_get_simple_indices

!------------------------------------------------------------------------------!

SUBROUTINE test_get_wrapped_indices
! Subroutine used to return sample dataset of indices - The target grid has been
! specified 'beyond' the source such that when it is adjusted to be at
! lambda_source(1) to lambda_source91) + 360 it will fit withing the source.
! the goal here isn't for a numerical workout but rather easy to identify and 
! work with numbers

USE f_shum_horizontal_field_interp_mod, ONLY : f_shum_find_source_box_indices

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: points = 150
INTEGER(KIND=int64), PARAMETER :: points_lambda_srce = 18_int64
INTEGER(KIND=int64), PARAMETER :: points_phi_srce = 9_int64

INTEGER(KIND=int64)            :: index_b_l(points)
INTEGER(KIND=int64)            :: index_b_r(points)
INTEGER(KIND=int64)            :: ix(points)
INTEGER(KIND=int64)            :: ixp1(points)
INTEGER(KIND=int64)            :: iy(points)

INTEGER(KIND=int64)            :: index_b_l_out(points)
INTEGER(KIND=int64)            :: index_b_r_out(points)
INTEGER(KIND=int64)            :: ix_out(points)
INTEGER(KIND=int64)            :: ixp1_out(points)
INTEGER(KIND=int64)            :: iy_out(points)

REAL(KIND=real64)   :: lambda_srce(points_lambda_srce)
REAL(KIND=real64)   :: phi_srce(points_phi_srce)
REAL(KIND=real64)   :: lambda_targ(points)
REAL(KIND=real64)   :: phi_targ(points)
REAL(KIND=real64)   :: t_lambda(points)
REAL(KIND=real64)   :: t_lambda_out(points)

LOGICAL(KIND=int64)             :: cyclic
LOGICAL                         :: create_last_point

INTEGER(KIND=int32)            :: i
INTEGER(KIND=int32) :: status
CHARACTER(LEN=500)  :: message

create_last_point = .FALSE.

CALL generate_1d_reg_grid_coords(0.000_real64, 360.000_real64,                 &
                                 points_lambda_srce, lambda_srce,              &
                                 .FALSE.)

CALL generate_1d_reg_grid_coords(-90.000_real64, 90.000_real64,                &
                                 points_phi_srce, phi_srce,                    &
                                 create_last_point)


CALL generate_2d_reg_grid_coords(-58.000_real64, 22.000_real64,                &
                                 392.000_real64, 512.000_real64,               &
                                 10_int64, 15_int64,                           &
                                 phi_targ, lambda_targ,                        &
                                 create_last_point)

CALL return_three_grid_indices( ix_out, iy_out, ixp1_out )

CALL return_two_grid_indices( index_b_l_out, index_b_r_out )

! t_lambda_out is lambda_targ mapped to between 0 and 360 degrees East of
! the West most point of lambda_srce.
! (which has to be the 1st point of lambda_srce)
DO i=1, points
  t_lambda_out(i) = MOD(((lambda_targ(i)-lambda_srce(1))+1440.0_real64),       &
                          360.0_real64) + lambda_srce(1)
END DO


cyclic = .TRUE.

CALL f_shum_find_source_box_indices                                            &
                  ( index_b_l, index_b_r                                       &
                  , lambda_srce, phi_srce, lambda_targ, phi_targ               &
                  , points_lambda_srce, points_phi_srce, cyclic                &
                  , t_lambda, ixp1, ix, iy )

CALL assert_equals(ix_out, ix, INT(points), "array of x indicies is incorrect")

CALL assert_equals(ixp1_out, ixp1, INT(points),                                &
                 "array of x_plus_1 indicies is incorrect")

CALL assert_equals(iy_out, iy, INT(points), "array of y indicies is incorrect")

CALL assert_equals(index_b_l_out, index_b_l, INT(points),                      &
                   "array of bottom left indicies is incorrect")

CALL assert_equals(index_b_r_out, index_b_r, INT(points),                      &
                   "array of bottom right indicies is incorrect")

CALL assert_equals(t_lambda_out, t_lambda, INT(points), tolerance,   &
                   "array of target lambda points is incorrect")

END SUBROUTINE test_get_wrapped_indices

!------------------------------------------------------------------------------!

SUBROUTINE test_get_simple_weights
! Subroutine used to return sample dataset of weights

USE f_shum_horizontal_field_interp_mod, ONLY : f_shum_calc_weights

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: points = 150
INTEGER(KIND=int64), PARAMETER :: points_lambda_srce = 18_int64
INTEGER(KIND=int64), PARAMETER :: points_phi_srce = 9_int64

INTEGER(KIND=int64)            :: ix(points)
INTEGER(KIND=int64)            :: ixp1(points)
INTEGER(KIND=int64)            :: iy(points)

REAL(KIND=real64)   :: weight_t_r(points)
REAL(KIND=real64)   :: weight_b_r(points)
REAL(KIND=real64)   :: weight_t_l(points)
REAL(KIND=real64)   :: weight_b_l(points)

REAL(KIND=real64)   :: weight_t_r_out(points)
REAL(KIND=real64)   :: weight_b_r_out(points)
REAL(KIND=real64)   :: weight_t_l_out(points)
REAL(KIND=real64)   :: weight_b_l_out(points)

REAL(KIND=real64)   :: lambda_srce(points_lambda_srce)
REAL(KIND=real64)   :: phi_srce(points_phi_srce)
REAL(KIND=real64)   :: phi_targ(points)
REAL(KIND=real64)   :: t_lambda(points)

LOGICAL             :: create_last_point

INTEGER(KIND=int32)            :: i
INTEGER(KIND=int32) :: status
CHARACTER(LEN=500)  :: message

create_last_point = .FALSE.

CALL generate_1d_reg_grid_coords(0.000_real64, 360.000_real64,                 &
                                 points_lambda_srce, lambda_srce,              &
                                 .FALSE.)

CALL generate_1d_reg_grid_coords(-90.000_real64, 90.000_real64,                &
                                 points_phi_srce, phi_srce,                    &
                                 create_last_point)


CALL generate_2d_reg_grid_coords(-58.000_real64, 22.000_real64,                &
                                 32.000_real64, 152.000_real64,                &
                                 10_int64, 15_int64,                           &
                                 phi_targ, t_lambda,                           &
                                 create_last_point)

CALL return_three_grid_indices( ix, iy, ixp1 )

CALL return_4_sets_of_weights( weight_t_r_out, weight_b_r_out,                 &
                               weight_t_l_out, weight_b_l_out )

CALL f_shum_calc_weights                                                       &
                  ( weight_t_r, weight_b_r, weight_t_l, weight_b_l             &
                  , lambda_srce, phi_srce, phi_targ                            &
                  , points_lambda_srce, points_phi_srce                        &
                  , t_lambda, ixp1, ix, iy )

CALL assert_equals(weight_t_r_out, weight_t_r, INT(points), tolerance,         &
                 "array of top right weights is incorrect")

CALL assert_equals(weight_b_r_out, weight_b_r, INT(points), tolerance,         &
                  "array of bottom right weights is incorrect")

CALL assert_equals(weight_t_l_out, weight_t_l, INT(points), tolerance,         &
                   "array of top left weights is incorrect")

CALL assert_equals(weight_b_l_out, weight_b_l, INT(points), tolerance,         &
                   "array of bottom left weights is incorrect")


END SUBROUTINE test_get_simple_weights

!------------------------------------------------------------------------------!

SUBROUTINE test_apply_weights
! Subroutine used to calculate interpolated values using indices and weights
! tested and calculated by previous routines.

USE f_shum_horizontal_field_interp_mod, ONLY :                                 &
                f_shum_horizontal_field_bi_lin_interp_calc

IMPLICIT NONE 

INTEGER(KIND=int64), PARAMETER :: rows = 10_int64
INTEGER(KIND=int64), PARAMETER :: row_length = 18_int64
INTEGER(KIND=int64), PARAMETER :: len_field = 150_int64

INTEGER(KIND=int64)            :: index_b_l(len_field)
INTEGER(KIND=int64)            :: index_b_r(len_field)
REAL(KIND=real64)   :: weight_t_r(len_field)
REAL(KIND=real64)   :: weight_b_r(len_field)
REAL(KIND=real64)   :: weight_t_l(len_field)
REAL(KIND=real64)   :: weight_b_l(len_field)

REAL(KIND=real64)   :: data_in(rows * row_length)
REAL(KIND=real64)   :: data_out(len_field)
REAL(KIND=real64)   :: data_out_ref(len_field)

INTEGER(KIND=int32) :: status
CHARACTER(LEN=500)  :: message

CALL return_4_sets_of_weights( weight_t_r, weight_b_r,                         &
                               weight_t_l, weight_b_l )

CALL return_two_grid_indices( index_b_l, index_b_r)

CALL create_four_value_source_grid( data_in, row_length, rows,                 &
                                    5_int64, 4_int64,                          &
                                    10.00_real64, 40.00_real64, 50.00_real64)

data_out_ref =    [                                                            &
    10.0000_real64,     10.0000_real64,     10.0000_real64,                    &
    10.0000_real64,     10.0000_real64,     10.0000_real64,                    &
    10.0000_real64,     26.0000_real64,     42.0000_real64,                    &
    50.0000_real64,     50.0000_real64,     50.0000_real64,                    &
    50.0000_real64,     50.0000_real64,     50.0000_real64,                    &
    10.0000_real64,     10.0000_real64,     10.0000_real64,                    &
    10.0000_real64,     10.0000_real64,     10.0000_real64,                    &
    10.0000_real64,     26.0000_real64,     42.0000_real64,                    &
    50.0000_real64,     50.0000_real64,     50.0000_real64,                    &
    50.0000_real64,     50.0000_real64,     50.0000_real64,                    &
    10.0000_real64,     10.0000_real64,     10.0000_real64,                    &
    10.0000_real64,     10.0000_real64,     10.0000_real64,                    &
    10.0000_real64,     26.0000_real64,     42.0000_real64,                    &
    50.0000_real64,     50.0000_real64,     50.0000_real64,                    &
    50.0000_real64,     50.0000_real64,     50.0000_real64,                    &
    10.0000_real64,     10.0000_real64,     10.0000_real64,                    &
    10.0000_real64,     10.0000_real64,     10.0000_real64,                    &
    10.0000_real64,     26.0000_real64,     42.0000_real64,                    &
    50.0000_real64,     50.0000_real64,     50.0000_real64,                    &
    50.0000_real64,     50.0000_real64,     50.0000_real64,                    &
    20.0000_real64,     20.0000_real64,     20.0000_real64,                    &
    20.0000_real64,     20.0000_real64,     20.0000_real64,                    &
    20.0000_real64,     36.0000_real64,     52.0000_real64,                    &
    60.0000_real64,     60.0000_real64,     60.0000_real64,                    &
    60.0000_real64,     60.0000_real64,     60.0000_real64,                    &
    40.0000_real64,     40.0000_real64,     40.0000_real64,                    &
    40.0000_real64,     40.0000_real64,     40.0000_real64,                    &
    40.0000_real64,     56.0000_real64,     72.0000_real64,                    &
    80.0000_real64,     80.0000_real64,     80.0000_real64,                    &
    80.0000_real64,     80.0000_real64,     80.0000_real64,                    &
    60.0000_real64,     60.0000_real64,     60.0000_real64,                    &
    60.0000_real64,     60.0000_real64,     60.0000_real64,                    &
    60.0000_real64,     76.0000_real64,     92.0000_real64,                    &
   100.0000_real64,    100.0000_real64,    100.0000_real64,                    &
   100.0000_real64,    100.0000_real64,    100.0000_real64,                    &
    60.0000_real64,     60.0000_real64,     60.0000_real64,                    &
    60.0000_real64,     60.0000_real64,     60.0000_real64,                    &
    60.0000_real64,     76.0000_real64,     92.0000_real64,                    &
   100.0000_real64,    100.0000_real64,    100.0000_real64,                    &
   100.0000_real64,    100.0000_real64,    100.0000_real64,                    &
    60.0000_real64,     60.0000_real64,     60.0000_real64,                    &
    60.0000_real64,     60.0000_real64,     60.0000_real64,                    &
    60.0000_real64,     76.0000_real64,     92.0000_real64,                    &
   100.0000_real64,    100.0000_real64,    100.0000_real64,                    &
   100.0000_real64,    100.0000_real64,    100.0000_real64,                    &
    60.0000_real64,     60.0000_real64,     60.0000_real64,                    &
    60.0000_real64,     60.0000_real64,     60.0000_real64,                    &
    60.0000_real64,     76.0000_real64,     92.0000_real64,                    &
   100.0000_real64,    100.0000_real64,    100.0000_real64,                    &
   100.0000_real64,    100.0000_real64,    100.0000_real64                     &
 ]

CALL f_shum_horizontal_field_bi_lin_interp_calc                                &
                  ( rows, row_length                                           &
                  , index_b_l, index_b_r, data_in                              &
                  , weight_b_l, weight_b_r, weight_t_l, weight_t_r             &
                  , data_out )

CALL assert_equals(data_out_ref, data_out, INT(len_field), tolerance,          &
                   "Horizontally interpolated data is incorrect")

END SUBROUTINE test_apply_weights

!------------------------------------------------------------------------------!

END MODULE fruit_test_shum_horizontal_field_interp_mod
