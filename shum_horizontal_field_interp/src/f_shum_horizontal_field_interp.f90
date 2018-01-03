! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
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
!
!*******************************************************************************
!
! Description : Routines to perform bi-liner interpolation on a horizontal
!             : field. Calculating the coefficients and then using them to
!             : calculate the new value.
!
MODULE f_shum_horizontal_field_interp_mod

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_FLOAT, C_DOUBLE

IMPLICIT NONE

PRIVATE

PUBLIC :: f_shum_horizontal_field_bi_lin_interp_get_coeffs                    &
        , f_shum_horizontal_field_bi_lin_interp_calc                          &
        , f_shum_find_source_box_indices                                      &
        , f_shum_calc_weights


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

!------------------------------------------------------------------------------!

SUBROUTINE f_shum_horizontal_field_bi_lin_interp_calc                          &
                  ( rows_in, row_length_in, len_field_out                      &
                  , index_b_l, index_b_r, data_in                              &
                  , weight_b_l, weight_b_r, weight_t_l, weight_t_r             &
                  , data_out )

IMPLICIT NONE

! Description:
!   Carries out bi-linear horizontal interpolation using coefficients
!   and gather indices calculated in subroutine
!   f_shum_horizontal_field_bi-lin_interp_get_coeffs

! Subroutine arguments
! Scalar arguments
INTEGER(KIND=int64), INTENT(IN) :: rows_in
                                 ! Number of P rows on source grid
INTEGER(KIND=int64), INTENT(IN) :: row_length_in
                                 ! Number of pts per row on source grid
INTEGER(KIND=int64), INTENT(IN) :: len_field_out
                                 ! Number of points on target grid

! Array arguments
INTEGER(KIND=int64), INTENT(IN) :: index_b_l(len_field_out)
                                 ! Index of bottom left
                                 ! corner of source gridbox
INTEGER(KIND=int64), INTENT(IN) :: index_b_r(len_field_out)
                                 ! Index of bottom right
                                 ! corner of source gridbox

REAL(KIND=real64), INTENT(IN) :: data_in(rows_in*row_length_in)
                               ! Data before interpolation

REAL(KIND=real64), INTENT(IN) :: weight_b_l(len_field_out)
                               ! Weight applied to value at bot.
                               ! left corner of source gridbox
REAL(KIND=real64), INTENT(IN) :: weight_b_r(len_field_out)
                               ! Weight applied to value at bot.
                               ! right corner of source gridbox
REAL(KIND=real64), INTENT(IN) :: weight_t_l(len_field_out)
                               ! Weight applied to value at top
                               ! left corner of source gridbox
REAL(KIND=real64), INTENT(IN) :: weight_t_r(len_field_out)
                               ! Weight applied to value at top
                               ! right corner of source gridbox
REAL(KIND=real64), INTENT(OUT) :: data_out(len_field_out)
                                ! Data after interpolation

! Local scalars:
INTEGER(KIND=int64) :: i  ! loop index

! 1. Carry out horizontal interpolation

!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE)                               &
!$OMP&         SHARED(len_field_out, data_out, weight_b_l, weight_b_r,         &
!$OMP&                weight_t_l, weight_t_r, index_b_l, index_b_r,            &
!$OMP&                data_in, row_length_in)                                  &
!$OMP&         PRIVATE(i)
DO i=1, len_field_out

  data_out(i) = weight_b_l(i)*data_in(index_b_l(i))                            &
              + weight_b_r(i)*data_in(index_b_r(i))                            &
              + weight_t_l(i)*data_in(index_b_l(i)+row_length_in)              &
              + weight_t_r(i)*data_in(index_b_r(i)+row_length_in)

END DO
!$OMP END PARALLEL DO

RETURN

END SUBROUTINE f_shum_horizontal_field_bi_lin_interp_calc

!------------------------------------------------------------------------------!

!    SUBROUTINE f_shum_horizontal_field_bi-lin_interp_get_coeffs -----
!
!    Purpose:  Calculates bi-linear horizontal interpolation
!              coefficients and gather indices for interpolating
!              between generalised latitude-longitude grids (eg
!              global, regional or rotated lat-lon grid) in which the
!              gridlength may vary with latitude and/or longitude. The
!              interpolation is carried out by subroutine
!              f_shum_horizontal_field_bi_lin_interp_calc.
!              Gather indices point to bottom left hand corner and bottom
!              right hand corner of each grid box on source grid enclosing a
!              target point. Two indices are needed to cater for east-west
!              (lambda direction) cyclic boundaries when the source data is
!              global. If a target point falls outside the domain of the source
!              data, one sided differencing is used. The source latitude
!              coordinates must be supplied in decreasing order. The source
!              long- itude coordinates must be supplied in increasing order,
!              starting at any value, but not wrapping round. The target
!              points may be specified in any order.
!
!   Vector Machines : The original versions of this code had sections designed
!              for improved performance on vector machines controlled by
!              compiler defs. Please see UM code at 10.9 or early revisions of
!              the branch that introduced this code.

SUBROUTINE f_shum_horizontal_field_bi_lin_interp_get_coeffs                    &
                  ( index_b_l, index_b_r                                       &
                  , weight_t_r, weight_b_r, weight_t_l, weight_b_l             &
                  , lambda_srce, phi_srce, lambda_targ, phi_targ               &
                  , points_lambda_srce, points_phi_srce, points, cyclic )

IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: points_lambda_srce
                                  ! Number of lambda points on source grid
INTEGER(KIND=int64), INTENT(IN)  :: points_phi_srce
                                  ! Number of phi points on source grid
INTEGER(KIND=int64), INTENT(IN)  :: points
                                  ! Total number of points on target grid
INTEGER(KIND=int64), INTENT(OUT) :: index_b_l(points)
                                  ! Index of bottom left corner
                                  ! of source gridbox
INTEGER(KIND=int64), INTENT(OUT) :: index_b_r(points)
                                  ! Index of bottom right corner
                                  ! of source gridbox

REAL(KIND=real64), INTENT(IN)  :: lambda_targ(points)
                                ! Lambda coords of target grid in degrees
                                ! using same rotation as source grid
REAL(KIND=real64), INTENT(IN)  :: phi_targ(points)
                                ! Phi coords of target grid in degrees using
                                ! same rotation as source grid
REAL(KIND=real64), INTENT(IN)  :: lambda_srce(points_lambda_srce)
                                ! Lambda coords of source grid in degrees
REAL(KIND=real64), INTENT(IN)  :: phi_srce(points_phi_srce)
                                ! Phi coords of source grid in degrees
REAL(KIND=real64), INTENT(OUT) :: weight_t_r(points)
                                ! Weight applied to value at top
                                ! right corner of source gridbox
REAL(KIND=real64), INTENT(OUT) :: weight_b_l(points)
                                ! Weight applied to value at bot.
                                ! left corner of source gridbox
REAL(KIND=real64), INTENT(OUT) :: weight_b_r(points)
                                ! Weight applied to value at bot.
                                ! right corner of source gridbox
REAL(KIND=real64), INTENT(OUT) :: weight_t_l(points)
                                ! Weight applied to value at top
                                ! left corner of source gridbox

LOGICAL(KIND=int64),  INTENT(IN) :: cyclic ! =T, then source data is cyclic
                                           ! =F, then source data is non-cyclic

!--- Local variables:---------------------------------------------------
REAL(KIND=real64)   :: t_lambda(points) ! Local value of target
                                                 ! longitude

INTEGER(KIND=int64) :: ixp1(points)     ! Longitudinal index plus 1
INTEGER(KIND=int64) :: ix(points)       ! Longitudinal index
INTEGER(KIND=int64) :: iy(points)       ! Latitudinal index
! ----------------------------------------------------------------------

CALL f_shum_find_source_box_indices                                            &
                  ( index_b_l, index_b_r                                       &
                  , lambda_srce, phi_srce, lambda_targ, phi_targ               &
                  , points_lambda_srce, points_phi_srce, points, cyclic        &
                  , t_lambda, ixp1, ix, iy )

CALL f_shum_calc_weights  &
                  ( weight_t_r, weight_b_r, weight_t_l, weight_b_l             &
                  , lambda_srce, phi_srce, phi_targ                            &
                  , points_lambda_srce, points_phi_srce, points                &
                  , t_lambda, ixp1, ix, iy )

END SUBROUTINE f_shum_horizontal_field_bi_lin_interp_get_coeffs

!------------------------------------------------------------------------------!

SUBROUTINE f_shum_find_source_box_indices                                      &
                  ( index_b_l, index_b_r                                       &
                  , lambda_srce, phi_srce, lambda_targ, phi_targ               &
                  , points_lambda_srce, points_phi_srce, points, cyclic        &
                  , t_lambda, ixp1, ix, iy )

IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: points_lambda_srce
                                  ! Number of lambda points on source grid
INTEGER(KIND=int64), INTENT(IN)  :: points_phi_srce
                                  ! Number of phi points on source grid
INTEGER(KIND=int64), INTENT(IN)  :: points
                                  ! Total number of points on target grid
INTEGER(KIND=int64), INTENT(OUT) :: index_b_l(points)
                                  ! Index of bottom left corner
                                  ! of source gridbox
INTEGER(KIND=int64), INTENT(OUT) :: index_b_r(points)
                                  ! Index of bottom right corner
                                  ! of source gridbox
INTEGER(KIND=int64), INTENT(OUT) :: ixp1(points)
                                  ! Longitudinal index plus 1
INTEGER(KIND=int64), INTENT(OUT) :: ix(points)
                                  ! Longitudinal index
INTEGER(KIND=int64), INTENT(OUT) :: iy(points)
                                  ! Latitudinal index

REAL(KIND=real64), INTENT(IN)  :: lambda_targ(points)
                                ! Lambda coords of target grid in degrees
                                ! using same rotation as source grid
REAL(KIND=real64), INTENT(IN)  :: phi_targ(points)
                                ! Phi coords of target grid in degrees using
                                ! same rotation as source grid
REAL(KIND=real64), INTENT(IN)  :: lambda_srce(points_lambda_srce)
                                ! Lambda coords of source grid in degrees
REAL(KIND=real64), INTENT(IN)  :: phi_srce(points_phi_srce)
                                ! Phi coords of source grid in degrees
REAL(KIND=real64), INTENT(OUT) :: t_lambda(points)
                                ! Local value of target longitude

LOGICAL(KIND=int64),  INTENT(IN) :: cyclic ! =T, then source data is cyclic
                                           ! =F, then source data is non-cyclic

!--- Local variables:---------------------------------------------------
INTEGER(KIND=int64)  :: i      ! Loop index

! Variables for divide-and-conquer search of latitude/longitude arrays
INTEGER(KIND=int64) :: iupr ! Uppermost-point
INTEGER(KIND=int64) :: ilwr ! Lowermost-point
INTEGER(KIND=int64) :: imid ! Mid-point
! ----------------------------------------------------------------------

! 1. Initialise arrays

! 1.1 Scale target longitude so that it falls between
!     lambda_srce(1) and lambda_srce(1) + 360
DO i=1, points
  t_lambda(i) = MOD(((lambda_targ(i)-lambda_srce(1))+720.0_real64),  &
                      360.0_real64) + lambda_srce(1)
END DO

IF (cyclic) THEN
  DO i=1_int64, points
    ix(i) = 0_int64
    iy(i) = 1_int64
  END DO
ELSE
  DO i=1_int64, points
    ix(i) = 1_int64
    iy(i) = 1_int64
  END DO
END IF

!  2. Calculate lat and lon index of bottom left hand corner of
!     source grid box enclosing each target point.

! Longitude

!$OMP PARALLEL DO SCHEDULE(STATIC)                                             &
!$OMP& SHARED(points,points_lambda_srce,t_lambda,lambda_srce)                  &
!$OMP& SHARED(points_phi_srce,phi_srce,phi_targ,ix,iy)                         &
!$OMP& PRIVATE(i,iupr,ilwr,imid) DEFAULT(NONE)
DO i=1_int64, points

  ! Divide and conquer should more efficient than a brute-force loop over
  ! all i

  ilwr = 0_int64                      !first point(-1 in case we are cyclic)
  iupr = points_lambda_srce + 1_int64 !last point (+1 in case we are cyclic)
  DO WHILE ( (iupr-ilwr) > 1_int64 )
    imid = (ilwr+iupr)/2_int64
    IF ( lambda_srce(imid) > t_lambda(i) ) THEN
      iupr = imid
    END IF
    IF ( lambda_srce(imid) <= t_lambda(i) ) THEN
      ilwr = imid
    END IF
  END DO

  ix(i) = ilwr

  ilwr = 1_int64              !first point
  iupr = points_phi_srce      !last point
  DO WHILE ( (iupr-ilwr) > 1_int64 )
    imid = (ilwr+iupr)/2_int64
    IF ( phi_srce(imid) > phi_targ(i) ) THEN
      iupr = imid
    END IF
    IF ( phi_srce(imid) <= phi_targ(i) ) THEN
      ilwr = imid
    END IF
  END DO

  iy(i) = ilwr

END DO
!$OMP END PARALLEL DO

! 3. Correct 1-D indices for wrap around etc and then calculate
!    2-D indices of bottom left and bottom right hand corner
!    of each grid box.

IF (cyclic) THEN
  ! 3.1 Cyclic case

  DO i=1, points

    ! Set index for cyclic wrap around (not sure this is ever met since
    ! t_lambda always runs from lambda_srce(1) -> lambda_srce(points_lambda_srce)
    ! but we shall keep it here in case).
    IF (ix(i) <  1) THEN
      ix(i) = points_lambda_srce
      t_lambda(i) = t_lambda(i) + 360.0_real64
    END IF

    ! Set index for one sided difference if target point to north or
    ! south of source area.
    iy(i) = MAX(iy(i),1_int64)
    iy(i) = MIN(iy(i),points_phi_srce-1_int64)

    ! 2-D indices
    index_b_l(i) = ix(i)+(iy(i)-1_int64)*points_lambda_srce
    index_b_r(i) = index_b_l(i)+1_int64

    ! Correct for cyclic boundaries if target point outside source grid.
    ixp1(i) = ix(i) + 1_int64
    IF (ix(i) == points_lambda_srce) THEN
      index_b_r(i) = index_b_r(i) - points_lambda_srce
      ixp1(i)      = ixp1(i)      - points_lambda_srce
    END IF

  END DO

ELSE

  ! 3.2 Non cyclic case
  DO i=1, points

    ! Check that the nearest source grid point is really the nearest.  Earlier
    ! we made sure t_lambda is between lambda_srce(1) and lambda_srce(1)+360.
    ! If we are a LAM the first boundary might be nearer so make sure t_lambda
    ! reflects this.
    ! Update 2017-12 : Move to SHUMLib : It's not clear to me this check and
    ! silent fix is required. This side of the Else indicates that the source
    ! grid is non-cyclic. The target grid already resides from  lambda_srce(1)
    ! to lambda_srce(1)+360 so to subtract 360 degrees from it would make it
    ! less than lambda_srce(1). I can't think of a legitimate case where this
    ! would be the case and it needs further investigating.
    IF (ix(i) == points_lambda_srce) THEN
      IF (ABS(t_lambda(i)-lambda_srce(1)-360.0_real64) <                       &
          t_lambda(i)-lambda_srce(ix(i))) THEN
        t_lambda(i) = t_lambda(i) - 360.0_real64
        ix(i) = 1
      END IF
    END IF

    ! Set index for one sided difference if outside source area
    ix(i) = MAX(ix(i),1_int64)
    ix(i) = MIN(ix(i),points_lambda_srce-1_int64)
    IF (ix(i) <  1_int64) THEN ! IX(I) < 1 if POINTS_LAMBDA_SRCE = 1
      ix(i) = 1_int64
    END IF

    ixp1(i) = ix(i) + 1_int64
    ixp1(i) = MIN(ixp1(i),points_lambda_srce)

    ! Set index for one sided difference if outside source area
    iy(i) = MAX(iy(i),1_int64)
    iy(i) = MIN(iy(i),points_phi_srce-1_int64)
    IF (iy(i) <  1_int64) THEN ! IY(I) < 1 if POINTS_PHI_SRCE = 1
      iy(i) = 1_int64
    END IF

    ! 2-D indices
    index_b_l(i) = ix(i)   + (iy(i)-1_int64)*points_lambda_srce
    index_b_r(i) = ixp1(i) + (iy(i)-1_int64)*points_lambda_srce

  END DO

END IF

END SUBROUTINE f_shum_find_source_box_indices

!------------------------------------------------------------------------------!

SUBROUTINE f_shum_calc_weights                                                 &
                  ( weight_t_r, weight_b_r, weight_t_l, weight_b_l             &
                  , lambda_srce, phi_srce, phi_targ                            &
                  , points_lambda_srce, points_phi_srce, points                &
                  , t_lambda, ixp1, ix, iy )

IMPLICIT NONE

INTEGER(KIND=int64), INTENT(IN)  :: points_lambda_srce
                                  ! Number of lambda points on source grid
INTEGER(KIND=int64), INTENT(IN)  :: points_phi_srce
                                  ! Number of phi points on source grid
INTEGER(KIND=int64), INTENT(IN)  :: points
                                  ! Total number of points on target grid
INTEGER(KIND=int64), INTENT(IN)  :: ixp1(points)
                                  ! Longitudinal index plus 1
INTEGER(KIND=int64), INTENT(IN)  :: ix(points)
                                  ! Longitudinal index
INTEGER(KIND=int64), INTENT(IN)  :: iy(points)
                                  ! Latitudinal index

REAL(KIND=real64), INTENT(IN)  :: phi_targ(points)
                                ! Phi coords of target grid in degrees using
                                ! same rotation as source grid
REAL(KIND=real64), INTENT(IN)  :: lambda_srce(points_lambda_srce)
                                ! Lambda coords of source grid in degrees
REAL(KIND=real64), INTENT(IN)  :: phi_srce(points_phi_srce)
                                ! Phi coords of source grid in degrees
REAL(KIND=real64), INTENT(IN)  :: t_lambda(points)
                                ! Local value of target longitude
REAL(KIND=real64), INTENT(OUT) :: weight_t_r(points)
                                ! Weight applied to value at top
                                ! right corner of source gridbox
REAL(KIND=real64), INTENT(OUT) :: weight_b_l(points)
                                ! Weight applied to value at bot.
                                ! left corner of source gridbox
REAL(KIND=real64), INTENT(OUT) :: weight_b_r(points)
                                ! Weight applied to value at bot.
                                ! right corner of source gridbox
REAL(KIND=real64), INTENT(OUT) :: weight_t_l(points)
                                ! Weight applied to value at top
                                ! left corner of source gridbox

!--- Local variables:---------------------------------------------------
REAL(KIND=real64)    :: a      ! Longitudinal weight
REAL(KIND=real64)    :: b      ! Latitudinal weight
INTEGER(KIND=int64)  :: i      ! Loop index
! ----------------------------------------------------------------------

!  1. Compute interpolation weights
DO i=1, points

  ! Calculate basic weights
  a = (MOD(360.0_real64+lambda_srce(ixp1(i))-lambda_srce(ix(i)),360.0_real64))
  IF (a /= 0.0_real64) THEN
    ! If t_lambda - lambda_source is negative then just copy last value across.
    a = (MAX(t_lambda(i)-lambda_srce(ix(i)),0.0_real64))/a
  ELSE
    a = 0.0_real64
  END IF

  ! If we only have 1 row then we need to make sure we can cope.
  b = ABS(phi_srce(iy(i))-phi_srce(MIN(iy(i)+1,points_phi_srce)))
  IF (b /= 0.0_real64) THEN
    b = MAX(phi_targ(i)-phi_srce(iy(i)),0.0_real64)/b
  ELSE
    b = 0.0_real64
  END IF

  ! Calculate bi-linear interpolation weights
  weight_t_r(i) = a*b
  weight_b_l(i) = (1.0_real64-a)*(1.0_real64-b)
  weight_t_l(i) = (1.0_real64-a)*b
  weight_b_r(i) = a*(1.0_real64-b)

END DO

RETURN

END SUBROUTINE f_shum_calc_weights

END MODULE f_shum_horizontal_field_interp_mod
