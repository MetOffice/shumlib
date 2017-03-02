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
! Description: Pack a UM field using WGDOS packing
!
MODULE f_shum_wgdos_packing_mod

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim

USE, INTRINSIC :: ISO_C_BINDING, ONLY:                                         &
  C_INT64_T, C_INT32_T, C_INT16_T, C_FLOAT, C_DOUBLE

IMPLICIT NONE 

PRIVATE

PUBLIC :: f_shum_wgdos_pack, f_shum_wgdos_unpack

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
  INTEGER, PARAMETER :: int16  = C_INT16_T
  INTEGER, PARAMETER :: real64 = C_DOUBLE
  INTEGER, PARAMETER :: real32 = C_FLOAT                                       
!------------------------------------------------------------------------------!

! Drhook handles
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

! Define various masks used to manipulate values later
INTEGER(KIND=int64), PARAMETER ::                                              &
                     mask16  = INT(HUGE(0_int16), KIND=int64)*2 + 1
INTEGER(KIND=int64), PARAMETER ::                                              &
                     mask32  = INT(HUGE(0_int32), KIND=int64)*2 + 1
INTEGER(KIND=int64), PARAMETER ::                                              &
                     mask_mant_ibm = INT(z'00FFFFFF', KIND=int64)
INTEGER(KIND=int64), PARAMETER ::                                              &
                     mask_expt_ibm = INT(z'7F000000', KIND=int64)
INTEGER(KIND=int64), PARAMETER ::                                              &
                     mask_sign_ibm = INT(z'80000000', KIND=int64)

INTERFACE f_shum_wgdos_pack
  MODULE PROCEDURE                                                             &
      f_shum_wgdos_pack_expl_arg64,                                            &
      f_shum_wgdos_pack_expl_arg32,                                            &
      f_shum_wgdos_pack_2d_arg64,                                              &
      f_shum_wgdos_pack_2d_arg32,                                              &
      f_shum_wgdos_pack_1d_arg64,                                              &
      f_shum_wgdos_pack_1d_arg32
END INTERFACE

INTERFACE f_shum_wgdos_unpack
  MODULE PROCEDURE                                                             &
      f_shum_wgdos_unpack_expl_arg64,                                          &
      f_shum_wgdos_unpack_expl_arg32,                                          &
      f_shum_wgdos_unpack_2d_arg64,                                            &
      f_shum_wgdos_unpack_2d_arg32,                                            &
      f_shum_wgdos_unpack_1d_arg64,                                            &
      f_shum_wgdos_unpack_1d_arg32
END INTERFACE

CONTAINS

!------------------------------------------------------------------------------!

FUNCTION f_shum_wgdos_pack_2d_arg64(field, accuracy, rmdi, packed_field,       &
                                    n_packed_words, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64) :: status

REAL(KIND=real64),   INTENT(IN)  :: field(:, :)
INTEGER(KIND=int64), INTENT(IN)  :: accuracy
REAL(KIND=real64),   INTENT(IN)  :: rmdi
INTEGER(KIND=int64), INTENT(OUT) :: packed_field(:)
INTEGER(KIND=int64), INTENT(OUT) :: n_packed_words
CHARACTER(LEN=*),    INTENT(OUT) :: message    

INTEGER(KIND=int64) :: cols
INTEGER(KIND=int64) :: rows
INTEGER(KIND=int64) :: len_packed_field

cols = SIZE(field, 1, KIND=int64)
rows = SIZE(field, 2, KIND=int64)
len_packed_field = SIZE(packed_field, 1, KIND=int64)

status = f_shum_wgdos_pack_expl_arg64(                                         &
                      field, cols, rows, accuracy, rmdi, packed_field,         &
                      len_packed_field, n_packed_words, message)

END FUNCTION f_shum_wgdos_pack_2d_arg64

!------------------------------------------------------------------------------!

FUNCTION f_shum_wgdos_pack_2d_arg32(field, accuracy, rmdi, packed_field,       &
                                    n_packed_words, message) RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int32) :: status

REAL(KIND=real64),   INTENT(IN)  :: field(:, :)
INTEGER(KIND=int32), INTENT(IN)  :: accuracy
REAL(KIND=real32),   INTENT(IN)  :: rmdi
INTEGER(KIND=int64), INTENT(OUT) :: packed_field(:)
INTEGER(KIND=int32), INTENT(OUT) :: n_packed_words
CHARACTER(LEN=*),    INTENT(OUT) :: message    

INTEGER(KIND=int64) :: status64
INTEGER(KIND=int64) :: n_packed_words64
INTEGER(KIND=int64) :: accuracy64
REAL(KIND=real64)   :: rmdi64
INTEGER(KIND=int64) :: cols
INTEGER(KIND=int64) :: rows
INTEGER(KIND=int64) :: len_packed_field 

accuracy64 = INT(accuracy, KIND=int64)
rmdi64 = REAL(rmdi, KIND=real64)
cols = SIZE(field, 1, KIND=int64)
rows = SIZE(field, 2, KIND=int64)
len_packed_field = SIZE(packed_field, 1, KIND=int64)

status64 = f_shum_wgdos_pack_expl_arg64(                                       &
                        field, cols, rows, accuracy64, rmdi64, packed_field,   &
                        len_packed_field, n_packed_words64, message)

status         = INT(status64, KIND=int32)
n_packed_words = INT(n_packed_words64, KIND=int32)

END FUNCTION f_shum_wgdos_pack_2d_arg32

!------------------------------------------------------------------------------!

FUNCTION f_shum_wgdos_pack_1d_arg64(field, stride, accuracy, rmdi,             &
                                    packed_field, n_packed_words, message)     &
                                    RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int64) :: status

REAL(KIND=real64),   INTENT(IN), TARGET :: field(:)
INTEGER(KIND=int64), INTENT(IN)         :: stride
INTEGER(KIND=int64), INTENT(IN)         :: accuracy
REAL(KIND=real64),   INTENT(IN)         :: rmdi
INTEGER(KIND=int64), INTENT(OUT)        :: packed_field(:)
INTEGER(KIND=int64), INTENT(OUT)        :: n_packed_words
CHARACTER(LEN=*),    INTENT(OUT)        :: message    

INTEGER(KIND=int64)        :: cols
INTEGER(KIND=int64)        :: rows
REAL(KIND=real64), POINTER :: field2d(:, :)
INTEGER(KIND=int64)        :: len_packed_field


IF (MOD(SIZE(field, KIND=int64), stride) /= 0) THEN
  status = 1_int64
  message = "1d Field length not divisible by given stride"
  RETURN
END IF

cols = stride
rows = SIZE(field)/cols
field2d(1:cols, 1:rows) => field(:)
len_packed_field = SIZE(packed_field, 1, KIND=int64)

status = f_shum_wgdos_pack_expl_arg64(                                         &
                      field2d, cols, rows, accuracy, rmdi, packed_field,       &
                      len_packed_field, n_packed_words, message)

END FUNCTION f_shum_wgdos_pack_1d_arg64

!------------------------------------------------------------------------------!

FUNCTION f_shum_wgdos_pack_1d_arg32(field, stride, accuracy, rmdi,             &
                                    packed_field, n_packed_words, message)     &
                                    RESULT(status)
IMPLICIT NONE 

INTEGER(KIND=int32) :: status

REAL(KIND=real64),   INTENT(IN), TARGET  :: field(:)
INTEGER(KIND=int32), INTENT(IN)          :: stride
INTEGER(KIND=int32), INTENT(IN)          :: accuracy
REAL(KIND=real32),   INTENT(IN)          :: rmdi
INTEGER(KIND=int64), INTENT(OUT)         :: packed_field(:)
INTEGER(KIND=int32), INTENT(OUT)         :: n_packed_words
CHARACTER(LEN=*),    INTENT(OUT)         :: message    

INTEGER(KIND=int64)        :: status64
INTEGER(KIND=int64)        :: n_packed_words64
INTEGER(KIND=int64)        :: accuracy64
REAL(KIND=real64)          :: rmdi64
INTEGER(KIND=int64)        :: cols
INTEGER(KIND=int64)        :: rows
REAL(KIND=real64), POINTER :: field2d(:, :)
INTEGER(KIND=int64)        :: len_packed_field

IF (MOD(SIZE(field, KIND=int32), stride) /= 0) THEN
  status = 1_int32
  message = "1d Field length not divisible by given stride"
  RETURN
END IF

accuracy64 = INT(accuracy, KIND=int64)
rmdi64 = REAL(rmdi, KIND=real64)
cols = INT(stride, KIND=int64)
rows = INT(SIZE(field)/cols, KIND=int64)
field2d(1:cols, 1:rows) => field(:)
len_packed_field = SIZE(packed_field, 1, KIND=int64)

status64 = f_shum_wgdos_pack_expl_arg64(                                       &
                        field2d, cols, rows, accuracy64, rmdi64, packed_field, &
                        len_packed_field, n_packed_words64, message)

status         = INT(status64, KIND=int32)
n_packed_words = INT(n_packed_words64, KIND=int32)

END FUNCTION f_shum_wgdos_pack_1d_arg32

!------------------------------------------------------------------------------!

FUNCTION f_shum_wgdos_pack_expl_arg32(                                         &
    field, cols, rows, accuracy, rmdi, packed_field, len_packed_field,         &
    n_packed_words, message) RESULT(status)

IMPLICIT NONE

INTEGER(KIND=int32) :: status

INTEGER(KIND=int32), INTENT(IN)  :: cols
INTEGER(KIND=int32), INTENT(IN)  :: rows
REAL(KIND=real64),   INTENT(IN)  :: field(cols, rows)
INTEGER(KIND=int32), INTENT(IN)  :: accuracy
REAL(KIND=real32),   INTENT(IN)  :: rmdi
INTEGER(KIND=int32), INTENT(IN)  :: len_packed_field
INTEGER(KIND=int64), INTENT(OUT) :: packed_field(len_packed_field)
INTEGER(KIND=int32), INTENT(OUT) :: n_packed_words
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64) :: status64
INTEGER(KIND=int64) :: cols64
INTEGER(KIND=int64) :: rows64
INTEGER(KIND=int64) :: accuracy64
REAL(KIND=real64)   :: rmdi64
INTEGER(KIND=int64) :: len_packed_field64
INTEGER(KIND=int64) :: n_packed_words64

accuracy64 = INT(accuracy, KIND=int64)
rmdi64 = REAL(rmdi, KIND=real64)
cols64 = INT(cols, KIND=int64)
rows64 = INT(rows, KIND=int64)
len_packed_field64 = INT(len_packed_field, KIND=int64)

status64 = f_shum_wgdos_pack_expl_arg64(                                       &
                        field, cols64, rows64, accuracy64, rmdi64,             &
                        packed_field, len_packed_field64, n_packed_words64,    &
                        message)

status = INT(status64, KIND=int32)
n_packed_words = INT(n_packed_words64, KIND=int32)

END FUNCTION f_shum_wgdos_pack_expl_arg32

!------------------------------------------------------------------------------!

FUNCTION f_shum_wgdos_pack_expl_arg64(                                         &
    field, cols, rows, accuracy, rmdi, packed_field, len_packed_field,         &
    n_packed_words, message) RESULT(status)

IMPLICIT NONE

INTEGER(KIND=int64) :: status

INTEGER(KIND=int64), INTENT(IN)  :: cols
INTEGER(KIND=int64), INTENT(IN)  :: rows
REAL(KIND=real64),   INTENT(IN)  :: field(cols, rows)
INTEGER(KIND=int64), INTENT(IN)  :: accuracy
REAL(KIND=real64),   INTENT(IN)  :: rmdi
INTEGER(KIND=int64), INTENT(IN)  :: len_packed_field
INTEGER(KIND=int64), INTENT(OUT) :: packed_field(len_packed_field)
INTEGER(KIND=int64), INTENT(OUT) :: n_packed_words
CHARACTER(LEN=*),    INTENT(OUT) :: message    

INTEGER(KIND=int64) :: i, j, npoint, nshft, ival
INTEGER(KIND=int64) :: iexp, iman
INTEGER(KIND=int64) :: is1, is2, is3
INTEGER(KIND=int64) :: nbits_bmap, nwords_bmap, nwords_data
INTEGER(KIND=int64) :: nbits_pack, nvals_pack
INTEGER(KIND=int64) :: i1, i2, j1, j2

REAL(KIND=real64)       :: aprec, bprec

LOGICAL :: obtmis, obtzer
LOGICAL :: l_thread_error     ! Error flag for each OMP thread

INTEGER(KIND=int64) :: ibase       ! Base values
INTEGER(KIND=int64) :: ibit
INTEGER(KIND=int64) :: nmiss       ! missing-data bitmaps
INTEGER(KIND=int64) :: nzero       ! zero bitmaps
INTEGER(KIND=int64) :: ibm         ! IBM repr of base
INTEGER(KIND=int64) :: iword(rows) ! per row sizes

INTEGER(KIND=int64) :: itmp(2*cols+32)             ! temporary storage
! compression storage
INTEGER(KIND=int64) :: icomp(2*cols+8,MAX(rows, INT(1,KIND=int64))) 

! start position in a row for data
INTEGER(KIND=int64), PARAMETER :: istart = 1  

REAL(KIND=real64)    :: atmp(cols)
REAL(KIND=real64)    :: base
REAL(KIND=real64)    :: fmax

! Drhook variables
CHARACTER(LEN=*),   PARAMETER :: RoutineName='UM_WGDOS_PACK'
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

! GENERAL REMARK:
! All lengths and alignments in WGDOS packing are for 32-bit words,
! so life gets much easier when we treat the packed data as 32-bit
! words.
! So we gather all compressed data in the low 32 bits of icomp
! and compress this array at the end to 64 bits

! Scale factor
aprec = 2.0_real64**accuracy
bprec = 1.0_real64/aprec

l_thread_error = .FALSE.
! Parallelisation is over rows - these can be be compressed
! independently of each other and combined into a single buffer
! at the end
!$OMP  PARALLEL DO DEFAULT(NONE) SCHEDULE(STATIC)                              &
!$OMP& PRIVATE(j, i, i2, iexp, iman, itmp, obtmis, obtzer, nbits_bmap, npoint, &
!$OMP&         is1, is2, is3, nshft, nvals_pack, ibm, nbits_pack, nwords_data, &
!$OMP&         ival, atmp, nwords_bmap, nzero, status, message, fmax, base,    &
!$OMP&         ibase, ibit, nmiss)                                             &
!$OMP& SHARED(rows, field, cols, rmdi, aprec, bprec, iword, icomp)             &
!$OMP& REDUCTION(.OR.: l_thread_error) 
DO j=1,rows

  ! If this thread's error flag has been triggered we should not continue
  ! (it is set below if any overflowing values are encountered)
  IF (l_thread_error) CYCLE

  !     Find minimum and maximum value for every row,
  !     count number of missing and zero numbers
  !     + pack the non-MDI data.
  base = HUGE(0.0_real64)
  fmax = -HUGE(0.0_real64)
  nmiss = 0
  nzero = 0

  DO i=1,cols
    ! filter denormal numbers
    IF (IBITS(TRANSFER(field(i,j),INT(0,KIND=int64)), 52, 11)==0) THEN
      ! exponent bits = 0; => field(i,j) is denormal
      atmp(i) = 0.0
      base = MIN(base, REAL(0.0, KIND=real64))
      fmax = MAX(fmax, REAL(0.0, KIND=real64))
      nzero = nzero + 1

    ELSE
      ! exponent bits /= 0; => field(i,j) is normal
      ! (or technically may be NaN, inf as well)
      IF (field(i,j)/=rmdi) THEN

        base = MIN(REAL(base,KIND=real64),field(i,j))
        fmax = MAX(REAL(fmax,KIND=real64),field(i,j))

        atmp(i) = NINT(field(i,j)*bprec, KIND=int64)
        IF (atmp(i) == 0.0) nzero = nzero + 1

      ELSE
        nmiss = nmiss+1
        atmp(i) = rmdi
      END IF

    END IF
  END DO

  IF (nmiss == cols) THEN
  ! If we have a row of rmdi then lets set fmax and base to -1.0.  This is not
  ! really defined anywhere - we want something sensible though but not rmdi or
  ! 0.0 since they are special already.
    fmax = -1.0
    base = -1.0
    ibase = -1
    ! as a consequence of setting fmax to -1.0, ibit must be 0
    ibit = 0
  ELSE
    ! nmiss must be in the range 0 <= nmiss < cols

  !     ROUND BASE TO PRECISION REQUIRED
  ibase = NINT(base*bprec, KIND=int64)
  base  = ibase*aprec

 !     Find maximum scaled value
   i = MAX(NINT(fmax*bprec, KIND=int64)-ibase, INT(0, KIND=int64))

    IF (i > 2147483647) THEN
      ! If the scaled value cannot be stored in a 32-bit integer
      ! we cannot continue with the packing.  Set this thread's 
      ! error flag and skip the rest of this row (the flag will also
      ! cause this thread to skip any remaining rows - see above)
      l_thread_error = .TRUE.
      CYCLE
    END IF

  !     FIND NUMBER OF BITS REQUIRED TO STORE MAX DIFFERENCE
  ibit = 0
  i2 = 1
  DO WHILE (i >= i2)
    ibit = ibit + 1
    i2 = i2*2
  END DO

  END IF

  !     IBM floating point representation of base:
  IF (base==0.0) THEN
    ibm = 0
  ELSE
    base = ABS(base)
    iexp = EXPONENT(base) + 256
    iman = FRACTION(base) * 16777216.0
    ! IAND(iexp,3) is equivalent to MOD(iexp,4)
    SELECT CASE (IAND(iexp, INT(3, KIND=int64)))
      CASE (1_int64)
        iman = ISHFT(iman, INT(-3, KIND=int64))
      CASE (2_int64)
        iman = ISHFT(iman, INT(-2, KIND=int64))
      CASE (3_int64)
        iman = ISHFT(iman, INT(-1, KIND=int64))
    END SELECT
    iexp = (iexp+3)/4
    ibm = IOR(IAND(ISHFT(iexp,24),mask_expt_ibm),iman)
    IF (ibase<0) ibm=IOR(ibm,mask_sign_ibm)
  END IF

  !     Fill data into output array

  ! iword is the addressing (number of words) for this row
  iword(j) = istart+2 ! the two header words are inserted at end

  ! Check which bitmaps we need
  obtmis = (nmiss>0)

  ! Check if it is worthwile to use zero-bitmap:
  obtzer = (ibit*nzero > cols)

  ! Set itmp with the bitmap pattern
  IF (obtmis) THEN
    itmp(1:cols) = MERGE(1,0,atmp(:)==rmdi)
    nbits_bmap = cols
  ELSE
    nbits_bmap = 0
  END IF

  ! The bitmap is actually non-zero rather than zeroes.
  IF (obtzer) THEN
    itmp(nbits_bmap+1:nbits_bmap+cols) = MERGE(1,0,atmp(:)/=0.0)
    nbits_bmap = nbits_bmap+cols
  END IF

  ! Insert bitmap - this is done row-by-row into icomp.
  IF (nbits_bmap>0) THEN

    ! add 1's to the end since bitmaps should be padded with 1's
    itmp(nbits_bmap+1:nbits_bmap+31) = 1

    ! The number of words to be used for bitmap
    nwords_bmap = (nbits_bmap+31)/32

    ! Compress itmp

    ! Combine 4 contiguous 1-bit-items to one 4-bit-item
    DO i=1,nwords_bmap*8
      itmp(i) = IOR(ior(ISHFT(itmp(4*i-3),3),                     &
                        ISHFT(itmp(4*i-2),2)),                    &
                    IOR(ISHFT(itmp(4*i-1),1),itmp(4*i)))
    END DO

    ! Combine 4 contiguous 4-bit-items to one 16-bit-item
    DO i=1,nwords_bmap*2
      itmp(i) = IOR(ior(ISHFT(itmp(4*i-3),12),                    &
                        ISHFT(itmp(4*i-2), 8)),                   &
                    IOR(ISHFT(itmp(4*i-1), 4),itmp(4*i)))
    END DO

    ! Combine 2 contiguous 16-bit-items to the final destination
    DO i=1,nwords_bmap
      icomp(iword(j)+i-1,j) = IOR(ISHFT(itmp(2*i-1),16),itmp(2*i))
    END DO

    iword(j) = iword(j) + nwords_bmap

  END IF

  ! Insert data

  IF (ibit>0) THEN

    ! Get rid of missing values
    IF (obtmis) THEN
      npoint = 0
      DO i=1,cols
        IF (atmp(i)/=rmdi) THEN
          npoint = npoint+1
          atmp(npoint) = atmp(i)
        END IF
      END DO
    ELSE
      npoint = cols
    END IF

    ! Now get rid of zero values
    IF (obtzer) THEN
      ival = npoint
      npoint = 0
      !CDIR NODEP
      DO i=1,ival
        IF (atmp(i)/=0.0) THEN
          npoint = npoint+1
          atmp(npoint) = atmp(i)
        END IF
      END DO
    END IF

    ! Number of words used for the compressed data
    nwords_data = (npoint*ibit+31)/32

    ! Use scaled value and find difference from base
    DO i=1,npoint
      itmp(i) =                                                                &
               MAX(NINT(atmp(i), KIND=int64)-ibase, INT(0, KIND=int64))
    END DO

    ! As long as ibit is <=16 we can combine two contiguous
    ! items to one with the double number of bits, halving the
    ! number of words to be compressed
    nbits_pack = ibit
    nvals_pack = npoint

    DO WHILE (nbits_pack <= 16)
      itmp(nvals_pack+1) = 0 ! for odd numbers
      DO i=1,(nvals_pack+1)/2
        itmp(i) = IOR(ISHFT(itmp(2*i-1),nbits_pack),itmp(2*i))
      END DO
      nbits_pack = 2*nbits_pack
      nvals_pack = (nvals_pack+1)/2
    END DO

    IF (nbits_pack == 32) THEN
      ! This is the case if ibit is 1, 2, 4, 8 or 16
      ! We have not much to do, just copy itmp
      DO i=1,nwords_data
        icomp(iword(j)+i-1,j) = itmp(i)
      END DO

    ELSE

      ! Shift every value in itmp to the left and append
      ! the bits of the following 2 words
      is1 = 64-nbits_pack   ! amount to shift itmp(i)
      is2 = 64-2*nbits_pack ! amount to shift itmp(i+1)
      is3 = 64-3*nbits_pack ! amount to shift itmp(i+2)

      itmp(nvals_pack+1) = 0
      itmp(nvals_pack+2) = 0

      DO i=1,nvals_pack
        itmp(i) = IOR(ior(ISHFT(itmp(i  ),is1),                   &
                          ISHFT(itmp(i+1),is2)),                  &
                          ISHFT(itmp(i+2),is3))
      END DO

      ! Now itmp contains enough data so that we can cut out
      ! the compressed data words
      DO i=1,nwords_data

        ! Word which contains compressed data word:
        ival = itmp(((i-1)*32)/nbits_pack + 1)

        ! Number of bits we have to shift to the left
        ! so that we have the compressed data word left packed:
        nshft = MOD((i-1)*32,nbits_pack)

        ival = ISHFT(ival,nshft)

        ! Shift to the right half and mask out upper bits
        ! (for the case that ISHFT does an arithmetic shift)
        icomp(iword(j)+i-1,j) = IAND(ISHFT(ival,-32),mask32)

      END DO
    END IF

    iword(j) = iword(j) + nwords_data

  END IF

  ! Now insert the header for this row:
  ! First word of compressed data: IBM representation of base
  ! Second word of compressed data:
  ! 16 bits: ibit + flags
  ! 16 bits: number of words of data following
  icomp(istart,j) = ibm
  IF (obtzer) ibit = ibit + 128
  IF (obtmis) ibit = ibit + 32
  icomp(istart+1,j) = IOR(ISHFT(ibit,16),iword(j)-istart-2)

END DO ! j
!$OMP END PARALLEL DO
IF (l_thread_error) THEN
  status = 2
  message = "Unable to WGDOS pack to this accuracy"
  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)
  RETURN
END IF

! Each row of icomp now has compressed data, in low bits. Need
! to fill the return buffer, packed_field, with both high and low bits
! of the final data.

! Fill first 3 words of compressed data (overall header)
n_packed_words = SUM(iword(1:rows))+3-rows
itmp(1) = n_packed_words
itmp(2) = IAND(accuracy,mask32)
itmp(3) = IOR(ISHFT(cols,16),rows)
IF (rows>0) icomp(iword(rows),rows) = 0

!     Compress to 64 bit words
IF ((n_packed_words+1)/2 > len_packed_field) THEN
  status = 2
  message='Array for returning packed data is not large enough'
  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)
  RETURN
END IF


! First put the header information and the first word into packed_field
packed_field(1) = IOR(ISHFT(itmp(1),32),IAND(itmp(2),mask32))
packed_field(2) = IOR(ISHFT(itmp(3),32),IAND(icomp(1,1),mask32))

IF (rows>0) THEN

  ! Setup addressing
  i1 = 2           ! column for first word to pack
  j1 = 1           ! row for first word to pack
  IF (iword(1) > 3) THEN
    i2 = 3         ! column for second word if on same row
    j2 = 1
  ELSE
    i2 = 1         ! column/row for second word if on next row
    j2 = 2
  END IF

  ! Put the rest of the data into packed_field
  DO i = 3, (n_packed_words+1)/2
    packed_field(i) = IOR(ISHFT(icomp(i1,j1),32),IAND(icomp(i2,j2),mask32))

    ! Increment addressing by 2 as we've put 2 words of icomp into packed_field
    i1 = i1 + 2
    i2 = i2 + 2

    ! If we've gone past the end of a row for 1st word, adjust addressing
    IF (i1 >= iword(j1)) THEN
      i1 = i1 - iword(j1) + 1
      j1 = j1 + 1
    END IF

    ! If we've gone past the end of a row for 2nd word, adjust addressing
    IF (i2 >= iword(j2) .AND. j2 /= rows) THEN
      i2 = i2 - iword(j2) + 1
      j2 = j2 + 1
    END IF
  END DO

END IF

status = 0

IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END FUNCTION f_shum_wgdos_pack_expl_arg64

!------------------------------------------------------------------------------!

FUNCTION f_shum_wgdos_unpack_2d_arg64(                                         &
    packed_field, rmdi, field, message) RESULT(status)

IMPLICIT NONE

INTEGER(KIND=int64) :: status

INTEGER(KIND=int64), INTENT(IN)  :: packed_field(:)
REAL(KIND=real64),   INTENT(IN)  :: rmdi
REAL(KIND=real64),   INTENT(OUT) :: field(:, :)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64) :: cols
INTEGER(KIND=int64) :: rows
INTEGER(KIND=int64) :: len_packed_field

cols = SIZE(field, 1, KIND=int64)
rows = SIZE(field, 2, KIND=int64)
len_packed_field = SIZE(packed_field, 1, KIND=int64)

status = f_shum_wgdos_unpack_expl_arg64(                                       &
                        packed_field, len_packed_field, rmdi,                  &
                        field, cols, rows, message)

END FUNCTION f_shum_wgdos_unpack_2d_arg64

!------------------------------------------------------------------------------!

FUNCTION f_shum_wgdos_unpack_2d_arg32(                                         &
    packed_field, rmdi, field, message) RESULT(status)

IMPLICIT NONE

INTEGER(KIND=int32) :: status

INTEGER(KIND=int64), INTENT(IN)  :: packed_field(:)
REAL(KIND=real32),   INTENT(IN)  :: rmdi
REAL(KIND=real64),   INTENT(OUT) :: field(:, :)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64) :: status64
REAL(KIND=real64)   :: rmdi64
INTEGER(KIND=int64) :: cols
INTEGER(KIND=int64) :: rows
INTEGER(KIND=int64) :: len_packed_field

rmdi64 = REAL(rmdi, KIND=real64)
cols = SIZE(field, 1, KIND=int64)
rows = SIZE(field, 2, KIND=int64)
len_packed_field = SIZE(packed_field, 1, KIND=int64)

status64 = f_shum_wgdos_unpack_expl_arg64(                                     &
                        packed_field, len_packed_field, rmdi64,                &
                        field, cols, rows, message)

status = INT(status64, KIND=int32)
    
END FUNCTION f_shum_wgdos_unpack_2d_arg32

!------------------------------------------------------------------------------!

FUNCTION f_shum_wgdos_unpack_1d_arg64(                                         &
    packed_field, rmdi, field, stride, message) RESULT(status)

IMPLICIT NONE

INTEGER(KIND=int64) :: status

INTEGER(KIND=int64), INTENT(IN)          :: packed_field(:)
REAL(KIND=real64),   INTENT(IN)          :: rmdi
INTEGER(KIND=int64), INTENT(IN)          :: stride
REAL(KIND=real64),   INTENT(OUT), TARGET :: field(:)
CHARACTER(LEN=*),    INTENT(OUT)         :: message

INTEGER(KIND=int64)        :: cols
INTEGER(KIND=int64)        :: rows
REAL(KIND=real64), POINTER :: field2d(:, :)
INTEGER(KIND=int64)        :: len_packed_field

IF (MOD(SIZE(field, KIND=int64), stride) /= 0) THEN
  status = 1_int64
  message = "1d Field length not divisible by given stride"
  RETURN
END IF

cols = stride
rows = SIZE(field)/cols
field2d(1:cols, 1:rows) => field(:)
len_packed_field = SIZE(packed_field, 1, KIND=int64)

status = f_shum_wgdos_unpack_expl_arg64(                                       &
                        packed_field, len_packed_field, rmdi,                  &
                        field, cols, rows, message)

END FUNCTION f_shum_wgdos_unpack_1d_arg64

!------------------------------------------------------------------------------!

FUNCTION f_shum_wgdos_unpack_1d_arg32(                                         &
    packed_field, rmdi, field, stride, message) RESULT(status)

IMPLICIT NONE

INTEGER(KIND=int32) :: status

INTEGER(KIND=int64), INTENT(IN)          :: packed_field(:)
REAL(KIND=real32),   INTENT(IN)          :: rmdi
INTEGER(KIND=int32), INTENT(IN)          :: stride
REAL(KIND=real64),   INTENT(OUT), TARGET :: field(:)
CHARACTER(LEN=*),    INTENT(OUT)         :: message

INTEGER(KIND=int64)        :: status64
REAL(KIND=real64)          :: rmdi64
INTEGER(KIND=int64)        :: cols
INTEGER(KIND=int64)        :: rows
REAL(KIND=real64), POINTER :: field2d(:, :)
INTEGER(KIND=int64)        :: len_packed_field

IF (MOD(SIZE(field, KIND=int32), stride) /= 0) THEN
  status = 1_int32
  message = "1d Field length not divisible by given stride"
  RETURN
END IF

rmdi64 = REAL(rmdi, KIND=real64)
cols = INT(stride, KIND=int64)
rows = INT(SIZE(field)/cols, KIND=int64)
field2d(1:cols, 1:rows) => field(:)
len_packed_field = SIZE(packed_field, 1, KIND=int64)

status64 = f_shum_wgdos_unpack_expl_arg64(                                     &
                        packed_field, len_packed_field, rmdi64,                &
                        field, cols, rows, message)

status = INT(status64, KIND=int32)
    
END FUNCTION f_shum_wgdos_unpack_1d_arg32

!------------------------------------------------------------------------------!

FUNCTION f_shum_wgdos_unpack_expl_arg32(                                       &
    packed_field, len_packed_field, rmdi, field, cols, rows, message)          &
    RESULT(status)

IMPLICIT NONE

INTEGER(KIND=int32) :: status

INTEGER(KIND=int32), INTENT(IN)  :: len_packed_field
INTEGER(KIND=int64), INTENT(IN)  :: packed_field(len_packed_field)
REAL(KIND=real32),   INTENT(IN)  :: rmdi
INTEGER(KIND=int32), INTENT(IN)  :: cols
INTEGER(KIND=int32), INTENT(IN)  :: rows
REAL(KIND=real64),   INTENT(OUT) :: field(cols, rows)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64) :: status64
INTEGER(KIND=int64) :: cols64
INTEGER(KIND=int64) :: rows64
REAL(KIND=real64)   :: rmdi64
INTEGER(KIND=int64) :: len_packed_field64

rmdi64 = REAL(rmdi, KIND=real64)
cols64 = INT(cols, KIND=int64)
rows64 = INT(rows, KIND=int64)
len_packed_field64 = INT(len_packed_field, KIND=int64)

status64 = f_shum_wgdos_unpack_expl_arg64(                                     &
                        packed_field, len_packed_field64, rmdi64,              &
                        field, cols64, rows64, message)

status = INT(status64, KIND=int32)

END FUNCTION f_shum_wgdos_unpack_expl_arg32

!------------------------------------------------------------------------------!

FUNCTION f_shum_wgdos_unpack_expl_arg64(                                       &
    packed_field, len_packed_field, rmdi, field, cols, rows, message)          &
    RESULT(status)

IMPLICIT NONE

INTEGER(KIND=int64) :: status

INTEGER(KIND=int64), INTENT(IN)  :: len_packed_field
INTEGER(KIND=int64), INTENT(IN)  :: packed_field(len_packed_field)
REAL(KIND=real64),   INTENT(IN)  :: rmdi
INTEGER(KIND=int64), INTENT(IN)  :: cols
INTEGER(KIND=int64), INTENT(IN)  :: rows
REAL(KIND=real64),   INTENT(OUT) :: field(cols, rows)
CHARACTER(LEN=*),    INTENT(OUT) :: message

INTEGER(KIND=int64) :: i, j, nshft, num, iword, ioff, mant, iexp

INTEGER(KIND=int64) :: ival

INTEGER(KIND=int64) :: i1, i2, nbits_bmap
INTEGER(KIND=int64) :: itmp(3*cols)
INTEGER(KIND=int64) :: idx(cols), imap(cols)
INTEGER(KIND=int64) :: istart(rows), nop(rows), nbits(rows)

INTEGER(KIND=int64) :: ibase(rows)
INTEGER(KIND=int64) :: icomp(rows*(2*cols+2)+4)

INTEGER(KIND=int64) :: accuracy
REAL(KIND=real64)   :: aprec
REAL(KIND=real64)   :: base(rows)

LOGICAL ::                                                                     &
  obtzer(rows), obtmin(rows), obtmis(rows), obtmap(rows)

INTEGER(KIND=int64), PARAMETER :: One64 = 1

INTEGER(KIND=int64), SAVE :: mask_bits(0:63)
LOGICAL, SAVE :: first = .TRUE.

! Drhook variables
CHARACTER(LEN=*), PARAMETER :: RoutineName = "UM_WGDOS_UNPACK"
REAL(KIND=jprb)             :: zhook_handle

IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

IF (first) THEN
  DO i=0,63
    mask_bits(i) = ISHFT(One64,63-i)
  END DO
  first = .FALSE.
END IF

! All lengths and alignments in WGDOS packing are for 32-bit words,
! so life gets much easier when we treat the packed data as 32-bit
! words.
! We split therefore the 64-bit compressed data into two 32 bit words

num = ISHFT(packed_field(1),-32) ! Number of 32 bit words

IF (num > SIZE(icomp, KIND=int64)-2) THEN
  status = 2
  message='Compressed data has too many elements'
  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)
  RETURN
END IF

DO i=1,(num+1)/2
  icomp(2*i-1) = IAND(ISHFT(packed_field(i),-32),mask32)
  icomp(2*i)   = IAND(packed_field(i),mask32)
END DO
! The following word MUST be 0, it is used during decomposition!
icomp(num+1) = 0
icomp(num+2) = 0

! Get the precision from the header
accuracy = icomp(2)
aprec = 2.0**accuracy

! Get start word and length of every row

istart(1) = 6
nop(1) = IAND(icomp(5),mask16)

DO j=2,rows
  istart(j) = istart(j-1) + nop(j-1) + 2
  nop(j) = IAND(icomp(istart(j)-1),mask16)
  IF (istart(j)+nop(j)-1>num) THEN
    status = 2
    message='Compressed data inconsistent'
    IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)
    RETURN
  END IF
END DO

! Get base (as a 32-bit IBM floating point number) and number of bits
! for every row and convert IBM floats to native floats
! The routine IBM2IEEE does a rather bad job, so we code it explicitly

!$OMP PARALLEL DO SCHEDULE(STATIC) DEFAULT(NONE)                        &
!$OMP&         SHARED(rows, obtmis, obtmin, obtzer, obtmap, nbits, ibase, &
!$OMP&                base, cols, mask_bits, field, icomp, rmdi, istart,  &
!$OMP&                aprec)                                            &
!$OMP&         PRIVATE(j, nbits_bmap, mant, iexp, ival, iword, itmp,    &
!$OMP&                 nshft, i1, i2, i, num, imap, idx, ioff)
DO j=1,rows
  ibase(j) = icomp(istart(j)-2)
  nbits(j) = IAND(ISHFT(icomp(istart(j)-1),-16),mask16)

  mant = IAND(ibase(j),mask_mant_ibm)
  iexp = ISHFT(IAND(ibase(j),mask_expt_ibm),-24)-64-6
  base(j) = 16.0**iexp*mant
  IF (IAND(ibase(j),mask_sign_ibm) /= 0) base(j) = -base(j)
  ! Check if bitmaps are used

  obtzer(j) = IAND(nbits(j), INT(128, KIND=int64)) /= 0
  obtmin(j) = IAND(nbits(j), INT(64, KIND=int64))  /= 0
  obtmis(j) = IAND(nbits(j), INT(32, KIND=int64))  /= 0
  obtmap(j) = obtzer(j) .OR. obtmin(j) .OR. obtmis(j)
  nbits(j)  = IAND(nbits(j), INT(31, KIND=int64))


  ! Decode data row by row


          ! Care about bitmaps

  imap(:) = 1 ! Data present indicator

  nbits_bmap = 0
  IF (obtmis(j)) nbits_bmap = nbits_bmap + cols
  IF (obtmin(j)) nbits_bmap = nbits_bmap + cols
  IF (obtzer(j)) nbits_bmap = nbits_bmap + cols

  IF (nbits_bmap > 0) THEN
    iword = istart(j)
    DO i1=1,nbits_bmap,64
      ival  = IOR(ISHFT(icomp(iword),32),icomp(iword+1))
      iword = iword+2
      DO i2=0,MIN(nbits_bmap-i1, INT(63, KIND=int64))
        itmp(i1+i2) = MERGE(1,0,IAND(ival,mask_bits(i2))/=0)
      END DO
    END DO
    istart(j) = istart(j) + (nbits_bmap+31)/32
  END IF

  nbits_bmap = 0

  ! Extract missing data bitmap

  IF (obtmis(j)) THEN
    WHERE (itmp(nbits_bmap+1:nbits_bmap+cols)/=0)
      field(:,j) = rmdi
      imap (:) = 0
    END WHERE
    nbits_bmap = nbits_bmap + cols
  END IF

  ! Extract minimum value bitmap

  IF (obtmin(j)) THEN
    WHERE (itmp(nbits_bmap+1:nbits_bmap+cols)/=0)
      field(:,j) = base(j)
      imap (:) = 0
    END WHERE
    nbits_bmap = nbits_bmap + cols
  END IF

  ! Extract zero value bitmap

  IF (obtzer(j)) THEN
    WHERE (itmp(nbits_bmap+1:nbits_bmap+cols)==0)
      field(:,j) = 0.0
      imap (:) = 0
    END WHERE
    nbits_bmap = nbits_bmap + cols
  END IF

  IF (nbits(j)==0) THEN

    ! All points in row have same value

    IF (obtmap(j)) THEN
      WHERE (imap(:)/=0) field(:,j) = base(j)
    ELSE
      field(:,j) = base(j)
    END IF

  ELSE

    ! Get number [and index] of values to decode

    IF (obtmap(j)) THEN
      num = 0
      DO i=1,cols
        IF (imap(i) /= 0) THEN
          num = num+1
          idx(num) = i
        END IF
      END DO
    ELSE
      num = cols
    END IF

    ! Decode data
    IF (obtmap(j)) THEN
      DO i=1,num

        ! Bit offset to value:
        ioff  = (i-1)*nbits(j)

        ! Number of word in icomp which contains first bit:
        iword = ISHFT(ioff,-5)+istart(j)

        ! We load this word and the following into ival,
        ! this way we don't have to care if a word boundary
        ! is crossed. This requires that ival is a 64 bit word!
        ival  = IOR(ISHFT(icomp(iword),32),icomp(iword+1))

        ! Number of bits we have to shift to the right:
        nshft = 64 - IAND(ioff, INT(31, KIND=int64)) - nbits(j)

        ! Mask ival and calculate decoded value:
        ival = IBITS(ival,nshft,nbits(j))
        field(idx(i),j) = ival*aprec + base(j)
      END DO
    ELSE
      DO i=1,num

        ! Bit offset to value:
        ioff  = (i-1)*nbits(j)

        ! Number of word in icomp which contains first bit:
        iword = ISHFT(ioff,-5)+istart(j)

        ! We load this word and the following into ival,
        ! this way we don't have to care if a word boundary
        ! is crossed. This requires that ival is a 64 bit word!
        ival  = IOR(ISHFT(icomp(iword),32),icomp(iword+1))

        ! Number of bits we have to shift to the right:
        nshft = 64 - IAND(ioff, INT(31, KIND=int64)) - nbits(j)

        ! Mask ival and calculate decoded value:
        ival = IBITS(ival,nshft,nbits(j))
        field(i,j) = ival*aprec + base(j)
      END DO
    END IF

  END IF

END DO
!$OMP END PARALLEL DO

status = 0

IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)
RETURN

END FUNCTION f_shum_wgdos_unpack_expl_arg64

!------------------------------------------------------------------------------!

END MODULE f_shum_wgdos_packing_mod
