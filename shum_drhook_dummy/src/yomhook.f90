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

MODULE yomhook

!
! Description:
!   Dummy module to replace the DrHook library.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Dummy libraries
!
! Code description:
!   Language: Fortran 90.
!   This code is written to UM programming standards version 8.1.

USE parkind1, ONLY: jpim, jprb
IMPLICIT NONE

LOGICAL, PARAMETER :: lhook = .FALSE.

INTERFACE dr_hook
MODULE PROCEDURE dr_hook
MODULE PROCEDURE dr_hook_comm
END INTERFACE dr_hook

CONTAINS

SUBROUTINE dr_hook(NAME,code,handle)
IMPLICIT NONE

!Arguments
CHARACTER(LEN=*),   INTENT(IN)    :: NAME
INTEGER(KIND=jpim), INTENT(IN)    :: code
REAL(KIND=jprb),    INTENT(INOUT) :: handle

!Nothing to do

RETURN
END SUBROUTINE dr_hook

SUBROUTINE dr_hook_comm(NAME,code,handle,lcomm,comm)
IMPLICIT NONE

!Arguments
CHARACTER(LEN=*),   INTENT(IN)    :: NAME
INTEGER(KIND=jpim), INTENT(IN)    :: code
LOGICAL(KIND=jpim), INTENT(IN)    :: lcomm
INTEGER(KIND=jpim), INTENT(IN)    :: comm
REAL(KIND=jprb),    INTENT(INOUT) :: handle

!Nothing to do

RETURN
END SUBROUTINE dr_hook_comm

END MODULE yomhook

