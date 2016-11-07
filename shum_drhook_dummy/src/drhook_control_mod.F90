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
!
!  Module: DRHOOK_CONTROL ---------------------------------------------
!
!  Purpose: Allows the value of lhook to be managed cleanly,
!           without a direct assignment in other code.  The dummy
!           DrHook code has lhook set as a parameter for performance
!           reasons, which prevents assignment.
!
!  Code Owner: Please refer to the UM file CodeOwners.txt
!  This file belongs in section: Dummy libraries
!
!  Code description:
!    Language: Fortran 90.
!    This code is written to UM programming standards version 9.0.

MODULE drhook_control_mod

USE yomhook, ONLY: lhook

IMPLICIT NONE
PRIVATE

PUBLIC :: drhook_control_enable
PUBLIC :: drhook_control_disable

!----------------------------------------------------------------------
! Contained functions/subroutines
!----------------------------------------------------------------------
CONTAINS

!----------------------------------------------------------------------
! 
!----------------------------------------------------------------------

SUBROUTINE drhook_control_enable()
IMPLICIT NONE

#if defined(DRHOOK)
lhook = .TRUE.
#endif

END SUBROUTINE drhook_control_enable

!----------------------------------------------------------------------
! 
!----------------------------------------------------------------------

SUBROUTINE drhook_control_disable()
IMPLICIT NONE

#if defined(DRHOOK)
lhook = .FALSE.
#endif

END SUBROUTINE drhook_control_disable

END MODULE drhook_control_mod


