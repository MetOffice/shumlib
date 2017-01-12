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

MODULE parkind1

!
! Description:
!   Dummy module to replace the DrHook library. Defines data types
!   which would otherwise be declared by the DrHook library.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Dummy libraries
!
! Code description:
!   Language: Fortran 90.
!   This code is written to UM programming standards version 8.1.

USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_INT64_T

IMPLICIT NONE

INTEGER, PARAMETER :: jpim = SELECTED_INT_KIND(9)
INTEGER, PARAMETER :: jprb = SELECTED_REAL_KIND(13,300)

END MODULE parkind1

