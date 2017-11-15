!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------------
! NASA GSFC Land surface Verification Toolkit (LVT) V1.0
!-------------------------END NOTICE -- DO NOT EDIT-----------------------------
!BOP
! 
! !MODULE: JULES_obsMod
! \label(JULES_obsMod)
!
! !INTERFACE:
module JULES_obsMod
! 
! !USES: 
  use ESMF

  implicit none

  PRIVATE 
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
! 
! !FILES USED:
!
! !REVISION HISTORY: 
!  8 July 2015   Sujay Kumar  Initial Specification
! 
!EOP
!

!-----------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!-----------------------------------------------------------------------------
  PUBLIC :: JULES_obsinit !Initializes structures for reading JULES data
!-----------------------------------------------------------------------------
! !PUBLIC TYPES:
!-----------------------------------------------------------------------------
  PUBLIC :: JULESobs !Object to hold JULES observation attributes
!EOP

  type, public :: julesdec
     character*100               :: odir
     real,    allocatable        :: time_val(:)
     type(ESMF_Time)             :: refTime
     integer                     :: ntimes
  end type julesdec
     
  type(julesdec), save :: JULESObs(2)

contains
  
!BOP
! 
! !ROUTINE: JULES_obsInit
! \label{JULES_obsInit}
!
! !INTERFACE: 
  subroutine JULES_obsinit(i)
! 
! !USES: 
    use LVT_coreMod
    use LVT_timeMgrMod
    use LVT_logMod

    implicit none
!
! !INPUT PARAMETERS: 

! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!   This subroutine initializes and sets up the data structures required
!   for reading the JULES data, including the computation of spatial 
!   interpolation weights. 
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP

    integer               :: i 
    integer               :: status

    call ESMF_ConfigGetAttribute(LVT_Config, JULESobs(i)%odir, &
         label='JULES data file: ',rc=status)
    call LVT_verify(status, 'JULES data file: not defined')

! TBD - need to make the refTime configurable or to be read from 
! the netcdf file. 

    call ESMF_TimeSet(JULESobs(i)%refTime,  yy=2010, &
         mm = 1, &
         dd = 1,&
         h = 0,&
         m = 0,&
         s = 0,&
         calendar = LVT_calendar, &
         rc=status)
    call LVT_verify(status, 'ESMF_TimeSet error in JULES_obsInit')

! TBD - need to make the timestep configurable. 
    call LVT_update_timestep(LVT_rc, 3600)

  end subroutine JULES_obsinit

end module JULES_obsMod
