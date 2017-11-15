!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------------
! NASA GSFC Land surface Verification Toolkit (LVT) V1.0
!-------------------------END NOTICE -- DO NOT EDIT-----------------------------
#include "LVT_misc.h"
!BOP
! 
! !ROUTINE: readJULESObs
! \label{readJULESObs}
!
! !INTERFACE: 
subroutine readJULESObs(source)
! 
! !USES:   
  use ESMF
  use LVT_coreMod
  use LVT_timeMgrMod
  use LVT_logMod
  use LVT_histDataMod
  use JULES_obsMod
  use map_utils

#if(defined USE_NETCDF3 || defined USE_NETCDF4)
  use netcdf
#endif

  implicit none
!
! !INPUT PARAMETERS: 
  integer, intent(in) :: source
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
! 
!  NOTES: 
!   This is a test implementation using the JULES model output
!   
! 
! !FILES USED:
!
! !REVISION HISTORY: 
!  8 July 2015: Sujay Kumar, Initial Specification
! 
!EOP

  character*100           :: filename
  logical                 :: file_exists
  integer                 :: nid, ios
  integer                 :: smcid, timeid, tId, xId, yId,soilId
  integer                 :: latid, lonid, stcId
  integer                 :: nx, ny
  real,  allocatable      :: smc_jules(:,:,:), lat(:,:), lon(:,:)
  real,  allocatable      :: stc_jules(:,:,:)
  real,  allocatable      :: smc(:,:,:), stc(:,:,:), time_val(:)
  integer                 :: c,r,t,kk, tindex
  integer                 :: num_secs
  integer                 :: yr, mo, da, hr, mn, ss
  type(ESMF_Time)         :: currTime
  type(ESMF_TimeInterval) :: ts
  integer                 :: nsoil
  integer                 :: status
  integer                 :: stn_row, stn_col
  real                    :: col,row
  real                    :: smc_ip(LVT_rc%lnc,LVT_rc%lnr)
  real                    :: stc_ip(LVT_rc%lnc,LVT_rc%lnr)


#if (defined USE_NETCDF3 || defined USE_NETCDF4)  
  filename = JULESobs(source)%odir
  inquire(file=trim(filename),exist=file_exists) 
  
  if(file_exists) then 
     write(LVT_logunit,*) '[INFO] Reading JULES file ',trim(filename)
     
     ios = nf90_open(path=trim(filename),mode=NF90_NOWRITE,ncid=nid)
     call LVT_verify(ios, 'Error opening file '//trim(filename))
     
     ! dimensions
     ios = nf90_inq_dimid(nid,'x',xId)
     call LVT_verify(ios, 'Error nf90_inq_dimid: x')
     
     ios = nf90_inquire_dimension(nid,xId, len=nx)
     call LVT_verify(ios, 'Error nf90_inquire_dimension: x')
     
     ios = nf90_inq_dimid(nid,'y',yId)
     call LVT_verify(ios, 'Error nf90_inq_dimid: y')
     
     ios = nf90_inquire_dimension(nid,yId, len=ny)
     call LVT_verify(ios, 'Error nf90_inquire_dimension: y')

     ios = nf90_inq_dimid(nid,'soil', soilId)
     call LVT_verify(ios, 'Error nf90_inq_dimid: soil')

     nsoil = 1
     ios = nf90_inquire_dimension(nid,soilId, len=nsoil)
     !        call LVT_verify(ios, 'Error nf90_inquire_dimension: soil')

     ios = nf90_inq_dimid(nid,'time',tId)
     call LVT_verify(ios, 'Error nf90_inq_dimid: t')

     ios = nf90_inquire_dimension(nid,tId, len=JULESobs(source)%ntimes)
     call LVT_verify(ios, 'Error nf90_inquire_dimension: time')

     allocate(smc_jules(nx,ny,nsoil))
     allocate(lat(nx,ny))
     allocate(lon(nx,ny))
     allocate(time_val(JULESobs(source)%ntimes))
     allocate(smc(LVT_rc%lnc, LVT_rc%lnr,nsoil))

     allocate(stc_jules(nx,ny,nsoil))
     allocate(stc(LVT_rc%lnc, LVT_rc%lnr,nsoil))
     !values
     ios = nf90_inq_varid(nid,'latitude',latid)
     call LVT_verify(ios, 'Error nf90_inq_varid: latitude')

     ios = nf90_get_var(nid,latid, lat)
     call LVT_verify(ios, 'Error nf90_get_var: latitude')

     ios = nf90_inq_varid(nid,'longitude',lonid)
     call LVT_verify(ios, 'Error nf90_inq_varid: longitude')

     ios = nf90_get_var(nid,lonid, lon)
     call LVT_verify(ios, 'Error nf90_get_var: longitude')

     ios = nf90_inq_varid(nid,'time',timeid)
     call LVT_verify(ios, 'Error nf90_inq_varid: time')

     ios = nf90_get_var(nid,timeid, time_val)
     call LVT_verify(ios, 'Error nf90_get_var: time')

     call ESMF_TimeSet(currTime, yy=LVT_rc%dyr(source), &
          mm=LVT_rc%dmo(source), dd=LVT_rc%dda(source), &
          h = LVT_rc%dhr(source), m = LVT_rc%dmn(source), &
          s = LVT_rc%dss(source), calendar = LVT_calendar, &
          rc=status)
     call LVT_verify(status, 'error in ESMF_TimeSet in readJULESobs')

     ts = currTime - JULESobs(source)%refTime

     call ESMF_TimeIntervalGet(ts, s=num_secs,rc=status)

     tindex = -1
     do t=1, JULESobs(source)%ntimes
        if(time_val(t).eq.num_secs) then 
           tindex = t
           exit
        endif
     enddo
!soil moisture
     ios = nf90_inq_varid(nid,'smcl',smcid)
     call LVT_verify(ios, 'Error nf90_inq_varid: smcl')

     ios = nf90_get_var(nid,smcid, smc_jules, &
          start=(/1,1,1,tindex/), &
          count=(/nx,ny,nsoil,1/))
     call LVT_verify(ios, 'Error nf90_get_var: smcl')
!soil temperature
     ios = nf90_inq_varid(nid,'t_soil',stcid)
     call LVT_verify(ios, 'Error nf90_inq_varid: stcl')

     ios = nf90_get_var(nid,stcid, stc_jules, &
          start=(/1,1,1,tindex/), &
          count=(/nx,ny,nsoil,1/))
     call LVT_verify(ios, 'Error nf90_get_var: stcl')

     ios = nf90_close(nid)
     call LVT_verify(ios, 'Error in nf90_close')

     !map to the LVT grid           
     do c=1,nx
        do r=1,ny
           call latlon_to_ij(LVT_domain%lvtproj, lat(c,r), lon(c,r),&
                col,row)
           stn_col = nint(col)
           stn_row = nint(row)
!soil moisture
           if(stn_col.ge.1.and.stn_col.le.LVT_rc%lnc.and.&
                stn_row.ge.1.and.stn_row.le.LVT_rc%lnr) then 
              smc(stn_col,stn_row,:) = &
                   smc_jules(c,r,:)
           endif
!soil temperature
           if(stn_col.ge.1.and.stn_col.le.LVT_rc%lnc.and.&
                stn_row.ge.1.and.stn_row.le.LVT_rc%lnr) then 
              stc(stn_col,stn_row,:) = &
                   stc_jules(c,r,:)
           endif
        enddo
     enddo

     write(LVT_logunit,*) '[INFO] Finished reading JULES data '
     do r=1, LVT_rc%lnr
        do c=1, LVT_rc%lnc
           smc_ip(c,r) = smc(c,r,1)
        enddo
     enddo

     do r=1, LVT_rc%lnr
        do c=1, LVT_rc%lnc
           stc_ip(c,r) = stc(c,r,1)
        enddo
     enddo

     deallocate(smc_jules)
     deallocate(lat)
     deallocate(lon)
     deallocate(time_val)
     deallocate(smc)

     deallocate(stc_jules)
     deallocate(stc)
  else
     smc_ip = LVT_rc%udef
     stc_ip = LVT_rc%udef
  endif
#endif

  call LVT_logSingleDataStreamVar(LVT_MOC_SoilMoist,source,smc_ip,vlevel=1,units="kg/m2")

!TBD - make this configurable
  do r=1, LVT_rc%lnr
     do c=1, LVT_rc%lnc
        if(smc_ip(c,r).ne.LVT_rc%udef) then 
           smc_ip(c,r) = smc_ip(c,r)/(1000.0*0.1)  ! density of water*layer depth
        else
           smc_ip(c,r) = LVT_rc%udef
        endif
     enddo
  enddo

  call LVT_logSingleDataStreamVar(LVT_MOC_SoilMoist,source,smc_ip,vlevel=1,units="m3/m3")

  call LVT_logSingleDataStreamVar(LVT_MOC_SoilTemp,source,stc_ip,vlevel=1,units="K")

end subroutine readJULESObs

