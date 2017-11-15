!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------------
! NASA GSFC Land surface Verification Toolkit (LVT) V1.0
!-------------------------END NOTICE -- DO NOT EDIT-----------------------------
module LVT_PSDMod
!
!BOP
! !MODULE: LVT_PSDMod
! 
!  !DESCRIPTION: 
!   This module handles the computation of the power spectral density
!   metric. 
!  
!  !REVISION HISTORY: 
!  30 Dec 2015    Sujay Kumar  Initial Specification
!
!EOP
  use ESMF
  use LVT_coreMod
  use LVT_histDataMod
  use LVT_statsDataMod
  use LVT_historyMod
  use LVT_TSMod
  use LVT_logMod
  use LVT_CIMod

  implicit none

  PRIVATE

!-----------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!-----------------------------------------------------------------------------
  public :: LVT_initPSD
  public :: LVT_diagnosePSD
  public :: LVT_computePSD 
  public :: LVT_writeMetric_PSD
  public :: LVT_resetMetric_PSD
  public :: LVT_writerestart_PSD
  public :: LVT_readrestart_PSD

  type, public :: psddec
     integer                 :: ngap_threshold
     integer                 :: nts_threshold
     integer, allocatable    :: nts(:,:)
     real, allocatable       :: value_model_ts(:)
  end type psddec

  type(psddec), save :: LVT_psd_struc

contains
  
!BOP
! !ROUTINE: LVT_initPSD 
! \label{LVT_initPSD}
!
! !INTERFACE: 
  subroutine LVT_initPSD(model,obs,stats,metric)
! !ARGUMENTS: 
    type(LVT_metaDataEntry) :: model
    type(LVT_metaDataEntry) :: obs
    type(LVT_statsEntry)    :: stats
    type(LVT_metricEntry)   :: metric
!
! !DESCRIPTION: 
!  This is the initialization routine for the metric entropy calculations. 
!  This routine allocates memory for the required data structures
!  and sets the number of required passes through the data (=1).
!  
!  The subroutine arguments are: 
!  \begin{description}
!    \item[model]
!      object to hold model variable information    
!    \item[obs]
!      object to hold observation variable information
!    \item[stats]
!     object to hold statistics computation related information
!    \item[metric]
!     object to hold metric-specific information
!   \end{description}
!EOP    
    if(metric%selectOpt.eq.1) then 
       allocate(stats%psd%value_model_ts(LVT_rc%ngrid,model%selectNlevs,&
            LVT_rc%nts))
       allocate(stats%psd%value_final(LVT_rc%ngrid,model%selectNlevs))
       allocate(stats%psd%value_ci(model%selectNlevs))

       stats%psd%value_model_ts = LVT_rc%udef
       stats%psd%value_final = LVT_rc%udef
       stats%psd%value_ci = LVT_rc%udef
    endif

!-------------------------------------------------------------------------
! Number of passes required to compute the metric
!-------------------------------------------------------------------------
    metric%npass = 1    
    metric%obsData = .true. 
    metric%stdevFlag = .false. 

!-------------------------------------------------------------------------
! These options are not supported
!-------------------------------------------------------------------------
    metric%timeOpt = 0 
    metric%extractTS = 0 
    metric%computeSC = 0 
    metric%computeADC = 0 

    LVT_psd_struc%ngap_threshold= 20 !5 days of gap
    LVT_psd_struc%nts_threshold= 10 !minimum number of values in the time series
    allocate(LVT_psd_struc%nts(LVT_rc%ngrid, model%selectNlevs))

  end subroutine LVT_initPSD

!BOP
! !ROUTINE: LVT_diagnosePSD
! \label{LVT_diagnosePSD}
! 
! !INTERFACE: 
  subroutine LVT_diagnosePSD(pass)

    implicit none
! !ARGUMENTS:
    integer                 :: pass
! !DESCRIPTION:
!   This routine invokes the call to update the metric entropy calculation
!   for each variable. 
! 
!   The arguments are: 
!   \begin{description}
!    \item[pass]
!     current pass index number over the data points
!   \end{description}
!EOP

    type(LVT_metadataEntry), pointer :: model
    type(LVT_metadataEntry), pointer :: obs
    type(LVT_statsEntry)   , pointer :: stats

    if(pass.eq.1) then 
       if(LVT_metrics%psd%selectOpt.eq.1) then 
          call LVT_getDataStream1Ptr(model)
          call LVT_getDataStream2Ptr(obs)
          call LVT_getstatsEntryPtr(stats)

          do while(associated(model))
             call diagnoseSinglePSD(&
                  obs,model,stats,&
                  LVT_metrics%psd)

             model => model%next
             obs => obs%next
             stats => stats%next

          enddo
       endif
    endif

  end subroutine LVT_diagnosePSD

!BOP
! !ROUTINE: diagnoseSinglePSD
! \label{diagnoseSinglePSD}
! 
! !INTERFACE: 
  subroutine diagnoseSinglePSD(obs,model,stats,metric)
! !USES: 
    use LVT_coreMod,    only : LVT_rc, LVT_domain
    use LVT_timeMgrMod, only : LVT_clock, LVT_calendar
    use LVT_histDataMod 
    use LVT_statsDataMod
    use LVT_logMod,     only : LVT_logunit, LVT_endrun

    implicit none
! !ARGUMENTS: 
    type(LVT_metaDataEntry) :: obs
    type(LVT_metaDataEntry) :: model
    type(LVT_statsEntry)    :: stats
    type(LVT_metricEntry)   :: metric
! !DESCRIPTION:  
!  This subroutine gathers the relevant information for the computation
!  of metric entropy during the pass through the data. The routine
!  simply stores the variable values into an array for computing
!  the metric at the end of the analysis time. 
!
!  The subroutine arguments are: 
!  \begin{description}
!    \item[model]
!      object to hold model variable information    
!    \item[obs]
!      object to hold observation variable information
!    \item[stats]
!     object to hold statistics computation related information
!    \item[metric]
!     object to hold metric-specific information
!   \end{description}
!EOP 
    type(ESMF_Time)         :: currTime,startTime
    type(ESMF_TimeInterval) :: timestep
    integer                 :: tindex
    integer                 :: status
    integer                 :: t,k,m_k,o_k
    
    if(stats%selectOpt.eq.1.and.&
         model%selectNlevs.ge.1) then
       do t=1,LVT_rc%ngrid
          do k=1,model%selectNlevs
             m_k = k+model%startNlevs -1
             o_k = k+obs%startNlevs -1
             if(model%count(t,m_k).gt.0) then
                if(metric%selectOpt.eq.1) then 
                   if(model%value(t,m_k).ne.LVT_rc%udef) then

                      call ESMF_TimeSet(currTime, yy = LVT_rc%yr, &
                           mm = LVT_rc%mo, &
                           dd = LVT_rc%da, &
                           h  = LVT_rc%hr, &
                           m  = LVT_rc%mn, & 
                           s  = LVT_rc%ss, &
                           calendar = LVT_calendar, & 
                           rc = status)
                      call LVT_verify(status,&
                           'Error in ESMF_TimeSet diagnosePSD')
                      call ESMF_ClockGet(LVT_clock,&
                           startTime = starttime,&
                           rc=status)
                      call LVT_verify(status,&
                           'Error in ESMF_TimeGet diagnosePSD')
                      call ESMF_TimeIntervalSet(timestep,&
                           s=LVT_rc%tavgInterval,rc=status)
                      call LVT_verify(status,&
                           'Error in ESMF_TimeIntervalSet diagnosePSD')
                      tindex = (nint((currTime - starttime)/timestep )+1)-1
                      
                      stats%psd%value_model_ts(t,k,tindex) = model%value(t,m_k)

                   endif
                endif
             endif
          enddo
       enddo
    endif

  end subroutine diagnoseSinglePSD

!BOP
! !ROUTINE: LVT_computePSD
! \label{LVT_computePSD}
! 
! !INTERFACE: 
  subroutine LVT_computePSD(pass,alarm)

    implicit none
! !ARGUMENTS:
    integer     :: pass
    logical     :: alarm
!
! !DESCRIPTION: 
! This subroutine invokes the call to compute the metric entropy 
!  values for each variable. 
!
!   The arguments are: 
!   \begin{description}
!    \item[pass]
!     current pass index number over the data points
!    \item[alarm]
!     flag indicating if the temporal output interval has been reached
!   \end{description}
! 
!EOP
    type(LVT_metadataEntry), pointer :: model
    type(LVT_metadataEntry), pointer :: obs
    type(LVT_statsEntry)   , pointer :: stats

    if(pass.eq.1) then 
       if(LVT_metrics%psd%selectOpt.eq.1.or.&
            LVT_metrics%psd%timeOpt.eq.1) then 
          call LVT_getDataStream1Ptr(model)
          call LVT_getDataStream2Ptr(obs)
          call LVT_getstatsEntryPtr(stats)

          do while(associated(model))
             call computeSinglePSD(&
                  alarm,model,obs,stats,&
                  LVT_metrics%psd)

             model => model%next
             obs => obs%next
             stats => stats%next             

          enddo
       endif
    endif
  end subroutine LVT_computePSD

!BOP
! !ROUTINE: computeSinglePSD
! \label{computeSinglePSD}
! 
! !INTERFACE: 
  subroutine computeSinglePSD(alarm,model,obs,stats,metric)
! !USES: 
    use LVT_informationContentMod

    implicit none
! !ARGUMENTS: 
    logical                 :: alarm
    type(LVT_metaDataEntry) :: model
    type(LVT_metaDataEntry) :: obs
    type(LVT_statsEntry)    :: stats
    type(LVT_metricEntry)   :: metric
!
! !DESCRIPTION: 
!  This subroutine performs the computation of the metric entropy values 
!  for each variable. The time series values are converted to a binary 
!  string based on the computed median (All values above the median are 
!  given a value of 1 and all values below the median are given a value
!  of 0). From this binary string, words are defined basec on a given 
!  word length. The metric entropy is calculcated based on 
!  Pachepsky et al. (2006)
! 
!  The subroutine arguments are: 
!  \begin{description}
!    \item[alarm]
!     flag indicating if the temporal output interval has been reached
!    \item[model]
!      object to hold model variable information    
!    \item[obs]
!      object to hold observation variable information
!    \item[stats]
!     object to hold statistics computation related information
!    \item[metric]
!     object to hold metric-specific information
!   \end{description}
!EOP
    integer :: t,k,kk,i,l,iprev
    integer :: binval
    integer     :: ts_class(LVT_rc%ngrid, model%selectNlevs)
    real        :: psd

    ts_class = 1

    if(LVT_rc%endtime.eq.1.and.metric%selectOpt.eq.1) then 
       if(stats%selectOpt.eq.1.and.&
            model%selectNlevs.ge.1) then
          
          do t=1,LVT_rc%ngrid
             do k=1,model%selectNlevs
                call trimTimeSeries (t,k,stats%psd%value_model_ts(t,k,:))
                if(LVT_psd_struc%nts(t,k).gt.LVT_psd_struc%nts_threshold) then 
                   call fillTimeSeries (t,k,LVT_psd_struc%value_model_ts)
                endif
                
                if(LVT_psd_struc%nts(t,k).le.LVT_psd_struc%nts_threshold) then 
                   ts_class(t,k) = 0
                else
                   do kk=1,LVT_psd_struc%nts(t,k)
                      if(LVT_psd_struc%value_model_ts(kk).eq.LVT_rc%udef) then 
                         ts_class(t,k) = 0
                      endif
                   enddo
                endif

                if(ts_class(t,k).eq.1) then 
                   call system("matlab -nosplash -nodisplay -nojvm < psd_calc.m");
!read the PSD values
                   stats%psd%value_final(t,k) =1.0
                else
                   stats%psd%value_final(t,k) = LVT_rc%udef
                endif
             enddo
          enddo
          
          do k=1,model%selectNlevs
             call LVT_computeCI(stats%psd%value_final(:,k),&
                  LVT_rc%ngrid, LVT_rc%pval_CI,&
                  stats%psd%value_ci(k))
          enddo
       endif
    endif
                   
  end subroutine computeSinglePSD

  subroutine trimTimeSeries( t, l,ts_value)

    integer         :: t,l
    real            :: ts_value(LVT_rc%nts)
    integer         :: st_index, en_index, k

    st_index = -1
    en_index = -1
    LVT_psd_struc%nts(t,l) = -1
    do k=1,LVT_rc%nts
       if(ts_value(k).ne.LVT_rc%udef) then 
          st_index = k
          exit
       endif
    enddo

    do k=LVT_rc%nts, 1, -1
       if(ts_value(k).ne.LVT_rc%udef) then 
          en_index = k
          exit
       endif
    enddo
    if(st_index.gt.0.and.en_index.gt.0) then 
       LVT_psd_struc%nts(t,l) = en_index - st_index + 1
    endif

    if(LVT_psd_struc%nts(t,l).gt.LVT_psd_struc%nts_threshold) then 
       if(allocated(LVT_psd_struc%value_model_ts)) then 
          deallocate(LVT_psd_struc%value_model_ts)
       endif
       allocate(LVT_psd_struc%value_model_ts(LVT_psd_struc%nts(t,l)))
       
       do k=st_index, en_index
          LVT_psd_struc%value_model_ts(k-st_index+1) =&
               ts_value(k)
       enddo
    endif
  end subroutine trimTimeSeries

  subroutine fillTimeSeries( t, l, ts_value)

    integer         :: t,l
    real            :: ts_value(LVT_psd_struc%nts(t,l))
    integer         :: k, k_new, k_new1, k_new2
    character*100   :: filloption
    integer         :: ftn
    logical         :: fillcheck
    real, allocatable :: temp_data(:)

    filloption = "DCT" !discrete cosine transform
    
    if(filloption.eq."Linear") then 
       do k=1,LVT_psd_struc%nts(t,l)
          if(ts_value(k).eq.LVT_rc%udef) then 
             call findNearestIndex2d(t, l, k,ts_value,k_new1, k_new2)
             if(k_new1.gt.0.and.k_new2.gt.0) then 
                if(((k-k_new1).le.LVT_psd_struc%ngap_threshold)&
                     .and.((k_new2-k).le.LVT_psd_struc%ngap_threshold)) then 
                   ts_value(k) = (ts_value(k_new1)*(k_new2-k) + &
                        ts_value(k_new2)*(k-k_new1))/&
                     (k_new2-k_new1)
                endif
             endif
          endif
       enddo
    elseif(filloption.eq."DCT") then
       fillcheck = .true. 
! first check to see if the gaps are within predefined thresholds. 
       do k=1,LVT_psd_struc%nts(t,l)
          if(ts_value(k).eq.LVT_rc%udef) then 
             call findNearestIndex2d(t, l, k,ts_value,k_new1, k_new2)
             if(((k-k_new1).gt.LVT_psd_struc%ngap_threshold)&
                  .or.((k_new2-k).gt.LVT_psd_struc%ngap_threshold)) then 
                fillcheck = .false.
                exit
             endif
          endif
       enddo
       if(fillcheck) then 
          ftn = LVT_getNextUnitNumber()
          open(ftn,file='matlab_input.txt',form='formatted')
          do k=1,LVT_psd_struc%nts(t,l)
             if(ts_value(k).eq.LVT_rc%udef) then 
                write(ftn,*) 'NaN'
             else
                write(ftn,fmt='(E14.6)') ts_value(k)
             endif
          enddo
          call LVT_releaseUnitNumber(ftn)

          call system("matlab -nosplash -nodisplay -nojvm < fill1ddata.m");
          ftn = LVT_getNextUnitNumber()
          open(ftn,file='matlab_output.txt',form='formatted')
          allocate(temp_data(LVT_psd_struc%nts(t,l)))

          do k=1,LVT_psd_struc%nts(t,l)
             read(ftn,*) temp_data(k)
             if(ts_value(k).eq.LVT_rc%udef) then 
                ts_value(k) = temp_data(k)
             endif
          enddo
          deallocate(temp_data)
          call LVT_releaseUnitNumber(ftn)
       endif
    endif
  end subroutine fillTimeSeries


  subroutine findNearestIndex2d(tile, level, k, ts_value, k_new1, k_new2)
    
    integer         :: tile, level
    real            :: ts_value(LVT_psd_struc%nts(tile, level))
    integer         :: k, k_new, k_new1, k_new2
    integer         :: t

    k_new1 = -1
    k_new2 = -1
    do t=k,1, -1
       if(ts_value(t).ne.LVT_rc%udef) then 
          k_new1 = t
          exit
       endif
    enddo

    do t=k,LVT_psd_struc%nts(tile,level)
       if(ts_value(t).ne.LVT_rc%udef) then 
          k_new2 = t
          exit
       endif
    enddo

  end subroutine findNearestIndex2d


  real function median(x,n)

    use LVT_SortingMod, only : LVT_sort

    IMPLICIT  NONE

    REAL,    DIMENSION(1:), INTENT(IN) :: X
    INTEGER, INTENT(IN)                :: N
    REAL,    DIMENSION(1:N)            :: Temp
    INTEGER                            :: i
    
    DO i = 1, N                       ! make a copy
       Temp(i) = X(i)
    END DO
    CALL  LVT_Sort(Temp, N)               ! sort the copy
    IF (MOD(N,2) == 0) THEN           ! compute the median
       Median = (Temp(N/2) + Temp(N/2+1)) / 2.0
    ELSE
       Median = Temp(N/2+1)
    END IF
  end function median

!BOP
! !ROUTINE: LVT_writeMetric_PSD
! \label{LVT_writeMetric_PSD}
!
! !INTERFACE: 
  subroutine LVT_writeMetric_PSD(pass,final,vlevels,stats,obs)
! !USES:
    use LVT_statsMod, only : LVT_writeSummaryStats2
    use LVT_pluginIndices
! !ARGUMENTS: 
    integer                 :: pass
    integer                 :: final
    integer                 :: vlevels
    type(LVT_statsEntry)    :: stats
    type(LVT_metaDataEntry) :: obs
!
! !DESCRIPTION:
!   This subroutine writes the computed metric entropy values to an 
!   external file
!
!  The subroutine arguments are: 
!  \begin{description}
!    \item[pass]
!     current pass index number over the data points
!    \item[final]
!     integer flag indicating if the end of the analysis period is reached
!    \item[vlevels]
!     number of vertical levels in the current variable
!    \item[obs]
!      object to hold observation variable information
!    \item[stats]
!     object to hold statistics computation related information
!   \end{description}
!EOP
    integer                 :: count_psd_final(LVT_rc%ngrid,1)
    integer                 :: k,dummy

    count_psd_final = 1
    if(pass.eq.LVT_metrics%psd%npass) then 
       if(stats%selectOpt.eq.1) then 
          if(LVT_metrics%psd%selectOpt.eq.1) then 
             do k=1,vlevels
                
                call LVT_writevar_gridded(LVT_metrics%psd%ftn_total, &
                     stats%psd%value_final(:,k),&
                     stats%vid_total(LVT_PSDid,1))
                call LVT_writevar_gridded(LVT_metrics%psd%ftn_total, &
                     real(count_psd_final(:,1)),&
                     stats%vid_count_total(LVT_PSDid,1))
                
                call LVT_writeSummaryStats2(&
                     LVT_metrics%psd%ftn_summ,&
                     LVT_metrics%psd%short_name,&
                     LVT_rc%ngrid,&
                     stats%psd%value_final(:,k), &
                     count_psd_final,&
                     stats%standard_name,&
                     stats%psd%value_ci(k))
                
             enddo
          endif
       endif
    endif

  end subroutine LVT_writeMetric_PSD

!BOP
! 
! !ROUTINE: LVT_resetMetric_PSD
! \label{LVT_resetMetric_PSD}
!
! !INTERFACE: 
  subroutine LVT_resetMetric_PSD
!
! !DESCRIPTION: 
!  This routine resets the relevant variables between each temporal averaging
!  interval. 
! 
!EOP
  end subroutine LVT_resetMetric_PSD

!BOP
! 
! !ROUTINE: LVT_writerestart_PSD
! 
! !INTERFACE:
  subroutine LVT_writerestart_PSD(ftn,pass)
! !USES: 

! 
! !ARGUMENTS: 
    integer                 :: ftn
    integer                 :: pass

! !DESCRIPTION: 
!  This routine writes the restart file for PSD metric computations
! 
!EOP
    
!
! !DESCRIPTION: 
! 
!EOP
    if(LVT_metrics%psd%selectOpt.eq.1) then 
       
       print*, 'The writerestart method is not implemented for PSD'
!       stop
    end if
    
  end subroutine LVT_writerestart_PSD

!BOP
! 
! !ROUTINE: LVT_readrestart_PSD
! 
! !INTERFACE:
  subroutine LVT_readrestart_PSD(ftn)
! !USES: 

! 
! !ARGUMENTS: 
    integer                 :: ftn

! !DESCRIPTION: 
!  This routine reads the restart file for PSD metric computations
! 
!EOP
    
!
! !DESCRIPTION: 
! 
!EOP
    if(LVT_metrics%psd%selectOpt.eq.1) then 
       
       print*, 'The readrestart method is not implemented for PSD'
       stop
    end if
    
  end subroutine LVT_readrestart_PSD

end module LVT_PSDMod
