!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------------
! NASA GSFC Land surface Verification Toolkit (LVT) V1.0
!-------------------------END NOTICE -- DO NOT EDIT-----------------------------
!
!BOP
! 
! !MODULE: LVT_FBIASMod
! \label(LVT_FBIASMod)
!
! !INTERFACE:
module LVT_FBIASMod
! 
! !USES:   
  use LVT_coreMod
  use LVT_histDataMod
  use LVT_statsDataMod
  use LVT_historyMod
  use LVT_TSMod
  use LVT_logMod
  use LVT_CIMod

  implicit none

  private
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
!  !DESCRIPTION: 
!   This module handles the Frequency bias computations by comparing 
!   the LIS output to the specified observations. Frequency bias 
!   indicates how frequently the model predicts (a hit) vs how 
!   frequently the event was observed. 
!
!    FBIAS = (hits + missed negatives)/(hits + misses)
! 
! !FILES USED:
!
!  !REVISION HISTORY: 
!  2 Oct 2008    Sujay Kumar  Initial Specification
! 
!EOP

!-----------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!-----------------------------------------------------------------------------
  public :: LVT_initFBIAS
  public :: LVT_diagnoseFBIAS
  public :: LVT_computeFBIAS 
  public :: LVT_writeMetric_FBIAS
  public :: LVT_resetMetric_FBIAS
  public :: LVT_writerestart_FBIAS
  public :: LVT_readrestart_FBIAS
contains
  
!BOP
! 
! !ROUTINE: LVT_initFBIAS
! \label{LVT_initFBIAS}
!
! !INTERFACE: 
  subroutine LVT_initFBIAS(model,obs,stats,metric)
! 
! !USES:   
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
! 
!EOP
!BOP
! !ARGUMENTS: 
    type(LVT_metaDataEntry) :: model
    type(LVT_metaDataEntry) :: obs
    type(LVT_statsEntry)    :: stats
    type(LVT_metricEntry)   :: metric
!EOP
    if(metric%selectOpt.eq.1.or.metric%timeopt.eq.1) then 
       allocate(stats%fbias%value_total(LVT_rc%ngrid, model%selectNlevs,&
            LVT_rc%strat_nlevels))
       allocate(stats%fbias%count_value_total(LVT_rc%ngrid, model%selectNlevs,&
            LVT_rc%strat_nlevels))   
       stats%fbias%value_total = 0.0
       stats%fbias%count_value_total = 0

       allocate(stats%fbias%value_ci(model%selectNlevs,LVT_rc%strat_nlevels))
       stats%fbias%value_ci = LVT_rc%udef    

       allocate(stats%fbias%value_ts(LVT_rc%ngrid, model%selectNlevs, &
            LVT_rc%strat_nlevels))
       allocate(stats%fbias%count_value_ts(LVT_rc%ngrid, model%selectNlevs,&
            LVT_rc%strat_nlevels))       

       stats%fbias%value_ts = 0.0
       stats%fbias%count_value_ts = 0 

       allocate(stats%fbias%tavg_value_ts(LVT_rc%ngrid, model%selectNlevs, &
            LVT_rc%strat_nlevels))
       allocate(stats%fbias%tavg_count_value_ts(LVT_rc%ngrid, model%selectNlevs,&
            LVT_rc%strat_nlevels))       

       stats%fbias%tavg_value_ts = 0.0
       stats%fbias%tavg_count_value_ts = 0 

       if(metric%computeSC.eq.1) then 
          allocate(stats%fbias%value_asc(LVT_rc%ngrid, model%selectNlevs,LVT_rc%nasc))
          allocate(stats%fbias%count_value_asc(LVT_rc%ngrid, model%selectNlevs,&
               LVT_rc%nasc))
          stats%fbias%value_asc = 0.0
          stats%fbias%count_value_asc = 0
       endif
       if(metric%computeADC.eq.1) then 
          allocate(stats%fbias%value_adc(LVT_rc%ngrid, model%selectNlevs,LVT_rc%nadc))
          allocate(stats%fbias%count_value_adc(LVT_rc%ngrid, model%selectNlevs,&
               LVT_rc%nadc))
          stats%fbias%value_adc = 0.0
          stats%fbias%count_value_adc = 0
       endif
    endif
!-------------------------------------------------------------------------
! Number of passes required to compute the metric
!-------------------------------------------------------------------------

    metric%npass = 1
    metric%obsData = .false. 
    metric%stdevFlag = .false. 

  end subroutine LVT_initFBIAS
  
!BOP
! 
! !ROUTINE: LVT_diagnoseFBIAS
! \label{LVT_diagnoseFBIAS}
!
! !INTERFACE: 
  subroutine LVT_diagnoseFBIAS(pass)
! 
! !USES:   
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!   This subroutine issues the calls to update the FBIAS calculation for 
!   desired variables.
!
!   The methods invoked are: 
!   \begin{description}
!    \item[diagnoseSingleFBIAS](\ref{diagnoseSingleFBIAS})
!     updates the FBIAS computation for a single variable 
!   \end{description}
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP

    implicit none
    integer                 :: pass
    type(LVT_metadataEntry), pointer :: model
    type(LVT_metadataEntry), pointer :: obs
    type(LVT_statsEntry)   , pointer :: stats

    if(pass.eq.1) then 
       if(LVT_metrics%fbias%selectOpt.eq.1.or.&
            LVT_metrics%fbias%timeOpt.eq.1) then 
          call LVT_getDataStream1Ptr(model)
          call LVT_getDataStream2Ptr(obs)
          call LVT_getstatsEntryPtr(stats)

          do while(associated(model))
             call diagnoseSingleFBIAS(obs, model, stats, &
                  LVT_metrics%fbias)

             model => model%next
             obs => obs%next
             stats => stats%next
             
          enddo
       endif
    endif
  end subroutine LVT_diagnoseFBIAS

!BOP
! 
! !ROUTINE: diagnoseSingleFBIAS
! \label{diagnoseSingleFBIAS}
!
! !INTERFACE: 
  subroutine diagnoseSingleFBIAS(obs, model, stats,metric)
! 
! !USES:   
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!  This routine updates the FBIAS computation (updates the running 
!  sum calculations of the squared error) 
!  The arguments are: 
!
!  \begin{description}
!   \item[obs] observation object
!   \item[model] model variable object
!   \item[stats] object to hold the updated statistics
!  \end{description}
! !DESCRIPTION: 
!  This routine updates the FBIAS computation (updates the running 
!  sum calculations of the squared error) 
!  The arguments are: 
!
!  \begin{description}
!   \item[obs] observation object
!   \item[model] model variable object
!   \item[stats] object to hold the updated statistics
!  \end{description}
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP

    implicit none

    type(LVT_metaDataEntry) :: obs
    type(LVT_metaDataEntry) :: model
    type(LVT_statsEntry) :: stats
    type(LVT_metricEntry)   :: metric

    integer    :: t,k,m_k,o_k
    real       :: aval,bval,cval

    if(stats%selectOpt.eq.1.and.obs%selectNlevs.ge.1) then 
       do t=1,LVT_rc%ngrid
          do k=1,model%selectNlevs
             m_k = k+model%startNlevs -1
             o_k = k+obs%startNlevs -1
             if(trim(obs%units).eq.trim(model%units)) then 
                if(obs%value(t,o_k).ne.0.and. &
                     model%value(t,m_k).ne.0) then                    
                   aval = 0.0
                   bval = 0.0
                   cval = 0.0                  
                   if(metric%selectOpt.eq.1.or.metric%timeOpt.eq.1) then
                      if(model%value(t,m_k).gt.metric%threshold.and.&
                           obs%value(t,o_k).gt.metric%threshold) then 
                         aval = aval + 1
                      endif
                      if(model%value(t,m_k).gt.metric%threshold.and.&
                           obs%value(t,o_k).le.metric%threshold) then 
                         bval = bval + 1
                      endif
                      if(model%value(t,m_k).le.metric%threshold.and.&
                           obs%value(t,o_k).gt.metric%threshold) then 
                         cval = cval + 1
                      endif
                      if(aval+cval.ne.0) then 
                         stats%fbias%value_ts(t,k,1)  = stats%fbias%value_ts(t,k,1) + &
                              (aval+bval)/(aval+cval)
                         stats%fbias%count_value_ts(t,k,1) = &
                              stats%fbias%count_value_ts(t,k,1) + 1
                         stats%fbias%value_total(t,k,1)  = stats%fbias%value_total(t,k,1) + &
                              (aval+bval)/(aval+cval)
                         stats%fbias%count_value_total(t,k,1) = &
                              stats%fbias%count_value_total(t,k,1) + 1
                      endif
                      if(LVT_rc%strat_nlevels.gt.1) then 
                         aval = 0.0
                         bval = 0.0
                         cval = 0.0
                         if(LVT_stats%strat_var(t,k).gt.&
                              LVT_rc%strat_var_threshold) then
                            if(model%value(t,m_k).gt.metric%threshold.and.&
                                 obs%value(t,o_k).gt.metric%threshold) then 
                               aval = aval+1
                            endif
                            if(model%value(t,m_k).gt.metric%threshold.and.&
                                 obs%value(t,o_k).le.metric%threshold) then 
                               bval = bval + 1
                            endif
                            if(model%value(t,m_k).le.metric%threshold.and.&
                                 obs%value(t,o_k).gt.metric%threshold) then 
                               cval = cval+1
                            endif
                            if(aval+cval.ne.0) then 
                               stats%fbias%value_ts(t,k,2) =  (aval+bval)/(aval+cval)
                               stats%fbias%count_value_ts(t,k,2) = &
                                    stats%fbias%count_value_ts(t,k,2) + 1
                               stats%fbias%value_total(t,k,2)  = &
                                    stats%fbias%value_total(t,k,2) + &
                                     (aval+bval)/(aval+cval)
                               stats%fbias%count_value_total(t,k,2) = &
                                    stats%fbias%count_value_total(t,k,2) + 1
                            endif

                         elseif(LVT_stats%strat_var(t,k).le.&
                              LVT_rc%strat_var_threshold) then 
                            if(model%value(t,m_k).gt.metric%threshold.and.&
                                 obs%value(t,o_k).gt.metric%threshold) then 
                               aval = aval+1
                            endif
                            if(model%value(t,m_k).gt.metric%threshold.and.&
                                 obs%value(t,o_k).le.metric%threshold) then 
                               bval = bval + 1
                            endif
                            if(model%value(t,m_k).le.metric%threshold.and.&
                                 obs%value(t,o_k).gt.metric%threshold) then 
                               cval = cval+1
                            endif 
                            if(aval+bval+cval.ne.0) then 
                               stats%fbias%value_ts(t,k,3) = (aval+bval)/(aval+cval)
                               stats%fbias%count_value_ts(t,k,3) = &
                                    stats%fbias%count_value_ts(t,k,3) + 1
                               stats%fbias%value_total(t,k,3)  = stats%fbias%value_total(t,k,3) + &
                                    (aval+bval)/(aval+cval)
                               stats%fbias%count_value_total(t,k,3) = &
                                    stats%fbias%count_value_total(t,k,3) + 1
                            endif
                         endif
                      endif
                   endif                   
                endif
             else
                write(LVT_logunit,*) 'For variable ',trim(model%standard_name)
                write(LVT_logunit,*) 'observations are in ',trim(obs%units)
                write(LVT_logunit,*) 'and LIS output is in ',trim(model%units)
                write(LVT_logunit,*) 'please add the support of ',&
                     trim(model%units), ' in the observation plugin'
                call LVT_endrun
             endif
          enddo
       enddo
    endif
    
  end subroutine diagnoseSingleFBIAS


!BOP
! 
! !ROUTINE: LVT_computeFBIAS
! \label{LVT_computeFBIAS}
!
! !INTERFACE: 
  subroutine LVT_computeFBIAS(pass,alarm)
! 
! !USES:   
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!   This subroutine issues the calls to compute FBIAS values for the 
!   desired variables
! 
!   The methods invoked are: 
!   \begin{description}
!    \item[computeSingleFBIAS](\ref{computeSingleFBIAS})
!     updates the FBIAS computation for a single variable 
!   \end{description}
! 
!   The arguments are: 
!   \begin{description}
!    \item[check]
!     boolean flag indicating if the specified interval for 
!     FBIAS computation has been reached
!   \end{description}
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP

    implicit none

    integer               :: pass

    logical     :: alarm
    integer     :: i
    type(LVT_metadataEntry), pointer :: model
    type(LVT_metadataEntry), pointer :: obs
    type(LVT_statsEntry)   , pointer :: stats


    if(pass.eq.1) then 
       if(LVT_metrics%fbias%selectOpt.eq.1.or.&
            LVT_metrics%fbias%timeOpt.eq.1) then 
          if(alarm) then 
             if(LVT_metrics%fbias%timeOpt.eq.1.and.&
                  LVT_metrics%fbias%extractTS.eq.1) then 
                do i=1,LVT_rc%ntslocs
                   write(LVT_metrics%fbias%ftn_ts_loc(i),200,advance='no') &
                        LVT_rc%yr, '',LVT_rc%mo, '', LVT_rc%da, '', &
                        LVT_rc%hr,'',LVT_rc%mn, '' 
                enddo
             endif
          endif
200       format(I4, a1, I2.2, a1, I2.2, a1, I2.2, a1, I2.2,a1)
          
          call LVT_getDataStream1Ptr(model)
          call LVT_getDataStream2Ptr(obs)
          call LVT_getstatsEntryPtr(stats)
          
          do while(associated(model))
             call computeSingleFBIAS(alarm,obs,model,stats,&
                  LVT_metrics%fbias)
             
             model => model%next
             obs => obs%next
             stats => stats%next
             
          enddo
          
          if(alarm) then 
             if(LVT_metrics%fbias%timeOpt.eq.1.and.&
                  LVT_metrics%fbias%extractTS.eq.1) then 
                do i=1,LVT_rc%ntslocs
                   write(LVT_metrics%fbias%ftn_ts_loc(i),fmt='(a1)') ''
                enddo
             endif
          endif
       endif
    endif
  end subroutine LVT_ComputeFBIAS

!BOP
! 
! !ROUTINE: computeSingleFBIAS
! \label{computeSingleFBIAS}
!
! !INTERFACE: 
  subroutine computeSingleFBIAS(alarm,obs, model,stats,metric)
! 
! !USES:   
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!  This routine computes the FBIAS values for a single variable
!  The arguments are: 
!
!  \begin{description}
!    \item[check]
!     boolean flag indicating if the specified interval for 
!     FBIAS computation has been reached
!    \item[obs] observation object
!    \item[model] model variable object
!    \item[stats] object to hold the updated statistics
!  \end{description}
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP

    implicit none
    logical                 :: alarm
    type(LVT_metaDataEntry) :: obs
    type(LVT_metaDataEntry) :: model
    type(LVT_statsEntry)    :: stats
    type(LVT_metricEntry)   :: metric

    integer  :: t,l,k,tind

    if((metric%selectOpt.eq.1.or.metric%timeOpt.eq.1)) then 
       if(stats%selectOpt.eq.1.and.obs%selectNlevs.ge.1) then 
          do t=1,LVT_rc%ngrid
             do k=1,model%selectNlevs
                do l=1,LVT_rc%strat_nlevels
                   if(stats%fbias%count_value_ts(t,k,l).ne.0) then 
                      stats%fbias%value_ts(t,k,l) = stats%fbias%value_ts(t,k,l)/&
                           (stats%fbias%count_value_ts(t,k,l))
                      stats%fbias%tavg_value_ts(t,k,l) = &
                           stats%fbias%tavg_value_ts(t,k,l) + &
                           stats%fbias%value_ts(t,k,l)
                      stats%fbias%tavg_count_value_ts(t,k,l) = &
                           stats%fbias%tavg_count_value_ts(t,k,l) + 1
                   else
                      stats%fbias%value_ts(t,k,l) = LVT_rc%udef
                   endif
                enddo                   
                if(metric%computeSC.eq.1) then 
                   if(stats%fbias%value_ts(t,k,1).ne.LVT_rc%udef) then 
                      call LVT_getSeasonalCycleTimeIndex(LVT_rc%scInterval,&
                           tind)
                      stats%fbias%value_asc(t,k,tind) = &
                           stats%fbias%value_asc(t,k,tind)+ &
                           stats%fbias%value_ts(t,k,1)
                      stats%fbias%count_value_asc(t,k,tind) = &
                           stats%fbias%count_value_asc(t,k,tind)+ 1
                   endif
                endif
                if(metric%computeADC.eq.1) then 
                   if(stats%fbias%value_ts(t,k,1).ne.LVT_rc%udef) then 
                      call LVT_getADCTimeIndex(tind)
                      stats%fbias%value_adc(t,k,tind) = &
                           stats%fbias%value_adc(t,k,tind)+ &
                           stats%fbias%value_ts(t,k,1)
                      stats%fbias%count_value_adc(t,k,tind) = &
                           stats%fbias%count_value_adc(t,k,tind)+ 1
                   endif                
                endif
             enddo
          enddo
          if(alarm) then 
             do t=1,LVT_rc%ngrid
                do k=1,model%selectNlevs
                   do l=1,LVT_rc%strat_nlevels
                      if(stats%fbias%tavg_count_value_ts(t,k,l).ne.0) then 
                         stats%fbias%tavg_value_ts(t,k,l) = &
                              stats%fbias%tavg_value_ts(t,k,l)/&
                              (stats%fbias%tavg_count_value_ts(t,k,l))
                      else
                         stats%fbias%tavg_value_ts(t,k,l) = LVT_rc%udef
                      endif
                   enddo
                enddo
             enddo

             if(metric%extractTS.eq.1) then 
                call LVT_writeTSinfo(metric%ftn_ts_loc,&
                     model,&
                     LVT_rc%ngrid,&
                     stats%fbias%tavg_value_ts,&
                     stats%fbias%tavg_count_value_ts)
             endif
          endif
       endif
    endif


    if(LVT_rc%endtime.eq.1.and.metric%selectOpt.eq.1) then 
       if(stats%selectOpt.eq.1.and.obs%selectNlevs.ge.1) then 
          do t=1,LVT_rc%ngrid
             do k=1,model%selectNlevs
                do l=1,LVT_rc%strat_nlevels
                   if((stats%fbias%count_value_total(t,k,l).ne.0.and.&
                        stats%fbias%count_value_total(t,k,l)&
                        .gt.LVT_rc%obsCountThreshold)) then 
                      
                      stats%fbias%value_total(t,k,l) = stats%fbias%value_total(t,k,l)/&
                           (stats%fbias%count_value_total(t,k,l))
                   else
                      stats%fbias%value_total(t,k,l) = LVT_rc%udef
                   endif
                enddo
                if(metric%computeSC.eq.1) then 
                   do l=1,LVT_rc%nasc
                      if(stats%fbias%count_value_asc(t,k,l).gt.&
                           LVT_rc%SCCountThreshold) then
                         stats%fbias%value_asc(t,k,l) = stats%fbias%value_asc(t,k,l)/&
                              stats%fbias%count_value_asc(t,k,l)
                      else
                         stats%fbias%value_asc(t,k,l) = LVT_rc%udef
                      endif
                   enddo
                endif
                if(metric%computeADC.eq.1) then 
                   do l=1,LVT_rc%nadc
                      if(stats%fbias%count_value_adc(t,k,l).gt.&
                           LVT_rc%ADCCountThreshold) then
                         stats%fbias%value_adc(t,k,l) = stats%fbias%value_adc(t,k,l)/&
                              stats%fbias%count_value_adc(t,k,l)
                      else
                         stats%fbias%value_adc(t,k,l) = LVT_rc%udef
                      endif
                   enddo
                endif
             enddo
          enddo
          
          do k=1,model%selectNlevs
             do l=1, LVT_rc%strat_nlevels
                call LVT_computeCI(stats%fbias%value_total(:,k,l),LVT_rc%ngrid,&
                     LVT_rc%pval_CI,stats%fbias%value_ci(k,l))
             enddo
          enddo
       
          call LVT_writeDataBasedStrat(model,obs,stats,metric,&
               LVT_rc%ngrid,stats%fbias%value_total)      
          if(metric%computeSC.eq.1) then 
             call LVT_writeSeasonalCycleInfo(model,obs,stats,metric,&
                  LVT_rc%ngrid,stats%fbias%value_asc,stats%fbias%count_value_asc)
          endif
          if(metric%computeADC.eq.1) then 
             call LVT_writeAvgDiurnalCycleInfo(model,obs,stats,metric,&
                  LVT_rc%ngrid,stats%fbias%value_adc,stats%fbias%count_value_adc)
          endif
       endif
    endif

  end subroutine computeSingleFBIAS

!BOP
! 
! !ROUTINE: LVT_writeMetric_FBIAS
! \label{LVT_writeMetric_FBIAS}
!
! !INTERFACE: 
  subroutine LVT_writeMetric_FBIAS(pass,final,vlevels,stats,obs)
! 
! !USES:
    use LVT_statsMod, only : LVT_writeSummaryStats
    use LVT_pluginIndices
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
! 
!EOP
!BOP
! !ARGUMENTS: 
    integer                 :: pass
    integer                 :: final
    integer                 :: vlevels
    type(LVT_statsEntry)   :: stats
    type(LVT_metaDataEntry) :: obs
!EOP
    integer                 :: l,tind
    integer                 :: k

    if(pass.eq.LVT_metrics%fbias%npass) then 
       if(final.ne.1) then 
          if(stats%selectOpt.eq.1) then 
             do k=1,vlevels
                if(LVT_metrics%fbias%timeOpt.eq.1) then 
                   do l=1,LVT_rc%strat_nlevels
                      call LVT_writevar_gridded(LVT_metrics%fbias%ftn_ts, &
                           stats%fbias%value_ts(:,k,l),stats%vid_ts(LVT_FBIASid,1),1)
                      call LVT_writevar_gridded(LVT_metrics%fbias%ftn_ts, &
                           real(stats%fbias%count_value_ts(:,k,l)),&
                           stats%vid_count_ts(LVT_FBIASid,1),1)
                   enddo
                endif
             enddo
          endif
       else
          if(pass.eq.1) then 
             if(LVT_metrics%fbias%selectOpt.eq.1) then
                if(stats%selectOpt.eq.1) then 
                   do k=1,vlevels
                      do l=1,LVT_rc%strat_nlevels
                         
                         call LVT_writevar_gridded(LVT_metrics%fbias%ftn_total, &
                              stats%fbias%value_total(:,k,l),stats%vid_total(LVT_FBIASid,1))
                         call LVT_writevar_gridded(LVT_metrics%fbias%ftn_total, &
                              real(stats%fbias%count_value_total(:,k,l)),&
                              stats%vid_count_total(LVT_FBIASid,1))
                         
                      enddo
                      if(LVT_metrics%fbias%computeSC.eq.1) then 
                         do tind = 1,LVT_rc%nasc
                            call LVT_writevar_gridded(LVT_metrics%fbias%ftn_total,&
                                 stats%fbias%value_asc(:,k,tind),&
                                 stats%vid_sc_total(tind,LVT_FBIASid,1))
                         enddo
                      endif
                      
                      if(LVT_metrics%fbias%computeADC.eq.1) then 
                         do tind = 1,LVT_rc%nadc
                            call LVT_writevar_gridded(LVT_metrics%fbias%ftn_total,&
                                 stats%fbias%value_adc(:,k,tind),&
                                 stats%vid_adc_total(tind,LVT_FBIASid,1))
                         enddo
                      endif
                      
                      call LVT_writeSummaryStats(&
                           LVT_metrics%fbias%ftn_summ,&
                           LVT_metrics%fbias%short_name,&
                           LVT_rc%ngrid,&
                           stats%fbias%value_total(:,k,:), &
                           stats%fbias%count_value_total(:,k,:),stats%standard_name,&
                           stats%fbias%value_ci(k,:))
                   enddo
                endif
             endif
          endif
       end if
    endif
  end subroutine LVT_writeMetric_FBIAS


!BOP
! 
! !ROUTINE: LVT_resetMetric_FBIAS
! \label(LVT_resetMetric_FBIAS)
!
! !INTERFACE:
  subroutine LVT_resetMetric_FBIAS(alarm)
! 
! !INPUT PARAMETERS: 
    logical                :: alarm
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP
    integer                 :: vlevels
    integer                 :: i,k,l
    type(LVT_metadataEntry), pointer :: model
    type(LVT_statsEntry)   , pointer :: stats


    call LVT_getDataStream1Ptr(model)
    call LVT_getstatsEntryPtr(stats)
    
    do while(associated(model))
       if(stats%selectOpt.eq.1) then 
          do k=1,model%selectNlevs
             if(LVT_metrics%fbias%timeOpt.eq.1) then 
                do l=1,LVT_rc%strat_nlevels
                   stats%fbias%value_ts(:,k,l) = 0.0
                   stats%fbias%count_value_ts(:,k,l)=0 
                   if(alarm) then 
                      stats%fbias%tavg_value_ts(:,k,l) = 0.0
                      stats%fbias%tavg_count_value_ts(:,k,l)=0 
                   endif
                enddo
             endif
          enddo
       endif
       
       model => model%next
       stats => stats%next
    enddo
  end subroutine LVT_resetMetric_FBIAS

!BOP
! 
! !ROUTINE: LVT_writerestart_FBIAS
! 
! !INTERFACE:
  subroutine LVT_writerestart_FBIAS(ftn,pass)
! !USES: 

! 
! !ARGUMENTS: 
    integer                 :: ftn
    integer                 :: pass

! !DESCRIPTION: 
!  This routine writes the restart file for FBIAS metric computations
! 
!EOP
    
!
! !DESCRIPTION: 
! 
!EOP
    if(LVT_metrics%fbias%selectOpt.eq.1) then 
       
       print*, 'The writerestart method is not implemented for FBIAS'
       stop
    end if
    
  end subroutine LVT_writerestart_FBIAS

!BOP
! 
! !ROUTINE: LVT_readrestart_FBIAS
! 
! !INTERFACE:
  subroutine LVT_readrestart_FBIAS(ftn)
! !USES: 

! 
! !ARGUMENTS: 
    integer                 :: ftn

! !DESCRIPTION: 
!  This routine reads the restart file for FBIAS metric computations
! 
!EOP
    
!
! !DESCRIPTION: 
! 
!EOP
    if(LVT_metrics%fbias%selectOpt.eq.1) then 
       
       print*, 'The readrestart method is not implemented for FBIAS'
       stop
    end if
    
  end subroutine LVT_readrestart_FBIAS
end module LVT_FBIASMod
