!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------------
! NASA GSFC Land surface Verification Toolkit (LVT) V1.0
!-------------------------END NOTICE -- DO NOT EDIT-----------------------------
!
!BOP
! 
! !MODULE: LVT_RnkCorrMod
! \label(LVT_RnkCorrMod)
!
! !INTERFACE:
module LVT_RnkCorrMod
! 
! !USES:   
  use ESMF
  use LVT_coreMod
  use LVT_histDataMod
  use LVT_statsDataMod
  use LVT_historyMod
  use LVT_TSMod
  use LVT_logMod
  use LVT_CIMod
  use LVT_timeMgrMod

  implicit none

  private
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
!  !DESCRIPTION: 
!   This module handles the raw correlation (pearson correlation
!   coefficient)  computations by comparing 
!   the LIS output to the specified observations. 
!
! Time series, seasonal cycles, average diurnal cycle options are 
! not supported for Rnk Correlation computation
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
  public :: LVT_initRnkCorr
  public :: LVT_diagnoseRnkCorr
  public :: LVT_computeRnkCorr 
  public :: LVT_writeMetric_RnkCorr
  public :: LVT_resetMetric_RnkCorr
  public :: LVT_writerestart_RnkCorr
  public :: LVT_readrestart_RnkCorr

 type, public :: rnkcorrdec
    integer          :: nsize_total
    integer          :: nsize_season
    integer          :: tscale
 end type rnkcorrdec

  type(rnkcorrdec) :: LVT_rnkcorr_struc
contains
  
!BOP
! 
! !ROUTINE: LVT_initRnkCorr
! \label{LVT_initRnkCorr}
!
! !INTERFACE: 
  subroutine LVT_initRnkCorr(model,obs,stats,metric)
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

    if(metric%selectOpt.eq.1) then 
       call computeAnalysisLength(LVT_rnkcorr_struc%nsize_total, &
            LVT_rc%tavgInterval)

       allocate(stats%rnkcorr%model_value_final(LVT_rc%ngrid, &
            model%selectNlevs,LVT_rnkcorr_struc%nsize_total,&
            LVT_rc%strat_nlevels))
       allocate(stats%rnkcorr%obs_value_final(LVT_rc%ngrid, &
            model%selectNlevs,LVT_rnkcorr_struc%nsize_total,&
            LVT_rc%strat_nlevels))
       allocate(stats%rnkcorr%count_value(LVT_rc%ngrid, &
            model%selectNlevs,LVT_rc%strat_nlevels))

       allocate(stats%rnkcorr%rval_r(LVT_rc%ngrid, &
            model%selectNlevs,LVT_rc%strat_nlevels))

       stats%rnkcorr%model_value_final = LVT_rc%udef 
       stats%rnkcorr%obs_value_final   = LVT_rc%udef 
       stats%rnkcorr%count_value       = 0 
       stats%rnkcorr%rval_r            = LVT_rc%udef

       allocate(stats%rnkcorr%value_ci(model%selectNlevs,LVT_rc%strat_nlevels))
       stats%rnkcorr%value_ci = LVT_rc%udef
       
       if(metric%timeOpt.eq.1) then 
          
          write(LVT_logunit,*) '[ERR] Temporal calculations are not '
          write(LVT_logunit,*) '[ERR] supported for the rank correlation metric.'
          write(LVT_logunit,*) '[ERR] Program stopping ..'
          call LVT_endrun()

          allocate(stats%rnkcorr%model_value_ts(LVT_rc%ngrid, &
               model%selectNlevs,&
               LVT_rnkcorr_struc%nsize_season,LVT_rc%strat_nlevels))
          allocate(stats%rnkcorr%obs_value_ts(LVT_rc%ngrid, &
               model%selectNlevs,&
               LVT_rnkcorr_struc%nsize_season,LVT_rc%strat_nlevels))
          allocate(stats%rnkcorr%count_value_ts(LVT_rc%ngrid, &
               model%selectNlevs,LVT_rc%strat_nlevels))
          
          allocate(stats%rnkcorr%rval_ts_r(LVT_rc%ngrid, &
               model%selectNlevs,LVT_rc%strat_nlevels))
          
          stats%rnkcorr%model_value_ts    = LVT_rc%udef 
          stats%rnkcorr%obs_value_ts      = LVT_rc%udef 
          stats%rnkcorr%count_value_ts    = 0 
          stats%rnkcorr%rval_ts_r            = LVT_rc%udef

          allocate(stats%rnkcorr%tavg_value_ts(LVT_rc%ngrid, &
               model%selectNlevs,LVT_rc%strat_nlevels))
          allocate(stats%rnkcorr%tavg_count_value_ts(LVT_rc%ngrid, &
               model%selectNlevs,LVT_rc%strat_nlevels))
          stats%rnkcorr%tavg_value_ts = 0.0
          stats%rnkcorr%tavg_count_value_ts=0 
       
          if(metric%computeSC.eq.1) then
             allocate(stats%rnkcorr%value_asc(LVT_rc%ngrid, &
                  model%selectNlevs, LVT_rc%nasc))
             stats%rnkcorr%value_asc = 0.0
             allocate(stats%rnkcorr%count_value_asc(LVT_rc%ngrid, &
                  model%selectNlevs, &
                  LVT_rc%nasc))
             stats%rnkcorr%count_value_asc = 0
          endif

          if(metric%computeADC.eq.1) then 
             allocate(stats%rnkcorr%value_adc(LVT_rc%ngrid, &
                  model%selectNlevs, LVT_rc%nadc))
             stats%rnkcorr%value_adc = 0.0
             allocate(stats%rnkcorr%count_value_adc(LVT_rc%ngrid, &
                  model%selectNlevs, &
                  LVT_rc%nadc))
             stats%rnkcorr%count_value_adc = 0
          endif
          
       endif

    endif
    
!-------------------------------------------------------------------------
! Number of passes required to compute the metric
!-------------------------------------------------------------------------

    metric%npass = 1
    metric%obsData = .false. 
    metric%stdevFlag = .false. 

  end subroutine LVT_initRnkCorr
  
!BOP
! 
! !ROUTINE: LVT_diagnoseRnkCorr
! \label{LVT_diagnoseRnkCorr}
!
! !INTERFACE:  
  subroutine LVT_diagnoseRnkCorr(pass)
! 
! !USES:   
    implicit none
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!   This subroutine issues the calls to update the RnkCorr calculation for 
!   desired variables.
!
!   The methods invoked are: 
!   \begin{description}
!    \item[diagnoseSingleRnkCorr](\ref{diagnoseSingleRnkCorr})
!     updates the RnkCorr computation for a single variable 
!   \end{description}
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP

    integer                 :: pass
    type(LVT_metadataEntry), pointer :: model
    type(LVT_metadataEntry), pointer :: obs
    type(LVT_statsEntry)   , pointer :: stats

    if(pass.eq.1) then 
       if(LVT_metrics%rnkcorr%selectOpt.eq.1.or.&
            LVT_metrics%rnkcorr%timeOpt.eq.1) then 

          call LVT_getDataStream1Ptr(model)
          call LVT_getDataStream2Ptr(obs)
          call LVT_getstatsEntryPtr(stats)

          do while(associated(model))
             call diagnoseSingleRnkCorr(obs,model,stats, &
                  LVT_metrics%rnkcorr)
             
             model => model%next
             obs   => obs%next
             stats => stats%next

          enddo
       endif
    endif
  end subroutine LVT_diagnoseRnkCorr

!BOP
! 
! !ROUTINE: diagnoseSingleRnkCorr
! \label{diagnoseSingleRnkCorr}
!
! !INTERFACE: 
  subroutine diagnoseSingleRnkCorr(obs, model, stats,metric)
! 
! !USES:   
    implicit none
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!  This routine updates the RnkCorr computation (updates the running 
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

    type(LVT_metaDataEntry) :: obs
    type(LVT_metaDataEntry) :: model
    type(LVT_statsEntry) :: stats
    type(LVT_metricEntry)   :: metric

    integer    :: t,k
    integer    :: tindex_f,m_k,o_k

    if(stats%selectOpt.eq.1.and.obs%selectNlevs.ge.1) then 

       call getDataTimeIndex(tindex_f, LVT_rc%tavgInterval)

       do t=1,LVT_rc%ngrid
          do k=1,model%selectNlevs
             m_k = k+model%startNlevs -1
             o_k = k+obs%startNlevs -1
             if(metric%selectOpt.eq.1) then
                if(model%value(t,m_k).ne.0.and.obs%value(t,o_k).ne.0) then 
                   stats%rnkcorr%model_value_final(t,k,tindex_f,1) = & 
                        model%value(t,m_k)
                   stats%rnkcorr%obs_value_final(t,k,tindex_f,1) = & 
                        obs%value(t,o_k)
                   stats%rnkcorr%count_value(t,k,1) = & 
                        stats%rnkcorr%count_value(t,k,1) + 1

                   if(LVT_rc%strat_nlevels.gt.1) then 
                      if(LVT_stats%strat_var(t,k).gt.&
                           LVT_rc%strat_var_threshold) then 
                         stats%rnkcorr%model_value_final(t,k,tindex_f,2) = & 
                              model%value(t,m_k)
                         stats%rnkcorr%obs_value_final(t,k,tindex_f,2) = & 
                              obs%value(t,o_k)
                         stats%rnkcorr%count_value(t,k,2) = & 
                              stats%rnkcorr%count_value(t,k,2) + 1

                      elseif(LVT_stats%strat_var(t,k).le.&
                           LVT_rc%strat_var_threshold) then 

                         stats%rnkcorr%model_value_final(t,k,tindex_f,3) = & 
                              model%value(t,m_k)
                         stats%rnkcorr%obs_value_final(t,k,tindex_f,3) = & 
                              obs%value(t,o_k)
                         stats%rnkcorr%count_value(t,k,3) = & 
                              stats%rnkcorr%count_value(t,k,2) + 1
                      endif
                   endif
                endif
                
             endif
          enddo
       enddo
    endif
    
  end subroutine diagnoseSingleRnkCorr


!BOP
! 
! !ROUTINE: LVT_computeRnkCorr
! \label{LVT_computeRnkCorr}
!
! !INTERFACE: 
  subroutine LVT_computeRnkCorr(pass,alarm)
! 
! !USES:   
    implicit none
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!   This subroutine issues the calls to compute RnkCorr values for the 
!   desired variables
! 
!   The methods invoked are: 
!   \begin{description}
!    \item[computeSingleRnkCorr](\ref{computeSingleRnkCorr})
!     updates the RnkCorr computation for a single variable 
!   \end{description}
! 
!   The arguments are: 
!   \begin{description}
!    \item[check]
!     boolean flag indicating if the specified interval for 
!     RnkCorr computation has been reached
!   \end{description}
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP

    integer               :: pass
    logical               :: alarm
    type(LVT_metadataEntry), pointer :: model
    type(LVT_metadataEntry), pointer :: obs
    type(LVT_statsEntry)   , pointer :: stats

    integer               :: i

    if(pass.eq.1) then 

       if(LVT_metrics%rnkcorr%selectOpt.eq.1.or.&
            LVT_metrics%rnkcorr%timeOpt.eq.1) then 
          if(alarm) then 
             if(LVT_metrics%rnkcorr%timeOpt.eq.1.and.&
                  LVT_metrics%rnkcorr%extractTS.eq.1) then 
                do i=1,LVT_rc%ntslocs
                   write(LVT_metrics%rnkcorr%ftn_ts_loc(i),200,advance='no') &
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
             call computeSingleRnkCorr(alarm,obs,model,stats,&
                  LVT_metrics%rnkcorr)

             model => model%next
             obs   => obs%next
             stats => stats%next
          enddo

          if(alarm) then 
             if(LVT_metrics%rnkcorr%timeOpt.eq.1.and.&
                  LVT_metrics%rnkcorr%extractTS.eq.1) then 
                do i=1,LVT_rc%ntslocs
                   write(LVT_metrics%rnkcorr%ftn_ts_loc(i),fmt='(a1)') ''
                enddo
             endif
          endif
       endif
    endif

  end subroutine LVT_ComputeRnkCorr

!BOP
! 
! !ROUTINE: computeSingleRnkCorr
! \label{computeSingleRnkCorr}
!
! !INTERFACE: 
  subroutine computeSingleRnkCorr(alarm,obs, model,stats,metric)
! 
! !USES:   
    implicit none
!
! !INPUT PARAMETERS: 
! 
! !OUTPUT PARAMETERS:
!
! !DESCRIPTION: 
!  This routine computes the RnkCorr values for a single variable
!  The arguments are: 
!
!  \begin{description}
!    \item[check]
!     boolean flag indicating if the specified interval for 
!     RnkCorr computation has been reached
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

    logical                 :: alarm
    type(LVT_metaDataEntry) :: obs
    type(LVT_metaDataEntry) :: model
    type(LVT_statsEntry)    :: stats
    type(LVT_metricEntry)   :: metric

    integer  :: t,l,k,m,tind
    real     :: numer
    real     :: denom_sq
    real     :: denom

    real, allocatable :: model_rnks(:)
    real, allocatable :: obs_rnks(:)
    real              :: sxy_r, sxx_r, syy_r, sy_r, sx_r
    integer           :: count_r

    if(metric%timeOpt.eq.1) then 
       if(alarm) then 
          do t=1,LVT_rc%ngrid
             do k=1,model%selectNlevs
                do l=1,LVT_rc%strat_nlevels
                   if(stats%rnkcorr%count_value_ts(t,k,l).ne.0.and.&
                        stats%rnkcorr%count_value_ts(t,k,l) &
                        .gt.LVT_rc%obsCountThreshold) then 
                      
                      call computeRnkArray(LVT_rnkcorr_struc%nsize_season,&
                           stats%rnkcorr%model_value_ts(t,k,:,l),&
                           model_rnks(:))
                      
                      call computeRnkArray(LVT_rnkcorr_struc%nsize_season,&
                           stats%rnkcorr%obs_value_ts(t,k,:,l),&
                           obs_rnks(:))                   
                      sxy_r = 0 
                      sxx_r = 0 
                      syy_r = 0 
                      sx_r = 0 
                      sy_r = 0 
                      count_r = 0
                      
                      do m = 1, LVT_rnkcorr_struc%nsize_season
                         if(model_rnks(m).ne.LVT_rc%udef.and.&
                              obs_rnks(m).ne.LVT_rc%udef) then 
                            sxy_r = sxy_r + model_rnks(m)*obs_rnks(m)
                            sxx_r = sxx_r + model_rnks(m)*model_rnks(m)
                            syy_r = syy_r + obs_rnks(m)*obs_rnks(m)
                            sx_r = sx_r + model_rnks(m)
                            sy_r = sy_r + obs_rnks(m)
                            count_r = count_r + 1
                         endif
                      enddo
                      
                      numer = (float(count_r)*sxy_r - sx_r*sy_r)
                      denom_sq =  (float(count_r)* sxx_r-sx_r**2)* &
                           (float(count_r)*syy_r-sy_r**2)
                      if(denom_sq.gt.0) then 
                         denom = sqrt(denom_sq)
                      else
                         denom = 0.0
                      endif
                   
                      if(denom.ne.0) then 
                         stats%rnkcorr%rval_ts_r(t,k,l) = numer/denom
                      else
                         stats%rnkcorr%rval_ts_r(t,k,l) = LVT_rc%udef
                      endif
                   end if
                enddo
                if(metric%computeSC.eq.1) then 
                   if(stats%rnkcorr%rval_ts_r(t,k,l).ne.LVT_rc%udef) then 
                      call LVT_getSeasonalCycleTimeIndex(LVT_rc%scInterval,&
                           tind)
                      stats%rnkcorr%value_asc(t,k,tind) = & 
                           stats%rnkcorr%value_asc(t,k,tind) + & 
                           stats%rnkcorr%rval_ts_r(t,k,l)
                      stats%rnkcorr%count_value_asc(t,k,tind) = & 
                           stats%rnkcorr%count_value_asc(t,k,tind) + 1
                   endif
                endif
                if(metric%computeADC.eq.1) then 
                   if(stats%rnkcorr%rval_ts_r(t,k,l).ne.LVT_rc%udef) then 
                      call LVT_getADCTimeIndex(tind)
                      stats%rnkcorr%value_adc(t,k,tind) = & 
                           stats%rnkcorr%value_adc(t,k,tind) + & 
                           stats%rnkcorr%rval_ts_r(t,k,l)
                      stats%rnkcorr%count_value_adc(t,k,tind) = & 
                           stats%rnkcorr%count_value_adc(t,k,tind) + 1
                   endif
                endif
             enddo
          enddo
          if(metric%extractTS.eq.1) then 
             do t=1,LVT_rc%ngrid
                do k=1,model%selectNlevs
                   do l=1,LVT_rc%strat_nlevels
                      if(stats%rnkcorr%tavg_count_value_ts(t,k,l).gt.0) then 
                         stats%rnkcorr%tavg_value_ts(t,k,l) = &
                              stats%rnkcorr%tavg_value_ts(t,k,l)/&
                              stats%rnkcorr%tavg_count_value_ts(t,k,l)
                      else
                         stats%rnkcorr%tavg_value_ts(t,k,l) = LVT_rc%udef
                      endif
                   enddo
                enddo
             enddo
             call LVT_writeTSinfo(metric%ftn_ts_loc,&
                  model,&
                  LVT_rc%ngrid,&
                  stats%rnkcorr%tavg_value_ts,&
                  stats%rnkcorr%tavg_count_value_ts)
          endif
          
       endif
    endif                  
 
    if(LVT_rc%endtime.eq.1.and.metric%selectOpt.eq.1) then 
       if(stats%selectOpt.eq.1.and.obs%selectNlevs.ge.1) then 

          allocate(model_rnks(LVT_rnkcorr_struc%nsize_total))
          allocate(obs_rnks(LVT_rnkcorr_struc%nsize_total))

          do t=1,LVT_rc%ngrid
             do k=1,model%selectNlevs
                do l=1,LVT_rc%strat_nlevels
                   if(stats%rnkcorr%count_value(t,k,l).ne.0.and.&
                        stats%rnkcorr%count_value(t,k,l) &
                        .gt.LVT_rc%obsCountThreshold) then 
                      
                      call computeRnkArray(LVT_rnkcorr_struc%nsize_total,&
                           stats%rnkcorr%model_value_final(t,k,:,l),&
                           model_rnks(:))
                      
                      call computeRnkArray(LVT_rnkcorr_struc%nsize_total,&
                           stats%rnkcorr%obs_value_final(t,k,:,l),&
                           obs_rnks(:))
                      
                      sxy_r = 0 
                      sxx_r = 0 
                      syy_r = 0 
                      sx_r = 0 
                      sy_r = 0 
                      count_r = 0
                      
                      do m = 1, LVT_rnkcorr_struc%nsize_total
                         if(model_rnks(m).ne.LVT_rc%udef.and.&
                              obs_rnks(m).ne.LVT_rc%udef) then 
                            sxy_r = sxy_r + model_rnks(m)*obs_rnks(m)
                            sxx_r = sxx_r + model_rnks(m)*model_rnks(m)
                            syy_r = syy_r + obs_rnks(m)*obs_rnks(m)
                            sx_r = sx_r + model_rnks(m)
                            sy_r = sy_r + obs_rnks(m)
                            count_r = count_r + 1
                         endif
                      enddo
                      
                      numer = (float(count_r)*sxy_r - sx_r*sy_r)
                      denom_sq =  (float(count_r)* sxx_r-sx_r**2)* &
                           (float(count_r)*syy_r-sy_r**2)
                      if(denom_sq.gt.0) then 
                         denom = sqrt(denom_sq)
                      else
                         denom = 0.0
                      endif
                      
                      if(denom.ne.0) then 
                         stats%rnkcorr%rval_r(t,k,l) = numer/denom
                      else
                         stats%rnkcorr%rval_r(t,k,l) = LVT_rc%udef
                      endif
                   endif
                enddo
                if(metric%computeSC.eq.1) then 
                   do l=1,LVT_rc%nasc
                      if(stats%rnkcorr%count_value_asc(t,k,l).gt.&
                           LVT_rc%SCCountThreshold) then 
                         stats%rnkcorr%value_asc(t,k,l) = &
                              stats%rnkcorr%value_asc(t,k,l)/&
                              stats%rnkcorr%count_value_asc(t,k,l) 
                      else
                         stats%rnkcorr%value_asc(t,k,l) = LVT_rc%udef
                      endif
                   enddo
                endif
                if(metric%computeADC.eq.1) then 
                   do l=1,LVT_rc%nadc
                      if(stats%rnkcorr%count_value_adc(t,k,l).gt.&
                           LVT_rc%ADCCountThreshold) then 
                         stats%rnkcorr%value_adc(t,k,l) = &
                              stats%rnkcorr%value_adc(t,k,l)/&
                              stats%rnkcorr%count_value_adc(t,k,l) 
                      else
                         stats%rnkcorr%value_adc(t,k,l) = LVT_rc%udef
                      endif
                   enddo
                endif
                
             enddo
          enddo
          
          do k=1,model%selectNlevs
             do l=1, LVT_rc%strat_nlevels
                call LVT_computeCI(stats%rnkcorr%rval_r(:,k,l),LVT_rc%ngrid,&
                     LVT_rc%pval_CI,stats%rnkcorr%value_ci(k,l))
             enddo
          enddo
       
          call LVT_writeDataBasedStrat(model,obs,stats,metric,&
               LVT_rc%ngrid,stats%rnkcorr%rval_r)      

          deallocate(model_rnks)
          deallocate(obs_rnks)
       endif
    endif

  end subroutine computeSingleRnkCorr

!BOP
! 
! !ROUTINE: LVT_writeMetric_RnkCorr
! \label{LVT_writeMetric_RnkCorr}
!
! !INTERFACE: 
  subroutine LVT_writeMetric_RnkCorr(pass,final,vlevels,stats,obs)
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

    if(pass.eq.LVT_metrics%rnkcorr%npass) then 
       if(final.ne.1) then 
          if(stats%selectOpt.eq.1) then 
             do k=1,vlevels
                do l=1,LVT_rc%strat_nlevels
                   
                   call LVT_writevar_gridded(LVT_metrics%rnkcorr%ftn_ts, &
                        stats%rnkcorr%tavg_value_ts(:,k,l),stats%vid_ts(LVT_Rnkcorrid,1))
                   call LVT_writevar_gridded(LVT_metrics%rnkcorr%ftn_ts, &
                        real(stats%rnkcorr%tavg_count_value_ts(:,k,l)),&
                        stats%vid_count_ts(LVT_Rnkcorrid,1))                
                enddo
             enddo
          endif
       else
          if(LVT_metrics%rnkcorr%selectOpt.eq.1) then
             if(stats%selectOpt.eq.1) then 
                do k=1,vlevels
                   do l=1,LVT_rc%strat_nlevels
                      
                      call LVT_writevar_gridded(LVT_metrics%rnkcorr%ftn_total, &
                           stats%rnkcorr%rval_r(:,k,l),stats%vid_total(LVT_Rnkcorrid,1))
                      call LVT_writevar_gridded(LVT_metrics%rnkcorr%ftn_total, &
                           real(stats%rnkcorr%count_value(:,k,l)),&
                           stats%vid_count_total(LVT_Rnkcorrid,1))                
                   enddo

                   if(LVT_metrics%rnkcorr%computeSC.eq.1) then 
                      do tind = 1, LVT_rc%nasc
                         call LVT_writevar_gridded(&
                              LVT_metrics%rnkcorr%ftn_total,&
                              stats%rnkcorr%value_asc(:,k,tind), &
                              stats%vid_sc_total(tind,LVT_Rnkcorrid,1),k)
                      enddo
                   endif
                   
                   if(LVT_metrics%rnkcorr%computeADC.eq.1) then 
                      do tind = 1,LVT_rc%nadc
                         call LVT_writevar_gridded(LVT_metrics%rnkcorr%ftn_total,&
                              stats%rnkcorr%value_adc(:,k,tind),&
                              stats%vid_adc_total(tind,LVT_Rnkcorrid,1),k)
                      enddo
                   endif

                   call LVT_writeSummaryStats(&
                        LVT_metrics%rnkcorr%ftn_summ,&
                        LVT_metrics%rnkcorr%short_name,&
                        LVT_rc%ngrid,&
                        stats%rnkcorr%rval_r(:,k,:), &
                        stats%rnkcorr%count_value(:,k,:),stats%standard_name,&
                        stats%rnkcorr%value_ci(k,:))
                enddo
             endif
          endif
       endif
    endif


  end subroutine LVT_writeMetric_RnkCorr


!BOP
! 
! !ROUTINE: LVT_resetMetric_RnkCorr
! \label(LVT_resetMetric_RnkCorr)
!
! !INTERFACE:
  subroutine LVT_resetMetric_RnkCorr(alarm)

! !INPUT PARAMETERS: 
    logical           :: alarm
 

!
! !DESCRIPTION: 
! 
! !FILES USED:
!
! !REVISION HISTORY: 
! 
!EOP

    integer                 :: vlevels
    
    integer                :: i,k,l
    type(LVT_metadataEntry), pointer :: model
    type(LVT_statsEntry)   , pointer :: stats


    call LVT_getDataStream1Ptr(model)
    call LVT_getstatsEntryPtr(stats)
    
    do while(associated(model))
       
       if(stats%selectOpt.eq.1) then 
          do k=1,model%selectNlevs
             if(LVT_metrics%rnkcorr%timeOpt.eq.1) then 
                do l=1,LVT_rc%strat_nlevels

                   if(alarm) then 
                      stats%rnkcorr%rval_ts_r(:,k,l) = 0.0
                      stats%rnkcorr%count_value_ts(:,k,l)=0 
                      
                      stats%rnkcorr%tavg_value_ts(:,k,l) = 0.0
                      stats%rnkcorr%tavg_count_value_ts(:,k,l)=0 
                   endif

                enddo
             endif
             
          enddo
          
       endif
       model => model%next
       stats => stats%next

    enddo
  end subroutine LVT_resetMetric_RnkCorr


!BOP
! 
! !ROUTINE: LVT_writerestart_RnkCorr
! 
! !INTERFACE:
  subroutine LVT_writerestart_RnkCorr(ftn,pass)
! !USES: 

! 
! !ARGUMENTS: 
    integer                 :: ftn
    integer                 :: pass

! !DESCRIPTION: 
!  This routine writes the restart file for RnkCorr metric computations
! 
!EOP
    
!
! !DESCRIPTION: 
! 
!EOP
    integer              :: k,l
    type(LVT_metadataEntry), pointer :: model
    type(LVT_metadataEntry), pointer :: obs
    type(LVT_statsEntry)   , pointer :: stats

    if(LVT_metrics%rnkcorr%selectOpt.eq.1) then 
       
       call LVT_getDataStream1Ptr(model)
       call LVT_getDataStream2Ptr(obs)
       call LVT_getstatsEntryPtr(stats)

       do while(associated(model))
          
          if(stats%selectOpt.eq.1.and.obs%selectNlevs.ge.1) then 
             do k=1,model%selectNlevs
                do l=1,LVT_rc%strat_nlevels     
                   call LVT_writevar_restart(ftn,stats%rnkcorr%rval_r(:,k,l))
                   call LVT_writevar_restart(ftn,stats%rnkcorr%count_value(:,k,l))
                enddo
             enddo

             if(LVT_metrics%rnkcorr%computeSC.eq.1) then 
                do k=1,model%selectNlevs
                   do l=1,LVT_rc%nasc
                      call LVT_writevar_restart(ftn,&
                           stats%rnkcorr%value_asc(:,k,l))
                      call LVT_writevar_restart(ftn,&
                           stats%rnkcorr%count_value_asc(:,k,l))
                   enddo
                enddo
             endif
             if(LVT_metrics%rnkcorr%computeADC.eq.1) then 
                do k=1,model%selectNlevs
                   do l=1,LVT_rc%nadc
                      call LVT_writevar_restart(ftn,&
                           stats%rnkcorr%value_adc(:,k,l))
                      call LVT_writevar_restart(ftn,&
                           stats%rnkcorr%count_value_adc(:,k,l))
                   enddo
                enddo
             endif
             
          endif

          model => model%next
          obs   => obs%next
          stats => stats%next
          
       enddo
    end if
    
  end subroutine LVT_writerestart_RnkCorr

!BOP
! 
! !ROUTINE: LVT_readrestart_RnkCorr
! 
! !INTERFACE:
  subroutine LVT_readrestart_RnkCorr(ftn)
! !USES: 

! 
! !ARGUMENTS: 
    integer                 :: ftn

! !DESCRIPTION: 
!  This routine reads the restart file for RnkCorr metric computations
! 
!EOP
    
!
! !DESCRIPTION: 
! 
!EOP
    type(LVT_metadataEntry), pointer :: model
    type(LVT_metadataEntry), pointer :: obs
    type(LVT_statsEntry)   , pointer :: stats
    integer              :: k,l

    if(LVT_metrics%rnkcorr%selectOpt.eq.1) then 
       call LVT_getDataStream1Ptr(model)
       call LVT_getDataStream2Ptr(obs)
       call LVT_getstatsEntryPtr(stats)
       
       do while(associated(model))
          if(stats%selectOpt.eq.1.and.obs%selectNlevs.ge.1) then 
             do k=1,model%selectNlevs
                do l=1,LVT_rc%strat_nlevels         
                   call LVT_readvar_restart(ftn,stats%rnkcorr%rval_r(:,k,l))
                   call LVT_readvar_restart(ftn,stats%rnkcorr%count_value(:,k,l))
                enddo
             enddo

             if(LVT_metrics%rnkcorr%computeSC.eq.1) then 
                do k=1,model%selectNlevs
                   do l=1,LVT_rc%nasc
                      call LVT_readvar_restart(ftn,&
                           stats%rnkcorr%value_asc(:,k,l))
                      call LVT_readvar_restart(ftn,&
                           stats%rnkcorr%count_value_asc(:,k,l))
                   enddo
                enddo
             endif
             if(LVT_metrics%rnkcorr%computeADC.eq.1) then 
                do k=1,model%selectNlevs
                   do l=1,LVT_rc%nadc
                      call LVT_readvar_restart(ftn,&
                           stats%rnkcorr%value_adc(:,k,l))
                      call LVT_readvar_restart(ftn,&
                           stats%rnkcorr%count_value_adc(:,k,l))
                   enddo
                enddo
             endif
             
          endif
          
          model => model%next
          obs   => obs%next
          stats => stats%next

       enddo
    end if

  end subroutine LVT_readrestart_RnkCorr

!BOP
! 
! !ROUTINE: computeAnalysisLength
! \label{computeAnalysisLength}
! 
! !INTERFACE: 
  subroutine computeAnalysisLength(nsize, tavgInterval)
! !ARGUMENTS:     
    integer             :: nsize
    integer, intent(in) :: tavgInterval
!
! !DESCRIPTION: 
!  This subroutine computes the length of the analysis time period
! 
!  The arguments are:
!  \begin{description}
!   \item[nsize] 
!     number of months computed by the routine
!   \item[tavgInterval] 
!     temporal averaging interval (expected to be a multiple of months)
!  \end{description}
!EOP    
    type(ESMF_Time)  :: startTime, stopTime
    type(ESMF_TimeInterval) :: timeStep
    integer          :: status

    call ESMF_TimeSet(startTime, yy = LVT_rc%syr, &
         mm = LVT_rc%smo, &
         dd = LVT_rc%sda, &
         h  = LVT_rc%shr, &
         m  = LVT_rc%smn, & 
         s  = LVT_rc%sss, &
         calendar = LVT_calendar, & 
         rc = status)
    call LVT_verify(status, 'ESMF_TimeSet failed')

    call ESMF_TimeSet(stopTime, yy = LVT_rc%eyr, &
         mm = LVT_rc%emo, &
         dd = LVT_rc%eda, &
         h  = LVT_rc%ehr, &
         m  = LVT_rc%emn, & 
         s  = LVT_rc%ess, &
         calendar = LVT_calendar, & 
         rc = status)
    call LVT_verify(status, 'ESMF_TimeSet failed')

    call ESMF_TimeIntervalSet(timeStep, s = tavgInterval, &
         rc=status)
    call LVT_verify(status,'ESMF_TimeIntervalSet failed')

    nsize = nint((stopTime - startTime)/timestep) + 1

  end subroutine computeAnalysisLength

!BOP
! 
! !ROUTINE: getDataTimeIndex
! \label{getDataTimeIndex}
! 
! !INTERFACE: 
  subroutine getDataTimeIndex(nsize, tavgInterval)
! !ARGUMENTS:     
    integer             :: nsize
    integer, intent(in) :: tavgInterval
!
! !DESCRIPTION: 
!  This subroutine computes the number of months from the start time
!  to the current time, based on the time interval
! 
!  The arguments are:
!  \begin{description}
!   \item[nsize] 
!     number of months computed by the routine
!   \item[tavgInterval] 
!     temporal averaging interval (expected to be a multiple of months)
!  \end{description}
!EOP    

    type(ESMF_Time)  :: currTime, startTime
    type(ESMF_TimeInterval) :: timeStep
    integer          :: status

    call ESMF_TimeSet(currTime, yy = LVT_rc%nyr, &
         mm = LVT_rc%nmo, &
         dd = LVT_rc%nda, &
         h  = LVT_rc%nhr, &
         m  = LVT_rc%nmn, & 
         s  = LVT_rc%nss, &
         calendar = LVT_calendar, & 
         rc = status)
    call LVT_verify(status, 'ESMF_TimeSet failed')

    call ESMF_TimeSet(startTime, yy = LVT_rc%syr, &
         mm = LVT_rc%smo, &
         dd = LVT_rc%sda, &
         h  = LVT_rc%shr, &
         m  = LVT_rc%smn, & 
         s  = LVT_rc%sss, &
         calendar = LVT_calendar, & 
         rc = status)
    call LVT_verify(status, 'ESMF_TimeSet failed')

    call ESMF_TimeIntervalSet(timeStep, s = tavgInterval, &
         rc=status)
    call LVT_verify(status, 'ESMF_TimeIntervalSet failed')

    nsize  = nint((currTime - startTime)/timeStep) + 1

  end subroutine getDataTimeIndex

  subroutine computeRnkArray(nsize,data_value, data_rnks)

    integer  :: nsize
    real     :: data_value(nsize)
    real     :: data_rnks(nsize)

    integer  :: i,j
    integer  :: tmprnk
    
    do i=1,nsize
       tmprnk = 0
       if(data_value(i).ne.LVT_rc%udef) then 
          do j=1,nsize
             if(data_value(j).ne.LVT_rc%udef) then 
                if(data_value(j).lt.data_value(i)) then 
                   tmprnk = tmprnk + 1
                endif
             endif
          enddo
          data_rnks(i) = tmprnk + 1
       else
          data_rnks(i)= LVT_rc%udef
       endif
    enddo
  end subroutine computeRnkArray
end module LVT_RnkCorrMod
