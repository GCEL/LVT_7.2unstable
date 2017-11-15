!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------------
! NASA GSFC Land surface Verification Toolkit (LVT) V1.0
!-------------------------END NOTICE -- DO NOT EDIT-----------------------------
module LVT_VarianceMod
!
!BOP
! 
! !ROUTINE:
!
! !INTERFACE:
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
! !MODULE: LVT_VarianceMod
! 
!  !DESCRIPTION: 
!   This module handles the computations required to compute mean values
!   of desired variables from the LIS output
!
!  !REVISION HISTORY: 
!  2 Oct 2008    Sujay Kumar  Initial Specification
!
  use LVT_coreMod
  use LVT_histDataMod
  use LVT_statsDataMod
  use LVT_historyMod
  use LVT_TSMod
  use LVT_logMod
  use LVT_CIMod
!-----------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!-----------------------------------------------------------------------------
  public :: LVT_initVariance
  public :: LVT_diagnoseVariance
  public :: LVT_computeVariance
  public :: LVT_writeMetric_Variance
  public :: LVT_resetMetric_Variance
  public :: LVT_writerestart_Variance
  public :: LVT_readrestart_Variance
!EOP
  
  private

contains
  subroutine LVT_initVariance(model, obs, stats,metric)
! !ARGUMENTS: 
    type(LVT_metaDataEntry) :: model
    type(LVT_metaDataEntry) :: obs
    type(LVT_statsEntry)    :: stats
    type(LVT_metricEntry)   :: metric
!
! !DESCRIPTION: 
! 
!EOP
    if(metric%selectOpt.eq.1) then 
       allocate(stats%variance%model_value_total(LVT_rc%ngrid, model%selectNlevs, &
            LVT_rc%strat_nlevels))
       allocate(stats%variance%model_value_total_sxsx(LVT_rc%ngrid, model%selectNlevs, &
            LVT_rc%strat_nlevels))
       stats%variance%model_value_total = 0.0
       stats%variance%model_value_total_sxsx = 0.0
       allocate(stats%variance%count_model_value_total(LVT_rc%ngrid, model%selectNlevs, &
            LVT_rc%strat_nlevels))
       stats%variance%count_model_value_total = 0
       allocate(stats%variance%model_value_ci(model%selectNlevs,LVT_rc%strat_nlevels))
       stats%variance%model_value_ci = LVT_rc%udef
       
       if(metric%computeSC.eq.1) then 
          allocate(stats%variance%model_value_asc(LVT_rc%ngrid, model%selectNlevs,&
               LVT_rc%nasc))
          stats%variance%model_value_asc = 0.0
          allocate(stats%variance%model_value_asc_sxsx(LVT_rc%ngrid, model%selectNlevs,&
               LVT_rc%nasc))
          stats%variance%model_value_asc_sxsx = 0.0
          allocate(stats%variance%count_model_value_asc(LVT_rc%ngrid, model%selectNlevs,&
               LVT_rc%nasc))
          stats%variance%count_model_value_asc = 0
       endif
       if(metric%computeADC.eq.1) then 
          allocate(stats%variance%model_value_adc(LVT_rc%ngrid, obs%selectNlevs,&
               LVT_rc%nadc))
          stats%variance%model_value_adc = 0.0
          allocate(stats%variance%model_value_adc_sxsx(LVT_rc%ngrid, obs%selectNlevs,&
               LVT_rc%nadc))
          stats%variance%model_value_adc_sxsx = 0.0
          allocate(stats%variance%count_model_value_adc(LVT_rc%ngrid, obs%selectNlevs,&
               LVT_rc%nadc))
          stats%variance%count_model_value_adc = 0
       endif
       
       if(LVT_rc%obssource(2).ne."none") then 
          allocate(stats%variance%obs_value_total(LVT_rc%ngrid, obs%selectNlevs, &
               LVT_rc%strat_nlevels))
          stats%variance%obs_value_total = 0.0
          allocate(stats%variance%obs_value_total_sxsx(LVT_rc%ngrid, obs%selectNlevs, &
               LVT_rc%strat_nlevels))
          stats%variance%obs_value_total_sxsx = 0.0
          allocate(stats%variance%count_obs_value_total(LVT_rc%ngrid, obs%selectNlevs, &
               LVT_rc%strat_nlevels))
          stats%variance%count_obs_value_total = 0
          allocate(stats%variance%obs_value_ci(obs%selectNlevs,LVT_rc%strat_nlevels))
          stats%variance%obs_value_ci = LVT_rc%udef
          
          if(metric%computeSC.eq.1) then 
             allocate(stats%variance%obs_value_asc(LVT_rc%ngrid, obs%selectNlevs,&
                  LVT_rc%nasc))
             stats%variance%obs_value_asc = 0.0
             allocate(stats%variance%obs_value_asc_sxsx(LVT_rc%ngrid, obs%selectNlevs,&
                  LVT_rc%nasc))
             stats%variance%obs_value_asc_sxsx = 0.0
             allocate(stats%variance%count_obs_value_asc(LVT_rc%ngrid, obs%selectNlevs,&
                  LVT_rc%nasc))
             stats%variance%count_obs_value_asc = 0
          endif
          if(metric%computeADC.eq.1) then 
             allocate(stats%variance%obs_value_adc(LVT_rc%ngrid, obs%selectNlevs,&
                  LVT_rc%nadc))
             stats%variance%obs_value_adc = 0.0
             allocate(stats%variance%obs_value_adc_sxsx(LVT_rc%ngrid, obs%selectNlevs,&
                  LVT_rc%nadc))
             stats%variance%obs_value_adc_sxsx = 0.0
             allocate(stats%variance%count_obs_value_adc(LVT_rc%ngrid, obs%selectNlevs,&
                  LVT_rc%nadc))
             stats%variance%count_obs_value_adc = 0
          endif
       endif
    endif
    if(metric%timeOpt.eq.1) then 
       allocate(stats%variance%model_value_ts(LVT_rc%ngrid, model%selectNlevs, &
            LVT_rc%strat_nlevels))
       stats%variance%model_value_ts = 0.0
       allocate(stats%variance%model_value_ts_sxsx(LVT_rc%ngrid, model%selectNlevs, &
            LVT_rc%strat_nlevels))
       stats%variance%model_value_ts_sxsx = 0.0
       allocate(stats%variance%count_model_value_ts(LVT_rc%ngrid, model%selectNlevs, &
            LVT_rc%strat_nlevels))
       stats%variance%count_model_value_ts = 0
       
       allocate(stats%variance%tavg_model_value_ts(LVT_rc%ngrid, model%selectNlevs, &
            LVT_rc%strat_nlevels))
       stats%variance%tavg_model_value_ts = 0.0
       allocate(stats%variance%tavg_count_model_value_ts(LVT_rc%ngrid, model%selectNlevs, &
            LVT_rc%strat_nlevels))
       stats%variance%tavg_count_model_value_ts = 0

       if(LVT_rc%obssource(2).ne."none") then 
          allocate(stats%variance%obs_value_ts(LVT_rc%ngrid, model%selectNlevs, &
               LVT_rc%strat_nlevels))
          stats%variance%obs_value_ts = 0.0
          allocate(stats%variance%obs_value_ts_sxsx(LVT_rc%ngrid, model%selectNlevs, &
               LVT_rc%strat_nlevels))
          stats%variance%obs_value_ts_sxsx = 0.0
          allocate(stats%variance%count_obs_value_ts(LVT_rc%ngrid, model%selectNlevs, &
               LVT_rc%strat_nlevels))
          stats%variance%count_obs_value_ts = 0

          allocate(stats%variance%tavg_obs_value_ts(LVT_rc%ngrid, model%selectNlevs, &
               LVT_rc%strat_nlevels))
          stats%variance%tavg_obs_value_ts = 0.0
          allocate(stats%variance%tavg_count_obs_value_ts(LVT_rc%ngrid, model%selectNlevs, &
               LVT_rc%strat_nlevels))
          stats%variance%tavg_count_obs_value_ts = 0
       endif
    endif

!-------------------------------------------------------------------------
! Number of passes required to compute the metric
!-------------------------------------------------------------------------

    metric%npass = 1    
    if(LVT_rc%obssource(2).ne."none") then 
       metric%obsData = .true. 
    else
       metric%obsData = .false. 
    endif
    metric%StdevFlag = .false. 
  end subroutine LVT_initVariance

!BOP
! 
! !ROUTINE:
!
! !INTERFACE:
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
! !ROUTINE: LVT_diagnoseVariance
! \label{LVT_diagnoseVariance}
! 
! !INTERFACE: 
  subroutine LVT_diagnoseVariance(pass)
! !USES:     

    implicit none

    integer       :: pass
! 
! !DESCRIPTION: 
!   This subroutine issues the calls to update the computations for 
!   calculating the std of desired variables.
!
!   The methods invoked are: 
!   \begin{description}
!    \item[diagnoseSingleModelVariance](\ref{diagnoseSingleModelVariance})
!     updates the std computation for a single variable 
!   \end{description}
! 
!EOP

    type(LVT_metadataEntry), pointer :: model
    type(LVT_metadataEntry), pointer :: obs
    type(LVT_statsEntry)   , pointer :: stats

    if(pass.eq.1) then 
       if(LVT_metrics%variance%selectOpt.eq.1.or.&
            LVT_metrics%variance%timeOpt.eq.1) then 

          call LVT_getDataStream1Ptr(model)
          call LVT_getDataStream2Ptr(obs)
          call LVT_getstatsEntryPtr(stats)
          
          do while(associated(model))

             call diagnoseSingleVariance(model,obs,stats,&
                  LVT_metrics%variance)
             
             model => model%next
             obs => obs%next
             stats => stats%next
          enddo
       endif
    endif
  end subroutine LVT_diagnoseVariance

!BOP
! 
! !ROUTINE:
!
! !INTERFACE:
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
! !ROUTINE: diagnoseSingleVariance
! \label{diagnoseSingleVariance}
! 
! !INTERFACE: 
  subroutine diagnoseSingleVariance(model, obs, stats,metric)
! !USES: 

    implicit none
! !ARGUMENTS: 
    type(LVT_metaDataEntry) :: model
    type(LVT_metadataEntry) :: obs
    type(LVT_statsEntry)    :: stats
    type(LVT_metricEntry)   :: metric
!
! !DESCRIPTION: 
!   This routine updates the std computation of the 
!   specified variable. 
!
!  The arguments are: 
!
!  \begin{description}
!   \item[model] model variable object
!   \item[stats] object to hold the updated statistics
!  \end{description}
!EOP
    integer    :: t,k,m_k,o_k,tind

    if(stats%selectOpt.eq.1.and.&
         model%selectNlevs.ge.1) then        
       do t=1,LVT_rc%ngrid
          do k=1,model%selectNlevs
             m_k = k+model%startNlevs -1
             o_k = k+obs%startNlevs -1
             if(model%count(t,m_k).gt.0) then
                if(metric%selectOpt.eq.1) then 
                   if(model%value(t,m_k).ne.LVT_rc%udef) then 
                      stats%variance%model_value_total(t,k,1) = &
                           stats%variance%model_value_total(t,k,1) + &
                           model%value(t,m_k)
                      stats%variance%model_value_total_sxsx(t,k,1) = &
                           stats%variance%model_value_total_sxsx(t,k,1) + &
                           model%value(t,m_k)*model%value(t,k)
                      stats%variance%count_model_value_total(t,k,1) = &
                           stats%variance%count_model_value_total(t,k,1) + 1
                   endif
                endif
                
                if(metric%timeOpt.eq.1) then 
                   if(model%value(t,m_k).ne.LVT_rc%udef) then 
                      stats%variance%model_value_ts(t,k,1) = &
                           stats%variance%model_value_ts(t,k,1)+&
                           model%value(t,m_k)
                      stats%variance%model_value_ts_sxsx(t,k,1) = &
                           stats%variance%model_value_ts_sxsx(t,k,1)+&
                           model%value(t,m_k)*model%value(t,k)
                      stats%variance%count_model_value_ts(t,k,1) = & 
                           stats%variance%count_model_value_ts(t,k,1)+1
                   endif
                endif
                if(metric%computeSC.eq.1) then 
                   if(model%value(t,m_k).ne.LVT_rc%udef) then 
                      call LVT_getSeasonalCycleTimeIndex(LVT_rc%scInterval,&
                           tind)
                      stats%variance%model_value_asc(t,k,tind) = &
                           stats%variance%model_value_asc(t,k,tind)+&
                           model%value(t,m_k)
                      stats%variance%model_value_asc_sxsx(t,k,tind) = &
                           stats%variance%model_value_asc_sxsx(t,k,tind)+&
                           model%value(t,m_k)*model%value(t,k)
                      stats%variance%count_model_value_asc(t,k,tind) = &
                           stats%variance%count_model_value_asc(t,k,tind) + 1
                   endif
                endif
                if(metric%computeADC.eq.1) then 
                   if(model%value(t,m_k).ne.LVT_rc%udef) then 
                      call LVT_getADCTimeIndex(tind)
                      stats%variance%model_value_adc(t,k,tind) = &
                           stats%variance%model_value_adc(t,k,tind)+&
                           model%value(t,m_k)
                      stats%variance%model_value_adc_sxsx(t,k,tind) = &
                           stats%variance%model_value_adc_sxsx(t,k,tind)+&
                           model%value(t,m_k)*model%value(t,k)
                      stats%variance%count_model_value_adc(t,k,tind) = &
                           stats%variance%count_model_value_adc(t,k,tind) + 1
                   endif
                endif
                if(LVT_rc%strat_nlevels.gt.1) then 
                   if(LVT_stats%strat_var(t,k).gt.&
                        LVT_rc%strat_var_threshold) then
                      if(metric%selectOpt.eq.1) then
                         if(model%value(t,m_k).ne.LVT_rc%udef) then  
                            stats%variance%model_value_total(t,k,2) = &
                                 stats%variance%model_value_total(t,k,2) + &
                                 model%value(t,m_k)
                            stats%variance%model_value_total_sxsx(t,k,2) = &
                                 stats%variance%model_value_total_sxsx(t,k,2) + &
                                 model%value(t,m_k)*model%value(t,k)
                            stats%variance%count_model_value_total(t,k,2) = &
                                 stats%variance%count_model_value_total(t,k,2) + 1
                         endif
                      endif
                      if(metric%timeOpt.eq.1) then 
                         if(model%value(t,m_k).ne.LVT_rc%udef) then 
                            stats%variance%model_value_ts(t,k,2) = &
                                 stats%variance%model_value_ts(t,k,2)+&
                                 model%value(t,m_k)
                            stats%variance%model_value_ts_sxsx(t,k,2) = &
                                 stats%variance%model_value_ts_sxsx(t,k,2)+&
                                 model%value(t,m_k)*model%value(t,k)
                            stats%variance%count_model_value_ts(t,k,2) = & 
                                 stats%variance%count_model_value_ts(t,k,2)+1
                         endif
                      endif
                   elseif(LVT_stats%strat_var(t,k).le.&
                        LVT_rc%strat_var_threshold) then
                      if(metric%selectOpt.eq.1) then 
                         if(model%value(t,m_k).ne.LVT_rc%udef) then 
                            stats%variance%model_value_total(t,k,3) = &
                                 stats%variance%model_value_total(t,k,3) + &
                                 model%value(t,m_k)
                            stats%variance%model_value_total_sxsx(t,k,3) = &
                                 stats%variance%model_value_total_sxsx(t,k,3) + &
                                 model%value(t,m_k)*model%value(t,k)
                            stats%variance%count_model_value_total(t,k,3) = &
                                 stats%variance%count_model_value_total(t,k,3) + 1
                         endif
                      endif
                      if(metric%timeOpt.eq.1) then 
                         if(model%value(t,m_k).ne.LVT_rc%udef) then 
                            stats%variance%model_value_ts(t,k,3) = &
                                 stats%variance%model_value_ts(t,k,3)+&
                                 model%value(t,m_k)
                            stats%variance%model_value_ts_sxsx(t,k,3) = &
                                 stats%variance%model_value_ts_sxsx(t,k,3)+&
                                 model%value(t,m_k)*model%value(t,k)
                            stats%variance%count_model_value_ts(t,k,3) = & 
                                 stats%variance%count_model_value_ts(t,k,3)+1
                         endif
                      endif
                   endif
                endif
             endif
          end do
          do k=1,obs%selectNlevs
             if(LVT_rc%obssource(2).ne."none") then 
                if(obs%selectNlevs.ge.1) then 
                   if(obs%count(t,o_k).gt.0) then 
                      if(metric%selectOpt.eq.1.and.&
                           obs%value(t,o_k).ne.LVT_rc%udef) then 
                         stats%variance%obs_value_total(t,k,1) = &
                              stats%variance%obs_value_total(t,k,1) + &
                              obs%value(t,o_k)
                         stats%variance%obs_value_total_sxsx(t,k,1) = &
                              stats%variance%obs_value_total_sxsx(t,k,1) + &
                              obs%value(t,o_k)*obs%value(t,o_k)
                         stats%variance%count_obs_value_total(t,k,1) = &
                              stats%variance%count_obs_value_total(t,k,1) + 1
                      endif
                      
                      if(metric%timeOpt.eq.1.and.&
                           obs%value(t,o_k).ne.LVT_rc%udef) then 
                         stats%variance%obs_value_ts(t,k,1) = &
                              stats%variance%obs_value_ts(t,k,1)+&
                              obs%value(t,o_k)
                         stats%variance%obs_value_ts_sxsx(t,k,1) = &
                              stats%variance%obs_value_ts_sxsx(t,k,1)+&
                              obs%value(t,o_k)*obs%value(t,o_k)
                         stats%variance%count_obs_value_ts(t,k,1) = & 
                              stats%variance%count_obs_value_ts(t,k,1)+1
                      endif
                      if(metric%computeSC.eq.1.and.&
                           obs%value(t,o_k).ne.LVT_rc%udef) then 
                         call LVT_getSeasonalCycleTimeIndex(LVT_rc%scInterval,tind)
                         stats%variance%obs_value_asc(t,k,tind) = &
                              stats%variance%obs_value_asc(t,k,tind)+&
                              obs%value(t,o_k)
                         stats%variance%obs_value_asc_sxsx(t,k,tind) = &
                              stats%variance%obs_value_asc_sxsx(t,k,tind)+&
                              obs%value(t,o_k)*obs%value(t,o_k)
                         stats%variance%count_obs_value_asc(t,k,tind) = &
                              stats%variance%count_obs_value_asc(t,k,tind) + 1
                      endif
                      if(metric%computeADC.eq.1.and.&
                           obs%value(t,o_k).ne.LVT_rc%udef) then 
                         call LVT_getADCTimeIndex(tind)
                         stats%variance%obs_value_adc(t,k,tind) = &
                              stats%variance%obs_value_adc(t,k,tind)+&
                              obs%value(t,o_k)
                         stats%variance%obs_value_adc_sxsx(t,k,tind) = &
                              stats%variance%obs_value_adc_sxsx(t,k,tind)+&
                              obs%value(t,o_k)*obs%value(t,o_k)
                         stats%variance%count_obs_value_adc(t,k,tind) = &
                              stats%variance%count_obs_value_adc(t,k,tind) + 1
                      endif
                      if(LVT_rc%strat_nlevels.gt.1) then 
                         if(LVT_stats%strat_var(t,k).gt.&
                              LVT_rc%strat_var_threshold) then
                            if(metric%selectOpt.eq.1 &
                                 .and.obs%value(t,o_k).ne.LVT_rc%udef) then 
                               stats%variance%obs_value_total(t,k,2) = &
                                    stats%variance%obs_value_total(t,k,2) + &
                                    obs%value(t,o_k)
                               stats%variance%obs_value_total_sxsx(t,k,2) = &
                                    stats%variance%obs_value_total_sxsx(t,k,2) + &
                                    obs%value(t,o_k)*obs%value(t,o_k)
                               stats%variance%count_obs_value_total(t,k,2) = &
                                    stats%variance%count_obs_value_total(t,k,2) + 1
                            endif
                            
                            if(metric%timeOpt.eq.1.and.&
                                 obs%value(t,o_k).ne.LVT_rc%udef) then 
                               stats%variance%obs_value_ts(t,k,2) = &
                                    stats%variance%obs_value_ts(t,k,2)+&
                                    obs%value(t,o_k)
                               stats%variance%obs_value_ts_sxsx(t,k,2) = &
                                    stats%variance%obs_value_ts_sxsx(t,k,2)+&
                                    obs%value(t,o_k)*obs%value(t,o_k)
                               stats%variance%count_obs_value_ts(t,k,2) = & 
                                    stats%variance%count_obs_value_ts(t,k,2)+1
                            endif
                         elseif(LVT_stats%strat_var(t,k).le.&
                              LVT_rc%strat_var_threshold) then
                            if(metric%selectOpt.eq.1.and.&
                                 obs%value(t,o_k).ne.LVT_rc%udef) then 
                               stats%variance%obs_value_total(t,k,3) = &
                                    stats%variance%obs_value_total(t,k,3) + &
                                    obs%value(t,o_k)
                               stats%variance%obs_value_total_sxsx(t,k,3) = &
                                    stats%variance%obs_value_total_sxsx(t,k,3) + &
                                    obs%value(t,o_k)*obs%value(t,o_k)
                               stats%variance%count_obs_value_total(t,k,3) = &
                                    stats%variance%count_obs_value_total(t,k,3) + 1
                            endif

                            if(metric%timeOpt.eq.1.and.&
                                 obs%value(t,o_k).ne.LVT_rc%udef) then 
                               stats%variance%obs_value_ts(t,k,3) = &
                                    stats%variance%obs_value_ts(t,k,3)+&
                                    obs%value(t,o_k)
                               stats%variance%obs_value_ts_sxsx(t,k,3) = &
                                    stats%variance%obs_value_ts_sxsx(t,k,3)+&
                                    obs%value(t,o_k)*obs%value(t,o_k)
                               stats%variance%count_obs_value_ts(t,k,3) = & 
                                    stats%variance%count_obs_value_ts(t,k,3)+1
                            endif

                         endif
                      endif
                   endif
                endif
             endif
          enddo
       enddo
    endif
  end subroutine diagnoseSingleVariance

!BOP
! 
! !ROUTINE:
!
! !INTERFACE:
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
! !ROUTINE: LVT_computeVariance
! \label{LVT_computeVariance}
! 
! !INTERFACE: 
  subroutine LVT_computeVariance(pass,alarm)
! !USES: 

    implicit none

    integer               :: pass
    logical               :: alarm

! 
! !DESCRIPTION: 
!   This subroutine issues the calls to compute the std values for 
!   desired variables.
!
!   The methods invoked are: 
!   \begin{description}
!    \item[computeSingleModelVariance](\ref{computeSingleModelVariance})
!     computes the std values for a single variable
!   \end{description}
! 
!EOP
    integer     :: i 
    type(LVT_metadataEntry), pointer :: model
    type(LVT_metadataEntry), pointer :: obs
    type(LVT_statsEntry)   , pointer :: stats

    if(pass.eq.1) then 
       if(LVT_metrics%variance%selectOpt.eq.1.or.LVT_metrics%variance%timeOpt.eq.1) then 
          if(alarm) then 
             if(LVT_metrics%variance%timeOpt.eq.1.and.&
                  LVT_metrics%variance%extractTS.eq.1) then 
                do i=1,LVT_rc%ntslocs
                   write(LVT_metrics%variance%ftn_ts_loc(i),200,advance='no') &
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
             call computeSingleVariance(alarm,model,obs,stats,&
                  LVT_metrics%variance)

             model => model%next
             obs => obs%next
             stats => stats%next

          enddo
          
          if(alarm) then 
             if(LVT_metrics%variance%timeOpt.eq.1.and.&
                  LVT_metrics%variance%extractTS.eq.1) then 
                do i=1,LVT_rc%ntslocs
                   write(LVT_metrics%variance%ftn_ts_loc(i),fmt='(a1)') ''
                enddo
             endif
          endif
       endif
    endif
  end subroutine LVT_computeVariance
  

!BOP
! 
! !ROUTINE:
!
! !INTERFACE:
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
! !ROUTINE: computeSingleVariance
! \label{computeSingleVariance}
! 
! !INTERFACE: 
  subroutine computeSingleVariance(alarm,model,obs,stats,metric)
! !USES: 

    implicit none
! !ARGUMENTS: 
    logical                 :: alarm
    type(LVT_metaDataEntry) :: model
    type(LVT_metaDataEntry) :: obs
    type(LVT_statsEntry)    :: stats
    type(LVT_metricEntry)   :: metric
! 
! !DESCRIPTION: 
! 
! !DESCRIPTION: 
!  This routine computes the std values
!  The arguments are: 
!
!  \begin{description}
!    \item[model] model variable object
!    \item[stats] object to hold the updated statistics
!  \end{description}
!EOP

    integer  :: t,l,k
    real     :: term1, term2

    if(metric%timeOpt.eq.1) then 
       if(stats%selectOpt.eq.1.and.&
            model%selectNlevs.ge.1) then 
          do t=1,LVT_rc%ngrid
             do k=1,model%selectNlevs
                do l=1,LVT_rc%strat_nlevels
                   if(stats%variance%count_model_value_ts(t,k,l).gt.0) then 
                      term1 = (stats%variance%model_value_ts_sxsx(t,k,l)/&
                           stats%variance%count_model_value_ts(t,k,l))
                      term2 =(stats%variance%model_value_ts(t,k,l)/&
                           stats%variance%count_model_value_ts(t,k,l))**2 

                      if(term1.gt.term2) then 
                         stats%variance%model_value_ts(t,k,l) = &
                              (term1 -term2)
                         
                         stats%variance%tavg_model_value_ts(t,k,l) = & 
                              stats%variance%tavg_model_value_ts(t,k,l) + & 
                              stats%variance%model_value_ts(t,k,l)
                         stats%variance%tavg_count_model_value_ts(t,k,l) = & 
                              stats%variance%tavg_count_model_value_ts(t,k,l) + 1
                      else
                         stats%variance%model_value_ts(t,k,l) = 0.0
                      endif

                   else
                      stats%variance%model_value_ts(t,k,l) = LVT_rc%udef
                   endif

                   if(LVT_rc%obssource(2).ne."none") then 
                      if(stats%variance%count_obs_value_ts(t,k,l).gt.0) then 
                         term1 = (stats%variance%obs_value_ts_sxsx(t,k,l)/&
                              stats%variance%count_obs_value_ts(t,k,l))
                         term2 = (stats%variance%obs_value_ts(t,k,l)/&
                              stats%variance%count_obs_value_ts(t,k,l))**2
                         if(term1.gt.term2) then 
                            stats%variance%obs_value_ts(t,k,l) = & 
                                 (term1 - term2)
                            stats%variance%tavg_obs_value_ts(t,k,l) = & 
                                 stats%variance%tavg_obs_value_ts(t,k,l) + & 
                                 stats%variance%obs_value_ts(t,k,l)
                            stats%variance%tavg_count_obs_value_ts(t,k,l) = & 
                                 stats%variance%tavg_count_obs_value_ts(t,k,l) + 1
                         else
                            stats%variance%obs_value_ts(t,k,l) = 0.0
                         endif
                      else
                         stats%variance%obs_value_ts(t,k,l) = LVT_rc%udef
                      endif
                   endif
                enddo
             enddo
          enddo
          if(alarm) then 

             do t=1,LVT_rc%ngrid
                do k=1,model%selectNlevs
                   do l=1,LVT_rc%strat_nlevels
                      if(stats%variance%tavg_count_model_value_ts(t,k,l).gt.0) then 
                         stats%variance%tavg_model_value_ts(t,k,l) = & 
                              stats%variance%tavg_model_value_ts(t,k,l) / & 
                              stats%variance%tavg_count_model_value_ts(t,k,l) 
                      else
                         stats%variance%tavg_model_value_ts(t,k,l) = LVT_rc%udef
                      endif
                   enddo
                enddo
             enddo

             if(metric%extractTS.eq.1) then 
                if(LVT_rc%obssource(2).ne."none") then 
                   call LVT_writeTSinfo(metric%ftn_ts_loc,&
                        model,&
                        LVT_rc%ngrid,&
                        stats%variance%tavg_model_value_ts,&
                        stats%variance%tavg_count_model_value_ts,&
                        LVT_rc%ngrid,&
                        stats%variance%tavg_obs_value_ts,&
                        stats%variance%tavg_count_obs_value_ts)
                   
                else
                   call LVT_writeTSinfo(metric%ftn_ts_loc,&
                        model,&
                        LVT_rc%ngrid,&
                        stats%variance%tavg_model_value_ts,&
                        stats%variance%tavg_count_model_value_ts)
                endif
             endif
          endif
       endif
    endif

       
    if(LVT_rc%endtime.eq.1.and.metric%selectOpt.eq.1) then 
       if(stats%selectOpt.eq.1.and.&
            model%selectNlevs.ge.1) then 
          do t=1,LVT_rc%ngrid
             do k=1,model%selectNlevs
                do l=1,LVT_rc%strat_nlevels
                   if(stats%variance%count_model_value_total(t,k,l).gt.&
                        LVT_rc%obsCountThreshold) then 
                      term1 = (stats%variance%model_value_total_sxsx(t,k,l)/&
                              stats%variance%count_model_value_total(t,k,l)) - & 
                              (stats%variance%model_value_total(t,k,l)/&
                              stats%variance%count_model_value_total(t,k,l))**2
                      if(term1.gt.0) then 
                         stats%variance%model_value_total(t,k,l) = &
                              (term1)
                      else
                         stats%variance%model_value_total(t,k,l) = LVT_rc%udef
                      endif
                   else
                      stats%variance%model_value_total(t,k,l) = LVT_rc%udef
                   endif
                enddo
                if(metric%computeSC.eq.1) then 
                   do l=1,LVT_rc%nasc
                      if(stats%variance%count_model_value_asc(t,k,l).gt.&
                           LVT_rc%SCCountThreshold) then 
                         stats%variance%model_value_asc(t,k,l) = &
                              ((stats%variance%model_value_asc_sxsx(t,k,l)/&
                              stats%variance%count_model_value_asc(t,k,l)) - & 
                              (stats%variance%model_value_asc(t,k,l)/&
                              stats%variance%count_model_value_asc(t,k,l))**2)
                      else
                         stats%variance%model_value_asc(t,k,l) = LVT_rc%udef
                      endif
                   enddo
                endif
                if(metric%computeADC.eq.1) then 
                   do l=1,LVT_rc%nadc
                      if(stats%variance%count_model_value_adc(t,k,l).gt.&
                           LVT_rc%ADCCountThreshold) then 
                         stats%variance%model_value_adc(t,k,l) = &
                              ((stats%variance%model_value_adc_sxsx(t,k,l)/&
                              stats%variance%count_model_value_adc(t,k,l)) - & 
                              (stats%variance%model_value_adc(t,k,l)/&
                              stats%variance%count_model_value_adc(t,k,l))**2)
                      else
                         stats%variance%model_value_adc(t,k,l) = LVT_rc%udef
                      endif
                   enddo
                endif
                   
                if(LVT_rc%obssource(2).ne."none") then 
                   do l=1,LVT_rc%strat_nlevels
                      if(stats%variance%count_obs_value_total(t,k,l).gt.&
                           LVT_rc%obsCountThreshold) then 
                         term1 = (stats%variance%obs_value_total_sxsx(t,k,l)/&
                              stats%variance%count_obs_value_total(t,k,l)) - & 
                              (stats%variance%obs_value_total(t,k,l)/&
                              stats%variance%count_obs_value_total(t,k,l))**2
                         if(term1.gt.0) then 
                            stats%variance%obs_value_total(t,k,l) = &
                                 (term1)
                         else
                            stats%variance%obs_value_total(t,k,l)= LVT_rc%udef
                         endif
                      else
                         stats%variance%obs_value_total(t,k,l) = LVT_rc%udef
                      endif
                   enddo
                   
                   if(metric%computeSC.eq.1) then
                      do l=1,LVT_rc%nasc
                         if(stats%variance%count_obs_value_asc(t,k,l).gt.&
                              LVT_rc%SCCountThreshold) then 

                            term1 = stats%variance%obs_value_asc_sxsx(t,k,l)/&
                                 stats%variance%count_obs_value_asc(t,k,l)
                            term2 = (stats%variance%obs_value_asc(t,k,l)/&
                                 stats%variance%count_obs_value_asc(t,k,l))**2
                            if(term1.gt.term2) then 
                               stats%variance%obs_value_asc(t,k,l) = &
                                    (term1-term2)
                            elseif(abs(term1-term2).lt.0.00001) then 
                               stats%variance%obs_value_asc(t,k,l) = 0.00
                            else
                               print*, 'Error in Variance calculation..'
                               stop
                            endif
                         else
                            stats%variance%obs_value_asc(t,k,l) = LVT_rc%udef
                         endif
                      enddo
                   endif

                   if(metric%computeADC.eq.1) then 
                      do l=1,LVT_rc%nadc
                         if(stats%variance%count_obs_value_adc(t,k,l).gt.&
                              LVT_rc%ADCCountThreshold) then 
                            stats%variance%obs_value_adc(t,k,l) = &
                                 ((stats%variance%obs_value_adc_sxsx(t,k,l)/&
                                 stats%variance%count_obs_value_adc(t,k,l)) - & 
                                 (stats%variance%obs_value_adc(t,k,l)/&
                                 stats%variance%count_obs_value_adc(t,k,l))**2)
                         else
                            stats%variance%obs_value_adc(t,k,l) = LVT_rc%udef
                         endif
                      enddo                      
                   endif
                endif
             enddo
          enddo
             
          do k=1,model%selectNlevs
             do l=1, LVT_rc%strat_nlevels
                call LVT_computeCI(stats%variance%model_value_total(:,k,l),LVT_rc%ngrid,&
                     LVT_rc%pval_CI,stats%variance%model_value_ci(k,l))
             enddo
          enddo
          if(LVT_rc%obssource(2).ne."none") then 
             do k=1,obs%selectNlevs
                do l=1, LVT_rc%strat_nlevels
                   call LVT_computeCI(stats%variance%obs_value_total(:,k,l),LVT_rc%ngrid,&
                        LVT_rc%pval_CI,stats%variance%obs_value_ci(k,l))
                enddo
             enddo
          endif
          if(LVT_rc%obssource(2).ne."none") then 
             call LVT_writeDataBasedStrat(model,obs,stats,metric,&
                  LVT_rc%ngrid,stats%variance%model_value_total,LVT_rc%ngrid,stats%variance%obs_value_total)

             if(metric%computeSC.eq.1) then 
                call LVT_writeSeasonalCycleInfo(model,obs,stats,metric,&
                     LVT_rc%ngrid,stats%variance%model_value_asc,stats%variance%count_model_value_asc,&
                     LVT_rc%ngrid,stats%variance%obs_value_asc,stats%variance%count_obs_value_asc)          
             endif
             if(metric%computeADC.eq.1) then 
                call LVT_writeAvgDiurnalCycleInfo(model,obs,stats,metric,&
                     LVT_rc%ngrid,stats%variance%model_value_adc,stats%variance%count_model_value_adc,&
                     LVT_rc%ngrid,stats%variance%obs_value_adc,stats%variance%count_obs_value_adc)     
             endif
          else
             call LVT_writeDataBasedStrat(model,obs,stats,metric,&
                  LVT_rc%ngrid,stats%variance%model_value_total)
             
             if(metric%computeSC.eq.1) then 
                call LVT_writeSeasonalCycleInfo(model,obs,stats,metric,&
                     LVT_rc%ngrid,stats%variance%model_value_asc,stats%variance%count_model_value_asc)
             endif
             if(metric%computeADC.eq.1) then 
                call LVT_writeAvgDiurnalCycleInfo(model,obs,stats,metric,&
                     LVT_rc%ngrid,stats%variance%model_value_adc,stats%variance%count_model_value_adc)
             endif
          endif
       endif
    endif
  end subroutine computeSingleVariance


  subroutine LVT_writeMetric_Variance(pass,final,vlevels,stats,obs)
! !USES:
    use LVT_statsMod, only : LVT_writeSummaryStats
    use LVT_pluginIndices
! !ARGUMENTS: 

    implicit none

    integer                 :: pass
    integer                 :: final
    integer                 :: vlevels
    type(LVT_statsEntry)   :: stats
    type(LVT_metaDataEntry) :: obs

    integer                 :: k,l,tind
    
    if(pass.eq.LVT_metrics%variance%npass) then 
       if(final.ne.1) then
           if(stats%selectOpt.eq.1) then 
             do k=1,vlevels
                if(LVT_metrics%variance%timeOpt.eq.1) then 
                   do l=1,LVT_rc%strat_nlevels
                      call LVT_writevar_gridded(LVT_metrics%variance%ftn_ts, &
                           stats%variance%model_value_ts(:,k,l),&
                           stats%vid_ts(LVT_Varianceid,1),k)
                      call LVT_writevar_gridded(LVT_metrics%variance%ftn_ts, &
                           real(stats%variance%count_model_value_ts(:,k,l)),&
                           stats%vid_count_ts(LVT_Varianceid,1),k)
                   enddo
                   if(LVT_rc%obssource(2).ne."none") then 
                      do l=1,LVT_rc%strat_nlevels
                         call LVT_writevar_gridded(LVT_metrics%variance%ftn_ts, &
                              stats%variance%obs_value_ts(:,k,l),&
                              stats%vid_ts(LVT_Varianceid,2),k)
                         call LVT_writevar_gridded(LVT_metrics%variance%ftn_ts, &
                              real(stats%variance%count_obs_value_ts(:,k,l)),&
                              stats%vid_count_ts(LVT_Varianceid,2),k)
                      enddo
                   endif
                endif
             enddo
          endif
       else
          if(pass.eq.1) then 
             if(stats%selectOpt.eq.1) then 
                do k=1,vlevels
                   if(LVT_metrics%variance%selectOpt.eq.1) then 
                      do l=1,LVT_rc%strat_nlevels                      
                         call LVT_writevar_gridded(LVT_metrics%variance%ftn_total, &
                              stats%variance%model_value_total(:,k,l),&
                              stats%vid_total(LVT_Varianceid,1),k)
                         call LVT_writevar_gridded(LVT_metrics%variance%ftn_total, &
                              real(stats%variance%count_model_value_total(:,k,l)),&
                              stats%vid_count_total(LVT_Varianceid,1),k)
                         
                      enddo
                      if(LVT_rc%obssource(2).ne."none") then 
                         do l=1,LVT_rc%strat_nlevels                      
                            call LVT_writevar_gridded(&
                                 LVT_metrics%variance%ftn_total, &
                                 stats%variance%obs_value_total(:,k,l),&
                                 stats%vid_total(LVT_Varianceid,2),k)
                            call LVT_writevar_gridded(&
                                 LVT_metrics%variance%ftn_total, &
                                 real(stats%variance%count_obs_value_total(:,k,l)),&
                                 stats%vid_count_total(LVT_Varianceid,2),k)
                         enddo
                      endif

                      if(LVT_metrics%variance%computeSC.eq.1) then 
                         do tind = 1,LVT_rc%nasc
                            call LVT_writevar_gridded(&
                                 LVT_metrics%variance%ftn_total,&
                                 stats%variance%model_value_asc(:,k,tind),&
                                 stats%vid_sc_total(tind,LVT_Varianceid,1),k)
                         enddo
                         if(LVT_rc%obssource(2).ne."none") then 
                            do tind = 1,LVT_rc%nasc
                               call LVT_writevar_gridded(&
                                    LVT_metrics%variance%ftn_total,&
                                    stats%variance%obs_value_asc(:,k,tind),&
                                    stats%vid_sc_total(tind,LVT_Varianceid,2),k)
                            enddo
                         endif
                      endif
                      
                      if(LVT_metrics%variance%computeADC.eq.1) then 
                         do tind = 1,LVT_rc%nadc
                            call LVT_writevar_gridded(&
                                 LVT_metrics%variance%ftn_total,&
                                 stats%variance%model_value_adc(:,k,tind),&
                                 stats%vid_adc_total(tind,LVT_Varianceid,1),k)
                         enddo
                         if(LVT_rc%obssource(2).ne."none") then 
                            do tind = 1,LVT_rc%nadc
                               call LVT_writevar_gridded(&
                                    LVT_metrics%variance%ftn_total,&
                                    stats%variance%obs_value_adc(:,k,tind),&
                                    stats%vid_adc_total(tind,LVT_Varianceid,2),k)
                            enddo
                         endif
                      endif
                      call LVT_writeSummaryStats(&
                           LVT_metrics%variance%ftn_summ,&
                           LVT_metrics%variance%short_name,&
                           LVT_rc%ngrid,&
                           stats%variance%model_value_total(:,k,:), &
                           stats%variance%count_model_value_total(:,k,:),&
                           stats%standard_name,&
                           stats%variance%model_value_ci(k,:))
                      if(LVT_rc%obssource(2).ne."none") then 
                         call LVT_writeSummaryStats(&
                              LVT_metrics%variance%ftn_summ,&
                              LVT_metrics%variance%short_name,&
                              LVT_rc%ngrid,&
                              stats%variance%obs_value_total(:,k,:), &
                              stats%variance%count_obs_value_total(:,k,:),&
                              "OBS_"//trim(stats%standard_name),&
                              stats%variance%obs_value_ci(k,:))
                      endif
                   endif
                enddo
             endif
          endif
       end if
    endif          
  end subroutine LVT_writeMetric_Variance

!BOP
! 
! !ROUTINE:
!
! !INTERFACE:
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
  subroutine LVT_resetMetric_Variance(alarm)

    logical                 :: alarm
    integer                 :: vlevels
    integer                :: i,k,l
    type(LVT_metadataEntry), pointer :: model
    type(LVT_statsEntry)   , pointer :: stats


    call LVT_getDataStream1Ptr(model)
    call LVT_getstatsEntryPtr(stats)
    
    do while(associated(model))
       if(stats%selectOpt.eq.1) then 
          do k=1,model%selectNlevs
             if(LVT_metrics%variance%timeOpt.eq.1) then 
                do l=1,LVT_rc%strat_nlevels
                   stats%variance%model_value_ts(:,k,l) = 0.0
                   stats%variance%model_value_ts_sxsx(:,k,l) = 0.0
                   stats%variance%count_model_value_ts(:,k,l)=0 
                   if(alarm) then 
                      stats%variance%tavg_model_value_ts(:,k,l) = 0.0
                      stats%variance%tavg_count_model_value_ts(:,k,l)=0 
                   endif
                enddo
                if(LVT_rc%obssource(2).ne."none") then 
                   do l=1,LVT_rc%strat_nlevels
                      stats%variance%obs_value_ts(:,k,l) = 0.0
                      stats%variance%obs_value_ts_sxsx(:,k,l) = 0.0
                      stats%variance%count_obs_value_ts(:,k,l)=0 
                      if(alarm) then 
                         stats%variance%tavg_obs_value_ts(:,k,l) = 0.0
                         stats%variance%tavg_count_obs_value_ts(:,k,l)=0 
                      endif
                   enddo
                   
                endif
             endif
          enddo
       endif

       model => model%next
       stats => stats%next
    enddo

  end subroutine LVT_resetMetric_Variance


!BOP
! 
! !ROUTINE: LVT_writerestart_Variance
! 
! !INTERFACE:
  subroutine LVT_writerestart_Variance(ftn, pass)
! !USES: 

! 
! !ARGUMENTS: 
    integer                 :: ftn
    integer                 :: pass

! !DESCRIPTION: 
!  This routine writes the restart file for Variance metric computations
! 
!EOP
    
!
! !DESCRIPTION: 
! 
!EOP
    integer              :: k,l
    type(LVT_metaDataEntry), pointer :: model
    type(LVT_metaDataEntry), pointer :: obs
    type(LVT_statsEntry),    pointer :: stats

    call LVT_getDataStream1Ptr(model)
    call LVT_getDataStream2Ptr(obs)
    call LVT_getstatsEntryPtr(stats)
    
    do while(associated(model))
       if(LVT_metrics%variance%selectOpt.eq.1) then 
          if(stats%selectOpt.eq.1.and.&
               model%selectNlevs.ge.1) then 
             do k=1,model%selectNlevs
                do l=1,LVT_rc%strat_nlevels
                   call LVT_writevar_restart(ftn,&
                        stats%variance%model_value_total(:,k,l))
                   call LVT_writevar_restart(ftn,&
                        stats%variance%model_value_total_sxsx(:,k,l))
                   call LVT_writevar_restart(ftn,&
                        stats%variance%count_model_value_total(:,k,l))
                enddo
             enddo
             if(LVT_metrics%variance%computeSC.eq.1) then 
                do k=1,model%selectNlevs
                   do l=1,LVT_rc%nasc
                      call LVT_writevar_restart(ftn, &
                           stats%variance%model_value_asc(:,k,l))
                      call LVT_writevar_restart(ftn, &
                           stats%variance%model_value_asc_sxsx(:,k,l))
                      call LVT_writevar_restart(ftn, &
                           stats%variance%count_model_value_asc(:,k,l))
                   enddo
                enddo
             endif
             if(LVT_metrics%variance%computeADC.eq.1) then 
                do k=1,model%selectNlevs
                   do l=1,LVT_rc%nadc
                      call LVT_writevar_restart(ftn,&
                           stats%variance%model_value_adc(:,k,l))
                      call LVT_writevar_restart(ftn,&
                           stats%variance%model_value_adc_sxsx(:,k,l))
                      call LVT_writevar_restart(ftn,&
                           stats%variance%count_model_value_adc(:,k,l))
                   enddo
                enddo
             endif
             if(LVT_rc%obssource(2).ne."none") then 
                if(obs%selectNlevs.ge.1) then 
                   do k=1,obs%selectNlevs
                      do l=1,LVT_rc%strat_nlevels                
                         call LVT_writevar_restart(ftn,&
                              stats%variance%obs_value_total(:,k,l))
                         call LVT_writevar_restart(ftn,&
                              stats%variance%obs_value_total_sxsx(:,k,l))
                         call LVT_writevar_restart(ftn,&
                              stats%variance%count_obs_value_total(:,k,l))
                      enddo
                   enddo
                   
                   if(LVT_metrics%variance%computeSC.eq.1) then 
                      do k=1,obs%selectNlevs
                         do l=1,LVT_rc%nasc
                            call LVT_writevar_restart(ftn,&
                                 stats%variance%obs_value_asc(:,k,l))
                            call LVT_writevar_restart(ftn,&
                                 stats%variance%obs_value_asc_sxsx(:,k,l))
                            call LVT_writevar_restart(ftn,&
                                 stats%variance%count_obs_value_asc(:,k,l))
                         enddo
                      enddo
                   endif
                   if(LVT_metrics%variance%computeADC.eq.1) then 
                      do k=1,obs%selectNlevs
                         do l=1,LVT_rc%nadc
                            call LVT_writevar_restart(ftn,&
                                 stats%variance%obs_value_adc(:,k,l))
                            call LVT_writevar_restart(ftn, &
                                 stats%variance%obs_value_adc_sxsx(:,k,l))
                            call LVT_writevar_restart(ftn,&
                                 stats%variance%count_obs_value_adc(:,k,l))
                         enddo
                      enddo
                   endif
                endif
             endif
          endif
       endif
       
       model => model%next
       obs   => obs%next
       stats => stats%next
    enddo
  end subroutine LVT_writerestart_Variance


!BOP
! 
! !ROUTINE: LVT_readrestart_Variance
! 
! !INTERFACE:
  subroutine LVT_readrestart_Variance(ftn)
! !USES: 
! 
! !ARGUMENTS: 
    integer                 :: ftn

! !DESCRIPTION: 
!  This routine reads the restart file for Variance metric computations
! 
!EOP
    
!
! !DESCRIPTION: 
! 
!EOP
   
    type(LVT_metaDataEntry), pointer :: model
    type(LVT_metaDataEntry), pointer :: obs
    type(LVT_statsEntry),    pointer :: stats
    integer              :: k,l


    call LVT_getDataStream1Ptr(model)
    call LVT_getDataStream2Ptr(obs)
    call LVT_getstatsEntryPtr(stats)
    
    do while(associated(model))
       if(LVT_metrics%variance%selectOpt.eq.1) then 
          if(stats%selectOpt.eq.1.and.&
               model%selectNlevs.ge.1) then 
             do k=1,model%selectNlevs
                do l=1,LVT_rc%strat_nlevels
                   call LVT_readvar_restart(ftn,&
                        stats%variance%model_value_total(:,k,l))
                   call LVT_readvar_restart(ftn,&
                        stats%variance%model_value_total_sxsx(:,k,l))
                   call LVT_readvar_restart(ftn,&
                        stats%variance%count_model_value_total(:,k,l))
                enddo
             enddo
             if(LVT_metrics%variance%computeSC.eq.1) then 
                do k=1,model%selectNlevs
                   do l=1,LVT_rc%nasc
                      call LVT_readvar_restart(ftn, &
                           stats%variance%model_value_asc(:,k,l))
                      call LVT_readvar_restart(ftn, &
                           stats%variance%model_value_asc_sxsx(:,k,l))
                      call LVT_readvar_restart(ftn, &
                           stats%variance%count_model_value_asc(:,k,l))
                   enddo
                enddo
             endif
             if(LVT_metrics%variance%computeADC.eq.1) then 
                do k=1,model%selectNlevs
                   do l=1,LVT_rc%nadc
                      call LVT_readvar_restart(ftn,&
                           stats%variance%model_value_adc(:,k,l))
                      call LVT_readvar_restart(ftn,&
                           stats%variance%model_value_adc_sxsx(:,k,l))
                      call LVT_readvar_restart(ftn,&
                           stats%variance%count_model_value_adc(:,k,l))
                   enddo
                enddo
             endif
             if(LVT_rc%obssource(2).ne."none") then 
                if(obs%selectNlevs.ge.1) then 
                   do k=1,obs%selectNlevs
                      do l=1,LVT_rc%strat_nlevels                
                         call LVT_readvar_restart(ftn,&
                              stats%variance%obs_value_total(:,k,l))
                         call LVT_readvar_restart(ftn,&
                              stats%variance%obs_value_total_sxsx(:,k,l))
                         call LVT_readvar_restart(ftn,&
                              stats%variance%count_obs_value_total(:,k,l))
                      enddo
                   enddo
                   
                   if(LVT_metrics%variance%computeSC.eq.1) then 
                      do k=1,obs%selectNlevs
                         do l=1,LVT_rc%nasc
                            call LVT_readvar_restart(ftn,&
                                 stats%variance%obs_value_asc(:,k,l))
                            call LVT_readvar_restart(ftn,&
                                 stats%variance%obs_value_asc_sxsx(:,k,l))
                            call LVT_readvar_restart(ftn,&
                                 stats%variance%count_obs_value_asc(:,k,l))
                         enddo
                      enddo
                   endif
                   if(LVT_metrics%variance%computeADC.eq.1) then 
                      do k=1,obs%selectNlevs
                         do l=1,LVT_rc%nadc
                            call LVT_readvar_restart(ftn,&
                                 stats%variance%obs_value_adc(:,k,l))
                            call LVT_readvar_restart(ftn, &
                                 stats%variance%obs_value_adc_sxsx(:,k,l))
                            call LVT_readvar_restart(ftn,&
                                 stats%variance%count_obs_value_adc(:,k,l))
                         enddo
                      enddo
                   endif
                endif
             endif
          endif
       endif

       model => model%next
       obs   => obs%next
       stats => stats%next

    enddo
  end subroutine LVT_readrestart_Variance
end module LVT_VarianceMod
