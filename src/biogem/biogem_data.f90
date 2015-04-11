! ******************************************************************************************************************************** !
! biogem_data.f90
! BioGeM
! DATA LOADING/SAVING ROUTINES
! ******************************************************************************************************************************** !


MODULE biogem_data


  USE biogem_lib
  USE biogem_box
  USE biogem_data_netCDF
  IMPLICIT NONE
  SAVE


CONTAINS


  ! ****************************************************************************************************************************** !
  ! DATA LOADING ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD BioGeM 'goin' FILE OPTIONS
  SUBROUTINE sub_load_goin_biogem()
    USE genie_util, ONLY: check_unit,check_iostat
    ! local variables
    integer::l,io,ia,ias                                                ! tracer counter
    integer::ios                                                        !
    ! read data_BIOGEM file
    call check_unit(in,__LINE__,__FILE__)
    open(unit=in,file='data_BIOGEM',status='old',action='read',iostat=ios)
    if (ios /= 0) then
       print*,'ERROR: could not open BIOGEM initialisation namelist file'
       stop
    end if
    ! read in namelist and close data_BIOGEM file
    read(UNIT=in,NML=ini_biogem_nml,IOSTAT=ios)
    if (ios /= 0) then
       print*,'ERROR: could not read BIOGEM namelist'
       stop
    else
       close(unit=in,iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    end if
    ! set and report namelist data
    par_indir_name = trim(par_indir_name)//'/'
    par_outdir_name = trim(par_outdir_name)//'/'
    par_rstdir_name = trim(par_rstdir_name)//'/'
    par_fordir_name = trim(par_fordir_name)//'/'
    ! *************************************************************
    ! *** HARD SET RESTART NAME BECAUSE NAMELIST IS BEING PANTS ***
    ! *************************************************************
    par_ncrst_name = '_restart.nc'
    ! *************************************************************
    if (ctrl_debug_init > 0) then
       ! --- TRACER INITIALIZATION  ---------------------------------------------------------------------------------------------- !
       print*,'--- INITIALIZATION ---------------------------------'
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          print*,'ocn tracer initial value: ',trim(string_ocn(io)),' = ',ocn_init(io)
          print*,'ocn tracer perturbation : ',trim(string_ocn(io)),' = ',ocn_dinit(io)
       end do
       print*,'Absolute (not relative) tracer re-start adjustment? : ',ctrl_ocn_dinit
       ! --- RUN CONTROL --------------------------------------------------------------------------------------------------------- !
       print*,'--- BIOGEM TIME CONTROL ----------------------------'
       print*,'Continuing run?                                     : ',ctrl_continuing
       print*,'Simulation start year                               : ',par_misc_t_start
       print*,'Simulation run length (yr)                          : ',par_misc_t_runtime
       print*,'Time as Years Before Present?                       : ',ctrl_misc_t_BP
       ! --- MISC CONTROL -------------------------------------------------------------------------------------------------------- !
       print*,'--- MISC CONTROL -----------------------------------'
       print*,'Salanity normalization?                             : ',ctrl_misc_Snorm
       print*,'No salanity normalization?                          : ',ctrl_misc_noSnorm
       print*,'No biological update (and transformations)?         : ',ctrl_misc_nobioupdate
       print*,'Sea-ice brine rejection fraction                    : ',par_misc_brinerejection_frac
       print*,'Max j for sea-ice brine rejection                   : ',par_misc_brinerejection_jmax
       print*,'Include biogeochem in sea-ice brine rejection?      : ',ctrl_misc_brinerejection_bgc
       print*,'Geoengineering scheme ID string                     : ',trim(opt_misc_geoeng)
       print*,'Filename for generic 2D field                       : ',trim(par_misc_2D_file)
       print*,'scalar for generic misc 2D field                    : ',par_misc_2D_scale
       print*,'Min k for geoengineering ocean pipes!               : ',par_misc_kmin_pipe
       print*,'Exclude DIC from geoenginering?                     : ',ctrl_misc_geoeng_noDIC
       print*,'Overwrite restart temperatures?                     : ',ctrl_ocn_rst_reset_T
       ! --- BOUNDARY CONDITIONS ------------------------------------------------------------------------------------------------- !
       print*,'--- BOUNDARY CONDITIONS ----------------------------'
       print*,'Set dissolution flux = rain flux to close system?   : ',ctrl_force_sed_closedsystem
       print*,'Allow temperature / salinity forcing of climate?    : ',ctrl_force_GOLDSTEInTS
       print*,'Allow ONLY temperature / salinity forcing?          : ',ctrl_force_GOLDSTEInTSonly
       print*,'Replace internal fractional sea-ice cover field?    : ',ctrl_force_seaice
       print*,'Replace internal wind-speed field?                  : ',ctrl_force_windspeed
       print*,'Replace internal CaCO3:POC export rain ratio?       : ',ctrl_force_CaCO3toPOCrainratio
       print*,'Replace internal POCd:POC export rain ratio?        : ',ctrl_force_POCdtoPOCrainratio
       print*,'Replace internal [Cd/P]POM/[Cd/P]SW alpha?          : ',ctrl_force_Cd_alpha
       print*,'Replace uniform CaCO3 scavenging coefficient?       : ',ctrl_force_CaCO3ballastcoeff
       print*,'Replace uniform opal scavenging coefficient?        : ',ctrl_force_opalballastcoeff
       print*,'Replace uniform det scavenging coefficient?         : ',ctrl_force_detballastcoeff
       print*,'Replace internal POC flux for 230Th/231Pa scav.     : ',ctrl_force_scav_fpart_POC
       print*,'Replace internal CaCO3 flux for 230Th/231Pa scav.   : ',ctrl_force_scav_fpart_CaCO3
       print*,'Replace internal opal flux for 230Th/231Pa scav.    : ',ctrl_force_scav_fpart_opal
       print*,'Replace internal det flux for 230Th/231Pa scav.     : ',ctrl_force_scav_fpart_det
       print*,'Value of Wanninkhof [1992] gas transfer coeff (a)   : ',par_gastransfer_a
       print*,'Filename for imposed seaice                         : ',trim(par_seaice_file)
       print*,'Filename for imposed windspeed                      : ',trim(par_windspeed_file)
       print*,'Filename for imposed CaCO3toPOCrainratio_file       : ',trim(par_CaCO3toPOCrainratio_file)
       print*,'Filename for imposed POCdtoPOCrainratio             : ',trim(par_POCdtoPOCrainratio_file)
       print*,'Filename for imposed Cd_alpha                       : ',trim(par_Cd_alpha_file)
       print*,'Filename for CaCO3 ballast coefficient field        : ',trim(par_CaCO3ballastcoeff_file)
       print*,'Filename for opal ballast coefficient field         : ',trim(par_opalballastcoeff_file)
       print*,'Filename for det ballast coefficient field          : ',trim(par_detballastcoeff_file)
       print*,'Filename for imposed scavenging POC flux            : ',trim(par_scav_fpart_POC_file)
       print*,'Filename for imposed scavenging CaCO3 flux          : ',trim(par_scav_fpart_CaCO3_file)
       print*,'Filename for imposed scavenging opal flux           : ',trim(par_scav_fpart_opal_file)
       print*,'Filename for imposed scavenging det flux            : ',trim(par_scav_fpart_det_file)
       print*,'Replace solar constant?                             : ',ctrl_force_solconst
       print*,'Use old tracer forcing file format?                 : ',ctrl_force_oldformat
       print*,'Forcings name                                       : ',trim(par_forcing_name)
       ! --- BIOLOGICAL NEW PRODUCTION ------------------------------------------------------------------------------------------- !
       print*,'--- BIOLOGICAL NEW PRODUCTION ----------------------'
       print*,'Biological scheme ID string                         : ',par_bio_prodopt
       print*,'Base [PO4] uptake rate (mol kg-1 yr-1)              : ',par_bio_k0_PO4
       print*,'Base [NO3] uptake rate (mol kg-1 yr-1)              : ',par_bio_k0_NO3
       print*,'[PO4] M-M half-sat value (mol kg-1)                 : ',par_bio_c0_PO4
       print*,'[PO4] M-M half-sat value [SP]                       : ',par_bio_c0_PO4_sp
       print*,'[PO4] M-M half-sat value [NSP]                      : ',par_bio_c0_PO4_nsp
       print*,'[NO3] M-M half-sat value (mol kg-1)                 : ',par_bio_c0_NO3
       print*,'[NO3]+[NH4] M-M half-sat value (mol kg-1)           : ',par_bio_c0_N
       print*,'[Fe] M-M half-sat value (mol kg-1)                  : ',par_bio_c0_Fe
       print*,'[Fe] M-M half-sat value for diazotrophs (mol kg-1)  : ',par_bio_c0_Fe_Diaz
       print*,'[Fe] M-M half-sat value [SP]                        : ',par_bio_c0_Fe_sp
       print*,'[Fe] M-M half-sat value [NSP]                       : ',par_bio_c0_Fe_nsp
       print*,'[H4SiO4] M-M half-sat value (mol kg-1)              : ',par_bio_c0_SiO2
       print*,'[H4SiO4] M-M half-sat value [SP]                    : ',par_bio_c0_SiO2_sp
       print*,'[H4SiO4] M-M half-sat value [NSP]                   : ',par_bio_c0_SiO2_nsp
       print*,'Biological production zone depth (m) (OCMIP-2)      : ',par_bio_zc
       print*,'Biological production time-scale (days) (OCMIP-2)   : ',par_bio_tau
       print*,'Biological production time-scale -- siliceous plank : ',par_bio_tau_sp
       print*,'Biological production time-scale -- non-siliceous   : ',par_bio_tau_nsp
       print*,'Fract. prod. of si. phytop. in Si/Fe-replete cond.  : ',par_bio_relprod_sp
       print*,'Light e-folding depth (m) (OCMIP-2)                 : ',par_bio_I_eL
       print*,'Coefficient for T-dep. uptake rate modifier         : ',par_bio_kT0
       print*,'e-folding temp. (K) for T-dep. uptake rate modifier : ',par_bio_kT_eT
       ! --- ORGANIC MATTER EXPORT RATIOS ---------------------------------------------------------------------------------------- !
       print*,'--- ORGANIC MATTER EXPORT RATIOS -------------------'
       print*,'N/P organic matter Redfield ratio                   : ',par_bio_red_POP_PON
       print*,'C/P organic matter Redfield ratio                   : ',par_bio_red_POP_POC
       print*,'O2/P organic matter pseudo-Redfield ratio           : ',par_bio_red_POP_PO2
       print*,'ALK/N alkalinty correction factor                   : ',par_bio_red_PON_ALK
       print*,'Production fraction of dissolved organic matter     : ',par_bio_red_DOMfrac
       print*,'Production fraction of R-dissolved organic matter   : ',par_bio_red_RDOMfrac
       print*,'P:C fractionation during POM->DOM production        : ',par_bio_red_rP_POM_DOM
       print*,'N:C fractionation during POM->DOM production        : ',par_bio_red_rN_POM_DOM
       print*,'P:C fractionation during POM->RDOM production       : ',par_bio_red_rP_POM_RDOM
       print*,'N:C fractionation during POM->RDOM production       : ',par_bio_red_rN_POM_RDOM
       ! --- INORGANIC MATTER EXPORT RATIOS -------------------------------------------------------------------------------------- !
       print*,'--- INORGANIC MATTER EXPORT RATIOS -----------------'
       print*,'CaCO3:POC rain ratio option ID string               : ',opt_bio_CaCO3toPOCrainratio
       print*,'Base CaCO3:POC export ratio                         : ',par_bio_red_POC_CaCO3
       print*,'Exponent for modifier of CaCO3:POC export ratio     : ',par_bio_red_POC_CaCO3_pP
       print*,'Ohmega half-sat constant [Gehlen et al., 2007]      : ',par_bio_red_POC_CaCO3_Kmax
       print*,'Heinze [2004] CO2aq reference conc (umol kg-1)      : ',par_bio_red_POC_CaCO3_CO2aqREF
       print*,'Barker et al. [2003] CO3 reference conc (umol kg-1) : ',par_bio_red_POC_CaCO3_CO3REF
       print*,'Base opal:POC export ratio                          : ',par_bio_red_POC_opal
       ! --- REMINERALIZATION ---------------------------------------------------------------------------------------------------- !
       print*,'--- REMINERALIZATION -------------------------------'
       print*,'Fraction of POM remin concverted to RDOM            : ',par_bio_remin_RDOMfrac
       print*,'DOM lifetime (yrs)                                  : ',par_bio_remin_DOMlifetime
       print*,'RDOM lifetime (yrs)                                 : ',par_bio_remin_RDOMlifetime
       print*,'RDOM degradation by (surface) photolysis only?      : ',ctrl_bio_remin_RDOM_photolysis
       print*,'Specific CH4 oxidation rate (d-1)                   : ',par_bio_remin_CH4rate
       print*,'Apply fixed-profile for POM remineralization?       : ',ctrl_bio_remin_POC_fixed
       print*,'Kinetic-based POM remineralization?                 : ',ctrl_bio_remin_POC_kinetic
       print*,'Remineralization functional form                    : ',par_bio_remin_fun
       print*,'Ballasting parameterization?                        : ',ctrl_bio_remin_POC_ballast
       print*,'Initial fractional abundance of POC component #2    : ',par_bio_remin_POC_frac2
       print*,'Remineralization length #1 for POC                  : ',par_bio_remin_POC_eL1
       print*,'Remineralization length #2 for POC                  : ',par_bio_remin_POC_eL2
       print*,'Power law power                                     : ',par_bio_remin_martin_b
       print*,'Power law z0                                        : ',par_bio_remin_z0
       print*,'Degradation rate constant #1 for POC                : ',par_bio_remin_POC_K1
       print*,'Degradation rate constant #2 for POC                : ',par_bio_remin_POC_K2
       print*,'Activation energy #1 for POC                        : ',par_bio_remin_POC_Ea1
       print*,'Activation energy #2 for POC                        : ',par_bio_remin_POC_Ea2
       print*,'Range of fractional abundance of POC component #2   : ',par_bio_remin_POC_dfrac2
       print*,'Fractional abundance of POC #2 half sat             : ',par_bio_remin_POC_c0frac2
       print*,'Apply fixed-profile for CaCO3 remineralization?     : ',ctrl_bio_remin_CaCO3_fixed
       print*,'Initial fractional abundance of CaCO3 component #2  : ',par_bio_remin_CaCO3_frac2
       print*,'Remineralization length #1 for CaCO3                : ',par_bio_remin_CaCO3_eL1
       print*,'Remineralization length #2 for CaCO3                : ',par_bio_remin_CaCO3_eL2
       print*,'Apply fixed-profile for opal remineralization?      : ',ctrl_bio_remin_opal_fixed
       print*,'Initial fractional abundance of opal component #2   : ',par_bio_remin_opal_frac2
       print*,'Remineralization length #1 for opal                 : ',par_bio_remin_opal_eL1
       print*,'Remineralization length #2 for opal                 : ',par_bio_remin_opal_eL2
       print*,'Prescribed particle sinking rate (m d-1)            : ',par_bio_remin_sinkingrate
       print*,'Prescribed scavenging sinking rate (m d-1)          : ',par_bio_remin_sinkingrate_scav
       print*,'Organic matter carrying capacity of CaCO3           : ',par_bio_remin_ballast_kc
       print*,'Organic matter carrying capacity of opal            : ',par_bio_remin_ballast_ko
       print*,'Organic matter carrying capacity of lithogenics     : ',par_bio_remin_ballast_kl
       print*,'Aerobic remineralization of OM -> NH4 (not NO3)?    : ',ctrl_bio_remin_ONtoNH4
       print*,'Denitrification [O2] threshold (mol kg-1)           : ',par_bio_remin_denitrO2thresh
       print*,'Catch rapidly-oxidizing species going < 0.0?        : ',ctrl_bio_remin_reminfix
       print*,'NH4 -> NO3 odixation option                         : ',trim(opt_bio_remin_oxidize_NH4toNO3)
       print*,'H2S -> SO4 oxidation option                         : ',trim(opt_bio_remin_oxidize_H2StoSO4)
       print*,'H2S -> POCS scavenging option                       : ',trim(opt_bio_remin_scavenge_H2StoPOMS)
       print*,'Remin rate -- oxic (yr-1)                           : ',par_bio_remin_k_O2
       print*,'Remin rate -- denitrification (yr-1)                : ',par_bio_remin_k_NO3
       print*,'Remin rate -- sulphate reduction (yr-1)             : ',par_bio_remin_k_SO4
       print*,'Remin rate -- methanogenesis (yr-1)                 : ',par_bio_remin_k_meth
       print*,'Half-saturation for oxic remin (mol kg-1)           : ',par_bio_remin_c0_O2
       print*,'Half-saturation for denitrification (mol kg-1)      : ',par_bio_remin_c0_NO3
       print*,'Half-saturation for sulphate reduction (mol kg-1)   : ',par_bio_remin_c0_SO4
       print*,'Inhibition constant by oxygen (mol kg-1)            : ',par_bio_remin_ci_O2
       print*,'Inhibition constant by nitrate (mol kg-1)           : ',par_bio_remin_ci_NO3
       print*,'Inhibition constant by sulphate (mol kg-1)          : ',par_bio_remin_ci_SO4
       print*,'Oxidation rate constant for H2S -> SO4              : ',par_bio_remin_kH2StoSO4
       print*,'Oxidation rate constant for NH4 -> NO2              : ',par_bio_remin_kNH4toNO2
       print*,'Oxidation rate constant for NO2 -> NO3              : ',par_bio_remin_kNO2toNO3
       print*,'Oxidation rate constant for NO2 -> N2O              : ',par_bio_remin_kNO2toN2O
       print*,'NH4 half-saturation for for NH4 -> NO2              : ',par_bio_remin_cNH4_NH4toNO2
       print*,'O2 half-saturation for for NH4 -> NO2               : ',par_bio_remin_cO2_NH4toNO2
       print*,'NO2 half-saturation for for NO2 -> NO3              : ',par_bio_remin_cNO2_NO2toNO3
       print*,'O2 half-saturation for for NO2 -> NO3               : ',par_bio_remin_cO2_NO2toNO3
       print*,'NO2 half-saturation for for NO2 -> N2O              : ',par_bio_remin_cNO2_NO2toN2O
       print*,'O2 half-saturation for for NO2 -> N2O               : ',par_bio_remin_cO2_NO2toN2O
       print*,'Fraction of NH4 oxidation -> N2O (rather than NO2)  : ',par_bio_remin_fracN2O
       print*,'Reaction rate constant for H2S -> POMS              : ',par_bio_remin_kH2StoPOMS
       ! ------------------- ISOTOPIC FRACTIONATION ------------------------------------------------------------------------------ !
       print*,'fractionation for intercellular C fixation          : ',par_d13C_DIC_Corg_ef
       print*,'fract. for intercell. C fixation of si. phytop.     : ',par_d13C_DIC_Corg_ef_sp
       print*,'fract. for intercell. C fixation of non-si. phytop. : ',par_d13C_DIC_Corg_ef_nsp
       print*,'30/28Si fractionation between H4SiO4 and opal       : ',par_d30Si_opal_epsilon
       print*,'*** d114Cd = 1.0006 ***                             : ',par_d114Cd_POCd_epsilon
       print*,'114/???Cd fractionation between Cd and CdCO3        : ',par_d114Cd_CdCO3_epsilon
       print*,'7/6Li fractionation between Li and LiCO3            : ',par_d7Li_LiCO3_epsilon
       print*,'Planktic foram 13C fractionation scheme ID string   : ',opt_bio_foram_p_13C_delta
       print*,'44/40Ca fractionation between Ca and LiCO3          : ',par_d44Ca_CaCO3_epsilon
       ! --- IRON CYCLING -------------------------------------------------------------------------------------------------------- !
       print*,'--- IRON CYCLING -----------------------------------'
       print*,'Aeolian Fe solubility                               : ',par_det_Fe_sol
       print*,'Exponent for aeolian Fe solubility                  : ',par_det_Fe_sol_exp
       print*,'Fixed cellular Fe:C ratio?                          : ',ctrl_bio_red_fixedFetoC
       print*,'C/Fe organic matter ratio                           : ',par_bio_red_POFe_POC
       print*,'Fixed scavening rate (if not: Parekh scheme)?       : ',ctrl_bio_Fe_fixedKscav
       print*,'Fixed Fe scavenging rate (d-1)                      : ',par_scav_Fe_Ks
       print*,'Parekh Fe scavenging rate scale factor: POC         : ',par_scav_Fe_sf_POC
       print*,'Parekh Fe scavenging rate scale factor: CaCO3       : ',par_scav_Fe_sf_CaCO3
       print*,'Parekh Fe scavenging rate scale factor: opal        : ',par_scav_Fe_sf_opal
       print*,'Parekh Fe scavenging rate scale factor: det         : ',par_scav_Fe_sf_det
       print*,'Fraction of scavenged Fe that can be remineralized  : ',par_scav_fremin
       print*,'Prevent return of Fe from the sediments?            : ',ctrl_bio_NO_fsedFe
       print*,'log10 of Fe ligand stability constant (K`(FeL))     : ',par_K_FeL_pP
       print*,'[FeT] dependent Fe:C ratio -- power                 : ',par_bio_FetoC_pP
       print*,'[FeT] dependent Fe:C ratio -- scaling               : ',par_bio_FetoC_K
       print*,'[FeT] dependent Fe:C ratio -- constant              : ',par_bio_FetoC_C
       ! --- SILICA CYCLING ------------------------------------------------------------------------------------------------------ !
       print*,'--- SILICA CYCLING ---------------------------------'
       ! --- NITROGEN CYCLING ---------------------------------------------------------------------------------------------------- !
       print*,'--- NITROGEN CYCLING -------------------------------'
       print*,'mu-1 max rate of export production (yr-1)           : ',par_bio_mu1
       print*,'mu-2 max rate of export from N2-fixation (yr-1)     : ',par_bio_mu2
       print*,'threshold NO3+NH4 for N2 fixation (mol kg-1)        : ',par_bio_N2fixthresh
       print*,'N:P ratio of diazotrophs                            : ',par_bio_NPdiaz
       print*,'constant for dynamical threshold for N2 fixation    : ',par_bio_N2fixdyn
       print*,'N* offset (mol kg-1)                                : ',par_bio_Nstar_offset
       ! --- TRACE METAL/ELEMENT CYCLING ----------------------------------------------------------------------------------------- !
       print*,'--- TRACE METAL/ELEMENT CYCLING --------------------'
       print*,'Default cellular C:Cd (Cd/C) ratio                  : ',par_bio_red_POC_POCd
       print*,'[Cd/P]POM/[Cd/P]SW partition coefficient (alpha)    : ',par_bio_red_POC_POCd_alpha
       print*,'Fe-limitation dependent Cd:C uptake ratio?          : ',ctrl_bio_red_CdtoC_Felim
       print*,'Minimum (Fe replete) Cd:C uptake ratio              : ',par_bio_red_CdtoC_Felim_min
       print*,'Maximum (Fe limited) Cd:C uptake ratio              : ',par_bio_red_CdtoC_Felim_max
       print*,'Default CaCO3 Ca:Li ratio                           : ',par_bio_red_CaCO3_LiCO3
       print*,'partition coefficient (alpha)                       : ',par_bio_red_CaCO3_LiCO3_alpha
       print*,'Default CaCO3 Ca:Cd ratio                           : ',par_bio_red_CaCO3_CdCO3
       print*,'partition coefficient (alpha)                       : ',par_bio_red_CaCO3_CdCO3_alpha
       print*,'Default cellular C:I (I/C) ratio                    : ',par_bio_red_POC_POI
       print*,'Reference [IO3-] value @ default C:I ratio          : ',par_bio_red_POC_POI_C0
       ! --- ABIOTIC PRECIPITATION ----------------------------------------------------------------------------------------------- !
       print*,'--- ABIOTIC PRECIPITATION --------------------------'
       print*,'Allow abiotic CaCO3 precipitation?                  : ',ctrl_bio_CaCO3precip
       print*,'Restrict precipitation to surface layer?            : ',ctrl_bio_CaCO3precip_sur
       print*,'Precipitate as calcite (otherwise aragonite)        : ',par_bio_CaCO3precip_calcite
       print*,'Minimum ohmega threshold for precip                 : ',par_bio_CaCO3precip_abioticohm_min
       print*,'Scale factor for CaCO3 precipitation                : ',par_bio_CaCO3precip_sf
       print*,'Rate law power for CaCO3 precipitation              : ',par_bio_CaCO3precip_exp
       ! --- I/O DIRECTORY DEFINITIONS ------------------------------------------------------------------------------------------- !
       print*,'--- I/O DIRECTORY DEFINITIONS ----------------------'
       print*,'Input dir. name                                     : ',trim(par_indir_name)
       print*,'Output dir. name                                    : ',trim(par_outdir_name)
       print*,'Restart (input) dir. name                           : ',trim(par_rstdir_name)
       print*,'Forcings (input) dir. name                          : ',trim(par_fordir_name)
       print*,'Filename for restart input                          : ',trim(par_infile_name)
       print*,'Filename for restart output                         : ',trim(par_outfile_name)
       ! --- DATA SAVING: TIME-SLICES -------------------------------------------------------------------------------------------- !
       print*,'--- BIOGEM DATA SAVING: TIME-SLICES ----------------'
       print*,'Atmospheric (interface) composition (2D)?           : ',ctrl_data_save_slice_ocnatm
       print*,'Ocean composition (3D)?                             : ',ctrl_data_save_slice_ocn
       print*,'Sediment (interface) composition (2D)?              : ',ctrl_data_save_slice_ocnsed
       print*,'Export flux?                                        : ',ctrl_data_save_sig_fexport
       print*,'Air-sea gas exchange flux (2D)?                     : ',ctrl_data_save_slice_fairsea
       print*,'Ocean-sediment flux (2D)?                           : ',ctrl_data_save_slice_focnsed
       print*,'Sediment-ocean flux (2D)?                           : ',ctrl_data_save_slice_fsedocn
       print*,'Biological fluxes (3D)?                             : ',ctrl_data_save_slice_bio
       print*,'Aqueous carbonate system properties (3D)?           : ',ctrl_data_save_slice_carb
       print*,'Aqueous carbonate system constants (3D)?            : ',ctrl_data_save_slice_carbconst
       print*,'Atmospheric physical properties (2D)?               : ',ctrl_data_save_slice_phys_atm
       print*,'Ocean physical properties (3D)?                     : ',ctrl_data_save_slice_phys_ocn
       print*,'Miscellaneous properties (-)?                       : ',ctrl_data_save_slice_misc
       print*,'Biogeochemical diagnostics (3D)?                    : ',ctrl_data_save_slice_diag
       print*,'Surface fields?                                     : ',ctrl_data_save_slice_sur
       print*,'Integration interval (yr)                           : ',par_data_save_slice_dt
       print*,'Filename for time-slice definition input            : ',trim(par_infile_slice_name)
       print*,'Number of timesteps in sub-inteval saving           : ',par_data_save_slice_n
       print*,'Auto save at run end?                               : ',ctrl_data_save_slice_autoend
       ! --- DATA SAVING: TIME-SERIES -------------------------------------------------------------------------------------------- !
       print*,'--- BIOGEM DATA SAVING: TIME-SERIES ----------------'
       print*,'Atmospheric (interface) composition?                : ',ctrl_data_save_sig_ocnatm
       print*,'Oceanic composition?                                : ',ctrl_data_save_sig_ocn
       print*,'Export flux?                                        : ',ctrl_data_save_sig_fexport
       print*,'Air-sea gas exchange flux ?                         : ',ctrl_data_save_sig_fairsea
       print*,'Sediment (interface) composition?                   : ',ctrl_data_save_sig_ocnsed
       print*,'Ocean->atmosphere flux?                             : ',ctrl_data_save_sig_focnatm
       print*,'Ocean->sediment flux?                               : ',ctrl_data_save_sig_focnsed
       print*,'Sediment->ocean flux/                               : ',ctrl_data_save_sig_fsedocn
       print*,'Ocean surface tracers?                              : ',ctrl_data_save_sig_ocn_sur
       print*,'Ocean surface carbonate chemistry?                  : ',ctrl_data_save_sig_carb_sur
       print*,'Miscellaneous properties?                           : ',ctrl_data_save_sig_misc
       print*,'Biogeochemical diagnostics?                         : ',ctrl_data_save_sig_diag
       print*,'Integration interval (yr)                           : ',par_data_save_sig_dt
       print*,'Filename for time-series definition input           : ',trim(par_infile_sig_name)
       print*,'Auto save at run end?                               : ',ctrl_data_save_sig_autoend
       print*,'Save high res 3D data (@ time-series frequency)?    : ',ctrl_data_save_3d_sig
       ! --- DATA SAVING: DIAGNOSTICS -------------------------------------------------------------------------------------------- !
       print*,'--- BIOGEM DATA SAVING: DIAGNOSTICS ----------------'
       print*,'Create pre-formed tracers?                          : ',ctrl_bio_preformed
       ! --- DATA SAVING: MISC --------------------------------------------------------------------------------------------------- !
       print*,'--- BIOGEM DATA SAVING: MISC -----------------------'
       print*,'Degree of comprehensivity of data saving            : ',par_data_save_level
       print*,'Save derived data (e.g., S-normalized tracers)?     : ',ctrl_data_save_derived
       print*,'Save global diagnostics (at time-slice intervals)?  : ',ctrl_data_save_GLOBAL
       print*,'Save time-slice data in ASCII format?               : ',ctrl_data_save_slice_ascii
       print*,'Save time-series data in ASCII format?              : ',ctrl_data_save_sig_ascii
       print*,'append data to output files on restart              : ',opt_append_data
       print*,'Minimum depth for benthic average (m)               : ',par_data_save_ben_Dmin
       print*,'Time-step for snap-shot saving (N)                  : ',par_t_sig_count_N
       print*,'Time-step for snap-shot saving (S)                  : ',par_t_sig_count_S
       print*,'Generic N j value (for time-series data saving)     : ',par_sig_j_N
       print*,'Generic S j value (for time-series data saving)     : ',par_sig_j_S
       print*,'Restart in netCDF format?                           : ',ctrl_ncrst
       print*,'netCDF restart file name                            : ',trim(par_ncrst_name)
       print*,'Save 2D netCDF data?                                : ',ctrl_data_save_2d
       print*,'Save 3D netCDF data?                                : ',ctrl_data_save_3d
       ! --- TRACER AUDITING AND DEBUGGING OPTIONS ------------------------------------------------------------------------------- !
       print*,'--- TRACER AUDITING AND DEBUGGING OPTIONS ----------'
       print*,'Audit tracer inventory?                             : ',ctrl_audit
       print*,'Halt on audit fail?                                 : ',ctrl_audit_fatal
       print*,'Max allowed relative tracer inventory change        : ',par_misc_audit_relerr
       print*,'Report all run-time warnings?                       : ',ctrl_debug_reportwarnings
       print*,'Report level #0 debug?                              : ',ctrl_debug_lvl0
       print*,'Report level #1 debug?                              : ',ctrl_debug_lvl1
       print*,'Report level #2 debug?                              : ',ctrl_debug_lvl2
       ! --- TRACER FORCING ------------------------------------------------------------------------------------------------------ !
       print*,'--- TRACER FORCING ---------------------------------'
       DO ia = 1, nt_atm
          ias = ia_ias(ia)
          print*,'atm tracer forcing time scale factor  : ',trim(string_atm(ias)),' = ',par_atm_force_scale_time(ias)
          print*,'atm tracer forcing value scale factor : ',trim(string_atm(ias)),' = ',par_atm_force_scale_val(ias)
       end do
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          print*,'ocn tracer forcing time scale factor  : ',trim(string_ocn(io)),' = ',par_ocn_force_scale_time(io)
          print*,'ocn tracer forcing value scale factor : ',trim(string_ocn(io)),' = ',par_ocn_force_scale_val(io)
       end do
       print*,'i coordinate of point forcing (0 = DISABLED)        : ',par_force_point_i
       print*,'j coordinate of point forcing (0 = DISABLED)        : ',par_force_point_j
       print*,'k coordinate of point forcing (0 = DISABLED)        : ',par_force_point_k
       print*,'Surface ocean saturation state target               : ',par_force_invert_ohmega
       print*,'Prevent negative inversion fluxes                   : ',ctrl_force_invert_noneg
       print*,'Calcite saturation as the saturation target?        : ',ctrl_force_ohmega_calcite
       print*,'Geothermal heat flux (W m-2)                        : ',par_Fgeothermal
       ! #### INSERT CODE TO LOAD ADDITIONAL PARAMETERS ########################################################################## !
       !
       ! ######################################################################################################################### !
    end if
    ! filter CaCO3:POC rain ratio options for backwards compatability
    if (ctrl_force_CaCO3toPOCrainratio) opt_bio_CaCO3toPOCrainratio = 'prescribed'
    ! ### TO BE CONVERTED TO NAMELIST ITEMS ###################################################################################### !
    par_misc_t_err = 3600.0*1.0/conv_yr_s ! time-stepping error == 1hr
    opt_data(iopt_data_save_timeslice_fnint) = .FALSE.
    opt_data(iopt_data_save_config) = .FALSE.
    opt_misc(iopt_misc_debugij) = .FALSE.
    par_misc_debug_i = 1
    par_misc_debug_j = 3
    opt_force(iopt_force_freshwater) = .FALSE.
    par_bio_c0_I = 20.0 ! half saturatin value for light (W m-2) [Doney et al., 2006] (30.0 in Parekth et al. [2005])
    par_det_Fe_frac = 0.035 ! mass fraction of Fe in dust
    par_K_FeL = 10**par_K_FeL_pP ! conditional stability constant of ligand-bound Fe [Parekth et al., 2005]
    par_scav_Fe_exp = 0.58 ! (see: Parekth et al. [2005])
    par_scav_Fe_k0  = 0.079 ! (see: Parekth et al. [2005])
    par_scav_Fe_k0 = par_scav_Fe_k0/conv_d_yr ! adjust units of scavening rate constant (d-1 -> yr-1)
    par_part_red_FeTmin = 0.125E-9 ! (see: Ridgwell [2001])
    par_part_red_FetoCmax = 250000.0 !
    ! ############################################################################################################################ !

    ! *** COMPLETE PATHS ***
    par_fordir_name = trim(par_fordir_name)//trim(par_forcing_name)//'/'

    ! *** adjust units ***
    ! adjust units of nutrient update time-scale from days to years
    par_bio_tau     = conv_d_yr*par_bio_tau
    par_bio_tau_sp  = conv_d_yr*par_bio_tau_sp
    par_bio_tau_nsp = conv_d_yr*par_bio_tau_nsp
    ! adjust units of scavening rate constant (d-1 -> yr-1)
    par_scav_Fe_ks = par_scav_Fe_ks/conv_d_yr
    ! adjust units of prescribed particulates sinking rate (m d-1 -> m yr-1)
    par_bio_remin_sinkingrate = par_bio_remin_sinkingrate/conv_d_yr
    par_bio_remin_sinkingrate_scav = par_bio_remin_sinkingrate_scav/conv_d_yr
    ! adjust units of CH4 oxidation (d-1 -> yr-1)
    par_bio_remin_CH4rate = par_bio_remin_CH4rate/conv_d_yr
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ballast coefficients (g POC m-2 yr-1 (g ballast m-2 yr-1)-1 -> mol POC m-2 yr-1 (mol ballast m-2 yr-1)-1)
    par_bio_remin_ballast_kc = (conv_POC_cm3_mol*conv_POC_g_cm3/(conv_cal_cm3_mol*conv_cal_g_cm3))*par_bio_remin_ballast_kc
    par_bio_remin_ballast_ko = (conv_POC_cm3_mol*conv_POC_g_cm3/(conv_opal_cm3_mol*conv_opal_g_cm3))*par_bio_remin_ballast_ko
    par_bio_remin_ballast_kl = (conv_POC_cm3_mol*conv_POC_g_cm3/(conv_det_cm3_mol*conv_det_g_cm3))*par_bio_remin_ballast_kl
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  END SUBROUTINE sub_load_goin_biogem
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD BioGeM RESTART DATA
  SUBROUTINE sub_data_load_rst()
    USE biogem_lib
    use gem_netcdf
    USE genie_util, ONLY:check_unit,check_iostat
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    integer::l,io,is,iv                                        ! local counting variables
    integer::ios                                               !
    integer::loc_ncid                                          !
    CHARACTER(len=255)::loc_filename                           ! filename string
    integer::loc_n_l_ocn,loc_nt_sed                           ! number of selected tracers in the re-start file
    integer,DIMENSION(n_ocn)::loc_conv_iselected_io            ! number of selected ocean tracers in restart
    integer,DIMENSION(nt_sed_all)::loc_conv_iselected_is            !
    real,dimension(n_i,n_j,n_k)::loc_ocn,loc_part              !
    integer::loc_ndims,loc_nvars
    integer,ALLOCATABLE,dimension(:)::loc_dimlen
    integer,ALLOCATABLE,dimension(:,:)::loc_varlen
    integer,ALLOCATABLE,dimension(:)::loc_vdims
    character(20),ALLOCATABLE,dimension(:)::loc_varname
    ! -------------------------------------------------------- !
    ! INITIALIZE
    ! -------------------------------------------------------- !
    loc_conv_iselected_io = 0 ; loc_conv_iselected_is = 0
    loc_ocn = 0.0 ; loc_part = 0.0
    ! -------------------------------------------------------- ! set filename
    IF (ctrl_ncrst) THEN
       loc_filename = TRIM(par_rstdir_name)//par_ncrst_name
    else
       loc_filename = TRIM(par_rstdir_name)//trim(par_infile_name)
    endif
    ! -------------------------------------------------------- ! check file status
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,status='old',file=loc_filename,form='unformatted',action='read',IOSTAT=ios)
    close(unit=in)
    If (ios /= 0) then
       CALL sub_report_error( &
            & 'atchem_data','sub_data_load_restart', &
            & 'You have requested a CONTINUING run, but restart file <'//trim(loc_filename)//'> does not exist', &
            & 'SKIPPING - using default initial values (FILE: gem_config_atm.par)', &
            & (/const_real_null/),.false. &
            & )
    else
       ! -------------------------------------------------------- !
       ! LOAD RESTART
       ! -------------------------------------------------------- !
       IF (ctrl_ncrst) THEN
          call sub_openfile(loc_filename,loc_ncid)
          ! -------------------------------------------------------- ! determine number of variables
          call sub_inqdims (loc_filename,loc_ncid,loc_ndims,loc_nvars)
          ! -------------------------------------------------------- ! allocate arrays
          ALLOCATE(loc_dimlen(loc_ndims),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_varlen(2,loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_vdims(loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ALLOCATE(loc_varname(loc_nvars),STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ! -------------------------------------------------------- ! get variable names
          call sub_inqvars(loc_ncid,loc_ndims,loc_nvars,loc_dimlen,loc_varname,loc_vdims,loc_varlen)
          ! -------------------------------------------------------- ! load and apply only tracers that are selected
          IF (ctrl_debug_init == 1) print*,' * Loading ocean restart fields (dissolved tracers): '
          DO iv=1,loc_nvars
             DO l=1,n_l_ocn
                io = conv_iselected_io(l)
                if ('ocn_'//trim(string_ocn(io)) == trim(loc_varname(iv))) then
                   IF (ctrl_debug_init == 1) print*,'   ',trim(loc_varname(iv))
                   loc_ocn = 0.0
                   call sub_getvarijk(loc_ncid,'ocn_'//trim(string_ocn(io)),n_i,n_j,n_k,loc_ocn(:,:,:))
                   ocn(io,:,:,:) = loc_ocn(:,:,:)
                endif
             end do
          end DO
          IF (ctrl_debug_init == 1) print*,' * Loading ocean restart fields (particulate tracers): '
          DO iv=1,loc_nvars
             DO l=1,nt_sed
                is = conv_iselected_is(l)
                if ('bio_part_'//trim(string_sed(is)) == trim(loc_varname(iv))) then
                   IF (ctrl_debug_init == 1) print*,'   ',trim(loc_varname(iv))
                   loc_part = 0.0
                   call sub_getvarijk(loc_ncid,'bio_part_'//trim(string_sed(is)),n_i,n_j,n_k,loc_part(:,:,:))
                   bio_part(is,:,:,:) = loc_part(:,:,:)
                endif
             end do
          end DO
          ! -------------------------------------------------------- ! deallocate arrays
          deALLOCATE(loc_dimlen,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_varlen,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_vdims,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          deALLOCATE(loc_varname,STAT=alloc_error)
          call check_iostat(alloc_error,__LINE__,__FILE__)
          ! -------------------------------------------------------- ! close file
          call sub_closefile(loc_ncid)
       else
          OPEN(unit=in,status='old',file=loc_filename,form='unformatted',action='read',IOSTAT=ios)
          read(unit=in,iostat=ios)                                          &
               & loc_n_l_ocn,                                               &
               & (loc_conv_iselected_io(l),l=1,loc_n_l_ocn),                &
               & (ocn(loc_conv_iselected_io(l),:,:,:),l=1,loc_n_l_ocn),     &
               & loc_nt_sed,                                               &
               & (loc_conv_iselected_is(l),l=1,loc_nt_sed),                &
               & (bio_part(loc_conv_iselected_is(l),:,:,:),l=1,loc_nt_sed)
          call check_iostat(ios,__LINE__,__FILE__)
          close(unit=in,iostat=ios)
          call check_iostat(ios,__LINE__,__FILE__)
       endif
       ! -------------------------------------------------------- ! adjust restart data
       DO l=1,n_l_ocn
          io = conv_iselected_io(l)
          if (ctrl_ocn_dinit) then
             ocn(io,:,:,:) = ocn(io,:,:,:) + ocn_dinit(io)*phys_ocn(ipo_mask_ocn,:,:,:)
          else
             ocn(io,:,:,:) = (1.0 + ocn_dinit(io))*ocn(io,:,:,:)
          end if
       end do
       if (ctrl_ocn_rst_reset_T) then
          ocn(io_T,:,:,:) = ocn_init(io_T)
       end if
    end If
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  end SUBROUTINE sub_data_load_rst
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! DATA INITIALIZATION ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE 'BIOLOGICAL' PARAMETERS AND VARIABLES
  SUBROUTINE sub_init_bio()
    ! local variables
    CHARACTER(len=255)::loc_filename

    ! *** initialize global arrays ***
    bio_part(:,:,:,:)     = 0.0
    bio_remin(:,:,:,:)    = 0.0
    bio_settle(:,:,:,:)   = 0.0
    bio_part_red(:,:,:,:) = 0.0

    ! *** set default 'Redfield' ratios ***
    ! trivial self-relationships(!)
    bio_part_red(iss_POP,iss_POP,:,:)     = 1.0
    bio_part_red(iss_POC,iss_POC,:,:)     = 1.0
    bio_part_red(iss_PON,iss_PON,:,:)     = 1.0
    bio_part_red(iss_CaCO3,iss_CaCO3,:,:) = 1.0
    bio_part_red(iss_opal,iss_opal,:,:)   = 1.0
    bio_part_red(iss_POCd,iss_POCd,:,:)   = 1.0
    bio_part_red(iss_POFe,iss_POFe,:,:)   = 1.0
    ! set values and derived values
    ! NOTE: relate everything to carbon units where it is not already
    IF (abs(par_bio_red_POP_POC) > const_real_nullsmall) then
       bio_part_red(iss_POP,iss_POC,:,:) = par_bio_red_POP_POC
       bio_part_red(iss_POC,iss_POP,:,:) = 1.0/bio_part_red(iss_POP,iss_POC,:,:)
    end if
    IF (abs(par_bio_red_POP_PON) > const_real_nullsmall) then
       bio_part_red(iss_POP,iss_PON,:,:) = par_bio_red_POP_PON
       bio_part_red(iss_POC,iss_PON,:,:) = bio_part_red(iss_POC,iss_POP,:,:)*bio_part_red(iss_POP,iss_PON,:,:)
       bio_part_red(iss_PON,iss_POC,:,:) = 1.0/bio_part_red(iss_POC,iss_PON,:,:)
    end if
    if (abs(par_bio_red_POC_CaCO3) > const_real_nullsmall) then
       bio_part_red(iss_POC,iss_CaCO3,:,:) = par_bio_red_POC_CaCO3
       bio_part_red(iss_CaCO3,iss_POC,:,:) = 1.0/bio_part_red(iss_POC,iss_CaCO3,:,:)
    end if
    if (abs(par_bio_red_POC_opal) > const_real_nullsmall) then
       bio_part_red(iss_POC,iss_opal,:,:) = par_bio_red_POC_opal
       bio_part_red(iss_opal,iss_POC,:,:) = 1.0/bio_part_red(iss_POC,iss_opal,:,:)
    end if
    IF (abs(par_bio_red_POC_POCd) > const_real_nullsmall) then
       bio_part_red(iss_POC,iss_POCd,:,:) = par_bio_red_POC_POCd
       bio_part_red(iss_POCd,iss_POC,:,:) = 1.0/bio_part_red(iss_POC,iss_POCd,:,:)
    end if
    IF (abs(par_bio_red_POFe_POC) > const_real_nullsmall) then
       bio_part_red(iss_POFe,iss_POC,:,:) = par_bio_red_POFe_POC
       bio_part_red(iss_POC,iss_POFe,:,:) = 1.0/bio_part_red(iss_POFe,iss_POC,:,:)
    end if
    ! denifrification and sulphate reduction
    if (par_bio_red_POP_PO2 == -138.0 ) then
       par_bio_red_O2_H2SO4 = 53.0/(-par_bio_red_POP_PO2)
       par_bio_red_O2_NO3 = 84.8/(-par_bio_red_POP_PO2)
    elseif (par_bio_red_POP_PO2 == -150.0 ) then
       par_bio_red_O2_H2SO4 = 59.0/(-par_bio_red_POP_PO2)
       par_bio_red_O2_NO3 = 104.0/(-par_bio_red_POP_PO2)
    else
       par_bio_red_O2_H2SO4 = 0.0
       par_bio_red_O2_NO3 = 0.0
    end if

    ! *** load prescribed CaCO3:POC field (if requested) ***
    if (ctrl_force_CaCO3toPOCrainratio) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_CaCO3toPOCrainratio_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_bio_CaCO3toPOCrainratio(:,:))
    end if

    ! *** load prescribed POCd:POC field (if requested) ***
    if (ctrl_force_POCdtoPOCrainratio) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_POCdtoPOCrainratio_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_bio_POCdtoPOCrainratio(:,:))
    end if

    ! *** load prescribed [Cd/P]POM/[Cd/P]SW partition coefficient field (if requested) ***
    if (ctrl_force_Cd_alpha) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_Cd_alpha_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_bio_Cd_alpha(:,:))
    end if

    ! *** load prescribed CaCO3 ballasting field (if requested) ***
    if (ctrl_force_CaCO3ballastcoeff) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_CaCO3ballastcoeff_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_bio_remin_kc(:,:))
    else
       par_bio_remin_kc(:,:) = par_bio_remin_ballast_kc
    end if

    ! *** load prescribed opal ballasting field (if requested) ***
    if (ctrl_force_opalballastcoeff) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_opalballastcoeff_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_bio_remin_ko(:,:))
    else
       par_bio_remin_ko(:,:) = par_bio_remin_ballast_ko
    end if

    ! *** load prescribed detrital ballasting field (if requested) ***
    if (ctrl_force_detballastcoeff) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_detballastcoeff_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_bio_remin_kl(:,:))
    else
       par_bio_remin_kl(:,:) = par_bio_remin_ballast_kl
    end if

    ! *** load prescribed POC scavenging coefficient field (if requested) ***
    if (ctrl_force_scav_fpart_POC) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_scav_fpart_POC_file)
       CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,par_scav_fpart_POC(:,:,:))
    end if

    ! *** load prescribed CaCO3 scavenging coefficient field (if requested) ***
    if (ctrl_force_scav_fpart_CaCO3) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_scav_fpart_CaCO3_file)
       CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,par_scav_fpart_CaCO3(:,:,:))
    end if

    ! *** load prescribed opal scavenging coefficient field (if requested) ***
    if (ctrl_force_scav_fpart_opal) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_scav_fpart_opal_file)
       CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,par_scav_fpart_opal(:,:,:))
    end if

    ! *** load prescribed det scavenging coefficient field (if requested) ***
    if (ctrl_force_scav_fpart_det) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_scav_fpart_det_file)
       CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,par_scav_fpart_det(:,:,:))
    end if

  END SUBROUTINE sub_init_bio
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! LOAD BioGeM RESTART DATA
  SUBROUTINE sub_init_misc2D()
    USE biogem_lib
    USE genie_util, ONLY:check_unit,check_iostat
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    CHARACTER(len=255)::loc_filename                           ! filename string
    ! -------------------------------------------------------- !
    ! LOAD 2D FIELD
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! load miscellaneous 2D field
    loc_filename = TRIM(par_indir_name)//TRIM(par_misc_2D_file)
    CALL sub_load_data_ij(loc_filename,n_i,n_j,par_misc_2D(:,:))
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_init_misc2D
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! UPDATE RELATIONSHIPS BETWEEN TRACERS
  SUBROUTINE sub_data_update_tracerrelationships()
    ! -------------------------------------------------------- !
    ! DEFINE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    real,dimension(1:n_ocn,1:nt_sed_all)::loc_conv_sed_ocn          !
    integer,dimension(0:n_ocn,0:nt_sed_all)::loc_tracerrelationships
    ! -------------------------------------------------------- !
    ! INITIALIZE LOCAL VARIABLES
    ! -------------------------------------------------------- !
    loc_conv_sed_ocn = 0.0
    loc_tracerrelationships = 0
    ! -------------------------------------------------------- !
    ! UPDATE REDFIELD RELATIONSHIPS
    ! -------------------------------------------------------- !
    ! if NO3 is employed;
    ! calculate alkalnity corrections associated with the formation and destruction of organic matter from NO3
    ! otherwise, convert PO4 units to NO3 via the P:N Redfield ratio and then calculate the ALK correction from NO3
    ! NOTE: ensure that both corrections are mutually exclusive (i.e., make sure that there can be no double ALK correction)
    ! NOTE: catch incidence of par_bio_red_PON_ALK set to 0.0
    if (abs(par_bio_red_PON_ALK) > const_real_nullsmall) then
       if (ocn_select(io_NO3)) then
          conv_sed_ocn(io_ALK,iss_PON) = par_bio_red_PON_ALK
          conv_ocn_sed(iss_PON,io_ALK) = 1.0/conv_sed_ocn(io_ALK,iss_PON)
          conv_sed_ocn(io_ALK,iss_POP) = 0.0
          conv_ocn_sed(iss_POP,io_ALK) = 0.0
       else
          conv_sed_ocn(io_ALK,iss_PON) = 0.0
          conv_ocn_sed(iss_PON,io_ALK) = 0.0
          conv_sed_ocn(io_ALK,iss_POP) = par_bio_red_PON_ALK*par_bio_red_POP_PON
          conv_ocn_sed(iss_POP,io_ALK) = 1.0/conv_sed_ocn(io_ALK,iss_POP)
       end if
    else
       conv_sed_ocn(io_ALK,iss_PON) = 0.0
       conv_ocn_sed(iss_PON,io_ALK) = 0.0
       conv_sed_ocn(io_ALK,iss_POP) = 0.0
       conv_ocn_sed(iss_POP,io_ALK) = 0.0
    end if
    ! update O2 demand assicated with organic matter (taken as the carbon component)
    ! reduce O2 demand associated with C (and H) oxidation => treat N and P explicitly
    ! NOTE: set no PON O2 demand if NO3 tracer not selected (and increase POC O2 demand)
    ! NOTE: NO3 updake assumed as: 2H+ + 2NO3- -> 2PON + (5/2)O2 + H2O
    if (abs(par_bio_red_POP_POC*par_bio_red_POP_PO2) > const_real_nullsmall) then
       conv_sed_ocn(io_O2,iss_POP) = -4.0/2.0
       conv_ocn_sed(iss_POP,io_O2) = 1.0/conv_sed_ocn(io_O2,iss_POP)
       if (ocn_select(io_NO3)) then
          conv_sed_ocn(io_O2,iss_PON) = -5.0/4.0
          conv_ocn_sed(iss_PON,io_O2) = 1.0/conv_sed_ocn(io_O2,iss_PON)
       else
          conv_sed_ocn(io_O2,iss_PON) = 0.0
          conv_ocn_sed(iss_PON,io_O2) = 0.0
       endif
       conv_sed_ocn(io_O2,iss_POC) = par_bio_red_POP_PO2/par_bio_red_POP_POC - &
            & conv_sed_ocn(io_O2,iss_POP)/par_bio_red_POP_POC - &
            & conv_sed_ocn(io_O2,iss_PON)*par_bio_red_POP_PON/par_bio_red_POP_POC
       conv_ocn_sed(iss_POC,io_O2) = 1.0/conv_sed_ocn(io_O2,iss_POC)
    else
       conv_sed_ocn(io_O2,iss_POP) = 0.0
       conv_sed_ocn(io_O2,iss_PON) = 0.0
       conv_sed_ocn(io_O2,iss_POC) = 0.0
       conv_ocn_sed(iss_POP,io_O2) = 0.0
       conv_ocn_sed(iss_PON,io_O2) = 0.0
       conv_ocn_sed(iss_POC,io_O2) = 0.0
    end if
    ! -------------------------------------------------------- !
    ! UPDATE ALT REDOX SED->OCN RELATIONSHIPS
    ! -------------------------------------------------------- !
    ! NOTE: arrays are only one-way (i.e. there is no equivalent ocn --> sed transformation
    ! NOTE: remember that conv_sed_ocn(io_O2,iss_POC) is *negative*
    ! -------------------------------------------------------- ! Modify for N-reducing conditions
    ! NOTE: oxidation equivalence assumption (NH4): NO3- + H2O + 2H+ <-> 2O2 + NH4+
    !                                           or: O2 == (1/2)NO3- + (1/2)H2O + H+ - (1/2)NH4+
    !       e.g. P + NO3- + H2O + 2H+ -> PO4 + NH4+ (ignoring charges associated with P)
    ! NOTE: oxidation equivalence assumption (N2):  2NO3- + 2H+ <-> (5/2)O2 + N2 + H2O
    !                                           or: O2 == (4/5)NO3- + (4/5)H+ - (2/5)N2 - (2/5)H2O
    !       e.g. P + (8/5)NO3- + (8/5)H+ -> PO4 + (4/5)N2 + (4/5)H2O
    ! NOTE: assumption for NO2: 2NO3- <-> O2 + 2NO2-
    !                       or: O2 == 2NO3- - 2NO2-
    if (ocn_select(io_NO3)) then
       conv_sed_ocn_N(:,:)  = conv_sed_ocn(:,:)
       ! N
       if (ocn_select(io_NH4)) then
          conv_sed_ocn_N(io_NO3,iss_PON) = 0.0
          conv_sed_ocn_N(io_NH4,iss_PON) = 1.0
          conv_sed_ocn_N(io_ALK,iss_PON) = conv_sed_ocn_N(io_NH4,iss_PON) - conv_sed_ocn_N(io_NO3,iss_PON)
          conv_sed_ocn_N(io_O2,iss_PON)  = 0.0
       else
          conv_sed_ocn_N(io_NO3,iss_PON) = 0.0
          conv_sed_ocn_N(io_N2,iss_PON)  = 0.5
          conv_sed_ocn_N(io_ALK,iss_PON) = 0.0
          conv_sed_ocn_N(io_O2,iss_PON)  = 0.0
       endif
       ! P,C
       if (ocn_select(io_NO2)) then
          conv_sed_ocn_N(io_NO3,iss_POP) = -4.0
          conv_sed_ocn_N(io_NO2,iss_POP) = 4.0
          conv_sed_ocn_N(io_ALK,iss_POP) = 0.0
          conv_sed_ocn_N(io_O2,iss_POP)  = 0.0
          conv_sed_ocn_N(io_NO3,iss_POC) = 2.0*conv_sed_ocn(io_O2,iss_POC)
          conv_sed_ocn_N(io_NO2,iss_POC) = -2.0*conv_sed_ocn(io_O2,iss_POC)
          conv_sed_ocn_N(io_ALK,iss_POC) = 0.0
          conv_sed_ocn_N(io_O2,iss_POC)  = 0.0
       else
          conv_sed_ocn_N(io_NO3,iss_POP) = -(8.0/5.0)
          conv_sed_ocn_N(io_N2,iss_POP)  = -0.5*conv_sed_ocn_N(io_NO3,iss_POP)
          conv_sed_ocn_N(io_ALK,iss_POP) = -conv_sed_ocn_N(io_NO3,iss_POP)
          conv_sed_ocn_N(io_O2,iss_POP)  = 0.0
          conv_sed_ocn_N(io_NO3,iss_POC) = (4.0/5.0)*conv_sed_ocn(io_O2,iss_POC)
          conv_sed_ocn_N(io_N2,iss_POC)  = -0.5*conv_sed_ocn_N(io_NO3,iss_POC)
          conv_sed_ocn_N(io_ALK,iss_POC) = -conv_sed_ocn_N(io_NO3,iss_POC)
          conv_sed_ocn_N(io_O2,iss_POC)  = 0.0
       endif
    else
       conv_sed_ocn_N(:,:) = 0.0
    end if
    ! -------------------------------------------------------- ! Modify for S-reducing conditions
    if (ocn_select(io_SO4)) then
       conv_sed_ocn_S(:,:) = conv_sed_ocn(:,:)
       ! N
       if (ocn_select(io_NH4)) then
          conv_sed_ocn_S(io_NO3,iss_PON) = 0.0
          conv_sed_ocn_S(io_NH4,iss_PON) = 1.0
          conv_sed_ocn_S(io_ALK,iss_PON) = conv_sed_ocn_N(io_NH4,iss_PON) - conv_sed_ocn_N(io_NO3,iss_PON)
          conv_sed_ocn_S(io_O2,iss_PON)  = 0.0
       else
          conv_sed_ocn_S(io_NO3,iss_PON) = 0.0
          conv_sed_ocn_S(io_N2,iss_PON)  = 0.5
          conv_sed_ocn_S(io_ALK,iss_PON) = 0.0
          conv_sed_ocn_S(io_O2,iss_PON)  = 0.0
       endif
       ! P,C
       conv_sed_ocn_S(io_SO4,iss_POP) = 0.5*conv_sed_ocn_S(io_O2,iss_POP)
       conv_sed_ocn_S(io_H2S,iss_POP) = -0.5*conv_sed_ocn_S(io_O2,iss_POP)
       conv_sed_ocn_S(io_ALK,iss_POP) = -2.0*conv_sed_ocn_S(io_SO4,iss_POP) + conv_sed_ocn_S(io_ALK,iss_POP)
       conv_sed_ocn_S(io_O2,iss_POP)  = 0.0
       conv_sed_ocn_S(io_SO4,iss_POC) = 0.5*conv_sed_ocn(io_O2,iss_POC)
       conv_sed_ocn_S(io_H2S,iss_POC) = -0.5*conv_sed_ocn(io_O2,iss_POC)
       conv_sed_ocn_S(io_ALK,iss_POC) = -2.0*conv_sed_ocn_S(io_SO4,iss_POC)
       conv_sed_ocn_S(io_O2,iss_POC)  = 0.0
    else
       conv_sed_ocn_S(:,:) = 0.0
    end if
    ! -------------------------------------------------------- ! Modify for methanogenesis
    if (ocn_select(io_CH4)) then
       conv_sed_ocn_meth(:,:) = conv_sed_ocn(:,:)
       ! ### INSERT CODE ##################################### !
       !
       ! ##################################################### !
    else
       conv_sed_ocn_meth(:,:) = 0.0
    end if
    ! -------------------------------------------------------- ! Set local remin array reflecting 'mix' of redox possibilities
    ! NOTE: this is the 'redox tree' of all enabled posibilities
    !       (possibilities of not having O2 but having a different oxidant are omitted, as are O2 + Fe without SO4)
    if (ocn_select(io_O2))  loc_conv_sed_ocn(:,:) = loc_conv_sed_ocn(:,:) + abs(conv_sed_ocn)
    if (ocn_select(io_NO3)) loc_conv_sed_ocn(:,:) = loc_conv_sed_ocn(:,:) + abs(conv_sed_ocn_N)
    if (ocn_select(io_SO4)) loc_conv_sed_ocn(:,:) = loc_conv_sed_ocn(:,:) + abs(conv_sed_ocn_S)
    if (ocn_select(io_CH4)) loc_conv_sed_ocn(:,:) = loc_conv_sed_ocn(:,:) + abs(conv_sed_ocn_meth)
    ! -------------------------------------------------------- !  indexing array (basic oxic-only)
    conv_sed_ocn_i(:,:) = fun_recalc_tracerrelationships_i(conv_sed_ocn(:,:))
    ! -------------------------------------------------------- !
    ! CREATE COMPACT TRACER INDEX FORMAT ARRAY EQUIVALENTS
    ! -------------------------------------------------------- !
    ! -------------------------------------------------------- ! sed -> ocn
    if (ocn_select(io_O2))  conv_ls_lo(:,:)      =  fun_conv_sedocn2lslo(conv_sed_ocn(:,:))
    if (ocn_select(io_NO3)) conv_ls_lo_N(:,:)    =  fun_conv_sedocn2lslo(conv_sed_ocn_N(:,:))
    if (ocn_select(io_SO4)) conv_ls_lo_S(:,:)    =  fun_conv_sedocn2lslo(conv_sed_ocn_S(:,:))
    if (ocn_select(io_CH4)) conv_ls_lo_meth(:,:) =  fun_conv_sedocn2lslo(conv_sed_ocn_meth(:,:))
    ! -------------------------------------------------------- !  indexing array (all possible)
    loc_tracerrelationships = fun_recalc_tracerrelationships_i(loc_conv_sed_ocn)
    conv_ls_lo_i(:,:) =  fun_conv_sedocn2lslo_i(loc_tracerrelationships)
    ! -------------------------------------------------------- ! POM -> DOM
    conv_lP_lD(:,:)   =  fun_conv_sedocn2lslo(conv_POM_DOM(:,:))
    conv_lP_lD_i(:,:) =  fun_conv_sedocn2lslo_i(conv_POM_DOM_i(:,:))
    ! -------------------------------------------------------- ! DOM -> POM
    conv_lD_lP(:,:)   =  fun_conv_ocnsed2lols(conv_DOM_POM(:,:))
    conv_lD_lP_i(:,:) =  fun_conv_ocnsed2lols_i(conv_DOM_POM_i(:,:))
    ! -------------------------------------------------------- ! POM -> RDOM
    conv_lP_lRD(:,:)   =  fun_conv_sedocn2lslo(conv_POM_RDOM(:,:))
    conv_lP_lRD_i(:,:) =  fun_conv_sedocn2lslo_i(conv_POM_RDOM_i(:,:))
    ! -------------------------------------------------------- ! RDOM -> POM
    conv_lRD_lP(:,:)   =  fun_conv_ocnsed2lols(conv_RDOM_POM(:,:))
    conv_lRD_lP_i(:,:) =  fun_conv_ocnsed2lols_i(conv_RDOM_POM_i(:,:))
    ! -------------------------------------------------------- !
    ! END
    ! -------------------------------------------------------- !
  END SUBROUTINE sub_data_update_tracerrelationships
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE INTEGRATED TIME-SLICE VALUE ARRAYS
  SUBROUTINE sub_init_int_timeslice()
    ! initialize integrated time
    int_t_timeslice = 0.0
    int_t_timeslice_count = 0
    ! initialize time-slice data - ocn
    int_ocn_timeslice(:,:,:,:)        = 0.0
    int_bio_part_timeslice(:,:,:,:)   = 0.0
    int_bio_settle_timeslice(:,:,:,:) = 0.0
    int_bio_remin_timeslice(:,:,:,:)  = 0.0
    int_phys_ocn_timeslice(:,:,:,:)   = 0.0
    int_carb_timeslice(:,:,:,:)       = 0.0
    int_carbconst_timeslice(:,:,:,:)  = 0.0
    int_carbisor_timeslice(:,:,:,:)   = 0.0
    ! initialize time-slice data - ocn-atm
    int_sfcatm1_timeslice = 0.0
    int_focnatm_timeslice = 0.0
    int_phys_ocnatm_timeslice(:,:,:) = 0.0
    ! initialize time-slice data - ocn-sed
    int_sfcsed1_timeslice(:,:,:) = 0.0
    int_focnsed_timeslice(:,:,:) = 0.0
    int_fsedocn_timeslice(:,:,:) = 0.0
    ! initialize time-slice data - GOLDSTEIn
    int_opsi_timeslice(:,:)  = 0.0
    int_opsia_timeslice(:,:) = 0.0
    int_opsip_timeslice(:,:) = 0.0
    int_zpsi_timeslice(:,:)  = 0.0
    int_psi_timeslice(:,:)   = 0.0
    int_u_timeslice(:,:,:,:) = 0.0
    ! integrated time slice storage arrays - diagnostics
    int_diag_bio_timeslice(:,:,:)       = 0.0
    int_diag_geochem_timeslice(:,:,:,:) = 0.0
    int_diag_weather_timeslice(:,:,:)   = 0.0
    int_diag_airsea_timeslice = 0.0
    ! ### ADD ADDITIONAL TIME-SLICE ARRAY INITIALIZATIONS HERE ################################################################### !
    !
    ! ############################################################################################################################ !
  END SUBROUTINE sub_init_int_timeslice
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE INTEGRATED TIME-SERIES VALUE ARRAYS
  SUBROUTINE sub_init_int_timeseries()
    ! initialize integrated time
    int_t_sig       = 0.0
    int_t_sig_count = 0
    ! initialize time-series data
    int_misc_gemlite_sig    = 0.0
    int_ocn_tot_M_sig       = 0.0
    int_ocn_tot_M_sur_sig   = 0.0
    int_ocn_tot_V_sig       = 0.0
    int_ocn_sig(:)          = 0.0
    int_fexport_sig(:)      = 0.0
    int_ocnatm_sig = 0.0
    int_focnatm_sig = 0.0
    int_focnsed_sig(:)      = 0.0
    int_fsedocn_sig(:)      = 0.0
    int_ocn_sur_sig(:)      = 0.0
    int_ocn_ben_sig(:)      = 0.0
    int_carb_sur_sig(:)     = 0.0
    int_carb_ben_sig(:)     = 0.0
    int_misc_seaice_sig     = 0.0
    int_misc_seaice_sig_th  = 0.0
    int_misc_seaice_sig_vol = 0.0
    int_misc_opsi_min_sig   = 0.0
    int_misc_opsi_max_sig   = 0.0
    int_misc_opsia_min_sig  = 0.0
    int_misc_opsia_max_sig  = 0.0
    int_misc_SLT_sig        = 0.0
    int_misc_det_Fe_tot_sig = 0.0
    int_misc_det_Fe_dis_sig = 0.0
    int_misc_ocn_solfor_sig = 0.0
    int_misc_ocn_fxsw_sig   = 0.0
    int_ocnsed_sig(:)       = 0.0
    int_diag_bio_sig(:)     = 0.0
    int_diag_geochem_sig(:) = 0.0
    int_diag_weather_sig(:) = 0.0
    int_diag_airsea_sig  = 0.0
    int_diag_misc_2D_sig(:) = 0.0
    int_diag_forcing_sig = 0.0
    ! high resolution 3D! (an exception to the time-series concept that rather spoils things)
    if (ctrl_data_save_3d_sig) int_misc_3D_sig(:,:,:,:) = 0.0
    ! ### ADD ADDITIONAL TIME-SERIES ARRAY INITIALIZATIONS HERE ################################################################## !
    !
    ! ############################################################################################################################ !
  END SUBROUTINE sub_init_int_timeseries
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE FORCING ARRAYS
  SUBROUTINE sub_init_force()
    force_restore_locn(:,:,:,:)    = 0.0
    force_restore_locn_I(:,:,:,:)  = 0.0
    force_restore_locn_II(:,:,:,:) = 0.0
    force_restore_ocn_sig(:,:,:)  = 0.0
    force_restore_ocn_sig_x(:)    = 0.0
    force_restore_ocn_sig_i(:,:)  = 0
    force_restore_ocn_tconst      = 0.0
    force_restore_ocn_select(:)   = .FALSE.
    force_restore_ocn_sur(:)      = .FALSE.
    force_restore_atm        = 0.0
    force_restore_atm_I      = 0.0
    force_restore_atm_II     = 0.0
    force_restore_atm_sig    = 0.0
    force_restore_atm_sig_x  = 0.0
    force_restore_atm_sig_i  = 0
    force_restore_atm_tconst = 0.0
    force_restore_atm_select = .FALSE.
    force_flux_locn(:,:,:,:)       = 0.0
    force_flux_locn_I(:,:,:,:)     = 0.0
    force_flux_locn_II(:,:,:,:)    = 0.0
    force_flux_ocn_sig(:,:,:)     = 0.0
    force_flux_ocn_sig_x(:)       = 0.0
    force_flux_ocn_sig_i(:,:)     = 0
    force_flux_ocn_select(:)      = .FALSE.
    force_flux_ocn_scale(:)       = .FALSE.
    force_flux_atm        = 0.0
    force_flux_atm_I      = 0.0
    force_flux_atm_II     = 0.0
    force_flux_atm_sig    = 0.0
    force_flux_atm_sig_x  = 0.0
    force_flux_atm_sig_i  = 0
    force_flux_atm_scale = .FALSE.
    force_flux_atm_select = .FALSE.
    force_flux_sed_scale(:)       = .FALSE.
    force_flux_sed(:,:,:)         = 0.0
    force_flux_sed_I(:,:,:)       = 0.0
    force_flux_sed_II(:,:,:)      = 0.0
    force_flux_sed_sig(:,:,:)     = 0.0
    force_flux_sed_sig_x(:)       = 0.0
    force_flux_sed_sig_i(:,:)     = 0
    force_flux_sed_select(:)      = .FALSE.
    force_flux_sed_scale(:)       = .FALSE.
    ! misc
    force_solconst_sig(:,:)       = 0.0
    force_restore_docn_nuts(:)    = 0.0
    force_atm_uniform(:)          = 2
    force_ocn_uniform(:)          = 2
    force_sed_uniform(:)          = 2
    force_atm_point_i(:)          = 01
    force_ocn_point_i(:)          = 01
    force_sed_point_i(:)          = 01
    force_atm_point_j(:)          = 01
    force_ocn_point_j(:)          = 01
    force_sed_point_j(:)          = 01
    force_ocn_point_k(:)          = 01
    ! ### ADD ADDITIONAL FORCINGS ARRAY INITIALIZATIONS HERE ##################################################################### !
    !
    ! ############################################################################################################################ !
  END SUBROUTINE sub_init_force
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE AUDIT INVENTORY ARRAYS
  SUBROUTINE sub_init_audit()
    audit_ocn_init(:)       = 0.0
    audit_ocn_old(:)        = 0.0
    audit_ocn_new(:)        = 0.0
    audit_ocn_delta(:)      = 0.0
  END SUBROUTINE sub_init_audit
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE DIAGNOSTICS ARRAYS
  SUBROUTINE sub_init_diag()
    diag_bio(:,:,:)       = 0.0
    diag_geochem(:,:,:,:) = 0.0
    diag_airsea(:,:,:)    = 0.0
  END SUBROUTINE sub_init_diag
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE 'PHYSICS' - OCEAN
  SUBROUTINE sub_init_phys_ocn()
    ! local variables
    INTEGER::i,j,k
    REAL,DIMENSION(0:n_k+1)::loc_grid_dz,loc_grid_dza
    ! initialize local variables
    loc_grid_dz = 0.0 ; loc_grid_dza = 0.0
    loc_grid_dz(1:n_k) = goldstein_dz(:)
    loc_grid_dza(1:n_k) = goldstein_dza(:); loc_grid_dza(n_k) = loc_grid_dz(n_k)/2.0
    ! zero array
    phys_ocn(:,:,:,:) = 0.0
    ! initialize array values
    ! NOTE: initialize basic grid structure values for the (i,j,k) grid, not just ocean-only points
    ! NOTE: depth in in unit of m BELOW sealevel (i.e., a +ve scale)
    ! NOTE: set default rho
    DO i=1,n_i
       DO j=1,n_j
          DO k=1,n_k
             phys_ocn(ipo_lat,i,j,k)      = (180.0/const_pi)*ASIN(goldstein_s(j))
             phys_ocn(ipo_lon,i,j,k)      = (360.0/n_i)*(real(i)-0.5) + par_grid_lon_offset
             phys_ocn(ipo_dlat,i,j,k)     = (180.0/const_pi)*(ASIN(goldstein_sv(j)) - ASIN(goldstein_sv(j-1)))
             phys_ocn(ipo_dlon,i,j,k)     = (360.0/n_i)
             phys_ocn(ipo_latn,i,j,k)     = (180.0/const_pi)*ASIN(goldstein_sv(j))
             phys_ocn(ipo_lone,i,j,k)     = (360.0/n_i)*real(i) + par_grid_lon_offset
             phys_ocn(ipo_Dmid,i,j,k)     = SUM(goldstein_dsc*loc_grid_dza(k:n_k))
             phys_ocn(ipo_dD,i,j,k)       = goldstein_dsc*loc_grid_dz(k)
             phys_ocn(ipo_Dbot,i,j,k)     = SUM(goldstein_dsc*loc_grid_dz(k:n_k))
             phys_ocn(ipo_Dtop,i,j,k)     = SUM(goldstein_dsc*loc_grid_dz(k+1:n_k+1))
          end do
          DO k=goldstein_k1(i,j),n_k
             phys_ocn(ipo_A,i,j,k)        = 2.0*const_pi*(const_rEarth**2)*(1.0/n_i)*(goldstein_sv(j) - goldstein_sv(j-1))
             phys_ocn(ipo_rA,i,j,k)       = 1.0 / phys_ocn(ipo_A,i,j,k)
             phys_ocn(ipo_V,i,j,k)        = phys_ocn(ipo_dD,i,j,k)*phys_ocn(ipo_A,i,j,k)
             phys_ocn(ipo_M,i,j,k)        = conv_m3_kg*phys_ocn(ipo_V,i,j,k)
             phys_ocn(ipo_rM,i,j,k)       = 1.0 / phys_ocn(ipo_M,i,j,k)
             phys_ocn(ipo_mask_ocn,i,j,k) = 1.0
             phys_ocn(ipo_rho,i,j,k)      = conv_m3_kg
          END DO
       END DO
    END DO
  END SUBROUTINE sub_init_phys_ocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE 'PHYSICS' - OCEAN-ATMOSPHERE INTERFACE
  SUBROUTINE sub_init_phys_ocnatm()
    ! local variables
    INTEGER::i,j
    CHARACTER(len=255)::loc_filename
    ! zero array
    phys_ocnatm(:,:,:) = 0.0
    ! initialize array values
    DO i=1,n_i
       DO j=1,n_j
          phys_ocnatm(ipoa_lat,i,j)  = (180.0/const_pi)*ASIN(goldstein_s(j))
          phys_ocnatm(ipoa_lon,i,j)  = (360.0/n_i)*(real(i)-0.5) + par_grid_lon_offset
          phys_ocnatm(ipoa_dlat,i,j) = (180.0/const_pi)*(ASIN(goldstein_sv(j)) - ASIN(goldstein_sv(j-1)))
          phys_ocnatm(ipoa_dlon,i,j) = (360.0/n_i)
          phys_ocnatm(ipoa_A,i,j)    = 2.0*const_pi*(const_rEarth**2)*(1.0/n_i)*(goldstein_sv(j) - goldstein_sv(j-1))
          phys_ocnatm(ipoa_rA,i,j)   = 1.0/ phys_ocnatm(ipoa_A,i,j)
          IF (n_k >= goldstein_k1(i,j)) THEN
             phys_ocnatm(ipoa_seaice,i,j) = 0.0
             phys_ocnatm(ipoa_wspeed,i,j)      = 0.0
             phys_ocnatm(ipoa_mask_ocn,i,j) = 1.0
          END IF
       END DO
    END DO
    ! load prescribed sea-ice cover (if requested)
    ! NOTE: convert from %cover to fractional cover
    if (ctrl_force_seaice) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_seaice_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_phys_seaice(:,:))
       par_phys_seaice(:,:) = par_phys_seaice(:,:)/100.0
    end if
    ! load prescribed wind-speed (if requested)
    ! NOTE: (m s-1)
    if (ctrl_force_windspeed) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_windspeed_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_phys_windspeed(:,:))
    end if
  END SUBROUTINE sub_init_phys_ocnatm
  ! ****************************************************************************************************************************** !


  ! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
  ! INITIALIZE 'PHYSICS' - OCEAN
  SUBROUTINE sub_data_init_phys_ocn()
    ! local variables
    INTEGER::i,j,k
    integer::loc_n,loc_k1
    REAL,DIMENSION(0:n_k+1)::loc_grid_dz,loc_grid_dza
    ! initialize local variables
    loc_grid_dz = 0.0 ; loc_grid_dza = 0.0
    loc_grid_dz(1:n_k) = goldstein_dz(:)
    loc_grid_dza(1:n_k) = goldstein_dza(:); loc_grid_dza(n_k) = loc_grid_dz(n_k)/2.0
    ! initialize array values
    ! NOTE: depth in in unit of m BELOW sealevel (i.e., a +ve scale)
    ! NOTE: set default rho
    loc_n = 0
    DO i=1,n_i
       DO j=1,n_j
          loc_k1 = goldstein_k1(i,j)
          IF (n_k >= loc_k1) THEN
             loc_n = loc_n + 1
             vphys_ocn(loc_n)%i = i
             vphys_ocn(loc_n)%j = j
             vphys_ocn(loc_n)%k1 = loc_k1
             ! initialize, becasue not all 'k' depths are valid
             vphys_ocn(loc_n)%mk(:,:) = 0.0
             DO k=n_k,loc_k1,-1
                vphys_ocn(loc_n)%mk(ipo_lat,k)      = (180.0/const_pi)*ASIN(goldstein_s(j))
                vphys_ocn(loc_n)%mk(ipo_lon,k)      = (360.0/n_i)*(real(i)-0.5) + par_grid_lon_offset
                vphys_ocn(loc_n)%mk(ipo_dlat,k)     = (180.0/const_pi)*(ASIN(goldstein_sv(j)) - ASIN(goldstein_sv(j-1)))
                vphys_ocn(loc_n)%mk(ipo_dlon,k)     = (360.0/n_i)
                vphys_ocn(loc_n)%mk(ipo_latn,k)     = (180.0/const_pi)*ASIN(goldstein_sv(j))
                vphys_ocn(loc_n)%mk(ipo_lone,k)     = (360.0/n_i)*real(i) + par_grid_lon_offset
                vphys_ocn(loc_n)%mk(ipo_Dmid,k)     = SUM(goldstein_dsc*loc_grid_dza(k:n_k))
                vphys_ocn(loc_n)%mk(ipo_dD,k)       = goldstein_dsc*loc_grid_dz(k)
                vphys_ocn(loc_n)%mk(ipo_Dbot,k)     = SUM(goldstein_dsc*loc_grid_dz(k:n_k))
                vphys_ocn(loc_n)%mk(ipo_Dtop,k)     = SUM(goldstein_dsc*loc_grid_dz(k+1:n_k+1))
                vphys_ocn(loc_n)%mk(ipo_A,k)        = 2.0*const_pi*(const_rEarth**2)*(1.0/real(n_i))* &
                     & (goldstein_sv(j) - goldstein_sv(j-1))
                vphys_ocn(loc_n)%mk(ipo_rA,k)       = 1.0 / vphys_ocn(loc_n)%mk(ipo_A,k)
                vphys_ocn(loc_n)%mk(ipo_V,k)        = vphys_ocn(loc_n)%mk(ipo_dD,k)*vphys_ocn(loc_n)%mk(ipo_A,k)
                vphys_ocn(loc_n)%mk(ipo_M,k)        = conv_m3_kg*vphys_ocn(loc_n)%mk(ipo_V,k)
                vphys_ocn(loc_n)%mk(ipo_rM,k)       = 1.0 / vphys_ocn(loc_n)%mk(ipo_M,k)
                vphys_ocn(loc_n)%mk(ipo_mask_ocn,k) = 1.0
                vphys_ocn(loc_n)%mk(ipo_rho,k)      = conv_m3_kg
             end DO
          end if
       end do
    end do
  END SUBROUTINE sub_data_init_phys_ocn
  ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !


  ! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv !
  ! INITIALIZE 'PHYSICS' - OCEAN-ATMOSPHERE INTERFACE
  SUBROUTINE sub_data_init_phys_ocnatm()
    ! local variables
    INTEGER::i,j
    integer::loc_n
    CHARACTER(len=255)::loc_filename
    loc_n = 0
    DO i=1,n_i
       DO j=1,n_j
          loc_n = loc_n + 1
          vphys_ocnatm(loc_n)%i = i
          vphys_ocnatm(loc_n)%j = j
          ! initialize, becasue not all 'k' depths are valid
          vphys_ocnatm(loc_n)%m(:) = 0.0
          vphys_ocnatm(loc_n)%m(ipoa_lat)  = (180.0/const_pi)*ASIN(goldstein_s(j))
          vphys_ocnatm(loc_n)%m(ipoa_lon)  = (360.0/n_i)*(real(i)-0.5) + par_grid_lon_offset
          vphys_ocnatm(loc_n)%m(ipoa_dlat) = (180.0/const_pi)*(ASIN(goldstein_sv(j)) - ASIN(goldstein_sv(j-1)))
          vphys_ocnatm(loc_n)%m(ipoa_dlon) = (360.0/n_i)
          vphys_ocnatm(loc_n)%m(ipoa_A)    = 2.0*const_pi*(const_rEarth**2)*(1.0/n_i)*(goldstein_sv(j) - goldstein_sv(j-1))
          vphys_ocnatm(loc_n)%m(ipoa_rA)   = 1.0/ vphys_ocnatm(loc_n)%m(ipoa_A)
          IF (n_k >= goldstein_k1(i,j)) THEN
             vphys_ocnatm(loc_n)%m(ipoa_seaice) = 0.0
             vphys_ocnatm(loc_n)%m(ipoa_wspeed)      = 0.0
             vphys_ocnatm(loc_n)%m(ipoa_mask_ocn) = 1.0
          END IF
       end do
    end do
    ! load prescribed sea-ice cover (if requested)
    ! NOTE: convert from %cover to fractional cover
    if (ctrl_force_seaice) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_seaice_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_phys_seaice(:,:))
       par_phys_seaice(:,:) = par_phys_seaice(:,:)/100.0
    end if
    ! load prescribed wind-speed (if requested)
    ! NOTE: (m s-1)
    if (ctrl_force_windspeed) then
       loc_filename = TRIM(par_indir_name)//TRIM(par_windspeed_file)
       CALL sub_load_data_ij(loc_filename,n_i,n_j,par_phys_windspeed(:,:))
    end if
  END SUBROUTINE sub_data_init_phys_ocnatm
  ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ !


  ! ****************************************************************************************************************************** !
  ! CONFIGURE AND INITIALIZE TRACER COMPOSITION - OCEAN
  SUBROUTINE sub_init_tracer_ocn_comp()
    ! local variables
    INTEGER::i,j,k,io
    real::loc_tot,loc_frac,loc_standard
    ! initialize global arrays
    ocn(:,:,:,:) = 0.0
    ! set <ocn> array
    DO i=1,n_i
       DO j=1,n_j
          DO k=goldstein_k1(i,j),n_k
             DO io=1,n_ocn
                IF (ocn_select(io)) THEN
                   SELECT CASE (ocn_type(io))
                   CASE (1)
                      ocn(io,i,j,k) = ocn_init(io)
                   case (n_itype_min:n_itype_max)
                      loc_tot  = ocn_init(ocn_dep(io))
                      loc_standard = const_standards(ocn_type(io))
                      loc_frac = fun_calc_isotope_fraction(ocn_init(io),loc_standard)
                      ocn(io,i,j,k) = loc_frac*loc_tot
                   END SELECT
                end IF
             end DO
          END DO
       END DO
    END DO
    ! close file pipe
    CLOSE(unit=in)
  END SUBROUTINE sub_init_tracer_ocn_comp
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONFIGURE AND INITIALIZE TRACER FORCING - ATMOSPHERE
  SUBROUTINE sub_init_tracer_forcing_atm()
    USE genie_util, ONLY:check_unit,check_iostat
    ! local variables
    INTEGER::n,ios,ia
    INTEGER::loc_n_elements,loc_n_start
    REAL::loc_force_restore_tconst
    LOGICAL::loc_force_restore_select,loc_force_flux_select,loc_force_flux_scale
    logical::loc_airsea_eqm
    integer::loc_ias
    integer::loc_force_uniform
    integer::loc_force_point_i,loc_force_point_j
    CHARACTER(len=255)::loc_filename
    ! initialize global variables.
    force_restore_atm_select = .FALSE.
    force_flux_atm_select    = .FALSE.
    force_flux_atm_scale     = .FALSE.
    ocnatm_airsea_eqm        = .FALSE.
    ! check file format
    loc_filename = TRIM(par_fordir_name)//'configure_forcings_atm.dat'
    CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)
    ! open file pipe
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    END DO
    ! read in default (uniform) atmopshere tracer values
    DO n = 1,loc_n_elements
       if (ctrl_force_oldformat) then
          READ(unit=in,FMT=*,iostat=ios)   &
               & loc_ias,                  & ! COLUMN #00: TRACER NUMBER
               & loc_force_restore_select, & ! COLUMN #01: include restoring forcing of tracer?
               & loc_force_restore_tconst, & ! COLUMN #02: time constant of restoring forcing (years)
               & loc_force_flux_select,    & ! COLUMN #03: include flux forcing of tracer?
               & loc_force_flux_scale,     & ! COLUMN #04: scale flux forcing of tracer?
               & loc_airsea_eqm              ! COLUMN #05: assume ocean in equilibrium with atmosphere?
          call check_iostat(ios,__LINE__,__FILE__)
          loc_force_uniform = -99
          loc_force_point_i = 0
          loc_force_point_j = 0
       else
          READ(unit=in,FMT=*,iostat=ios)   &
               & loc_ias,                  & ! COLUMN #00: TRACER NUMBER
               & loc_force_restore_select, & ! COLUMN #01: include restoring forcing of tracer?
               & loc_force_restore_tconst, & ! COLUMN #02: time constant of restoring forcing (years)
               & loc_force_flux_select,    & ! COLUMN #03: include flux forcing of tracer?
               & loc_force_flux_scale,     & ! COLUMN #04: scale flux forcing of tracer?
               & loc_airsea_eqm,           & ! COLUMN #05: assume ocean in equilibrium with atmosphere?
               & loc_force_uniform,        & ! COLUMN #06: make forcing uniform over this dimension
               & loc_force_point_i,        & ! COLUMN #07: i grid location of point forcing
               & loc_force_point_j           ! COLUMN #08: j grid location of point forcing
          call check_iostat(ios,__LINE__,__FILE__)
       end if
       ia = ias_ia(loc_ias)
       force_restore_atm_select(ia) = loc_force_restore_select
       force_restore_atm_tconst(ia) = loc_force_restore_tconst
       force_flux_atm_select(ia)    = loc_force_flux_select
       force_flux_atm_scale(ia)     = loc_force_flux_scale
       ocnatm_airsea_eqm(ia)        = loc_airsea_eqm
       force_atm_uniform(ia)        = loc_force_uniform
       force_atm_point_i(ia)        = loc_force_point_i
       force_atm_point_j(ia)        = loc_force_point_j
       if (force_atm_uniform(ia) > 0) force_flux_atm_scale(ia) = .true.
       if (loc_force_restore_select .AND. (loc_force_restore_tconst < const_real_nullsmall)) then
          CALL sub_report_error( &
               & 'biogem_data','sub_init_atm', &
               & 'Please do not set elected tracer restoring constants to zero (gem_config_atm.par) - '// &
               & 'it can only lead to much unpleasantness later on', &
               & 'STOPPING', &
               & (/const_real_null/),.TRUE. &
               & )
       end if
       IF (loc_force_restore_select .AND. loc_force_flux_select) then
          CALL sub_report_error( &
               & 'biogem_data','init_atm', &
               & 'You are being greedy ... and have both flux AND restoring atmospheric forcing selected'// &
               & '(gem_config_atm.par) - Is this really what you intended?', &
               & 'CONTINUING', &
               & (/const_real_null/),.false. &
               & )
       end if
    END DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! blanket namelist over-ride of forcing point source
    IF (par_force_point_i > 0 .AND. par_force_point_j > 0) then
       force_atm_point_i(:) = par_force_point_i
       force_atm_point_j(:) = par_force_point_j
    end IF
  END SUBROUTINE sub_init_tracer_forcing_atm
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONFIGURE AND INITIALIZE TRACER FORCING - OCEAN
  SUBROUTINE sub_init_tracer_forcing_ocn()
    USE genie_util, ONLY:check_unit,check_iostat
    ! local variables
    INTEGER::n,io,ios
    INTEGER::loc_n_elements,loc_n_start
    REAL::loc_force_restore_tconst
    LOGICAL::loc_force_restore_select,loc_force_restore_sur
    LOGICAL::loc_force_flux_select,loc_force_flux_scale
    integer::loc_io
    integer::loc_force_uniform
    integer::loc_force_point_i,loc_force_point_j,loc_force_point_k
    CHARACTER(len=255)::loc_filename
    ! initialize global arrays
    force_restore_ocn_select(:) = .FALSE.
    force_flux_ocn_select(:)    = .FALSE.
    force_flux_ocn_scale(:)     = .FALSE.
    ! check file format
    loc_filename = TRIM(par_fordir_name)//'configure_forcings_ocn.dat'
    CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)
    ! open file pipe
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    END DO
    !
    DO n = 1,loc_n_elements
       if (ctrl_force_oldformat) then
          READ(unit=in,FMT=*,iostat=ios)   &
               & loc_io,                   & ! COLUMN #00: TRACER NUMBER
               & loc_force_restore_select, & ! COLUMN #01: include restoring forcing of tracer?
               & loc_force_restore_sur,    & ! COLUMN #02: restrict restoring forcing to surface?
               & loc_force_restore_tconst, & ! COLUMN #03: time constant of restoring forcing (years)
               & loc_force_flux_select,    & ! COLUMN #04: include flux forcing of tracer?
               & loc_force_flux_scale        ! COLUMN #05: scale flux forcing of tracer?
          call check_iostat(ios,__LINE__,__FILE__)
          loc_force_uniform = -99
          loc_force_point_i = 0
          loc_force_point_j = 0
          loc_force_point_k = 0
       else
          READ(unit=in,FMT=*,iostat=ios)   &
               & loc_io,                   & ! COLUMN #00: TRACER NUMBER
               & loc_force_restore_select, & ! COLUMN #01: include restoring forcing of tracer?
               & loc_force_restore_sur,    & ! COLUMN #02: restrict restoring forcing to surface?
               & loc_force_restore_tconst, & ! COLUMN #03: time constant of restoring forcing (years)
               & loc_force_flux_select,    & ! COLUMN #04: include flux forcing of tracer?
               & loc_force_flux_scale,     & ! COLUMN #05: scale flux forcing of tracer?
               & loc_force_uniform,        & ! COLUMN #06: make forcing uniform over this dimension
               & loc_force_point_i,        & ! COLUMN #07: i grid location of point forcing
               & loc_force_point_j,        & ! COLUMN #08: j grid location of point forcing
               & loc_force_point_k           ! COLUMN #09: k grid location of point forcing
          call check_iostat(ios,__LINE__,__FILE__)
       endif
       io = loc_io
       force_restore_ocn_select(io) = loc_force_restore_select
       force_restore_ocn_sur(io)    = loc_force_restore_sur
       force_restore_ocn_tconst(io) = loc_force_restore_tconst
       force_flux_ocn_select(io)    = loc_force_flux_select
       force_flux_ocn_scale(io)     = loc_force_flux_scale
       force_ocn_uniform(io)        = loc_force_uniform
       force_ocn_point_i(io)        = loc_force_point_i
       force_ocn_point_j(io)        = loc_force_point_j
       force_ocn_point_k(io)        = loc_force_point_k
       if (force_ocn_uniform(io) > 0) force_flux_ocn_scale(io) = .true.
       ! set local depth limit for ocean boundary conditions (i.e., as as to achieve a surface-only forcing)
       ! NOTE: ensure that land-surface information is preserved (i.e, 'k > n_k')
       ! NOTE: there is currently no restriction of flux forcing to the ocean surface only
       IF (force_restore_ocn_sur(io)) THEN
          force_restore_ocn_k1(io,:,:) = MAX(goldstein_k1(:,:),n_k)
       ELSE
          force_restore_ocn_k1(io,:,:) = goldstein_k1(:,:)
       END IF
       force_flux_ocn_k1(io,:,:) = goldstein_k1(:,:)
       if (loc_force_restore_select .AND. (loc_force_restore_tconst < const_real_nullsmall)) then
          CALL sub_report_error( &
               & 'biogem_data','sub_init_tracer_forcing_ocn', &
               & 'Please do not set selected tracer restoring constants to zero (gem_config_ocn.par) - '// &
               & 'it can only lead to much unpleasantness later on', &
               & 'STOPPING', &
               & (/const_real_null/),.TRUE. &
               & )
       end if
       IF (loc_force_restore_select .AND. loc_force_flux_select) then
          CALL sub_report_error( &
               & 'biogem_data','sub_init_tracer_forcing_ocn', &
               & 'You are being greedy ... and have both flux AND restoring atmospheric forcing selected'// &
               & '(gem_config_atm.par) - Is this really what you intended?', &
               & 'CONTINUING', &
               & (/const_real_null/),.false. &
               & )
       end if
    end DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! blanket namelist over-ride of forcing point source
    IF ((par_force_point_i > 0) .AND. (par_force_point_j > 0) .AND. (par_force_point_k > 0)) then
       force_ocn_point_i(:) = par_force_point_i
       force_ocn_point_j(:) = par_force_point_j
       force_ocn_point_k(:) = par_force_point_k
    end IF

  END SUBROUTINE sub_init_tracer_forcing_ocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! CONFIGURE AND INITIALIZE TRACER FORCING - SEDIMENTS
  SUBROUTINE sub_init_tracer_forcing_sed()
    USE genie_util, ONLY:check_unit,check_iostat
    ! local variables
    INTEGER::n,is,ios
    INTEGER::loc_n_elements,loc_n_start
    LOGICAL::loc_force_flux_select,loc_force_flux_scale
    integer::loc_is
    integer::loc_force_uniform
    integer::loc_force_point_i,loc_force_point_j
    CHARACTER(len=255)::loc_filename
    ! initialize global variables
    force_flux_sed_select(:) = .FALSE.
    force_flux_sed_scale(:)  = .FALSE.
    ! check file format
    loc_filename = TRIM(par_fordir_name)//'configure_forcings_sed.dat'
    CALL sub_check_fileformat(loc_filename,loc_n_elements,loc_n_start)
    ! open file pipe
    call check_unit(in,__LINE__,__FILE__)
    OPEN(unit=in,file=loc_filename,action='read',iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! goto start-of-file tag
    DO n = 1,loc_n_start
       READ(unit=in,fmt='(1X)',iostat=ios)
       call check_iostat(ios,__LINE__,__FILE__)
    END DO
    ! read in default sediment tracer info
    DO n = 1,loc_n_elements
       if (ctrl_force_oldformat) then
          READ(unit=in,FMT=*,iostat=ios) &
               & loc_is,                 & ! COLUMN #00: TRACER NUMBER
               & loc_force_flux_select,  & ! COLUMN #01: include flux forcing of tracer?
               & loc_force_flux_scale      ! COLUMN #02: scale flux forcing of tracer?
          call check_iostat(ios,__LINE__,__FILE__)
          loc_force_uniform = -99
          loc_force_point_i = 0
          loc_force_point_j = 0
       else
          READ(unit=in,FMT=*,iostat=ios) &
               & loc_is,                 & ! COLUMN #00: TRACER NUMBER
               & loc_force_flux_select,  & ! COLUMN #01: include flux forcing of tracer?
               & loc_force_flux_scale,   & ! COLUMN #02: scale flux forcing of tracer?
               & loc_force_uniform,      & ! COLUMN #03: make forcing uniform over this dimension
               & loc_force_point_i,      & ! COLUMN #04: i grid location of point forcing
               & loc_force_point_j         ! COLUMN #05: j grid location of point forcing
          call check_iostat(ios,__LINE__,__FILE__)
       end if
       is = loc_is
       force_flux_sed_select(is) = loc_force_flux_select
       force_flux_sed_scale(is)  = loc_force_flux_scale
       force_sed_uniform(is)     = loc_force_uniform
       force_sed_point_i(is)     = loc_force_point_i
       force_sed_point_j(is)     = loc_force_point_j
       if (force_sed_uniform(is) > 0) force_flux_sed_scale(is) = .true.
    END DO
    ! close file pipe
    CLOSE(unit=in,iostat=ios)
    call check_iostat(ios,__LINE__,__FILE__)
    ! blanket namelist over-ride of forcing point source
    IF ((par_force_point_i > 0) .AND. (par_force_point_j > 0)) then
       force_sed_point_i(:) = par_force_point_i
       force_sed_point_j(:) = par_force_point_j
    end IF
  END SUBROUTINE sub_init_tracer_forcing_sed
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! META-OPTION SETUP AND PARAMETER VALUE CONSISTENCY CHECK
  SUBROUTINE sub_check_par_biogem()
    ! local variables
    LOGICAL::loc_flag
    integer::loc_i,loc_tot_i
    CHARACTER(len=255)::loc_string
    CHARACTER(len=255)::loc_string1,loc_string2
    integer::l,io,is,ias,ia

    ! *** set-up ***
    ! initialize variables
    loc_flag = .FALSE.
    opt_select(:) = .FALSE.
    ! set derived tracer selection options
    opt_select(iopt_select_carbchem)   = ocn_select(io_DIC) .AND. ocn_select(io_ALK)
    opt_select(iopt_select_ocnatm_CO2) = opt_select(iopt_select_carbchem) .AND. atm_select(ias_pCO2)

    ! *** parameter consistency check - biological productivity ***
    ! check first-order consistency between biologial option, and selected dissolved and sedimentary tracers
    ! NOTE: only the existence of inconsistency will be highlighted, not exactly what the problem is ...
    SELECT CASE (par_bio_prodopt)
    CASE (                        &
         & '1N1T_PO4restore',     &
         & '1N1T_PO4restoreLL',   &
         & '1N1T_PO4MM',          &
         & '1N1T_PO4MM_Tdep',     &
         & '2N1T_PO4MM_SiO2',     &
         & '2N2T_PN_Tdep',        &
         & '3N2T_PNFe_Tdep',      &
         & 'bio_PFe',             &
         & 'bio_PFeSi',           &
         & 'bio_PFeSi_Ridgwell02' &
         & )
       IF (.NOT. ocn_select(io_PO4)) loc_flag = .TRUE.
       IF (.NOT. sed_select(iss_POP)) loc_flag = .TRUE.
    end select
    SELECT CASE (par_bio_prodopt)
    CASE (                        &
         & '2N1T_PO4MM_SiO2',     &
         & 'bio_PFeSi',           &
         & 'bio_PFeSi_Ridgwell02' &
         & )
       IF (.NOT. ocn_select(io_SiO2)) loc_flag = .TRUE.
       IF (.NOT. sed_select(iss_opal)) loc_flag = .TRUE.
    end select
    SELECT CASE (par_bio_prodopt)
    case (                    &
         & '2N2T_PO4MM_NO3',   &
         & '2N2T_PN_Tdep',     &
         & '3N2T_PNFe_Tdep'    &
         & )
       IF (.NOT. ocn_select(io_NO3)) loc_flag = .TRUE.
       IF (.NOT. ocn_select(io_N2)) loc_flag = .TRUE.
       IF (.NOT. ocn_select(io_NH4)) loc_flag = .TRUE.
       IF (.NOT. sed_select(iss_PON)) loc_flag = .TRUE.
    end select
    SELECT CASE (par_bio_prodopt)
    case (                    &
         & '3N2T_PNFe_Tdep'   &
         & )
       IF (.NOT. ocn_select(io_Fe)) loc_flag = .TRUE.
       IF (.NOT. ocn_select(io_FeL)) loc_flag = .TRUE.
       IF (.NOT. ocn_select(io_L)) loc_flag = .TRUE.
       IF (.NOT. sed_select(iss_POFe)) loc_flag = .TRUE.
       IF (.NOT. sed_select(iss_POM_Fe)) loc_flag = .TRUE.
    end select
    if (loc_flag) then
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'Your chosen biological option '//trim(par_bio_prodopt)// &
            & ' is not consistent with the selected ocean (gem_config_ocn.par) and/or sediment (gem_config_sed) tracers. '// &
            & 'Go double-check your selected options, because frankly, I cant be bothered to do your job for you.', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
       loc_flag = .FALSE.
    end IF
    If (par_bio_prodopt == 'NONE') then
       If (ctrl_data_save_sig_diag_bio) then
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'Selected data saving is redundant in the event of no biological scheme being activated.', &
               & '[ctrl_data_save_sig_diag_bio] HAS BEEN DE-SELECTED; CONTINUING', &
               & (/const_real_null/),.false. &
               & )
          ctrl_data_save_sig_diag_bio = .FALSE.
       end IF
       If (ctrl_data_save_slice_diag_bio) then
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'Selected data saving is redundant in the event of no biological scheme being activated', &
               & '[ctrl_data_save_slice_diag_bio] HAS BEEN DE-SELECTED; CONTINUING', &
               & (/const_real_null/),.false. &
               & )
          ctrl_data_save_slice_diag_bio = .FALSE.
       end IF
    end if
    ! #### ADD CHECKS OF ADDITIONAL BIOLOGICAL OPTIONS HERE ###################################################################### !
    !
    ! ############################################################################################################################ !
    ! check nutrient restoring tracer self-consistency
    SELECT CASE (par_bio_prodopt)
    CASE (                     &
         & '1N1T_PO4restore',  &
         & '1N1T_PO4restoreLL' &
         & )
       IF (.NOT. force_restore_ocn_select(io_PO4)) THEN
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'PO4 restoring MUST be enabled (FILE: configure_forcings_ocn.dat) in conjunction with the '//&
               & '<1 x nutrient, 1 x taxa: PO4 restoring biological production> option', &
               & 'ALTERING INTERNAL PARAMETER VALUE; CONTINUING', &
               & (/const_real_null/),.false. &
               & )
          force_restore_ocn_select(io_PO4) = .TRUE.
          force_flux_ocn_select(io_PO4) = .FALSE.
          force_restore_ocn_sur(io_PO4) = .TRUE.
          force_restore_ocn_k1(io_PO4,:,:) = MAX(goldstein_k1(:,:),n_k)
       END IF
    CASE (               &
         & 'bio_POCflux' &
         & )
       IF (.NOT. force_flux_sed_select(iss_POC)) THEN
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'POC flux forcing MUST be enabled (FILE: configure_forcings_sed.dat) in conjunction with the '//&
               & 'POC flux based biological production option', &
               & 'STOPPING', &
               & (/const_real_null/),.true. &
               & )
       END IF
    end select
    ! check that the necessary dissolved organic matter tracers have been selected for each particulate (sed) tracer selected and
    ! de-select all DOM tracers (including dependents) if no DOM production is specified
    if (par_bio_red_DOMfrac > const_real_nullsmall) then
       DO l=1,nt_sed
          is = conv_iselected_is(l)
          loc_tot_i = conv_POM_DOM_i(0,is)
          do loc_i=1,loc_tot_i
             io = conv_POM_DOM_i(loc_i,is)
             if (.NOT. ocn_select(io)) THEN
                loc_flag = .TRUE.
                loc_string = string_ocn(io)
             end if
          end do
       end do
       if (loc_flag) then
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'you have rather cheekily set a non-zero fraction of dissolved organic matter production '//&
               & 'but have failed to ensure that you have the necessary DOM tracers selected - '//TRIM(loc_string)// &
               & '[HINT: there must be a corresponding dissolved tracer for each particulate tracer selected '// &
               & 'Sadly, it is too late to automatically select '//TRIM(loc_string)// &
               & ' and a bit risky to set DOM to zero for you :(', &
               & 'STOPPING', &
               & (/const_real_null/),.true. &
               & )
          loc_flag = .FALSE.
       end if
    else
       if (loc_flag) then
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'although the production of dissolved organic matter production is set to zero '//&
               & 'you have carelessly left some DOM tracers selected (FILE: gem_config_ocn.par) '// &
               & 'The model will run quicker by de-selecting all currently selected DOM tracers.', &
               & 'CONTINUING', &
               & (/const_real_null/),.FALSE. &
               & )
          loc_flag = .FALSE.
       end if
    end if

    ! *** parameter consistency check - isotopes, forcings ***
    ! OCEAN TRACERS
    do io=1,n_ocn
       IF (ocn_select(io)) THEN
          if (.not. ocn_select(ocn_dep(io))) then
             loc_string = string_ocn(ocn_dep(io))
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'If an isotope tracer is selected, the associated bulk ocean tracer '//TRIM(loc_string)// &
                  & ' must be selected', &
                  & 'STOPPING', &
                  & (/const_real_null/),.true. &
                  & )
          end if
          if (ocn_select(ocn_dep(io)) .AND. (io /= ocn_dep(io))) then
             if ( &
                  & (force_restore_ocn_select(io) .AND. (.NOT. force_restore_ocn_select(ocn_dep(io)))) &
                  & .OR. &
                  & (.NOT. (force_restore_ocn_select(io)) .AND. force_restore_ocn_select(ocn_dep(io))) &
                  & ) then
                loc_string1 = string_ocn(io)
                loc_string2 = string_ocn(ocn_dep(io))
                CALL sub_report_error( &
                     & 'biogem_data','sub_check_par', &
                     & 'An isotope tracer '//TRIM(loc_string1)//' and associated bulk tracer '//TRIM(loc_string2)// &
                     & ' have been selected, but a restoring forcing for only one of them has been selected.', &
                     & 'CONTINUING', &
                     & (/const_real_null/),.false. &
                     & )
             end if
             if ( &
                  & (force_flux_ocn_select(io) .AND. (.NOT. force_flux_ocn_select(ocn_dep(io)))) &
                  & .OR. &
                  & (.NOT. (force_flux_ocn_select(io)) .AND. force_flux_ocn_select(ocn_dep(io))) &
                  & ) then
                loc_string1 = string_ocn(io)
                loc_string2 = string_ocn(ocn_dep(io))
                CALL sub_report_error( &
                     & 'biogem_data','sub_check_par', &
                     & 'An isotope tracer '//TRIM(loc_string1)//' and associated bulk tracer '//TRIM(loc_string2)// &
                     & ' have been selected, but a flux forcing for only one of them has been selected.', &
                     & 'CONTINUING', &
                     & (/const_real_null/),.false. &
                     & )
             end if
          end if
       else
          if (force_restore_ocn_select(io)) then
             loc_string = string_ocn(io)
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'Because the ocean tracer '//TRIM(loc_string)//' has not been selected, '// &
                  & 'a restoring forcing of this tracer cannot be performed', &
                  & 'RESTORING HAS BEEN DE-SELECTED; CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
             force_restore_ocn_select(io) = .FALSE.
          end if
          if (force_flux_ocn_select(io)) then
             loc_string = string_ocn(io)
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'Because the ocean tracer '//TRIM(loc_string)//' has not been selected, '// &
                  & 'a flux forcing of this tracer cannot be performed', &
                  & 'FLUX FORCING HAS BEEN DE-SELECTED; CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
             force_flux_ocn_select(io) = .FALSE.
          end if
       end if
    end do
    ! ATMOSPHERE TRACERS
    do ias=1,nt_atm_all
       ia = ias_ia(ias)
       IF (atm_select(ias)) THEN
          if (.not. atm_select(atm_dep(ias))) then
             loc_string = string_atm(atm_dep(ias))
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'If an isotope tracer is selected, the associated bulk atmosphere tracer '//TRIM(loc_string)// &
                  & ' must be selected', &
                  & 'STOPPING', &
                  & (/const_real_null/),.true. &
                  & )
          end if
          if (atm_select(atm_dep(ias)) .AND. (io /= atm_type(ias))) then
             if ( &
                  & (force_restore_atm_select(ia) .AND. (.NOT. force_restore_atm_select(ias_ia(atm_dep(ias))))) &
                  & .OR. &
                  & (.NOT. (force_restore_atm_select(ia)) .AND. force_restore_atm_select(ias_ia(atm_dep(ias)))) &
                  & ) then
                loc_string1 = string_atm(ias)
                loc_string2 = string_atm(atm_dep(ias))
                CALL sub_report_error( &
                     & 'biogem_data','sub_check_par', &
                     & 'An isotope tracer '//TRIM(loc_string1)//' and associated bulk tracer '//TRIM(loc_string2)// &
                     & ' have been selected, but a restoring forcing for only one of them has been selected.', &
                     & 'CONTINUING', &
                     & (/const_real_null/),.false. &
                     & )
             end if
             if ( &
                  & (force_flux_atm_select(ia) .AND. (.NOT. force_flux_atm_select(ias_ia(atm_dep(ias))))) &
                  & .OR. &
                  & (.NOT. (force_flux_atm_select(ia)) .AND. force_flux_atm_select(ias_ia(atm_dep(ias)))) &
                  & ) then
                loc_string1 = string_atm(ias)
                loc_string2 = string_atm(atm_dep(ias))
                If ((ias == ias_pCO2_14C) .AND. (atm_dep(ias) == ias_pCO2)) then
                   CALL sub_report_error( &
                        & 'biogem_data','sub_check_par', &
                        & 'An isotope tracer '//TRIM(loc_string1)//' and associated bulk tracer '//TRIM(loc_string2)// &
                        & ' have been selected, but a flux forcing for only one of them has been selected.', &
                        & 'CONTINUING', &
                        & (/const_real_null/),.false. &
                        & )
                else
                   CALL sub_report_error( &
                        & 'biogem_data','sub_check_par', &
                        & 'An isotope tracer '//TRIM(loc_string1)//' and associated bulk tracer '//TRIM(loc_string2)// &
                        & ' have been selected, but a flux forcing for only one of them has been selected.', &
                        & 'CONTINUING', &
                        & (/const_real_null/),.false. &
                        & )
                end If
             end if
          end if
       else
          if (ia > 0 .AND. force_restore_atm_select(ia)) then
             loc_string = string_atm(ias)
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'Because the atmospheric tracer '//TRIM(loc_string)//' has not been selected, '// &
                  & 'a restoring forcing of this tracer cannot be performed', &
                  & 'RESTORING HAS BEEN DE-SELECTED; CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
             force_restore_atm_select(ia) = .FALSE.
          end if
          if (ia > 0 .AND. force_flux_atm_select(ia)) then
             loc_string = string_atm(ias)
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'Because the atmospheric tracer '//TRIM(loc_string)//' has not been selected, '// &
                  & 'a flux forcing of this tracer cannot be performed', &
                  & 'RESTORING HAS BEEN DE-SELECTED; CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
             force_flux_atm_select(ia) = .FALSE.
          end if
       end IF
    end do
    ! SEDIMENT TRACERS
    do is=1,nt_sed_all
       IF (sed_select(is)) THEN
          if (.not. sed_select(sed_dep(is))) then
             loc_string1 = string_sed(is)
             loc_string2 = string_sed(sed_dep(is))
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'If an isotope or other dependent tracer  '//TRIM(loc_string1)//' is selected, '// &
                  & 'the associated bulk sediment tracer '//TRIM(loc_string), &
                  & 'STOPPING', &
                  & (/const_real_null/),.true. &
                  & )
          end if
          if (sed_select(sed_dep(is)) .AND. (is /= sed_dep(is)) .AND. (sed_type(is) /= par_sed_type_scavenged)) then
             if ( &
                  & (force_flux_sed_select(is) .AND. (.NOT. force_flux_sed_select(sed_dep(is)))) &
                  & .OR. &
                  & (.NOT. (force_flux_sed_select(is)) .AND. force_flux_sed_select(sed_dep(is))) &
                  & ) then
                loc_string1 = string_sed(is)
                loc_string2 = string_sed(sed_dep(is))
                CALL sub_report_error( &
                     & 'biogem_data','sub_check_par', &
                     & 'An isotope tracer '//TRIM(loc_string1)//' and associated bulk tracer '//TRIM(loc_string2)// &
                     & ' have been selected, but a flux forcing for only one of them has been selected.', &
                     & 'CONTINUING', &
                     & (/const_real_null/),.false. &
                     & )
             end if
          end if
       else
          if (force_flux_sed_select(is)) then
             loc_string = string_sed(is)
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'Because the sediment tracer '//TRIM(loc_string)//' has not been selected, '// &
                  & 'a flux forcing of this tracer cannot be performed', &
                  & 'RESTORING HAS BEEN DE-SELECTED; CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
             force_flux_sed_select(is) = .FALSE.
          end if
       end IF
    end do

    ! *** FIX UP AND MAKE GENERIC ***
    ! verify ocn-atm carbon cycle option selection
    IF (atm_select(ias_pCO2) .NEQV. ocn_select(io_DIC)) THEN
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'Do you really mean to select CO2 in the atmosphere but not in the ocean (or vice versa)?', &
            & 'CONTINUING', &
            & (/const_real_null/),.false. &
            & )
    ENDIF

    ! *** parameter consistency check - selected tracers and sediment-sediment option combinations ***
    do is=1,nt_sed_all
       if (sed_type(is) == par_sed_type_frac) then
          if (( .NOT. sed_select(is)) .AND. (sed_select(sed_dep(is)))) then
             loc_string2 = string_sed(is)
             CALL sub_report_error( &
                  & 'biogem_data','sub_check_par', &
                  & 'A frac2 tracer must be selected associated with '//TRIM(loc_string2), &
                  & 'STOPPING', &
                  & (/const_real_null/),.true. &
                  & )
          end if
       end if
    end do
    IF (sed_select(iss_CaCO3) .AND. (.NOT. sed_select(iss_POC))) THEN
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par','The POC tracer must be selected with CaCO3 in biogem_config_sed.par ', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    ENDIF
    If (sed_select(iss_CaCO3_age) .AND. (.NOT. sed_select(iss_CaCO3))) then
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'If the sediment CaCO3 age tracer is requested, then the solid CaCO3 tracer must be selected ', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    ENDIF
    !
    If (sed_select(iss_POC) .AND. (.NOT. sed_select(iss_POC_frac2))) then
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'If the organic matter tracer is requested, then the 2nd fraction (iss_POC_frac2) tracer must be selected ', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    ENDIF
    If (sed_select(iss_CaCO3) .AND. (.NOT. sed_select(iss_CaCO3_frac2))) then
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'If the CaCO3 tracer is requested, then the 2nd fraction (iss_CaCO3_frac2) tracer must be selected ', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    ENDIF
    If (sed_select(iss_opal) .AND. (.NOT. sed_select(iss_opal_frac2))) then
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'If the opal tracer is requested, then the 2nd fraction (iss_opal_frac2) tracer must be selected ', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    ENDIF

    ! *** parameter consistency check - selected sediment-ocean tracer option combinations ***
    if (par_bio_prodopt /= 'NONE') then
       DO l=1,nt_sed
          is = conv_iselected_is(l)
          select case (sed_type(is))
          case (par_sed_type_bio,par_sed_type_POM,par_sed_type_CaCO3,par_sed_type_opal,par_sed_type_scavenged, &
               & n_itype_min:n_itype_max)
             loc_tot_i = conv_sed_ocn_i(0,is)
             do loc_i=1,loc_tot_i
                io = conv_sed_ocn_i(loc_i,is)
                if (abs(conv_sed_ocn(io,is)) > const_real_nullsmall) then
                   if (.NOT. ocn_select(io)) then
                      loc_string1 = string_ocn(io)
                      loc_string2 = string_sed(is)
                      CALL sub_report_error( &
                           & 'biogem_data','sub_check_par', &
                           & 'Particulate tracer '//TRIM(loc_string2)// &
                           & ' does does not have the corresponding ocean tracer '//TRIM(loc_string1)//' selected', &
                           & 'CONTINUING', &
                           & (/const_real_null/),.false. &
                           & )
                   end if
                end if
             end do
          end SELECT
       end DO
    end if

    ! *** parameter consistency check - 14C ***
    ! NOTE: just test ocean tracers

    IF (ocn_select(io_DIC_14C) .AND. (.NOT. ocn_select(io_DIC_13C))) THEN
       CALL sub_report_error( &
            & 'biogem_data','sub_check_par', &
            & 'To select 14C isotope tracers, 13C isotope tracers MUST also be selected', &
            & 'STOPPING', &
            & (/const_real_null/),.true. &
            & )
    end if

    ! *** parameter consistency check - data save options ***
    IF (.NOT. opt_select(iopt_select_carbchem)) THEN
       IF (ctrl_data_save_sig_carb_sur) THEN
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'You do not have sufficent ocean tracers selected for a marine carbon cycle', &
               & '[ctrl_data_save_sig_carb_sur] HAS BEEN DE-SELECTED; CONTINUING', &
               & (/const_real_null/),.false. &
               & )
          ctrl_data_save_sig_carb_sur = .FALSE.
       end if
       If (ctrl_data_save_slice_carb) then
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'You do not have sufficent ocean tracers selected for a marine carbon cycle', &
               & '[ctrl_data_save_slice_carb] HAS BEEN DE-SELECTED; CONTINUING', &
               & (/const_real_null/),.false. &
               & )
          ctrl_data_save_slice_carb = .FALSE.
       end if
       If (ctrl_data_save_slice_carbconst) then
          CALL sub_report_error( &
               & 'biogem_data','sub_check_par', &
               & 'You do not have sufficent ocean tracers selected for a marine carbon cycle', &
               & '[ctrl_data_save_slice_carbconst] HAS BEEN DE-SELECTED; CONTINUING', &
               & (/const_real_null/),.false. &
               & )
          ctrl_data_save_slice_carbconst = .FALSE.
       end IF
    end IF

  END SUBROUTINE sub_check_par_biogem
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! DATA SAVE META CONFIG
  SUBROUTINE sub_adj_par_save()

    select case (par_data_save_level)
    case (0:99)
       ctrl_data_save_slice_ocnatm = .false.
       ctrl_data_save_slice_ocn = .false.
       ctrl_data_save_slice_ocnsed = .false.
       ctrl_data_save_slice_fairsea = .false.
       ctrl_data_save_slice_focnatm = .false.
       ctrl_data_save_slice_focnsed = .false.
       ctrl_data_save_slice_fsedocn = .false.
       ctrl_data_save_slice_bio = .false.
       ctrl_data_save_slice_carb = .false.
       ctrl_data_save_slice_carbconst = .false.
       ctrl_data_save_slice_phys_atm = .false.
       ctrl_data_save_slice_phys_ocn = .false.
       ctrl_data_save_slice_misc = .false.
       ctrl_data_save_slice_diag = .false.
       ctrl_data_save_slice_diag_bio = .false.
       ctrl_data_save_slice_diag_geochem = .false.
       ctrl_data_save_slice_diag_proxy = .false.
       ctrl_data_save_slice_diag_tracer = .false.
       ctrl_data_save_sig_ocnatm = .false.
       ctrl_data_save_sig_ocn = .false.
       ctrl_data_save_sig_fexport = .false.
       ctrl_data_save_sig_fairsea = .false.
       ctrl_data_save_sig_ocnsed = .false.
       ctrl_data_save_sig_focnatm = .false.
       ctrl_data_save_sig_focnsed = .false.
       ctrl_data_save_sig_fsedocn = .false.
       ctrl_data_save_sig_ocn_sur = .false.
       ctrl_data_save_sig_carb_sur = .false.
       ctrl_data_save_sig_misc = .false.
       ctrl_data_save_sig_diag = .false.
       ctrl_data_save_derived = .false.
       ctrl_data_save_GLOBAL = .false.
    case default
       ! set new *non namelist* defined sub-options (to broadly retain back-compatability)
       ctrl_data_save_slice_diag_bio = ctrl_data_save_slice_diag
       ctrl_data_save_slice_diag_geochem = ctrl_data_save_slice_diag
       ctrl_data_save_slice_diag_proxy = ctrl_data_save_slice_diag
       ctrl_data_save_slice_diag_tracer = ctrl_data_save_slice_diag
       ctrl_data_save_sig_diag_bio = ctrl_data_save_sig_diag
       ctrl_data_save_sig_diag_geochem = ctrl_data_save_sig_diag
    end select

    ! set BASIC options
    select case (par_data_save_level)
    case (2:11)
       ctrl_data_save_slice_ocnatm = .true.
       ctrl_data_save_slice_ocn = .true.
       ctrl_data_save_slice_misc = .true.
       if (flag_sedgem) ctrl_data_save_slice_ocnsed = .true.
       ctrl_data_save_sig_ocnatm = .true.
       ctrl_data_save_sig_ocn = .true.
       ctrl_data_save_sig_ocn_sur = .true.
       ctrl_data_save_sig_misc = .true.
       ctrl_data_save_GLOBAL = .true.
       if (flag_sedgem) ctrl_data_save_sig_ocnsed = .true.
    case default
       ! NOTHING
    end select

    select case (par_data_save_level)
    case (0)
       ! save NOTHING
    case (1)
       ! MINIMUM (biogeochem ONLY)
       ctrl_data_save_slice_misc = .false.
       ctrl_data_save_sig_misc = .false.
    case (2)
       ! BASIC (biogeochem + BASIC physics)
    case (3)
       ! BASIC + biology diagnostics
       ctrl_data_save_slice_bio = .true.
       ctrl_data_save_slice_diag_bio = .true.
       ctrl_data_save_sig_fexport = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_bio = .true.
    case (4)
       ! BASIC + geochem diagnostics
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_slice_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_fsedocn = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_focnatm = .true.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_sig_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_fsedocn = .true.
    case (5)
       ! BASIC + biology + geochem diagnostics
       ctrl_data_save_slice_focnatm = .true.
       ctrl_data_save_slice_bio = .true.
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_diag_bio = .true.
       ctrl_data_save_slice_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_slice_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_fsedocn = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_fexport = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_focnatm = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_bio = .true.
       ctrl_data_save_sig_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_sig_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_fsedocn = .true.
    case (6)
       ! BASIC + tracer diagnostics
       ctrl_data_save_slice_diag_tracer = .true.
       ctrl_data_save_sig_diag = .true.
    case (7)
       ! BASIC + tracer + proxy diagnostics
       ctrl_data_save_slice_diag_proxy = .true.
       ctrl_data_save_slice_diag_tracer = .true.
       ctrl_data_save_sig_diag = .true.
    case (8)
       ! BASIC + biology + geochem + tracer + proxy diagnostics
       ctrl_data_save_slice_focnatm = .true.
       ctrl_data_save_slice_bio = .true.
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_diag_bio = .true.
       ctrl_data_save_slice_diag_geochem = .true.
       ctrl_data_save_slice_diag_proxy = .true.
       ctrl_data_save_slice_diag_tracer = .true.
       if (flag_sedgem) ctrl_data_save_slice_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_fsedocn = .true.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_fexport = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_focnatm = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_bio = .true.
       ctrl_data_save_sig_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_sig_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_fsedocn = .true.
       ctrl_data_save_derived = .true.
    case (9)
       ! BASIC + full physics
       ctrl_data_save_slice_phys_atm = .true.
       ctrl_data_save_slice_phys_ocn = .true.
    case (10)
       ! OCEAN ACIDIFICATION
       ctrl_data_save_slice_bio = .true.
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_sur = .true.
       if (flag_sedgem) ctrl_data_save_slice_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_fsedocn = .true.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_fexport = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_sig_diag_geochem = .true.
       if (flag_sedgem) ctrl_data_save_sig_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_fsedocn = .true.
    case (11)
       ! SURFACE (CARBCHEM)
       ctrl_data_save_slice_focnatm = .true.
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_sur = .true.
       ctrl_data_save_slice_ocnsed = .false.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_focnatm = .true.
       ctrl_data_save_sig_diag = .true.
    case (99)
       ! EVERYTHING
       ctrl_data_save_slice_ocnatm = .true.
       ctrl_data_save_slice_ocn = .true.
       ctrl_data_save_slice_focnatm = .true.
       ctrl_data_save_slice_fairsea = .true.
       ctrl_data_save_slice_bio = .true.
       ctrl_data_save_slice_carb = .true.
       ctrl_data_save_slice_carbconst = .true.
       ctrl_data_save_slice_phys_atm = .true.
       ctrl_data_save_slice_phys_ocn = .true.
       ctrl_data_save_slice_misc = .true.
       ctrl_data_save_slice_diag = .true.
       ctrl_data_save_slice_diag_bio = .true.
       ctrl_data_save_slice_diag_geochem = .true.
       ctrl_data_save_slice_diag_proxy = .true.
       ctrl_data_save_slice_diag_tracer = .true.
       if (flag_sedgem) ctrl_data_save_slice_ocnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_slice_fsedocn = .true.
       ctrl_data_save_sig_ocnatm = .true.
       ctrl_data_save_sig_ocn = .true.
       ctrl_data_save_sig_fexport = .true.
       ctrl_data_save_sig_fairsea = .true.
       ctrl_data_save_sig_focnatm = .true.
       ctrl_data_save_sig_ocn_sur = .true.
       ctrl_data_save_sig_carb_sur = .true.
       ctrl_data_save_sig_misc = .true.
       ctrl_data_save_sig_diag = .true.
       ctrl_data_save_derived = .true.
       ctrl_data_save_GLOBAL = .true.
       if (flag_sedgem) ctrl_data_save_sig_ocnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_focnsed = .true.
       if (flag_sedgem) ctrl_data_save_sig_fsedocn = .true.
    case default
       ! [leave user-specified settings]
    end select

    ! detrmine whether to save inversion diagnostics
    ctrl_data_save_inversion = .false.
    IF ( &
         & (force_restore_ocn_select(io_colr) .AND. (force_flux_atm_select(ia_pCO2) .OR. force_flux_ocn_select(io_DIC))) &
         & .OR. &
         & (force_restore_atm_select(ia_pCO2_13C) .AND. force_flux_atm_select(ia_pCO2_13C)) &
         & .OR. &
         & (force_restore_ocn_select(io_DIC_13C) .AND. force_flux_atm_select(ia_pCO2_13C)) &
         & .OR. &
         & (force_restore_ocn_select(io_DIC_13C) .AND. force_flux_ocn_select(io_DIC_13C)) &
         & .OR. &
         & (force_restore_ocn_select(io_ALK) .AND. force_flux_ocn_select(io_ALK)) &
         & .OR. &
         & (force_restore_ocn_select(io_Ca_44Ca) .AND. force_flux_ocn_select(io_Ca_44Ca)) &
         & ) THEN
       if (par_force_invert_ohmega < const_real_nullsmall) ctrl_data_save_inversion = .true.
    end IF

  END SUBROUTINE sub_adj_par_save
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE CARBONATE SYSTEM
  SUBROUTINE sub_init_carb()
    ! local variables
    INTEGER::i,j,k
    ! zero arrays
    ! NOTE: leave carb_TSn array at its initialized state
    !       so that a full update of carb constants etc is ALWAYS performed upon the first call to tstep_biogem
    carbconst(:,:,:,:) = 0.0
    carb(:,:,:,:)      = 0.0
    carbalk(:,:,:,:)   = 0.0
    carbisor(:,:,:,:)  = 0.0
    carb_TSn(:,:,:,:)  = 0.0
    ! initialize arrays
    DO i=1,n_i
       DO j=1,n_j
          DO k=goldstein_k1(i,j),n_k
             ! calculate carbonate constants
             CALL sub_calc_carbconst(         &
                  & phys_ocn(ipo_Dmid,i,j,k), &
                  & ocn(io_T,i,j,k),          &
                  & ocn(io_S,i,j,k),          &
                  & carbconst(:,i,j,k)        &
                  & )
             ! adjust carbonate constants
             if (ocn_select(io_Ca) .AND. ocn_select(io_Mg)) then
                call sub_adj_carbconst(   &
                     & ocn(io_Ca,i,j,k),  &
                     & ocn(io_Mg,i,j,k),  &
                     & carbconst(:,i,j,k) &
                     & )
             END if
             IF (opt_select(iopt_select_carbchem)) then
                ! estimate Ca and borate concentrations (if not selected and therefore explicitly treated)
                IF (.NOT. ocn_select(io_Ca))  ocn(io_Ca,i,j,k)  = fun_calc_Ca(ocn(io_S,i,j,k))
                IF (.NOT. ocn_select(io_B))   ocn(io_B,i,j,k)   = fun_calc_Btot(ocn(io_S,i,j,k))
                IF (.NOT. ocn_select(io_SO4)) ocn(io_SO4,i,j,k) = fun_calc_SO4tot(ocn(io_S,i,j,k))
                IF (.NOT. ocn_select(io_F))   ocn(io_F,i,j,k)   = fun_calc_Ftot(ocn(io_S,i,j,k))
                ! seed default initial ocean pH
                carb(ic_H,i,j,k) = 10**(-7.8)
                ! calculate carbonate chemistry
                CALL sub_calc_carb(        &
                     & ocn(io_DIC,i,j,k),  &
                     & ocn(io_ALK,i,j,k),  &
                     & ocn(io_Ca,i,j,k),   &
                     & ocn(io_PO4,i,j,k),  &
                     & ocn(io_SiO2,i,j,k), &
                     & ocn(io_B,i,j,k),    &
                     & ocn(io_SO4,i,j,k),  &
                     & ocn(io_F,i,j,k),    &
                     & ocn(io_H2S,i,j,k),  &
                     & ocn(io_NH4,i,j,k),  &
                     & carbconst(:,i,j,k), &
                     & carb(:,i,j,k),      &
                     & carbalk(:,i,j,k)    &
                     & )
                ! estimate Revelle factor
                CALL sub_calc_carb_RF0(      &
                     & ocn(io_DIC,i,j,k),  &
                     & ocn(io_ALK,i,j,k),  &
                     & ocn(io_PO4,i,j,k),  &
                     & ocn(io_SiO2,i,j,k), &
                     & ocn(io_B,i,j,k),    &
                     & ocn(io_SO4,i,j,k),  &
                     & ocn(io_F,i,j,k),    &
                     & ocn(io_H2S,i,j,k),  &
                     & ocn(io_NH4,i,j,k),  &
                     & carbconst(:,i,j,k), &
                     & carb(:,i,j,k)    &
                     & )
                ! calculate carbonate system isotopic properties
                if (ocn_select(io_DIC_13C)) then
                   call sub_calc_carb_r13C(      &
                        & ocn(io_T,i,j,k),       &
                        & ocn(io_DIC,i,j,k),     &
                        & ocn(io_DIC_13C,i,j,k), &
                        & carb(:,i,j,k),         &
                        & carbisor(:,i,j,k)      &
                        & )
                end IF
                if (ocn_select(io_DIC_14C)) then
                   call sub_calc_carb_r14C(      &
                        & ocn(io_T,i,j,k),       &
                        & ocn(io_DIC,i,j,k),     &
                        & ocn(io_DIC_14C,i,j,k), &
                        & carb(:,i,j,k),         &
                        & carbisor(:,i,j,k)      &
                        & )
                end IF
             end if
          END DO
       END DO
    END DO
  END SUBROUTINE sub_init_carb
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE SOLUBILITY CONSTANTS
  SUBROUTINE sub_init_solconst()
    ! local variables
    INTEGER::i,j
    ! zero arrays
    ocnatm_airsea_solconst = 0.0
    ! initialize array
    DO i=1,n_i
       DO j=1,n_j
          if (n_k >= goldstein_k1(i,j)) then
             call sub_calc_solconst(i,j)
          end if
       END DO
    END DO
  END SUBROUTINE sub_init_solconst
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE DATA SAVING
  SUBROUTINE sub_init_data_save()
    ! local variables
    INTEGER::n
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_n_elements
    integer::loc_i
    real::loc_data_scale

    ! *** set time series data save interval details ***
    ! initialize time series indices
    par_data_save_sig_i = n_data_max
    par_data_save_sig(:) = 0.0
    ! load data
    loc_filename = TRIM(par_indir_name)//TRIM(par_infile_sig_name)
    loc_data_scale = 1.0
    CALL sub_load_data_t1(loc_filename,loc_data_scale,par_data_save_sig,loc_n_elements)
    ! if no elements, populate array with default time interval steps
    IF (loc_n_elements == 0) THEN
       ! limit the time-series integration interval
       if (par_data_save_sig_dt > const_real_nullsmall) then
          loc_n_elements = INT(par_misc_t_runtime/par_data_save_sig_dt + const_real_nullsmall)
          do while (loc_n_elements > n_data_max)
             par_data_save_sig_dt = 10.0*par_data_save_sig_dt
             loc_n_elements = INT(par_misc_t_runtime/par_data_save_sig_dt + const_real_nullsmall)
             CALL sub_report_error( &
                  & 'biogem_data','sub_init_data_save','time-series save interval (biogem_config.par) too short - '// &
                  & 'was [lower value] and is now [upper value] (years)', &
                  & 'CONTINUING', &
                  & (/par_data_save_sig_dt,par_data_save_sig_dt/10.0/),.FALSE. &
                  & )
          end do
          DO n=1,loc_n_elements
             par_data_save_sig(n) = &
                  & real(n - 0.5)*par_data_save_sig_dt + (par_misc_t_runtime - real(loc_n_elements)*par_data_save_sig_dt)
          END DO
       else
          CALL sub_report_error( &
               & 'biogem_data','sub_init_data_save','time-series save interval (biogem_config.par) '// &
               & 'must be non-zero and positive', &
               & 'STOPPING', &
               & (/const_real_null/),.TRUE. &
               & )
       endif
    end IF
    ! find first save time lying within total model run-time
    ! NOTE: <loc_i> will be zero if no valid time points have been requested in the time series input file,
    !       and the array has not been populated automatically
    ! NOTE: ensure that the first identified time-series time is at least a full integration interval (required value)
    !       from the start time of the model run
    loc_i = loc_n_elements
    DO while (loc_i > 0)
       IF ( &
            & par_data_save_sig(loc_i) &
            & < &
            & (par_misc_t_runtime - par_data_save_sig_dt/2.0 + par_misc_t_err) &
            & ) THEN
          EXIT
       ELSE
          loc_i = loc_i - 1
       END IF
    END DO
    par_data_save_sig_i = loc_i
    ! automatically populate run end (if requested)
    if (ctrl_data_save_sig_autoend) then
       DO loc_i=par_data_save_sig_i,1,-1
          if (par_data_save_sig(loc_i) < (1.0 - par_data_save_sig_dt/2.0 + par_misc_t_err)) then
             IF (par_data_save_sig(loc_i) > (par_data_save_sig_dt/2.0 - par_misc_t_err)) then
                exit
             else
                par_data_save_sig(loc_i) = par_data_save_sig_dt/2.0
                exit
             end if
          end if
       END DO
    end if

    ! *** set time slice data save details ***
    ! NOTE: DO NOT populate the time-slice array automatically if the data file is empty
    ! initialize time slice indices
    par_data_save_timeslice_i = n_data_max
    par_data_save_timeslice(:) = 0.0
    ! load data
    loc_filename = TRIM(par_indir_name)//TRIM(par_infile_slice_name)
    loc_data_scale = 1.0
    CALL sub_load_data_t1(loc_filename,loc_data_scale,par_data_save_timeslice,loc_n_elements)
    ! find first save time lying within total model run-time
    ! NOTE: <par_data_save_timeslice_i> will be zero if no valid time slices have been requested in the time slice input file
    ! NOTE: ensure that the first identified time-slice time is at least a full integration interval (required value)
    !       from the start time of the model run
    loc_i = loc_n_elements
    DO while (loc_i > 0)
       IF (par_data_save_timeslice(loc_i) < (par_misc_t_runtime - par_data_save_slice_dt/2.0 + par_misc_t_err)) THEN
          EXIT
       ELSE
          loc_i = loc_i - 1
       END IF
    END DO
    if (par_data_save_timeslice(loc_i) < (par_data_save_slice_dt/2.0 - par_misc_t_err)) then
       loc_i = 0
    end if
    par_data_save_timeslice_i = loc_i
    if (par_data_save_timeslice_i == 0) then
       CALL sub_report_error( &
            & 'biogem_data','sub_init_data_save', &
            & 'No time-slice dates listed in file biogem_save_timeslice.dat fall within the model start and end years', &
            & 'CONTINUING', &
            & (/const_real_null/),.false. &
            & )
    end if
    ! automatically populate run end (if requested)
    if (ctrl_data_save_slice_autoend) then
       if (par_data_save_timeslice_i == 0) then
          par_data_save_timeslice(1) = par_data_save_slice_dt/2.0
          par_data_save_timeslice_i = 1
          CALL sub_report_error( &
               & 'biogem_data','sub_init_data_save', &
               & 'ADDED: single save at end of run', &
               & 'CONTINUING', &
               & (/const_real_null/),.false. &
               & )
       else
          DO loc_i=par_data_save_timeslice_i,1,-1
             if (par_data_save_timeslice(loc_i) < (1.0 - par_data_save_slice_dt/2.0 + par_misc_t_err)) then
                IF (par_data_save_timeslice(loc_i) > (par_data_save_slice_dt/2.0 - par_misc_t_err)) then
                   exit
                else
                   par_data_save_timeslice(loc_i) = par_data_save_slice_dt/2.0
                   exit
                end if
             end if
          END DO
       end if
    end if

  END SUBROUTINE sub_init_data_save
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE RESTORING FORCING - OCEAN
  SUBROUTINE sub_init_force_restore_ocn()
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_n_elements
    integer::l,i,j,k,io
    real,DIMENSION(n_i,n_j)::loc_ij
    real,DIMENSION(n_i,n_j,n_k)::loc_ijk
    real,DIMENSION(2)::loc_data_scale
    loc_ij = 0.0 ; loc_ijk = 0.0
    ! LOOP
    DO l=1,n_l_ocn
       io = conv_iselected_io(l)
       IF (force_restore_ocn_select(io)) THEN
          force_restore_ocn_sig_i(io,:) = n_data_max
          force_restore_ocn_sig(io,:,:) = 0.0
          ! load forcing data array #I
          loc_ijk(:,:,:) = const_real_zero
          if (force_ocn_uniform(io) == 3) then
             loc_ijk(:,:,:) = 0.0
          elseif (force_ocn_uniform(io) == 2) then
             loc_ijk(:,:,n_k) = 0.0
          elseif (force_ocn_uniform(io) == 0) then
             loc_ijk(force_ocn_point_i(io),force_ocn_point_j(io),force_ocn_point_k(io)) = 0.0
          elseif (force_ocn_uniform(io) == -2) then
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(:,:,goldstein_k1(i,j)) = 0.0
                   end if
                end DO
             end DO
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_ocn_'//TRIM(string_ocn(io))//'_I'//TRIM(string_data_ext)
             CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,loc_ijk(:,:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                DO k=force_restore_ocn_k1(io,i,j),n_k
                   force_restore_locn_I(l,i,j,k) = loc_ijk(i,j,k)
                end do
             end do
          end DO
          ! load forcing data array #II
          loc_ijk(:,:,:) = const_real_zero
          if (force_ocn_uniform(io) == 3) then
             loc_ijk(:,:,:) = phys_ocn(ipo_mask_ocn,:,:,:)
          elseif (force_ocn_uniform(io) == 2) then
             loc_ijk(:,:,n_k) = phys_ocn(ipo_mask_ocn,:,:,n_k)
          elseif (force_ocn_uniform(io) == 0) then
             loc_ijk(force_ocn_point_i(io),force_ocn_point_j(io),force_ocn_point_k(io)) = 1.0
          elseif (force_ocn_uniform(io) == -2) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_ocn_'//TRIM(string_ocn(io))//'_BEN'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(i,j,goldstein_k1(i,j)) = loc_ij(i,j)
                   end if
                end do
             end DO
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_ocn_'//TRIM(string_ocn(io))//'_II'//TRIM(string_data_ext)
             CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,loc_ijk(:,:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                DO k=force_restore_ocn_k1(io,i,j),n_k
                   force_restore_locn_II(l,i,j,k) = loc_ijk(i,j,k)
                end do
             end do
          end DO
          ! load forcing time series data
          loc_filename =TRIM(par_fordir_name)//'biogem_force_restore_ocn_'//TRIM(string_ocn(io))//'_sig'//TRIM(string_data_ext)
          loc_data_scale(:) = (/par_ocn_force_scale_time(io), par_ocn_force_scale_val(io)/)
          CALL sub_load_data_t2(loc_filename,loc_data_scale(:),force_restore_ocn_sig(io,:,:),loc_n_elements)
          ! set default forcing index values
          ! NOTE: catch missing time series data
          if (loc_n_elements == 0) THEN
             CALL sub_report_error( &
                  & 'biogem_data','init_force_restore_ocn','PLEASE PUT SOME DATA IN TIME SERIES FILE: '//TRIM(loc_filename), &
                  & 'STOPPING', &
                  & (/const_real_null/),.TRUE. &
                  & )
          else
             force_restore_ocn_sig_i(io,:) = loc_n_elements
          end if
          ! warn if forcing information appears to be 'incomplete'
          ! NOTE: this will catch both possible mismatches of forcing signal and model integration specification
          !       i.e., for _not_ the BP option;
          !             a signal time start year that is after the model start year
          !             or a signal time year that is before the model end year
          !             (or visa versa for a BP time scale)
          if ( &
               & (minval(force_restore_ocn_sig(io,1,1:loc_n_elements)) > 0.0) &
               & .OR. &
               & (maxval(force_restore_ocn_sig(io,1,1:loc_n_elements)) < par_misc_t_runtime) &
               & ) then
             CALL sub_report_error( &
                  & 'biogem_data','sub_init_force_restore_ocn', &
                  & 'The time interval of forcing data does not fully span the requested model integration, either; '// &
                  & '(1) alter the model start time year and/or run length (the goin file) or'// &
                  & '(2) alter the forcing signal stop time year (FILE: '//TRIM(loc_filename)//') ', &
                  & 'CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
          end if
       end IF
    end DO
  END SUBROUTINE sub_init_force_restore_ocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE RESTORING FORCING - ATMOSPHERE
  SUBROUTINE sub_init_force_restore_atm()
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_n_elements
    integer::i,j,ia,ias
    real,DIMENSION(n_i,n_j)::loc_ij
    real,DIMENSION(2)::loc_data_scale
    loc_ij = 0.0
    ! LOOP
    DO ia = 3, nt_atm
       ias = ia_ias(ia)
       IF (force_restore_atm_select(ia)) THEN
          force_restore_atm_sig_i(ia,:) = n_data_max
          force_restore_atm_sig(ia,:,:) = 0.0
          ! load forcing data array #I
          loc_ij(:,:) = const_real_zero
          if (force_atm_uniform(ia) == 2) then
             loc_ij(:,:) = 0.0
          elseif (force_atm_uniform(ia) == 0) then
             loc_ij(force_atm_point_i(ia),force_atm_point_j(ia)) = 0.0
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_atm_'//TRIM(string_atm(ias))//'_I'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   force_restore_atm_I(ia,i,j) = loc_ij(i,j)
                end IF
             end DO
          end DO
          ! load forcing data array #II
          loc_ij(:,:) = const_real_zero
          if (force_atm_uniform(ia) == 2) then
             loc_ij(:,:) = phys_ocn(ipo_mask_ocn,:,:,n_k)
          elseif (force_atm_uniform(ia) == 0) then
             loc_ij(force_atm_point_i(ia),force_atm_point_j(ia)) = 1.0
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_atm_'//TRIM(string_atm(ias))//'_II'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   force_restore_atm_II(ia,i,j) = loc_ij(i,j)
                end IF
             end DO
          end DO
          ! load forcing time series data
          loc_filename = TRIM(par_fordir_name)//'biogem_force_restore_atm_'//TRIM(string_atm(ias))//'_sig'//TRIM(string_data_ext)
          loc_data_scale(:) = (/par_atm_force_scale_time(ias), par_atm_force_scale_val(ias)/)
          CALL sub_load_data_t2(loc_filename,loc_data_scale(:),force_restore_atm_sig(ia,:,:),loc_n_elements)
          ! set default forcing index values
          ! NOTE: catch missing time series data
          if (loc_n_elements == 0) THEN
             CALL sub_report_error( &
                  & 'biogem_data','init_force_restore_atm','PLEASE PUT SOME DATA IN TIME SERIES FILE: '//TRIM(loc_filename), &
                  & 'STOPPING', &
                  & (/const_real_null/),.TRUE. &
                  & )
          else
             force_restore_atm_sig_i(ia,:) = loc_n_elements
          end if
          ! warn if forcing information appears to be 'incomplete'
          ! NOTE: this will catch both possible mismatches of forcing signal and model integration specification
          !       i.e., for _not_ the BP option;
          !             a signal time start year that is after the model start year
          !             or a signal time year that is before the model end year
          !             (or visa versa for a BP time scale)
          if ( &
               & (minval(force_restore_atm_sig(ia,1,1:loc_n_elements)) > 0.0) &
               & .OR. &
               & (maxval(force_restore_atm_sig(ia,1,1:loc_n_elements)) < par_misc_t_runtime) &
               & ) then
             CALL sub_report_error( &
                  & 'biogem_data','sub_init_force_restore_atm', &
                  & 'The time interval of forcing data does not fully span the requested model integration, either; '// &
                  & '(1) alter the model start time year and/or run length (the goin file) or'// &
                  & '(2) alter the forcing signal stop time year (FILE: '//TRIM(loc_filename)//') ', &
                  & 'CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
          end if
       end IF
    end DO
  END SUBROUTINE sub_init_force_restore_atm
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE FLUX FORCING - OCEAN
  SUBROUTINE sub_init_force_flux_ocn()
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_n_elements
    integer::l,i,j,k,io
    real,DIMENSION(n_i,n_j)::loc_ij
    real,DIMENSION(n_i,n_j,n_k)::loc_ijk
    real,DIMENSION(2)::loc_data_scale
    loc_ij = 0.0 ; loc_ijk = 0.0
    ! LOOP
    DO l=1,n_l_ocn
       io = conv_iselected_io(l)
       IF (force_flux_ocn_select(io)) THEN
          force_flux_ocn_sig_i(io,:) = n_data_max
          force_flux_ocn_sig(io,:,:) = 0.0
          ! load forcing data array #I
          loc_ijk(:,:,:) = const_real_zero
          if (force_ocn_uniform(io) == 3) then
             loc_ijk(:,:,:) = 0.0
          elseif (force_ocn_uniform(io) == 2) then
             loc_ijk(:,:,n_k) = 0.0
          elseif (force_ocn_uniform(io) == 0) then
             loc_ijk(force_ocn_point_i(:),force_ocn_point_j(:),force_ocn_point_k(:)) = 0.0
          elseif (force_ocn_uniform(io) == -1) then
             loc_ijk(:,:,n_k) = 0.0
          elseif (force_ocn_uniform(io) == -2) then
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(:,:,goldstein_k1(i,j)) = 0.0
                   end if
                end DO
             end DO
          elseIF ((par_force_point_i > 0) .AND. (par_force_point_j > 0) .AND. (par_force_point_k > 0)) then
             loc_ijk(force_ocn_point_i(:),force_ocn_point_j(:),force_ocn_point_k(:)) = 0.0
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_ocn_'//TRIM(string_ocn(io))//'_I'//TRIM(string_data_ext)
             CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,loc_ijk(:,:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                DO k=force_flux_ocn_k1(io,i,j),n_k
                   force_flux_locn_I(l,i,j,k) = loc_ijk(i,j,k)
                end do
             end do
          end DO
          ! load forcing data array #II
          loc_ijk(:,:,:) = const_real_zero
          if (force_ocn_uniform(io) == 3) then
             loc_ijk(:,:,:) = phys_ocn(ipo_mask_ocn,:,:,:)
          elseif (force_ocn_uniform(io) == 2) then
             loc_ijk(:,:,n_k) = phys_ocn(ipo_mask_ocn,:,:,n_k)
          elseif (force_ocn_uniform(io) == 0) then
             loc_ijk(force_ocn_point_i(io),force_ocn_point_j(io),force_ocn_point_k(io)) = 1.0
          elseif (force_ocn_uniform(io) == -1) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_ocn_'//TRIM(string_ocn(io))//'_SUR'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(i,j,n_k) = loc_ij(i,j)
                   end if
                end do
             end DO
          elseif (force_ocn_uniform(io) == -2) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_ocn_'//TRIM(string_ocn(io))//'_BEN'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
             DO i=1,n_i
                DO j=1,n_j
                   if (goldstein_k1(i,j) <= n_k) then
                      loc_ijk(i,j,goldstein_k1(i,j)) = loc_ij(i,j)
                   end if
                end do
             end DO
          elseIF ((par_force_point_i > 0) .AND. (par_force_point_j > 0) .AND. (par_force_point_k > 0)) then
             loc_ijk(force_ocn_point_i(io),force_ocn_point_j(io),force_ocn_point_k(io)) = 1.0
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_ocn_'//TRIM(string_ocn(io))//'_II'//TRIM(string_data_ext)
             CALL sub_load_data_ijk(loc_filename,n_i,n_j,n_k,loc_ijk(:,:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                DO k=force_flux_ocn_k1(io,i,j),n_k
                   force_flux_locn_II(l,i,j,k) = loc_ijk(i,j,k)
                end do
             end do
          end DO
          ! load forcing time series data
          loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_ocn_'//TRIM(string_ocn(io))//'_sig'//TRIM(string_data_ext)
          loc_data_scale(:) = (/par_ocn_force_scale_time(io), par_ocn_force_scale_val(io)/)
          CALL sub_load_data_t2(loc_filename,loc_data_scale(:),force_flux_ocn_sig(io,:,:),loc_n_elements)
          ! set default forcing index values
          ! NOTE: catch missing time series data
          if (loc_n_elements == 0) THEN
             CALL sub_report_error( &
                  & 'biogem_data','init_force_flux_ocn','PLEASE PUT SOME DATA IN TIME SERIES FILE: '//TRIM(loc_filename), &
                  & 'STOPPING', &
                  & (/const_real_null/),.TRUE. &
                  & )
          else
             force_flux_ocn_sig_i(io,:) = loc_n_elements
          end if
          ! warn if forcing information appears to be 'incomplete'
          ! NOTE: this will catch both possible mismatches of forcing signal and model integration specification
          !       i.e., for _not_ the BP option;a signal time start year that is after the model start year
          !             or a signal time year that is before the model end year (or visa versa for a BP time scale)
          if ( &
               & (minval(force_flux_ocn_sig(io,1,1:loc_n_elements)) > 0.0) &
               & .OR. &
               & (maxval(force_flux_ocn_sig(io,1,1:loc_n_elements)) < par_misc_t_runtime) &
               & ) then
             CALL sub_report_error( &
                  & 'biogem_data','sub_init_force_flux_ocn', &
                  & 'The time interval of forcing data does not fully span the requested model integration, either; '// &
                  & '(1) alter the model start time year and/or run length (the goin file) or '// &
                  & '(2) alter the forcing signal stop time year (FILE: '//TRIM(loc_filename)//') ', &
                  & 'CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
          end if
       end IF
    end DO
  END SUBROUTINE sub_init_force_flux_ocn
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE FLUX FORCING - ATMOSPHERE
  SUBROUTINE sub_init_force_flux_atm()
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_n_elements
    integer::i,j,ia,ias
    real,DIMENSION(n_i,n_j)::loc_ij
    real,DIMENSION(2)::loc_data_scale
    loc_ij = 0.0
    ! LOOP
    DO ia = 3, nt_atm
       ias = ia_ias(ia)
       IF (force_flux_atm_select(ia)) THEN
          force_flux_atm_sig_i(ia,:) = n_data_max
          force_flux_atm_sig(ia,:,:) = 0.0
          ! load forcing data array #I
          loc_ij(:,:) = const_real_zero
          if (force_atm_uniform(ia) == 2) then
             loc_ij(:,:) = 0.0
          elseif (force_atm_uniform(ia) == 0) then
             loc_ij(force_atm_point_i(ia),force_atm_point_j(ia)) = 0.0
          elseif (force_atm_uniform(ia) == -1) then
             loc_ij(:,:) = 0.0
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_atm_'//TRIM(string_atm(ias))//'_I'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   force_flux_atm_I(ia,i,j) = loc_ij(i,j)
                end IF
             end DO
          end DO
          ! load forcing data array #II
          loc_ij(:,:) = const_real_zero
          if (force_atm_uniform(ia) == 2) then
             loc_ij(:,:) = phys_ocn(ipo_mask_ocn,:,:,n_k)
          elseif (force_atm_uniform(ia) == 0) then
             loc_ij(force_atm_point_i(ia),force_atm_point_j(ia)) = 1.0
          elseif (force_atm_uniform(ia) == -1) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_atm_'//TRIM(string_atm(ias))//'_SUR'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_atm_'//TRIM(string_atm(ias))//'_II'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   force_flux_atm_II(ia,i,j) = loc_ij(i,j)
                end IF
             end DO
          end DO
          ! load forcing time series data
          loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_atm_'//TRIM(string_atm(ias))//'_sig'//TRIM(string_data_ext)
          loc_data_scale(:) = (/par_atm_force_scale_time(ias), par_atm_force_scale_val(ias)/)
          CALL sub_load_data_t2(loc_filename,loc_data_scale(:),force_flux_atm_sig(ia,:,:),loc_n_elements)
          ! set default forcing index values
          if (loc_n_elements == 0) THEN
             CALL sub_report_error( &
                  & 'biogem_data','init_force_flux_atm','PLEASE PUT SOME DATA IN TIME SERIES FILE: '//TRIM(loc_filename), &
                  & 'STOPPING', &
                  & (/const_real_null/),.TRUE. &
                  & )
          else
             force_flux_atm_sig_i(ia,:) = loc_n_elements
          end if
          ! warn if forcing information appears to be 'incomplete'
          if ( &
               & (minval(force_flux_atm_sig(ia,1,1:loc_n_elements)) > 0.0) &
               & .OR. &
               & (maxval(force_flux_atm_sig(ia,1,1:loc_n_elements)) < par_misc_t_runtime) &
               & ) then
             CALL sub_report_error( &
                  & 'biogem_data','sub_init_force_flux_atm', &
                  & 'The time interval of forcing data does not fully span the requested model integration, either; '// &
                  & '(1) alter the model start time year and/or run length (the goin file) or '// &
                  & '(2) alter the forcing signal stop time year (FILE: '//TRIM(loc_filename)//') ', &
                  & 'CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
          end if
       end IF
    end DO
  END SUBROUTINE sub_init_force_flux_atm
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE FLUX FORCING - SEDIMENTS
  SUBROUTINE sub_init_force_flux_sed()
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_n_elements
    integer::l,i,j,is
    real,DIMENSION(n_i,n_j)::loc_ij
    real,DIMENSION(2)::loc_data_scale
    loc_ij = 0.0
    ! LOOP
    DO l=1,nt_sed
       is = conv_iselected_is(l)
       IF (force_flux_sed_select(is)) THEN
          force_flux_sed_sig_i(is,:) = n_data_max
          force_flux_sed_sig(is,:,:) = 0.0
          ! load forcing data array #I
          loc_ij(:,:) = 0.0
          if (force_sed_uniform(is) == 2) then
             loc_ij(:,:) = 0.0
          elseif (force_sed_uniform(is) == 0) then
             loc_ij(force_sed_point_i(is),force_sed_point_j(is)) = 0.0
          elseif (force_sed_uniform(is) == -1) then
             loc_ij(:,:) = 0.0
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_sed_'//TRIM(string_sed(is))//'_I'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   force_flux_sed_I(is,i,j) = loc_ij(i,j)
                end IF
             end DO
          end DO
          ! load forcing data array #II
          loc_ij(:,:) = 0.0
          if (force_sed_uniform(is) == 2) then
             loc_ij(:,:) = phys_ocn(ipo_mask_ocn,:,:,n_k)
          elseif (force_sed_uniform(is) == 0) then
             loc_ij(force_sed_point_i(is),force_sed_point_j(is)) = 1.0
          elseif (force_sed_uniform(is) == -1) then
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_sed_'//TRIM(string_sed(is))//'_SUR'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          else
             loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_sed_'//TRIM(string_sed(is))//'_II'//TRIM(string_data_ext)
             CALL sub_load_data_ij(loc_filename,n_i,n_j,loc_ij(:,:))
          end if
          DO i=1,n_i
             DO j=1,n_j
                IF (n_k >= goldstein_k1(i,j)) THEN
                   force_flux_sed_II(is,i,j) = loc_ij(i,j)
                end IF
             end DO
          end DO
          ! load forcing time series data
          loc_filename = TRIM(par_fordir_name)//'biogem_force_flux_sed_'//TRIM(string_sed(is))//'_sig'//TRIM(string_data_ext)
          loc_data_scale(:) = 1.0
          CALL sub_load_data_t2(loc_filename,loc_data_scale(:),force_flux_sed_sig(is,:,:),loc_n_elements)
          ! set default forcing index values
          if (loc_n_elements == 0) THEN
             CALL sub_report_error( &
                  & 'biogem_data','init_force_flux_sed','PLEASE PUT SOME DATA IN TIME SERIES FILE: '//TRIM(loc_filename), &
                  & 'STOPPING', &
                  & (/const_real_null/),.TRUE. &
                  & )
          else
             force_flux_sed_sig_i(is,:) = loc_n_elements
          end if
          ! warn if forcing information appears to be 'incomplete'
          if ( &
               & (minval(force_flux_sed_sig(is,1,1:loc_n_elements)) > 0.0) &
               & .OR. &
               & (maxval(force_flux_sed_sig(is,1,1:loc_n_elements)) < par_misc_t_runtime) &
               & ) then
             CALL sub_report_error( &
                  & 'biogem_data','sub_init_force_flux_sed', &
                  & 'The time interval of forcing data does not fully span the requested model integration, either; '// &
                  & '(1) alter the model start time year and/or run length (the goin file) or'// &
                  & '(2) alter the forcing signal stop time year (FILE: '//TRIM(loc_filename)//') ', &
                  & 'CONTINUING', &
                  & (/const_real_null/),.false. &
                  & )
          end if
       end IF
    end DO
  END SUBROUTINE sub_init_force_flux_sed
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! MISCELLANEOUS ROUTINES
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! OUTPUT AUDIT DIAGNOSTICS
  SUBROUTINE sub_data_audit_diagnostics()
    ! local variables
    INTEGER::l,io
    REAL::loc_ocn_rM
    ! calculate local constants
    loc_ocn_rM = 1.0/SUM(phys_ocn(ipo_M,:,:,:))
    DO l=3,n_l_ocn
       io = conv_iselected_io(l)
       SELECT CASE (io)
          ! \/\/\/ MAKE MODIFICATIONS TO REPORT ADDITIONAL TRACER AUDIT HERE
       CASE (io_DIC,io_NO3,io_PO4,io_Fe,io_O2,io_SiO2,io_ALK,io_Ca,io_B,io_SO4,io_F,io_colr,io_colb)
          PRINT*,'INITIAL / FINAL ',string_ocn(io),' inventory:', &
               & audit_ocn_init(io), &
               & '/', &
               & audit_ocn_new(io), &
               & '( == ', audit_ocn_new(io)*loc_ocn_rM, ')'
       end SELECT
    END DO
  END SUBROUTINE sub_data_audit_diagnostics
  ! ****************************************************************************************************************************** !


  ! ****************************************************************************************************************************** !
  ! INITIALIZE SOLAR CONSTANT FORCING
  SUBROUTINE sub_init_force_solconst()
    ! local variables
    CHARACTER(len=255)::loc_filename
    INTEGER::loc_n_elements
    real,DIMENSION(2)::loc_data_scale
    ! load forcing time series data
    loc_filename = TRIM(par_fordir_name)//'biogem_force_solconst_sig'//TRIM(string_data_ext)
    loc_data_scale(:) = 1.0
    CALL sub_load_data_t2(loc_filename,loc_data_scale(:),force_solconst_sig(:,:),loc_n_elements)
    ! check that time elements are identical to those for time-series data saving
    if (size(force_solconst_sig(1,:)) /= size(par_data_save_sig(:))) THEN
       CALL sub_report_error( &
            & 'biogem_data','sub_init_force_solconst','PLEASE ENSURE THAT THE SAME TIME ELEMENTS ARE PRESENT IN: '&
            & //TRIM(loc_filename)//'AS IN THE TIME-SERIES SPECIFICATION FILE', &
            & 'STOPPING', &
            & (/const_real_null/),.TRUE. &
            & )
    end if
  END SUBROUTINE sub_init_force_solconst
  ! ****************************************************************************************************************************** !



END MODULE biogem_data
