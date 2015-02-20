! ******************************************************************************************************************************** !
! sedgem_box_benthic.f90
! Sandra Arndt sediment diagenesis routines
! ******************************************************************************************************************************** !


MODULE sedgem_box_benthic


  use genie_control
  USE sedgem_lib
  IMPLICIT NONE
  SAVE


CONTAINS


  function fun_arndtetal2013_sedflx(dum_frac_POC)

    ! result variable
    real::fun_arndtetal2013_sedflx

    ! dummy variables
    REAL,INTENT(IN)::dum_frac_POC

    ! local variables
real*8 wdepth, oc1_0, oc2_0
real*8 w, dbio, zbio, zbur
real*8 doc1_1, doc2_1, doc1_2, doc2_2
real*8 koc1, koc2
real*8 a_oc1_1, a_oc2_1, a_oc1_2, a_oc2_2
real*8 aa_oc1_1, aa_oc2_1, aa_oc1_2, aa_oc2_2
real*8 bb_oc1_1, bb_oc2_1
real*8 oc1_zbur, oc2_zbur
real*8 f_oc1, f_oc2
!__________________________________________________________

!initalize
!__________________________________________________________

!THE FOLLOWING VALUES WILL BE PASSED DOWN FROM GENIE
! *****************************************************************
!water depth [m]
wdepth=2000

!boundary concentrations [mol cm-3]
oc1_0=0.1/12.0*2.5! first organic matter fraction
oc2_0=0.1/12.0*2.5! second organic matter fraction
!
! *****************************************************************

!burial velocity [cm yr^-1](Middelburg et al., Deep Sea Res. 1, 1997)
w=10.0**(-0.87478367-0.00043512*wdepth)*3.3

!"burial depth" [cm]depth at wich burial fluxes are calculated, ensure zbio<zbur
zbur=100

!bioturbation depths [cm](Boudreau, 1997)
zbio=10

!bioturbation coefficient [cm^2 yr^-1](Pb210-based, Middelburg et al., Deep Sea Res. 1, 1997)
dbio=10.0**(0.76241122-0.00039724*wdepth)*5.2

!dispersion coefficients [cm^2 yr^-1]
doc1_1=dbio! first organic matter fraction, bioturbated layer
doc2_1=dbio! second organic matter fraction, bioturbated layer
doc1_2=0.0! first organic matter fraction, non-bioturbated layer
doc2_2=0.0! second organic matter fraction, non-bioturbated layer

!organic carbon degradation rate constant [yr-1]
koc1=0.01! first organic matter fraction
koc2=0.001! second organic matter fraction


!__________________________________________________________

!calculate benthic burial/recycling fluxes (see documentation for details!)
!__________________________________________________________

!organic matter burial

!calculate integration constants and parameters
aa_oc1_1=(w-sqrt(w**2.0+4.0*doc1_1*koc1))/(2.0*doc1_1)
bb_oc1_1=(w+sqrt(w**2.0+4.0*doc1_1*koc1))/(2.0*doc1_1)
aa_oc1_2=(-koc1/w)
a_oc1_1=-(oc1_0*bb_oc1_1*exp(bb_oc1_1*zbio))/(aa_oc1_1*exp(aa_oc1_1*zbio)-bb_oc1_1*exp(bb_oc1_1*zbio))
a_oc1_2=(a_oc1_1*(exp(aa_oc1_1*zbio) - exp(bb_oc1_1*zbio))+oc1_0*exp(bb_oc1_1*zbio))/exp(aa_oc1_2*zbio)


aa_oc2_1=(w-sqrt(w**2.0+4.0*doc2_1*koc2))/(2.0*doc2_1)
bb_oc2_1=(w+sqrt(w**2.0+4.0*doc2_1*koc2))/(2.0*doc2_1)
aa_oc2_2=(-koc2/w)
a_oc2_1=-(oc2_0*bb_oc2_1*exp(bb_oc2_1*zbio))/(aa_oc2_1*exp(aa_oc2_1*zbio)-bb_oc2_1*exp(bb_oc2_1*zbio))
a_oc2_2=(a_oc2_1*(exp(aa_oc2_1*zbio) - exp(bb_oc2_1*zbio))+oc2_0*exp(bb_oc2_1*zbio))/exp(aa_oc2_2*zbio)


!calculate concentration at "burial depth" zburial
oc1_zbur=a_oc1_2*exp(aa_oc1_2*zbur)
oc2_zbur=a_oc2_2*exp(aa_oc2_2*zbur)

!THE FOLLOWING VALUES WILL BE PASSED TO GENIE
! *****************************************************************
!calculate buried oc fractions
f_oc1=oc1_zbur/oc1_0
f_oc2=oc2_zbur/oc2_0

write(*,*) '% of deposited OC buried'
write(*,*) 'OC1:', f_oc1*100, 'OC2:', f_oc2*100
! *****************************************************************

    !
    fun_arndtetal2013_sedflx = 0.0

end function fun_arndtetal2013_sedflx

END MODULE sedgem_box_benthic
