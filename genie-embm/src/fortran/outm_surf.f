c subroutine outm.f writes out data for goldstein last change  6/6/95
c expanded to write out atmos and sea ice data (Bob 10/5/02)
c
c AY (05/02/04) : modified to output surface fluxes
c                 note the organisation of output file (one field after
c                 the other, no clever interlacing)
c
c AY (12/07/05) : removed surplus input argument to function

      subroutine outm_surf(unit,co2_in,
     :     albedo_ocn,usurf_ocn,latent_ocn,sensible_ocn,
     :     netsolar_ocn,netlong_ocn,
     :     evap_ocn,pptn_ocn,runoff_ocn,
     :     latent_atm,sensible_atm,
     :     netsolar_atm,netlong_atm,
     :     evap_atm,pptn_atm,
     :     dhght_sic,darea_sic)

#include "embm.cmn"

      integer unit

      real co2_in(imax,jmax), usurf_ocn(imax,jmax)

      real
     :     albedo_ocn(imax,jmax),
     :     latent_ocn(imax,jmax),sensible_ocn(imax,jmax),
     :     netsolar_ocn(imax,jmax),netlong_ocn(imax,jmax),
     :     evap_ocn(imax,jmax),pptn_ocn(imax,jmax),
     :     runoff_ocn(imax,jmax),
     :     latent_atm(imax,jmax), sensible_atm(imax,jmax),
     :     netsolar_atm(imax,jmax),netlong_atm(imax,jmax),
     :     evap_atm(imax,jmax),pptn_atm(imax,jmax),
     :     dhght_sic(imax,jmax),darea_sic(imax,jmax)

            write(unit,10 )latent_ocn
            write(unit,10 )sensible_ocn
            write(unit,10 )netsolar_ocn
            write(unit,10 )netlong_ocn
            write(unit,10 )evap_ocn
            write(unit,10 )pptn_ocn
            write(unit,10 )runoff_ocn
            write(unit,10 )albedo_ocn
            write(unit,10 )usurf_ocn
            write(unit,10 )latent_atm
            write(unit,10 )sensible_atm
            write(unit,10 )netsolar_atm
            write(unit,10 )netlong_atm
            write(unit,10 )evap_atm
            write(unit,10 )pptn_atm
            write(unit,10 )co2_in
            write(unit,10 )dhght_sic
            write(unit,10 )darea_sic

  10  format(e21.13)
      end
