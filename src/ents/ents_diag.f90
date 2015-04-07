MODULE ents_diag

  USE ents_lib
  USE ents_netcdf
  IMPLICIT NONE

CONTAINS

  ! Makes a file averaged over the last year for ENTS model.  Files
  ! written every ianav timesteps.
  SUBROUTINE annav_diags(istep, iout, dum_fx0a, dum_fx0o, dum_fxsen, dum_fxlw, &
       & dum_evap, dum_pptn, dum_relh, albs_lnd, land_snow_lnd)
    USE netcdf
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: istep, iout
    REAL, DIMENSION(:,:), INTENT(IN) :: &
         & dum_fx0a, dum_fx0o, dum_fxsen, dum_fxlw, dum_evap, dum_pptn, &
         & dum_relh, albs_lnd, land_snow_lnd

    INTEGER :: l, i, j, yearosc
    REAL :: rnyear, Gtatm, Gtocn
    REAL :: cfavg(8,maxi,maxj), sumavg(8), avgsl(8)
    REAL :: pfavg(13,maxi,maxj), sumavg2(13), avgsl2(13)
    CHARACTER(LEN=200) :: filename, fname, label, refname
    REAL, DIMENSION(:,:,:), ALLOCATABLE :: var_data

    CHARACTER(LEN=10), DIMENSION(8) :: labels = &
         & (/ 'sphoto   ', 'srespveg ', 'sleaf    ', 'srespsoil', &
         &    'sCveg    ', 'sCsoil   ', 'sfv      ', 'sepsv    '  /)
    CHARACTER(LEN=10), DIMENSION(9) :: avlabels = &
         & (/ 'avgsl1', 'avgsl2', 'avgsl3', 'avgsl4', 'avgsl5', &
         &    'avgsl6', 'avgsl7', 'avgsl8', 'Gtatm2' /)
    CHARACTER(LEN=10), DIMENSION(13) :: avlabels2 = &
         & (/ '1avgs1  ', '2avgs2  ', '3avgs3  ', '4avgs4  ', '5avgs5  ', &
         &    '6avgs6  ', '7avgs7  ', '8avgs8  ', '9avgs9  ','10avgs10',  &
         &   '11avgs11', '12avgs12', '13avgs13' /)

    INTEGER :: kk, myyear, ncid, var_id, myday, timedim_id
    LOGICAL :: fexist
    REAL :: var_data1

    IF (dosc) THEN
       filename = TRIM(outdir_name) // TRIM(ents_out_name) // '.slavgt'
       OPEN(46,FILE=TRIM(filename),POSITION='APPEND')
       filename = TRIM(outdir_name) // TRIM(ents_out_name) // '.pslavgt'
       OPEN(47,FILE=TRIM(filename),POSITION='APPEND')

       ! Sum up quantities since last .avg calculation
       pco2ld_tot = pco2ld_tot + pco2ld
       DO i = 1, maxi
          DO j = 1, maxj
             IF (ents_k1(i,j) > ents_kmax) THEN
                ! Carbon diagnostics
                sphoto(i,j) = sphoto(i,j) + photo(i,j)
                srveg(i,j) = srveg(i,j) + respveg(i,j)
                sleaf(i,j) = sleaf(i,j) + leaf(i,j)
                srsoil(i,j) = srsoil(i,j) + respsoil(i,j)
                sCveg1(i,j) = sCveg1(i,j) + Cveg(i,j)
                sCsoil1(i,j) = sCsoil1(i,j) + Csoil(i,j)
                sfv1(i,j) = sfv1(i,j) + fv(i,j)
                sepsv1(i,j) = sepsv1(i,j) + epsv(i,j)
                ! Physical diagnostics
                ! temp and water
                stqld(1,i,j) = stqld(1,i,j) + tqld(1,i,j)
                stqld(2,i,j) = stqld(2,i,j) + tqld(2,i,j)
                ! heat fluxes
                sfx0a(i,j) = sfx0a(i,j) + dum_fx0a(i,j)
                sfx0o(i,j) = sfx0o(i,j) + dum_fx0o(i,j)
                sfxsens(i,j) = sfxsens(i,j) + dum_fxsen(i,j)
                sfxlw(i,j) = sfxlw(i,j) + dum_fxlw(i,j)
                ! water fluxes
                sevap(i,j) = sevap(i,j) + dum_evap(i,j)
                spptn(i,j) = spptn(i,j) + dum_pptn(i,j)
                srelh(i,j) = srelh(i,j) + dum_relh(i,j)
                ! other quantities
                sbcap(i,j) = sbcap(i,j) + bcap(i,j)
                salbs(i,j) = salbs(i,j) + albs_lnd(i,j)
                ssnow(i,j) = ssnow(i,j) + land_snow_lnd(i,j)
                sz0(i,j) = sz0(i,j) + z0(i,j)
             END IF
          END DO
       END DO

       rnyear = 1.0 / ents_nyear
       IF (iout == 1) THEN
          pco2ld_tot = pco2ld_tot + pco2ld
          sumavg = 0.0
          sumavg2 = 0.0
          DO i = 1, maxi
             DO j = 1, maxj
                IF (ents_k1(i,j) > ents_kmax) THEN
                   ! Average over time
                   cfavg(1,i,j) = sphoto(i,j) * rnyear
                   cfavg(2,i,j) = srveg(i,j) * rnyear
                   cfavg(3,i,j) = sleaf(i,j) * rnyear
                   cfavg(4,i,j) = srsoil(i,j) * rnyear
                   cfavg(5,i,j) = sCveg1(i,j) * rnyear
                   cfavg(6,i,j) = sCsoil1(i,j) * rnyear
                   cfavg(7,i,j) = sfv1(i,j) * rnyear
                   cfavg(8,i,j) = sepsv1(i,j) * rnyear
                   pfavg(1,i,j) = stqld(1,i,j) * rnyear
                   pfavg(2,i,j) = stqld(2,i,j) * rnyear
                   pfavg(3,i,j) = sfx0a(i,j) * rnyear
                   pfavg(4,i,j) = sfx0o(i,j) * rnyear
                   pfavg(5,i,j) = sfxsens(i,j) * rnyear
                   pfavg(6,i,j) = sfxlw(i,j) * rnyear
                   pfavg(7,i,j) = sevap(i,j) * rnyear
                   pfavg(8,i,j) = spptn(i,j) * rnyear
                   pfavg(9,i,j) = srelh(i,j) * rnyear
                   pfavg(10,i,j) = sbcap(i,j) * rnyear
                   pfavg(11,i,j) = salbs(i,j) * rnyear
                   pfavg(12,i,j) = ssnow(i,j) * rnyear
                   pfavg(13,i,j) = sz0(i,j) * rnyear
                   ! Land fluxes
                   sumavg(1) = sumavg(1) + cfavg(1,i,j)
                   sumavg(2) = sumavg(2) + cfavg(2,i,j)
                   sumavg(3) = sumavg(3) + cfavg(3,i,j)
                   sumavg(4) = sumavg(4) + cfavg(4,i,j)
                   ! Carbon reservoirs
                   sumavg(5) = sumavg(5) + cfavg(5,i,j)
                   sumavg(6) = sumavg(6) + cfavg(6,i,j)
                   sumavg(7) = sumavg(7) + cfavg(7,i,j)
                   sumavg(8) = sumavg(8) + cfavg(8,i,j)
                   ! Physical quantities
                   sumavg2(1) = sumavg2(1) + pfavg(1,i,j)
                   sumavg2(2) = sumavg2(2) + pfavg(2,i,j)
                   sumavg2(3) = sumavg2(3) + pfavg(3,i,j)
                   sumavg2(4) = sumavg2(4) + pfavg(4,i,j)
                   sumavg2(5) = sumavg2(5) + pfavg(5,i,j)
                   sumavg2(6) = sumavg2(6) + pfavg(6,i,j)
                   sumavg2(7) = sumavg2(7) + pfavg(7,i,j)
                   sumavg2(8) = sumavg2(8) + pfavg(8,i,j)
                   sumavg2(9) = sumavg2(9) + pfavg(9,i,j)
                   sumavg2(10) = sumavg2(10) + pfavg(10,i,j)
                   sumavg2(11) = sumavg2(11) + pfavg(11,i,j)
                   sumavg2(12) = sumavg2(12) + pfavg(12,i,j)
                   sumavg2(13) = sumavg2(13) + pfavg(13,i,j)
                ELSE
                   cfavg(1,i,j) = -99999.
                   cfavg(2,i,j) = -99999.
                   cfavg(3,i,j) = -99999.
                   cfavg(4,i,j) = -99999.
                   cfavg(5,i,j) = -99999.
                   cfavg(6,i,j) = -99999.
                   cfavg(7,i,j) = -99999.
                   cfavg(8,i,j) = -99999.
                END IF
             END DO
          END DO

          yearosc = istep / ents_nyear

          ! Convert to GtC
          ! land
          avgsl = sumavg  *rgtk * asurfrea
          avgsl(7) = sumavg(7) / REAL(land_pts_ents)
          avgsl(8) = sumavg(8) / REAL(land_pts_ents)
          ! atmosphere
          Gtatm = (pco2ld_tot * k_a) * rgtm * mtp * rnyear
          ! ocean
          Gtocn = tot_mass_ocn_c * rgtk * rnyear

          ! Write to a timeseries file
          WRITE (46,'(11e24.16)') REAL(yearosc) - 0.5, avgsl(1), avgsl(2), &
               & avgsl(3), avgsl(4), avgsl(5), avgsl(6), avgsl(7), &
               & avgsl(8), Gtatm

          ! Days since the beginning of the run
          myday = INT(ents_yearlen * yearosc)
          fname = TRIM(outdir_name) // TRIM(ents_out_name) // '_TSannual.nc'

          DO kk = 1, 8
             label = avlabels(kk)
             var_data1 = avgsl(kk)
             CALL netcdf_ts_ents(fname, var_data1, label, myday)
          END DO
          var_data1 = Gtatm
          label = avlabels(9)
          CALL netcdf_ts_ents(fname, var_data1, label, myday)

          ! For physical timeseries average down
          avgsl2 = sumavg2 / REAL(land_pts_ents)

          ! write to file
          WRITE (47,'(14e24.16)') REAL(yearosc) - 0.5, avgsl2(1), avgsl2(2), &
               & avgsl2(3), avgsl2(4), avgsl2(5), avgsl2(6), avgsl2(7), &
               & avgsl2(8), avgsl2(9), avgsl2(10), avgsl2(11), avgsl2(12), &
               & avgsl2(13)

          DO kk = 1, 13
             label = avlabels2(kk)
             var_data1 = avgsl2(kk)
             CALL netcdf_ts_ents(fname, var_data1, label, myday)
          END DO

          ! Spatial average file
          filename = TRIM(outdir_name) // TRIM(ents_out_name) // '.sland.avg'
          OPEN(1,FILE=TRIM(filename))

          ! Write to file
          DO l = 1, 8
             DO i = 1, maxi
                DO j = 1, maxj
                   WRITE (1,10) cfavg(l,i,j)
                END DO
             END DO
          END DO
          WRITE (1,10) REAL(yearosc)
10        FORMAT(e14.4e3)
          CLOSE(1)

          IF (MOD(istep, ents_ianav) == 0 .AND. istep >= ents_ianav) THEN
             myyear = INT(yearosc)
             refname = TRIM(outdir_name) // TRIM(ents_out_name) // &
                  & '_restartav_' // TRIM(ConvertFunc(myyear, 10)) // '.nc'
             INQUIRE(FILE=refname,EXIST=fexist)
             IF (fexist) THEN
                OPEN(8,FILE=refname,STATUS='old')
                CLOSE(8,STATUS='delete')
             END IF

             DO kk = 1, 8
                ALLOCATE(var_data(1,maxj,maxi))
                label = labels(kk)
                DO j = 1, maxj
                   DO i = 1, maxi
                      SELECT CASE (kk)
                      CASE (1)
                         var_data(1,j,i) = cfavg(1,i,j)
                      CASE (2)
                         var_data(1,j,i) = cfavg(2,i,j)
                      CASE (3)
                         var_data(1,j,i) = cfavg(3,i,j)
                      CASE (4)
                         var_data(1,j,i) = cfavg(4,i,j)
                      CASE (5)
                         var_data(1,j,i) = cfavg(5,i,j)
                      CASE (6)
                         var_data(1,j,i) = cfavg(6,i,j)
                      CASE (7)
                         var_data(1,j,i) = cfavg(7,i,j)
                      CASE (8)
                         var_data(1,j,i) = cfavg(8,i,j)
                      END SELECT
                   END DO
                END DO
                CALL netcdf_ents(refname, var_data, label, myday)
                DEALLOCATE(var_data)
             END DO

             ! Adding final restart value (single)
             CALL check_err(NF90_OPEN(refname, NF90_WRITE, ncid))
             CALL check_err(NF90_REDEF(ncid))
             CALL check_err(NF90_INQ_DIMID(ncid, 'time', timedim_id))
             CALL check_err(NF90_DEF_VAR(ncid, 'yearosc', &
                  & NF90_FLOAT, (/ timedim_id /), var_id))
             CALL check_err(NF90_PUT_ATT(ncid, var_id, 'long_name', 'yearosc'))
             CALL check_err(NF90_ENDDEF(ncid))
             CALL check_err(NF90_PUT_VAR(ncid, var_id, REAL(yearosc)))
             CALL check_err(NF90_CLOSE(ncid))
          END IF

          ! Zero arrays ready for next average
          sphoto = 0.0 ; srveg = 0.0 ; sleaf = 0.0 ; srsoil = 0.0
          sCveg1 = 0.0 ; sCsoil1 = 0.0 ; sfv1 = 0.0 ; sepsv1 = 0.0
          cfavg = 0.0
          stqld = 0.0
          sfx0a = 0.0 ; sfx0o = 0.0 ; sfxsens = 0.0 ; sfxlw = 0.0
          sevap = 0.0 ; spptn = 0.0 ; srelh = 0.0
          sbcap = 0.0 ; salbs = 0.0 ; ssnow = 0.0 ; sz0 = 0.0
          pfavg = 0.0
          tot_mass_ocn_c=0.
          pco2ld_tot=0.
       END IF
    END IF
  END SUBROUTINE annav_diags


  ! Write out reservoir sizes every itstp timesteps
  SUBROUTINE carbt_diags(istep)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: istep

    INTEGER :: i, j
    REAL :: diagtime, sumveg, sumsoil, sumfv
    REAL :: sumphoto, sumrveg, sumrsoil, sumleaf
    REAL :: Gtveg, Gtsoil, Gtatm, Gfv
    REAL :: Gtphoto, Gtrveg, Gtrsoil, Gtleaf
    CHARACTER(LEN=200) :: filename, fname, label
    REAL :: var_data

    CHARACTER(LEN=8), DIMENSION(9) :: labels = &
         & (/ 'Gtveg  ', 'Gtsoil ', 'Gtatm  ', 'Gfv    ', 'pco2ld ', &
         &    'Gtphoto', 'Gtrveg ', 'Gtleaf ', 'Gtrsoil' /)
    INTEGER :: kk, myday
    LOGICAL :: fexist

    ! Open slandt file for diagnostics
    filename = TRIM(outdir_name) // TRIM(ents_out_name) // '.slandt'
    OPEN(43,FILE=TRIM(filename),POSITION='APPEND')

    diagtime = REAL(istep) / REAL(ents_nyear)

    sumveg = 0.0 ; sumsoil = 0.0 ; sumfv = 0.0
    sumphoto = 0.0 ; sumrveg = 0.0 ; sumrsoil = 0.0 ; sumleaf = 0.0

    ! Sum up all carbon spatially in each reservoir
    DO i = 1, maxi
       DO j = 1, maxj
          IF (ents_k1(i,j) > ents_kmax) THEN
             sumveg = sumveg + Cveg(i,j)
             sumsoil = sumsoil + Csoil(i,j)
             sumfv = sumfv + fv(i,j)
             sumphoto = sumphoto + photo(i,j)
             sumrveg = sumrveg + respveg(i,j)
             sumrsoil = sumrsoil + respsoil(i,j)
             sumleaf = sumleaf + leaf(i,j)
          END IF
       END DO
    END DO

    ! Convert back to GtC
    Gtveg = sumveg * rgtk * asurfrea
    Gtsoil = sumsoil * rgtk * asurfrea
    Gtatm = pco2ld * k_a * rgtm * mtp

    ! Covert back to average fraction
    Gfv = sumfv / land_pts_ents

    ! Convert to GtC/yr
    Gtphoto = sumphoto * rgtk * asurfrea
    Gtrveg = sumrveg * rgtk * asurfrea
    Gtrsoil = sumrsoil * rgtk * asurfrea
    Gtleaf = sumleaf * rgtk * asurfrea

    ! Write to file
    WRITE (43,'(10e24.16)') diagtime, Gtveg, Gtsoil, Gtatm, Gfv, pco2ld, &
         & Gtphoto, Gtrveg, Gtleaf, Gtrsoil
    CLOSE (43)

    myday = INT(360 * istep / ents_nyear)
    fname = TRIM(outdir_name) // TRIM(ents_out_name) // '_TS.nc'
    IF (istep == ents_itstp) THEN
       INQUIRE(FILE=fname,EXIST=fexist)
       IF (fexist) THEN
          OPEN(8,FILE=fname,STATUS='old')
          CLOSE(8,STATUS='delete')
       END IF
    END IF
    DO kk = 1, 9
       label = labels(kk)
       SELECT CASE (kk)
       CASE (1)
          var_data = Gtveg
       CASE (2)
          var_data = Gtsoil
       CASE (3)
          var_data = Gtatm
       CASE (4)
          var_data = Gfv
       CASE (5)
          var_data = pco2ld
       CASE (6)
          var_data = Gtphoto
       CASE (7)
          var_data = Gtrveg
       CASE (8)
          var_data = Gtleaf
       CASE (9)
          var_data = Gtrsoil
       END SELECT
       CALL netcdf_ts_ents(fname, var_data, label, myday)
    END DO
  END SUBROUTINE carbt_diags


  ! Instantaneous globally averaged values.  Produces file *.plandt of
  ! exactly same quantities as *.pslavgt but at a snap shot in time
  ! rather than annually averaged.
  SUBROUTINE physt_diags(istep, dum_fx0a, dum_fx0o, &
       & dum_fxsen, dum_fxlw, dum_evap, dum_pptn, dum_relh, &
       & albs_lnd, land_snow_lnd)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: istep
    REAL, DIMENSION(:,:), INTENT(IN) :: &
         & dum_fx0a, dum_fx0o, dum_fxsen, dum_fxlw, dum_evap, dum_pptn, &
         & dum_relh, albs_lnd, land_snow_lnd

    REAL :: diagtime, sumavg2(13), avgsl2(13)
    INTEGER :: i, j
    CHARACTER(LEN=200) :: filename, fname, label
    REAL :: var_data
    CHARACTER(LEN=6), DIMENSION(13) :: labels = &
         & (/ 'avgs1 ','avgs2 ','avgs3 ','avgs4 ','avgs5 ', &
         &    'avgs6 ','avgs7 ','avgs8 ','avgs9 ','avgs10', &
         &    'avgs11','avgs12','avgs13' /)
    INTEGER :: kk, myday

    ! Open physt_diags file for disgnostics
    filename = TRIM(outdir_name) // TRIM(ents_out_name) // '.pslandt'
    OPEN(48,FILE=TRIM(filename),POSITION='APPEND')

    diagtime = REAL(istep) / REAL(ents_nyear)
    sumavg2 = 0.0
    DO i = 1, maxi
       DO j = 1, maxj
          IF (ents_k1(i,j) > ents_kmax) THEN
             sumavg2(1) = sumavg2(1) + tqld(1,i,j)
             sumavg2(2) = sumavg2(2) + tqld(2,i,j)
             sumavg2(3) = sumavg2(3) + dum_fx0a(i,j)
             sumavg2(4) = sumavg2(4) + dum_fx0o(i,j)
             sumavg2(5) = sumavg2(5) + dum_fxsen(i,j)
             sumavg2(6) = sumavg2(6) + dum_fxlw(i,j)
             sumavg2(7) = sumavg2(7) + dum_evap(i,j)
             sumavg2(8) = sumavg2(8) + dum_pptn(i,j)
             sumavg2(9) = sumavg2(9) + dum_relh(i,j)
             sumavg2(10) = sumavg2(10) + bcap(i,j)
             sumavg2(11) = sumavg2(11) + albs_lnd(i,j)
             sumavg2(12) = sumavg2(12) + land_snow_lnd(i,j)
             sumavg2(13) = sumavg2(13) + z0(i,j)
          END IF
       END DO
    END DO

    ! For physical timeseries average down
    avgsl2 = sumavg2 / REAL(land_pts_ents)

    ! write to file
    WRITE (48,'(15e24.16)') diagtime, avgsl2(1), avgsl2(2), &
         & avgsl2(3), avgsl2(4), avgsl2(5), avgsl2(6), avgsl2(7), &
         & avgsl2(8), avgsl2(9), avgsl2(10), avgsl2(11), avgsl2(12), &
         & avgsl2(13)
    CLOSE(48)

    myday = INT(360 * istep / ents_nyear)
    fname = TRIM(outdir_name) // TRIM(ents_out_name) // '_TS.nc'

    DO kk = 1, 13
       label = labels(kk)
       var_data = avgsl2(kk)
       CALL netcdf_ts_ents(fname, var_data, label, myday)
    END DO
  END SUBROUTINE physt_diags


  ! Diagnostics for ENTS: prints to the screen.
  SUBROUTINE screen_diags
    IMPLICIT NONE

    REAL :: amin, amax, tot_h2o, tot_c, tot_v, tot_s, tot_a
    INTEGER :: iamin, iamax, jamin, jamax

    PRINT *

    CALL aminmaxl(maxi, maxj, ents_kmax, ents_k1(:,:), tqld(1,:,:), &
         & amin, amax, iamin, iamax, jamin, jamax)
    PRINT *, 'min land T ', amin, ' at ', iamin, jamin
    PRINT *, 'max land T ', amax, ' at ', iamax, jamax

    call aminmaxl(maxi, maxj, ents_kmax, ents_k1(:,:), tqld(2,:,:), &
         & amin, amax, iamin, iamax, jamin, jamax)
    PRINT *, 'min land q ', amin, ' at ', iamin, jamin
    PRINT *, 'max land q ', amax, ' at ', iamax, jamax

    call slnd_h2o_invent(tot_h2o)
    PRINT *, 'Total water on land is ', tot_h2o, '(*10^12 m^3)'

    call slnd_c_invent(tot_c, tot_v, tot_s, tot_a)
    PRINT *, 'Total carbon is ', tot_c, '(GtC)'
    PRINT *, tot_v, '(GtC) veg'
    PRINT *, tot_s, '(GtC) soil'
    PRINT *, tot_a, '(GtC) atm'
  END SUBROUTINE screen_diags

  SUBROUTINE aminmaxl(maxi, maxj, ents_kmax, k1, a, amin, amax, &
       & iamin, iamax, jamin, jamax)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: maxi, maxj, ents_kmax
    INTEGER, DIMENSION(:,:), INTENT(IN) :: k1
    REAL, DIMENSION(:,:), INTENT(IN) :: a
    REAL, INTENT(OUT) :: amin, amax
    INTEGER, INTENT(OUT) :: iamin, iamax, jamin, jamax

    INTEGER :: i, j

    amin = a(1,1) ; amax = a(1,1)
    iamin = 1 ; iamax = 1 ; jamin = 1 ; jamax = 1

    DO j = 1, maxj
       DO i = 1, maxi
          IF (k1(i,j) > ents_kmax) THEN
             IF (a(i,j) < amin) THEN
                amin = a(i,j)
                iamin = i
                jamin = j
             END IF
             IF (a(i,j) > amax) THEN
                amax = a(i,j)
                iamax = i
                jamax = j
             END IF
          END IF
       END DO
    END DO
  END SUBROUTINE aminmaxl


  SUBROUTINE slnd_h2o_invent(sumh2o)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: sumh2o

    sumh2o = SUM(tqld(2,:,:), MASK=(ents_k1 > ents_kmax))
    sumh2o = sumh2o * asurfrea * 1.0E-12
  END SUBROUTINE slnd_h2o_invent


  SUBROUTINE slnd_c_invent(sumc, sumv, sums, suma)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: sumc, sumv, sums, suma

    sumv = SUM(Cveg, MASK=(ents_k1 > ents_kmax))
    sums = SUM(Csoil, MASK=(ents_k1 > ents_kmax))
    IF (ANY(ents_k1 > ents_kmax)) THEN
       suma = pco2ld * k_a * mu * mtp * rasurf
    ELSE
       suma = 0.0
    END IF

    sumv = sumv * asurfrea * 1.0E-12
    sums = sums * asurfrea * 1.0E-12
    suma = suma * asurfrea * 1.0E-12
    sumc = sumv + sums + suma
  END SUBROUTINE slnd_c_invent


  ! Extra diagnostic routine for c-goldstein v2 with seasonal cycle
  ! calculate average over nyear timesteps.
  SUBROUTINE entsdiagosc(nyear, istep, iout, albs_lnd, land_snow_lnd)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nyear, istep, iout
    REAL, DIMENSION(:,:), INTENT(IN) :: albs_lnd, land_snow_lnd

    REAL :: rnyear
    INTEGER :: i, j
    CHARACTER(LEN=200) :: fname, label
    REAL, DIMENSION(:,:,:), ALLOCATABLE :: var_data
    INTEGER :: myyear, mymonth, myday
    LOGICAL :: fexist

    IF (dosc) THEN
       rnyear = 1.0 / nyear
       tqldavg(1,:,:) = tqldavg(1,:,:) + tqld(1,:,:) * rnyear
       tqldavg(2,:,:) = tqldavg(2,:,:) + tqld(2,:,:) * rnyear
       snowavg = snowavg + REAL(land_snow_lnd) * rnyear
       albsavg = albsavg + albs_lnd * rnyear
       bcapavg = bcapavg + bcap * rnyear
       z0avg = z0avg + z0 * rnyear

       IF (iout == 1 .AND. MOD(istep, ents_ianav) == 0 &
            & .AND. istep >= ents_ianav) THEN
          PRINT *, 'writing averaged data at istep ', istep
          OPEN(15,FILE=TRIM(outdir_name) // TRIM(ents_out_name) // '.ltavg')

          myyear = INT(istep / ents_nyear)
          mymonth = INT(12 * MOD(istep, ents_nyear) / ents_nyear)
          myday = INT(360 * istep / ents_nyear - &
               & mymonth * 30 - (myyear - 1) * 360)

          fname = TRIM(outdir_name) // TRIM(ents_out_name) // &
               & '_yearav_' // TRIM(ConvertFunc(myyear, 10)) // '.nc'

          INQUIRE(FILE=fname,EXIST=fexist)
          IF (fexist) THEN
             OPEN(8,FILE=fname,STATUS='old')
             CLOSE(8,STATUS='delete')
          END IF

          ALLOCATE(var_data(1,maxj,maxi))
          DO j = 1, maxj
             DO i = 1, maxi
                WRITE (15,10) tqldavg(1,i,j)
                var_data(1,j,i) = tqldavg(1,i,j)
             END DO
          END DO
          CLOSE(15)
          label = 'ltavg'
          CALL netcdf_ents(fname, var_data, label, myday)
          DEALLOCATE(var_data)

          OPEN(15,FILE=TRIM(outdir_name) // TRIM(ents_out_name) // '.lqavg')
          ALLOCATE(var_data(1,maxj,maxi))
          DO j = 1, maxj
             DO i = 1, maxi
                WRITE (15,10) tqldavg(2,i,j)
                var_data(1,j,i) = tqldavg(2,i,j)
             END DO
          END DO
          CLOSE(15)
          label = 'lqavg'
          CALL netcdf_ents(fname, var_data, label, myday)
          DEALLOCATE(var_data)

          OPEN(15,FILE=TRIM(outdir_name) // TRIM(ents_out_name) // '.snowavg')
          ALLOCATE(var_data(1,maxj,maxi))
          DO j = 1, maxj
             DO i = 1, maxi
                WRITE (15,10) snowavg(i,j)
                var_data(1,j,i) = snowavg(i,j)
             END DO
          END DO
          CLOSE(15)
          label = 'snowavg'
          CALL netcdf_ents(fname, var_data, label, myday)
          DEALLOCATE(var_data)

          OPEN(15,FILE=TRIM(outdir_name) // TRIM(ents_out_name) // '.z0avg')
          ALLOCATE(var_data(1,maxj,maxi))
          DO j = 1, maxj
             DO i = 1, maxi
                WRITE (15,10) z0avg(i,j)
                var_data(1,j,i) = z0avg(i,j)
             END DO
          END DO
          CLOSE(15)
          label = 'z0avg'
          CALL netcdf_ents(fname, var_data, label, myday)
          DEALLOCATE(var_data)

          OPEN(15,FILE=TRIM(outdir_name) // TRIM(ents_out_name) // '.albsavg')
          ALLOCATE(var_data(1,maxj,maxi))
          DO j = 1, maxj
             DO i = 1, maxi
                WRITE (15,10) albsavg(i,j)
                var_data(1,j,i) = albsavg(i,j)
             END DO
          END DO
          CLOSE(15)
          label = 'albsavg'
          CALL netcdf_ents(fname, var_data, label, myday)
          DEALLOCATE(var_data)

          OPEN(15,FILE=TRIM(outdir_name) // TRIM(ents_out_name) // '.bcapavg')
          ALLOCATE(var_data(1,maxj,maxi))
          DO j = 1, maxj
             DO i = 1, maxi
                WRITE (15,10) bcapavg(i,j)
                var_data(1,j,i) = bcapavg(i,j)
             END DO
          END DO
          CLOSE(15)
          label = 'bcapavg'
          CALL netcdf_ents(fname, var_data, label, myday)
          DEALLOCATE(var_data)

          OPEN(45,FILE=TRIM(outdir_name) // TRIM(ents_out_name) // '.gmairt')
          WRITE (45,'(2e18.7)') REAL(istep / nyear) - 0.5, &
               & gmairttot / (REAL(maxi * maxj))

          ! perform diagnostics on averaged data, either by rewriting
          ! other diag routines to accept data as argument, or by
          ! simply copying code, otherwise diagnose by integrating one
          ! (short) step from .avg file.

          PRINT *, 'resetting averaged data arrays at step', istep
          tqldavg = 0.0 ; snowavg = 0.0 ; albsavg = 0.0
          bcapavg = 0.0 ; z0avg   = 0.0
       END IF
    END IF
10  FORMAT(e14.7)
  END SUBROUTINE entsdiagosc

END MODULE ents_diag
