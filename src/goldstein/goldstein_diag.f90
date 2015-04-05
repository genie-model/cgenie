MODULE goldstein_diag

  USE goldstein_lib
  USE goldstein_netcdf
  IMPLICIT NONE

CONTAINS

  ! Frequent diagnostics for GOLDSTEIN
  SUBROUTINE diag2(sum, avn, avs)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: sum(8*maxl), avn,  avs

    INTEGER :: i, j, k, l, mo, ndep
    LOGICAL :: first=.TRUE.
    REAL :: vol(8)=0.0

    sum=0
    avn = 0
    avs = 0
    DO k = 1, maxk
       DO j = 1, maxj
          DO i = 1, maxi
             IF (k >= k1(i,j)) THEN
                ndep = (2 * k - 1) / maxk
                IF (j <= jsf) THEN
                   mo = 4
                ELSEIF (i >= ips(j) .AND. i <= ipf(j)) THEN
                   mo = 1
                ELSE IF ((i >= ias(j) .AND. i <= iaf(j)) .OR. &
                       & (iaf(j) <= ias(j) .AND. (i <= iaf(j) .OR. &
                       & ias(j) <= i))) THEN
                   mo = 2
                ELSE
                   mo = 3
                END IF
                DO l = 1, maxl
                   sum(mo+4*ndep+8*(l-1)) = sum(mo+4*ndep+8*(l-1)) + &
                        & ts(l,i,j,k) * dphi * ds(j) * dz(k)
                END DO
                IF (first) &
                     & vol(mo+4*ndep) = vol(mo+4*ndep) + dphi * ds(j) * dz(k)
                IF (k < maxk) &
                     & avn = avn + ABS(rho(i,j,k) - rho(i,j,k+1)) / dza(k)
                avs = avs + u(1,i,j,k)**2 + u(2,i,j,k)**2
             END IF
          END DO
       END DO
    END DO

    ! crash barrier added 111099
    IF (avs < 1.0E20) THEN
       CONTINUE
    ELSE
       PRINT *,  'big avs , stopping'
       STOP
    END IF

    DO i = 1, 8
       DO l = 1, maxl
          IF (vol(i) /=  0.0) THEN
             sum(i+8*(l-1)) = sum(i+8*(l-1)) / vol(i)
          END IF
       END DO
    END DO

    avs = SQRT(avs / ntot)
    avn = avn / (intot)
    first = .FALSE.
  END SUBROUTINE diag2


  ! Diagnostics for GOLDSTEIN
  SUBROUTINE diag
    IMPLICIT NONE

    REAL :: sum(maxl), sumsq(maxl), umax(3), cnmax, avs, vol, area, &
         & tmax,tmin, ubmax(2), tv, tmax1, tmin1, tv1, tv2
    INTEGER :: i, j, k, l, imm(3,3), isl

    vol = 0.0
    sum = 0.0
    sumsq = 0.0
    umax = 0
    imm = 0
    cnmax = 0
    tv = 0
    avs = 0
    cnmax = 0
    ubmax = 0
    DO k = 1, maxk
       DO j = 1, maxj
          DO i = 1, maxi
             IF (k >= k1(i,j)) THEN
                vol = vol + dphi * ds(j) * dz(k)
                DO l = 1, maxl
                   sum(l) = sum(l) + ts(l,i,j,k) * dphi * ds(j) * dz(k)
                   sumsq(l) = sumsq(l) + ts(l,i,j,k)**2 * dphi * ds(j) * dz(k)
                END DO
                DO l = 1, 3
                   IF (ABS(u(l,i,j,k)) > umax(l)) THEN
                      umax(l) = ABS(u(l,i,j,k))
                      imm(1,l) = i
                      imm(2,l) = j
                      imm(3,l) = k
                   END IF
                END DO

                tv = MAX(ABS(u(1,i,j,k)) * rc(j) * rdphi, &
                     &   ABS(u(3,i,j,k)) * rdz(k)) * dt(k)
                IF (j < maxj) &
                     & tv = MAX(tv, ABS(u(2,i,j,k)) * cv(j) * rdsv(j) * dt(k))
                cnmax = MAX(cnmax, tv)

                avs = avs + u(1,i,j,k)**2 + u(2,i,j,k)**2
             END IF
          END DO
       END DO
    END DO

    tmax1 = -1.0E10
    tmin1 = 1.0E10
    DO i = 1, maxi
       DO j = 1, maxj
          IF (k1(i,j) == 1) THEN
             IF (ts(1,i,j,1) > tmax1) tmax1 = ts(1,i,j,1)
             IF (ts(1,i,j,1) < tmin1) tmin1 = ts(1,i,j,1)
          END IF
       END DO
    END DO

    tmax = -1.0E10
    tmin = 1.0E10
    DO i = 1, maxi
       DO j = 1, maxj
          IF (k1(i,j) <= maxk) THEN
             IF (ts(1,i,j,maxk) > tmax) tmax = ts(1,i,j,maxk)
             IF (ts(1,i,j,maxk) < tmin) tmin = ts(1,i,j,maxk)
          END IF
       END DO
    END DO

    ! Find max barotropic velocity components
    DO j = 1, maxj
       DO i = 1, maxi
          DO l = 1, 2
             IF (ABS(ub(l,i,j)) > ubmax(l)) ubmax(l) = ABS(ub(l,i,j))
          END DO
       END DO
    END DO

    ! Write out SST and SSS
    PRINT *,  'GOLD : ocean outputs'

    tv1 = 0.0
    tv2 = 0.0
    area = 0.0
    DO j = 1, maxj
       DO i = 1, maxi
          IF (k1(i,j) <= maxk) THEN
             tv1 = tv1 + ts(1,i,j,maxk) * ds(j)
             tv2 = tv2 + ts(2,i,j,maxk) * ds(j)
             area = area + ds(j)
          END IF
       END DO
    END DO
    tv1 = tv1 / area
    tv2 = tv2 / area
    WRITE (6,*) 'average SST', tv1
    WRITE (6,*) 'average SSS', tv2

    PRINT *, 'max and min T at maxk ', tmax, tmin
    PRINT *, 'max and min T at k=1  ', tmax1, tmin1
    PRINT *, '<T>, <S> ..., <T**2>, <S**2> ...', &
         & (sum(l) / vol, l = 1, maxl), (sumsq(l) / vol, l = 1, maxl)
    PRINT *, 'ts(1,1,1,1)', ts(1,1,1,1)
    PRINT *, 'Cn ', cnmax
    PRINT *, 'Dmax', dmax
    PRINT *, 'flux limited at ', limps, ' points'
    PRINT *, 'max absolute velocities', &
         & ((imm(i,l), i = 1, 3), umax(l), l = 1, 3)
    PRINT *, 'r.m.s. horiz. flow speed (pointwise)', SQRT(avs / ntot)
    PRINT *, 'max barotropic velocities', (ubmax(i), i = 1, 2)
    DO isl = 1, isles
       CALL island(ub, tv, isl, 1)
       PRINT *, 'path integral error on island ', isl, tv
    END DO
  END SUBROUTINE diag


  ! End-of-run diagnostics for GOLDSTEIN
  SUBROUTINE diagend
    use genie_util, ONLY: check_unit, check_iostat
    IMPLICIT NONE

    REAL :: tv2, err, err1, err2
    REAL :: tsdata(maxl,maxi,maxj,maxk), errwts(maxl,maxk), tv3, tv4, tv5
    REAL :: opsi(0:maxj,0:maxk)
    REAL :: opsia(0:maxj,0:maxk), omina, omaxa
    REAL :: opsip(0:maxj,0:maxk), ominp, omaxp
    INTEGER :: iposa(2)

    INTEGER :: i, j, k, l, ios, ntot1, ntot2
    REAL, DIMENSION(2) :: tsav=0.0, tsvar=0.0, ntotal=0.0
    REAL, PARAMETER :: nullvalue=-1.0E10
    REAL :: lon(maxi), lat(maxj), depth(maxk)

    ! If 'tsinterp' is '.true.': i) discontinue writing out of model-data
    ! field, ii) replace error score with the score calculated using the
    ! 'err_gold(...)' function further below (which should be the same to
    ! computational precision)
    IF (.NOT. tsinterp) THEN
       ! Read interpolated Levitus and NCEP data
       CALL check_unit(30, __LINE__, __FILE__)
       OPEN(30,FILE=indir_name(1:lenin)//tdatafile(1:lentdata),IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       READ (30,*,IOSTAT=ios) &
            & (((tsdata(1,i,j,k), k = 1, maxk), i = 1, maxi), j = 1, maxj)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(30,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       CALL check_unit(31, __LINE__, __FILE__)
       OPEN(31,FILE=indir_name(1:lenin)//sdatafile(1:lensdata),IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       READ (31,*,IOSTAT=ios) &
            & (((tsdata(2,i,j,k), k = 1, maxk), i = 1, maxi), j = 1, maxj)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(31,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       tsdata(2,:,:,:) = tsdata(2,:,:,:) - saln0

       ! Calculate weights based on variance of data NB not real spatial but
       ! computational spatial
       DO k = 1, maxk
          DO j = 1, maxj
             DO i = 1, maxi
                IF (k >= k1(i,j)) THEN
                   DO l = 1, 2
                      IF (tsdata(l,i,j,k) > nullvalue) THEN
                         tsav(l) = tsav(l) + tsdata(l,i,j,k)
                         tsvar(l) = tsvar(l) + tsdata(l,i,j,k) * tsdata(l,i,j,k)
                         ntotal(l) = ntotal(l) + 1
                      END IF
                   END DO
                END IF
             END DO
          END DO
       END DO

       tsav = tsav / ntotal
       tsvar = tsvar / ntotal - tsav * tsav

       ! Specify weights
       errwts(1,:) = 1.0 / tsvar(1)
       errwts(2,:) = 1.0 / tsvar(2)

       ! Calculate error compared to observations (!)
       CALL check_unit(25, __LINE__, __FILE__)
       OPEN(25,FILE=outdir_name(1:lenout)//'tmp.err',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       err = 0.0
       DO j = 1, maxj
          DO i = 1, maxi
             DO k = 1, maxk
                IF (k >= k1(i,j)) THEN
                   DO l = 1, 2
                      IF (tsdata(l,i,j,k) > nullvalue) THEN
                         err = err + &
                              & errwts(l,k) * (ts(l,i,j,k) - tsdata(l,i,j,k))**2
                         WRITE (25,10,IOSTAT=ios) ts(l,i,j,k) - tsdata(l,i,j,k)
                         CALL check_iostat(ios, __LINE__, __FILE__)
                      END IF
                   END DO
                ELSE
                   WRITE (25,10,IOSTAT=ios) 0.0, 0.0
                   CALL check_iostat(ios, __LINE__, __FILE__)
                END IF
             END DO
          END DO
       END DO
10     FORMAT(e15.5)
       CLOSE(25,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       err = SQRT(err / (ntotal(1) + ntotal(2)))
       PRINT *, 'GOLD : weighted r.m.s. model-data error ', err
    ELSE
       PRINT *, "Writing out of model-data error fields (file" // &
            & " 'tmp.err') is inactive when observational dataset" // &
            & " is interpolated at runtime (i.e., 'tsinterp' is" // &
            & " '.true.')."
    END IF

    FORALL (i=1:maxi) lon(i) = 180.0 * (phi0 + (i - 0.5) * dphi) / pi
    FORALL (j=1:maxj) lat(j) = 180.0 * ASIN(s(j)) / pi
    FORALL (k=1:maxk) depth(k) = ABS(zro(maxk+1-k) * dsc)
    ntot1 = 0
    err1 = err_gold(ts(1,1:maxi,1:maxj,1:maxk), 1, k1, ntot1, &
         & maxi, maxj, maxk, indir_name, lenin, tdatafile, lentdata, &
         & tdata_scaling, tdata_offset, tsinterp, tdata_varname, &
         & tdata_missing, lon, lat, depth)
    ntot2 = 0
    err2 = err_gold(ts(2,1:maxi,1:maxj,1:maxk), 2, k1, ntot2, &
         & maxi, maxj, maxk, indir_name, lenin, sdatafile, lensdata, &
         & sdata_scaling, sdata_offset + saln0, tsinterp, &
         & sdata_varname, sdata_missing, lon, lat, depth)
    PRINT *, 'err_gold composite = ', &
         & SQRT(((err1**2 * ntot1) + (err2**2 * ntot2)) / (ntot1 + ntot2))
    IF (tsinterp) THEN
       err = SQRT(((err1**2 * ntot1) +(err2**2 * ntot2)) / (ntot1 + ntot2))
       print * , 'GOLD : weighted r.m.s. model-data error ', err
    END IF

    ! Calc temp for comparison with Jia (2003)
    CALL diagopsi(ominp, omaxp, omina, omaxa, opsi, opsia, opsip, iposa)

    ! Nearest point to 24deg North if i=36
    j = 26

    tv4 = 0.0
    tv5 = 0.0
    DO k = 1, maxk
       ! First calculate average temp at this depth and lat.
       tv2 = 0.0
       tv3 = 0.0
       DO i = ias(j), iaf(j)
          IF (k1(i,j) <= k .AND. k1(i,j+1) <= k) THEN
             tv2 = tv2 + (ts(1,i,j+1,k) + ts(1,i,j,k)) * dphi
             tv3 = tv3 + dphi
          END IF
       END DO
       IF (tv3 > 1.0E-9) tv3 = 0.5 * tv2 / tv3
       DO i = ias(j), iaf(j)
          IF (k1(i,j) <= k .AND. k1(i,j+1) <= k) THEN
             IF (k <= 4) THEN
                tv4 = tv4 + cv(j) * u(2,i,j,k) * tv3 * dz(k) * dphi
             ELSE
                tv5 = tv5 + cv(j) * u(2,i,j,k) * tv3 * dz(k) * dphi
             END IF
          END IF
       END DO
    END DO
    PRINT *, 'GOLD : volm transport weighted temperatures j=26 and opsia'
    PRINT *, tv4, tv5, opsia(j,4)
  END SUBROUTINE diagend


  ! Calculate overturning streamfunctions
  SUBROUTINE diagopsi(ominp, omaxp, omina, omaxa, opsi, opsia, opsip, iposa)
    IMPLICIT NONE
    REAL, INTENT(OUT) :: opsi(0:maxj,0:maxk)
    REAL, INTENT(OUT) :: opsia(0:maxj,0:maxk), omina, omaxa
    REAL, INTENT(OUT) :: opsip(0:maxj,0:maxk), ominp, omaxp
    INTEGER, INTENT(OUT) :: iposa(2)

    REAL :: ou(maxj,maxk)
    integer i, j, k

    ! Calculate meridional overturning streamfunction opsi on C grid only
    opsi = 0
    opsia = 0
    opsip = 0
    DO j = 1, maxj-1
       DO k = 1, maxk-1
          ou(j,k) = SUM(cv(j) * u(2,1:maxi,j,k) * dphi)
          opsi(j,k) = opsi(j,k-1) - dz(k) * ou(j,k)
       END DO
    END DO

    ! Pacific and Atlantic overturning streamfunctions

    ominp = 0
    omaxp = 0
    DO j=  jsf+1, maxj-1
       DO k = 1, maxk-1
          ou(j,k) = SUM(cv(j) * u(2,ips(j):ipf(j),j,k) * dphi)
          opsip(j,k) = opsip(j,k-1) - dz(k) * ou(j,k)
          IF (opsip(j,k) < ominp .AND. k <= overdep) ominp = opsip(j,k)
          IF (opsip(j,k) > omaxp .AND. k <= overdep) omaxp = opsip(j,k)
       END DO
    END DO

    omina = 0
    omaxa = 0
    DO j = jsf+1, maxj-1
       DO k = 1, maxk-1
          ou(j,k) = 0
          ! Conditionality to take care of "split Atlantic" (rma, 5/10/05)
          IF (ias(j) > iaf(j)) THEN
             DO i=ias(j), maxi
                ou(j,k) = ou(j,k) + cv(j) * u(2,i,j,k) * dphi
             END DO
             DO i = 1, iaf(j)
                ou(j,k) = ou(j,k) + cv(j) * u(2,i,j,k) * dphi
             END DO
          ELSE
             DO i=ias(j), iaf(j)
                ou(j,k) = ou(j,k) + cv(j) * u(2,i,j,k) * dphi
             END DO
          END IF
          opsia(j,k) = opsia(j,k-1) - dz(k) * ou(j,k)
          IF (opsia(j,k) < omina .AND. k <= overdep) omina = opsia(j,k)
          IF (opsia(j,k) > omaxa .AND. k <= overdep) THEN
             omaxa = opsia(j,k)
             iposa(1) = j
             iposa(2) = k
          END IF
       END DO
    END DO
  END SUBROUTINE diagopsi


  ! Extra diagnostic routine for c-goldstein v2 with seasonal cycle
  SUBROUTINE diagosc(istep, iout, ext, fx0flux, fwflux, wstress)
    USE genie_util, ONLY: check_unit, check_iostat
    IMPLICIT NONE
    INTEGER, intent(in) :: istep, iout
    CHARACTER(LEN=3), INTENT(IN) :: ext
    REAL, INTENT(IN) :: fx0flux(5,maxi,maxj), fwflux(4,maxi,maxj)
    REAL, INTENT(IN) :: wstress(4,maxi,maxj)

    REAL :: rnyear, err1, err2
    INTEGER :: ntot1, ntot2, ios, i, j, k, l
    REAL :: work(0:maxi+1, 0:maxj+1, 0:maxk+1)

    ! Stream function variables
    REAL, DIMENSION(0:maxj,0:maxk) :: opsi, opsia, opsip
    REAL :: omina, omaxa, ominp, omaxp
    INTEGER :: iposa(2)

    REAL, DIMENSION(:,:), ALLOCATABLE, SAVE :: opsiavg, opsipavg, opsiaavg

    REAL :: outputfile(maxl+3,maxk,maxi,maxj)
    REAL :: outfx0(5,maxi,maxj), outfw(4,maxi,maxj)

    REAL :: lon(maxi), lat(maxj), depth(maxk)

    IF (.NOT. ALLOCATED(opsiavg)) THEN
       ALLOCATE(opsiavg(0:maxj,0:maxk))  ; opsiavg = 0.0
       ALLOCATE(opsipavg(0:maxj,0:maxk)) ; opsipavg = 0.0
       ALLOCATE(opsiaavg(0:maxj,0:maxk)) ; opsiaavg= 0.0
    END IF

    rnyear = 1.0 / nyear
    tsavg = tsavg + ts * rnyear
    uavg = uavg + u * rnyear
    rhoavg = rhoavg + rho * rnyear
    fx0avg = fx0avg + fx0flux * rnyear
    fwavg = fwavg + fwflux * rnyear
    windavg = windavg + wstress * rnyear

    CALL diagopsi(ominp, omaxp, omina, omaxa, opsi, opsia, opsip, iposa)

    opsiavg = opsiavg + opsi * rnyear
    opsipavg = opsipavg + opsip * rnyear
    opsiaavg = opsiaavg + opsia * rnyear

    IF (iout == 1) THEN
       PRINT *,  'GOLD : writing averaged data at istep ', istep

       ! Write averaged data (a near-copy of outm.f) not a restart
       ! as such, therefore can write less accurate, more economical output
       outputfile = 0.0
       CALL check_unit(2, __LINE__, __FILE__)
       OPEN(2,FILE=outdir_name(1:lenout)//lout//'.'//'osc.'//ext,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       DO j = 1, maxj
          DO i = 1, maxi
             DO k = 1, maxk
                DO l = 1, maxl
                   IF (k >= k1(i,j)) outputfile(l,k,i,j) = tsavg(l,i,j,k)
                END DO
                DO l = 1, 2
                   outputfile(maxl+l,k,i,j) = uavg(l,i,j,k)
                END DO
                outputfile(maxl+3,k,i,j) = rhoavg(i,j,k)
             END DO
          END DO
       END DO
       WRITE (2,10,IOSTAT=ios) outputfile
       CALL check_iostat(ios, __LINE__, __FILE__)
       ! Heat fluxes
       outfx0 = fx0avg(1:5,1:maxi,1:maxj)
       DO l = 1, 5
          DO j = 1, maxj
             DO i = 1, maxi
                IF (k1(i,j) >= 90) outfx0(l,i,j) = 0.0
             END DO
          END DO
       END DO
       WRITE (2,10,IOSTAT=ios) &
            & (((outfx0(l,i,j), i = 1, maxi), j = 1, maxj), l = 1, 5)
       CALL check_iostat(ios, __LINE__, __FILE__)
       ! Freshwater fluxes
       outfw = fwavg(1:4,1:maxi,1:maxj)
       WRITE (2,10,IOSTAT=ios) &
            & (((outfw(l,i,j), i = 1, maxi), j = 1, maxj), l = 1, 4)
       CALL check_iostat(ios, __LINE__, __FILE__)
       ! Wind stresses (u-point x, u-point y, v-point x, v-point y)
       WRITE (2,10,IOSTAT=ios) &
            & (((windavg(l,i,j), i = 1, maxi), j = 1, maxj), l = 1, 4)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(2,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       CALL check_unit(10, __LINE__, __FILE__)
       OPEN(10,FILE=outdir_name(1:lenout)//lout//'.opav',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       WRITE (10,10,IOSTAT=ios) ((opsiavg(j,k), j=0, maxj), k=0, maxk)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(10,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       OPEN(10,FILE=outdir_name(1:lenout)//lout//'.opavp',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       WRITE (10,10,IOSTAT=ios) ((opsipavg(j,k), j=0, maxj), k=0, maxk)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(10,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       OPEN(10,FILE=outdir_name(1:lenout)//lout//'.opava',IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       WRITE (10,10,IOSTAT=ios) ((opsiaavg(j,k), j=0, maxj), k=0, maxk)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(10,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       PRINT *,  'Writing GOLDSTEIN mean annual netCDF file at time',  istep

       CALL ini_netcdf_ocn(istep, 2)
       CALL write_netcdf_ocn(k1, ncdepth1, &
            & opsiavg, opsiaavg, opsipavg, tsavg, uavg, rhoavg, &
            & fx0avg, fwavg, work, dsc, usc, rsc, saln0, &
            & maxi, maxj, maxk, maxl, 2)
       CALL end_netcdf_ocn(2)
       PRINT *

       ! Increment average counter
       iav = iav + 1

       ! Perform diagnostics on averaged data, either by rewriting other diag
       ! routines to accept data as argument, or by simply copying code,
       ! otherwise diagnose by integrating one (short) step from .avg file.

       FORALL (i=1:maxi) lon(i) = 180.0 * (phi0 + (i - 0.5) * dphi) / pi
       FORALL (j=1:maxj) lat(j) = 180.0 * ASIN(s(j)) / pi
       FORALL (k=1:maxk) depth(k) = ABS(zro(maxk+1-k) * dsc)
       ntot1 = 0
       err1 = err_gold(tsavg(1,1:maxi,1:maxj,1:maxk), 1, k1, ntot1, &
            & maxi, maxj, maxk, indir_name, lenin, tdatafile, lentdata, &
            & tdata_scaling, tdata_offset, tsinterp, tdata_varname, &
            & tdata_missing, lon, lat, depth)
       ntot2 = 0
       err2 = err_gold(tsavg(2,1:maxi,1:maxj,1:maxk), 2, k1, ntot2, &
            & maxi, maxj, maxk, indir_name, lenin, sdatafile, lensdata, &
            & sdata_scaling, sdata_offset + saln0, tsinterp, sdata_varname, &
            & sdata_missing, lon, lat, depth)
       PRINT *,  'err_gold annual average composite = ', &
            & SQRT(((err1**2 * ntot1) + (err2**2 * ntot2)) / (ntot1 + ntot2))
       ! End of diagnostic error calculation

       ! Compute total water content of planet (should be numerically const.)
       PRINT *, 'resetting averaged data arrays at step', istep
       tsavg = 0.0
       uavg = 0.0
       rhoavg = 0.0
       fx0avg = 0.0
       fwavg = 0.0
       windavg = 0.0
       fx0avg = 0.0
       opsiavg = 0.0
       opsipavg = 0.0
       opsiaavg = 0.0
    END IF

10  FORMAT(e14.7)

  END SUBROUTINE diagosc


  ! Return an RMS error value for the specified GOLDSTEIN model field
  ! compared with the contents of the supplied data file.
  FUNCTION err_gold(modeldata, tracerid, k1, ntot, maxi, maxj, maxk, &
       & indir_name, lenin, obsdatafile, lenobsdata, datascaling, &
       & dataoffset, interpolate, varname, missing, lon, lat, z)
    IMPLICIT NONE
    REAL :: err_gold

    REAL :: modeldata(maxi,maxj,maxk)      ! Model data field
    INTEGER :: tracerid                    ! Data field type
    INTEGER :: k1(0:maxi+1,0:maxj+1)       ! Topography
    INTEGER :: ntot                        ! Number of wet grid cells
    INTEGER :: maxi, maxj, maxk            ! Grid size
    CHARACTER(LEN=200) :: indir_name       ! GOLD input/output directories
    INTEGER :: lenin
    CHARACTER(LEN=128) :: obsdatafile      ! GOLD T/S data files
    INTEGER :: lenobsdata
    REAL :: datascaling, dataoffset        ! Used to convert observational data
    LOGICAL :: interpolate                 ! Interpolate obs. data?
    CHARACTER(LEN=25) :: varname           ! Observation-based dataset
    REAL :: missing
    REAL :: lon(maxi), lat(maxj), z(maxk)  ! GOLDSTEIN grid

    ! Weighting factor (reciprocal of obs. data variance)
    REAL :: errw

    ! Indices
    INTEGER :: i, j, k

    ! Observational data, average and variance
    REAL :: obsdata(maxi,maxj,maxk), obsdata_av, obsdata_var

    ! Error value.
    REAL :: err

    CALL read_gold_target_field(tracerid, k1, maxi, maxj, maxk, indir_name, &
         & lenin, obsdatafile, lenobsdata, datascaling, dataoffset, &
         & interpolate, varname, missing, lon, lat, z, obsdata)

    ! Calculate weights based on variance of data NB not real spatial
    ! but computational spatial
    obsdata_av = 0.0
    obsdata_var = 0.0
    ntot = 0
    DO k = 1, maxk
       DO j = 1, maxj
          DO i = 1, maxi
             IF (k >= k1(i,j) .AND. obsdata(i,j,k) > -1.0E10) THEN
                obsdata_av = obsdata_av + obsdata(i,j,k)
                obsdata_var= obsdata_var + obsdata(i,j,k) * obsdata(i,j,k)
                ntot = ntot + 1
             END IF
          END DO
       END DO
    END DO
    obsdata_av = obsdata_av / ntot
    obsdata_var = obsdata_var / ntot - obsdata_av * obsdata_av
    errw = 1.0 / obsdata_var

    ! Calculate the RMS error
    err = 0.
    DO j = 1, maxj
       DO i = 1, maxi
          DO k = 1, maxk
             IF (k >= k1(i,j) .AND. obsdata(i,j,k) > -1.0E10) THEN
                err = err + errw * (modeldata(i,j,k) - obsdata(i,j,k))**2
             END IF
          END DO
       END DO
    END DO
    err_gold = SQRT(err / ntot)

    ! Print error value
    print 400, tracerid, err_gold
400 format (' err_gold: weighted r.m.s. model-data error (', i1, ') ', g24.16)
  END FUNCTION err_gold


  ! Read in data-based target fields for comparison with the model's
  ! internal fields.
  SUBROUTINE read_gold_target_field(tracerid, k1, maxi, maxj, maxk, &
       & indir_name, lenin, obsdatafile, lenobsdata, datascaling, &
       & dataoffset, interpolate, varname, missing, lon, lat, z, obsdata)
    USE genie_util, ONLY: check_unit, check_iostat, die
    USE local_netcdf
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: tracerid
    INTEGER, INTENT(in) :: k1(0:maxi+1,0:maxj+1)
    INTEGER, INTENT(IN) :: maxi, maxj, maxk
    CHARACTER(LEN=200), INTENT(IN) :: indir_name
    INTEGER, INTENT(IN) :: lenin
    CHARACTER(LEN=128), INTENT(IN) :: obsdatafile
    INTEGER, INTENT(IN) :: lenobsdata
    REAL, INTENT(IN) :: datascaling, dataoffset
    LOGICAL, INTENT(IN) :: interpolate
    CHARACTER(LEN=25), INTENT(IN) :: varname
    REAL, INTENT(IN) :: missing
    REAL, INTENT(INOUT) :: lon(maxi)
    REAL, INTENT(IN) :: lat(maxj), z(maxk)
    REAL, INTENT(OUT) :: obsdata(maxi,maxj,maxk)


    ! Observation-based dataset
    TYPE(real3dVar), DIMENSION(1) :: ts_obs
    TYPE(real1dVar), DIMENSION(3) :: ts_obs_axis
    INTEGER :: nx_obs, ny_obs, nz_obs
    REAL, POINTER, DIMENSION(:,:) :: sinlat_obs
    INTEGER :: ncid_in, ncstatus, i_obs, j_obs, k_obs, i_obs_min, j_obs_min
    INTEGER :: i0, i1, jtmp, ii, jj, iii, nwidth
    REAL :: obstmp, sinlat(2,maxj)

    ! Interpolation
    INTEGER :: n_int, n_ext
    REAL :: distmean, distmax, rlon, rlat, rz, testmask
    REAL :: dist, distmin, distminocean, cosdlon

    ! Miscelaneous variables
    INTEGER :: status, i, j, k, ios

    ! Salinity scaling factor
    REAL :: saln0

    i_obs_min = 0
    j_obs_min = 0

    IF (.NOT. interpolate) THEN
       ! Open the data file
       CALL check_unit(30, __LINE__, __FILE__)
       OPEN(30,FILE=indir_name(1:lenin)//obsdatafile(1:lenobsdata),IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)
       READ (30,*,IOSTAT=ios) &
            & (((obsdata(i,j,k), k = 1, maxk), i = 1, maxi), j = 1, maxj)
       CALL check_iostat(ios, __LINE__, __FILE__)
       CLOSE(30,IOSTAT=ios)
       CALL check_iostat(ios, __LINE__, __FILE__)

       ! Apply a scaling for the salinity data
       IF (tracerid == 2) THEN
          saln0 = 34.9
          obsdata = obsdata - saln0
       END IF
    ELSE
       ! Read in NetCDF dataset and interpolate onto model grid
       CALL openNetCDFREAD(indir_name(1:lenin)//obsdatafile(1:lenobsdata), &
            & ncid_in)
       ts_obs(1)%name = varname
       CALL lookupVars(ncid_in, ts_obs)
       ! Size of observational dataset
       nx_obs=ts_obs(1)%dimLens(1)
       ny_obs=ts_obs(1)%dimLens(2)
       nz_obs=ts_obs(1)%dimLens(3)
       ALLOCATE(ts_obs(1)%data(0:nx_obs,1:ny_obs,1:nz_obs), stat=status)
       IF (status /= 0) CALL die("Could not allocate memory")
       ncstatus = NF90_GET_VAR(ncid_in, ts_obs(1)%id, &
            & ts_obs(1)%data(1:nx_obs,1:ny_obs,1:nz_obs))
       IF (ncstatus /= NF90_NOERR) CALL handle_nc_err(ncstatus)
       ts_obs_axis(1)%name = ts_obs(1)%dimnames(1)
       ts_obs_axis(2)%name = ts_obs(1)%dimnames(2)
       ts_obs_axis(3)%name = ts_obs(1)%dimnames(3)
       ! Note, the zeroth longitude index represents the same values
       ! as for the last value (the actual coordinate is offset by 360
       ! degrees) to facilitate dealing with periodicity of the
       ! longitude
       CALL lookupVars(ncid_in, ts_obs_axis)
       ALLOCATE(ts_obs_axis(1)%data(0:nx_obs), stat=status)
       IF (status /= 0) CALL die("Could not allocate memory")
       ALLOCATE(ts_obs_axis(2)%data(1:ny_obs), stat=status)
       IF (status /= 0) CALL die("Could not allocate memory")
       ALLOCATE(ts_obs_axis(3)%data(1:nz_obs), stat=status)
       IF (status /= 0) CALL die("Could not allocate memory")
       ncstatus = NF90_GET_VAR(ncid_in, ts_obs_axis(1)%id, &
            & ts_obs_axis(1)%data(1:nx_obs))
       IF (ncstatus /= NF90_NOERR) CALL handle_nc_err(ncstatus)
       ts_obs_axis(1)%data(0) = ts_obs_axis(1)%data(nx_obs)-360.
       DO j = 1, ny_obs
          DO k = 1, nz_obs
             DO i = 1, nx_obs
                IF (ABS((ts_obs(1)%data(i,j,k) - missing) / &
                     & missing) < 1.0E-5) THEN
                   ts_obs(1)%data(i,j,k) = 9.99999E19
                END IF
             END DO
             ts_obs(1)%data(0,j,k) = ts_obs(1)%data(nx_obs,j,k)
          END DO
       END DO
       ncstatus = NF90_GET_VAR(ncid_in, ts_obs_axis(2)%id, ts_obs_axis(2)%data)
       IF (ncstatus /= NF90_NOERR) CALL handle_nc_err(ncstatus)
       ncstatus = NF90_GET_VAR(ncid_in, ts_obs_axis(3)%id, ts_obs_axis(3)%data)
       IF (ncstatus /= NF90_NOERR) CALL handle_nc_err(ncstatus)
       ! prepare auxiliary arrays:
       !     first index    function
       !     1            sin()
       !     2            cos()
       ALLOCATE(sinlat_obs(2,ny_obs), STAT=status)
       IF (status /= 0) CALL die("Could not allocate memory")
       ! For grid of observation-based dataset
       DO j = 1, ny_obs
          sinlat_obs(1,j) = SIN(ts_obs_axis(2)%data(j) * pi / 180.0)
          sinlat_obs(2,j) = COS(ts_obs_axis(2)%data(j) * pi / 180.0)
       END DO
       ! For GOLDSTEIN grid
       DO j = 1, maxj
          sinlat(1,j) = SIN(lat(j) * pi / 180.0)
          sinlat(2,j) = COS(lat(j) * pi / 180.0)
       END DO
       ! Flip latitudinal and depth axes if required; if necessary,
       ! convert depth axis to positive depth; test monotonicity of
       ! axis of observation-based dataset; convert GENIE's
       ! longitudinal axis to same range as that of the observational
       ! dataset
       IF (ts_obs_axis(2)%data(ny_obs) < ts_obs_axis(2)%data(1)) THEN
          DO j = 1, int(ny_obs / 2 + 0.5)
             obstmp = ts_obs_axis(2)%data(j)
             ts_obs_axis(2)%data(j) = ts_obs_axis(2)%data(ny_obs+1-j)
             ts_obs_axis(2)%data(ny_obs+1-j) = obstmp
             DO i=0, nx_obs
                DO k = 1, nz_obs
                   obstmp = ts_obs(1)%data(i,j,k)
                   ts_obs(1)%data(i,j,k) = ts_obs(1)%data(i,ny_obs+1-j,k)
                   ts_obs(1)%data(i,ny_obs+1-j,k) = obstmp
                END DO
             END DO
          END DO
       END IF
       DO k = 1, nz_obs
          IF (ts_obs_axis(3)%data(k) < 0) &
               & ts_obs_axis(3)%data(k) = ABS(ts_obs_axis(3)%data(k))
       END DO
       IF (ts_obs_axis(3)%data(ny_obs) < ts_obs_axis(3)%data(1)) THEN
          DO k = 1, int(nz_obs / 2 + 0.5)
             obstmp = ts_obs_axis(3)%data(k)
             ts_obs_axis(3)%data(k) = ts_obs_axis(3)%data(nz_obs+1-k)
             ts_obs_axis(3)%data(nz_obs+1-k) = obstmp
             DO i = 1, nx_obs
                DO j = 1, ny_obs
                   obstmp = ts_obs(1)%data(i,j,k)
                   ts_obs(1)%data(i,j,k) = ts_obs(1)%data(i,j,nz_obs+1-k)
                   ts_obs(1)%data(i,j,nz_obs+1-k) = obstmp
                END DO
             END DO
          END DO
       END IF
       DO i = 2, nx_obs
          IF (ts_obs_axis(1)%data(i) <= ts_obs_axis(1)%data(i-1)) THEN
             CALL die("Non-incremental longitudinal axis")
          END IF
       END DO
       DO j = 2, ny_obs
          IF (ts_obs_axis(2)%data(j) <= ts_obs_axis(2)%data(j-1)) THEN
             CALL die("Non-incremental latitudinal axis")
          END IF
       END DO
       DO k = 2, nz_obs
          IF (ts_obs_axis(3)%data(k) <= ts_obs_axis(3)%data(k-1)) THEN
             CALL die("Non-incremental depth axis")
          END IF
       END DO
       DO i = 1, maxi
          DO WHILE (lon(i) <= ts_obs_axis(1)%data(0))
             lon(i) = lon(i) + 360.0
          END DO
          DO WHILE (lon(i) > ts_obs_axis(1)%data(nx_obs))
             lon(i) = lon(i) - 360.0
          END DO
       END DO
       ! Tri-linear interpolation, parts of this code is based on the
       ! interpolation routine 'genie-cgoldstein/laz2siz.f', the
       ! "extrapolation" part has been replaced by a horizontal search
       ! for the nearest valid point on the sphere.
       n_int = 0
       distmean = 0.0
       distmax = 0.0
       n_ext = 0
       DO k = 1, maxk
          DO j = 1, maxj
             DO i = 1, maxi
                IF (k1(i,j) <= (maxk+1-k)) THEN
                   ! Find location of model grid point on
                   ! observation-based grid.
                   i_obs = 0
                   DO WHILE (ts_obs_axis(1)%data(i_obs) < lon(i) .AND. &
                        & i_obs <= nx_obs)
                      i_obs = i_obs + 1
                   END DO
                   ! This could possibly be done more general without
                   ! the restriction that any model point has to be
                   ! inside the extremes of the latitude coordinates
                   ! of the observation-based grid
                   j_obs = 1
                   DO WHILE (ts_obs_axis(2)%data(j_obs) < lat(j) .AND. &
                        & j_obs <= ny_obs)
                      j_obs = j_obs + 1
                   END DO
                   k_obs = 1
                   DO WHILE (ts_obs_axis(3)%data(k_obs) < z(k) .AND. &
                        & k_obs <= nz_obs)
                      k_obs = k_obs + 1
                   END DO
                   IF (i_obs == 0 .OR. i_obs > nx_obs .OR. &
                        & j_obs == 1 .OR. j_obs > ny_obs .OR. &
                        & k_obs == 1 .OR. k_obs > nz_obs) THEN
                      CALL die("Coordinates or depth outside of the" // &
                           & " boundaries set by observational dataset")
                   END IF
                   rlon = (lon(i) - ts_obs_axis(1)%data(i_obs-1)) / &
                        & (ts_obs_axis(1)%data(i_obs) - &
                        &  ts_obs_axis(1)%data(i_obs-1))
                   rlat = (lat(j) - ts_obs_axis(2)%data(j_obs-1)) / &
                        & (ts_obs_axis(2)%data(j_obs) - &
                        &  ts_obs_axis(2)%data(j_obs-1))
                   rz = (z(k) - ts_obs_axis(3)%data(k_obs-1)) / &
                        & (ts_obs_axis(3)%data(k_obs) - &
                        &  ts_obs_axis(3)%data(k_obs-1))
                   testmask = MAX(ts_obs(1)%data(i_obs,j_obs,k_obs), &
                        & ts_obs(1)%data(i_obs-1,j_obs,k_obs), &
                        & ts_obs(1)%data(i_obs,j_obs-1,k_obs), &
                        & ts_obs(1)%data(i_obs,j_obs,k_obs-1), &
                        & ts_obs(1)%data(i_obs-1,j_obs-1,k_obs), &
                        & ts_obs(1)%data(i_obs,j_obs-1,k_obs-1), &
                        & ts_obs(1)%data(i_obs-1,j_obs,k_obs-1), &
                        & ts_obs(1)%data(i_obs-1,j_obs-1,k_obs-1))
                   ! Interpolate if no land at corners of cube
                   ! encompassing the model grid location
                   IF (testmask < 1.0E10) THEN
                      obsdata(i,j,maxk+1-k) = &
                           & (1.0 - rz) * &
                           &  ((1.0 - rlon) * &
                           &   ((1.0 - rlat) * &
                           &     ts_obs(1)%data(i_obs-1,j_obs-1,k_obs-1) + &
                           &    rlat * &
                           &     ts_obs(1)%data(i_obs-1,j_obs,k_obs-1)) + &
                           &   rlon * &
                           &   ((1.0-rlat) * &
                           &     ts_obs(1)%data(i_obs,j_obs-1,k_obs-1) +&
                           &    rlat * &
                           &     ts_obs(1)%data(i_obs,j_obs,k_obs-1))) + &
                           & rz * &
                           &  ((1.0-rlon) * &
                           &   ((1.0-rlat) * &
                           &     ts_obs(1)%data(i_obs-1,j_obs-1,k_obs) + &
                           &    rlat * &
                           &     ts_obs(1)%data(i_obs-1,j_obs,k_obs)) + &
                           &   rlon * &
                           &   ((1.0-rlat) * &
                           &     ts_obs(1)%data(i_obs,j_obs-1,k_obs) + &
                           &    rlat * &
                           &     ts_obs(1)%data(i_obs,j_obs,k_obs)))
                      n_int = n_int + 1
                   ELSE
                      ! Find horizonatlly nearest (true distance on
                      ! sphere) vertically adjacant pair of ocean
                      ! points, BUT retain vertical interpolation
                      ! (analogous to 'genie-cgoldstein/laz2siz.f')

                      ! To compute arc distance dist between two
                      ! points ((lon1,lat1) and (lon2,lat2)) on a
                      ! sphere use:
                      !  dist=arccos(sin(lat1) * sin(lat2)+cos(lat1) *
                      !  cos(lat2) * cos(lat2-lat1))
                      ! Note, this formula is affected by rounding
                      ! errors for small angles, so resolution of
                      ! close points is limited, especially if 4-byte
                      ! arithmetic/trigonometry is used

                      ! Start with rectangle defined by
                      ! (i_obs-1,j_obs-1), (i_obs,j_obs), find within
                      ! newly added points both the nearest valid
                      ! vertical pair of points AND the nearest pair
                      ! of all points,
                      distmin = pi
                      distminocean = pi
                      DO ii = 1, 2
                         DO jj = 1, 2
                            cosdlon = COS(pi * (lon(i) - &
                                 & ts_obs_axis(1)%data(i_obs+1-ii)) / 180.0)
                            jtmp = j_obs+1-jj
                            dist = ACOS(sinlat(1,j) * sinlat_obs(1,jtmp) + &
                                 & sinlat(2,j) * sinlat_obs(2,jtmp) * cosdlon)
                            distmin = MIN(distmin, dist)
                            testmask = &
                                 & MAX(ts_obs(1)%data(i_obs+1-ii,jtmp,k_obs), &
                                 & ts_obs(1)%data(i_obs+1-ii,jtmp,k_obs-1))
                            IF (testmask < 1.0E10 .AND. &
                                 & distminocean > dist) THEN
                               distminocean = dist
                               i_obs_min = i_obs+1-ii
                               j_obs_min = jtmp
                            END IF
                         END DO
                      END DO
                      nwidth = 1
                      ! Repeat until nearest pair of newly added
                      ! points is farther away than nearest valid
                      ! pair,
                      DO WHILE (distmin < distminocean .AND. &
                           & nwidth < INT(nx_obs / 2) + 1 .AND. &
                           & nwidth < ny_obs)
                         distmin = pi
                         ! add grid-point circumference around
                         ! rectangle, take into account periodicity in
                         ! longitudinal direction and also look across
                         ! northern and southern poles.  find nearest
                         ! valid pair of points AND nearest pair of
                         ! points within newly added points
                         nwidth = nwidth + 1
                         ! Reflect i range if rectangle spreads across
                         ! northern or southern border
                         IF (j_obs - nwidth < 1) THEN
                            i0 = i_obs-nwidth - INT(nx_obs / 2)
                            i1 = i_obs-1+nwidth - INT(nx_obs / 2)
                            jtmp = ABS(j_obs-nwidth-1)
                         ELSE
                            i0 = i_obs-nwidth
                            i1 = i_obs-1+nwidth
                            jtmp = j_obs-nwidth
                         END IF
                         DO ii=i0, i1
                            iii = MODULO(ii-1, nx_obs)+1
                            cosdlon = COS(pi * (lon(i) - &
                                 & ts_obs_axis(1)%data(iii)) / 180.0)
                            dist = ACOS(sinlat(1,j) * sinlat_obs(1,jtmp) + &
                                 & sinlat(2,j) * sinlat_obs(2,jtmp) * cosdlon)
                            distmin = MIN(distmin, dist)
                            testmask = &
                                 & MAX(ts_obs(1)%data(iii,j_obs-nwidth,k_obs), &
                                 & ts_obs(1)%data(iii,j_obs-nwidth,k_obs-1))
                            IF (testmask < 1.0E10 .AND. &
                                 & distminocean > dist) THEN
                               distminocean = dist
                               i_obs_min = iii
                               j_obs_min = jtmp
                            END IF
                         END DO
                         ! Reflect i range if rectangle spreads across
                         ! northern or southern border
                         IF (j_obs-1+nwidth > ny_obs) THEN
                            i0 = i_obs-nwidth - INT(nx_obs / 2)
                            i1 = i_obs-1+nwidth - INT(nx_obs / 2)
                            jtmp = 2 * ny_obs - (j_obs+nwidth-2)
                         ELSE
                            i0 = i_obs-nwidth
                            i1 = i_obs-1+nwidth
                            jtmp = j_obs-1+nwidth
                         END IF
                         DO ii = i0, i1
                            iii = MODULO(ii-1, nx_obs)+1
                            cosdlon = COS(pi * (lon(i) - &
                                 & ts_obs_axis(1)%data(iii)) / 180.0)
                            dist = ACOS(sinlat(1,j) * sinlat_obs(1,jtmp) + &
                                 & sinlat(2,j) * sinlat_obs(2,jtmp) * cosdlon)
                            distmin = MIN(distmin, dist)
                            testmask = &
                               & MAX(ts_obs(1)%data(iii,j_obs-1+nwidth,k_obs), &
                               & ts_obs(1)%data(iii,j_obs-1+nwidth,k_obs-1))
                            IF (testmask < 1.0E10 .AND. &
                                 & distminocean > dist) THEN
                               distminocean=dist
                               i_obs_min=iii
                               j_obs_min=jtmp
                            END IF
                         END DO
                         DO jj=j_obs-nwidth+1, j_obs-2+nwidth
                            ! Reflect i range if rectangle spreads
                            ! across northern or southern border
                            IF (jj < 1 .OR. jj > ny_obs) THEN
                               iii = MODULO(i_obs-nwidth-1 + INT(nx_obs / 2), &
                                    & nx_obs)
                               IF (jj < 1) THEN
                                  jtmp = ABS(jj-1)
                               ELSE
                                  jtmp = 2 * ny_obs - (jj-1)
                               END IF
                            ELSE
                               iii = MODULO(i_obs-nwidth-1, nx_obs) + 1
                               jtmp = jj
                            END IF
                            cosdlon = COS(pi * (lon(i) - &
                                 & ts_obs_axis(1)%data(iii)) / 180.0)
                            dist = ACOS(sinlat(1,j) * sinlat_obs(1,jtmp) + &
                                 & sinlat(2,j) * sinlat_obs(2,jtmp) * cosdlon)
                            distmin = MIN(distmin, dist)
                            testmask = &
                                 & MAX(ts_obs(1)%data(iii,jj,k_obs), &
                                 & ts_obs(1)%data(iii,jj,k_obs-1))
                            IF (testmask < 1.0E10 .AND. &
                                 & distminocean > dist) THEN
                               distminocean = dist
                               i_obs_min = iii
                               j_obs_min = jtmp
                            END IF
                         END DO
                         DO jj  =j_obs-nwidth+1, j_obs-2+nwidth
                            ! Reflect i range if rectangle spreads
                            ! across northern or southern border
                            IF (jj < 1 .OR. jj > ny_obs) THEN
                               iii = MODULO(i_obs-2+nwidth + INT(nx_obs / 2), &
                                    & nx_obs)
                               IF (jj < 1) THEN
                                  jtmp = ABS(jj-1)
                               ELSE
                                  jtmp = 2 * ny_obs - (jj-1)
                               END IF
                            ELSE
                               iii = MODULO(i_obs-2+nwidth, nx_obs) + 1
                               jtmp = jj
                            END IF
                            cosdlon = COS(pi * (lon(i) - &
                                 & ts_obs_axis(1)%data(iii)) / 180.0)
                            dist = ACOS(sinlat(1,j) * sinlat_obs(1,jtmp) + &
                                 & sinlat(2,j) * sinlat_obs(2,jtmp) * cosdlon)
                            distmin = MIN(distmin, dist)
                            testmask = &
                                 & MAX(ts_obs(1)%data(iii,jj,k_obs), &
                                 & ts_obs(1)%data(iii,jj,k_obs-1))
                            IF (testmask < 1.0E10 .AND. &
                                 & distminocean > dist) THEN
                               distminocean = dist
                               i_obs_min = iii
                               j_obs_min = jtmp
                            END IF
                         END DO
                      END DO
                      ! Vertically interpolate at point with shortest
                      ! distance from target point
                      obsdata(i,j,maxk+1-k) = &
                           & (1.0-rz) * &
                           &  ts_obs(1)%data(i_obs_min,j_obs_min,k_obs-1) + &
                           & rz * ts_obs(1)%data(i_obs_min,j_obs_min,k_obs)
                      distmean = distmean + distminocean
                      IF (distminocean > distmax) distmax = distminocean
                      n_ext = n_ext + 1
                   END IF
                ELSE
                   obsdata(i,j,maxk+1-k) = -99.9999E19
                END IF
             END DO
          END DO
       END DO
       IF (n_ext > 0) distmean = distmean / REAL(n_ext)
       PRINT *, 'fraction of interpolated points,'
       PRINT *, 'fraction of extrapolated points,'
       PRINT *, 'mean distance of extrapolated points (degrees),'
       PRINT *, 'maximum distance of extrapolated point (degrees)'
       PRINT *, REAL(n_int) / REAL(n_int+n_ext), &
            & REAL(n_ext) / REAL(n_int+n_ext), &
            & distmean * 180.0 / pi, distmax * 180.0 / pi
       ! Clean up
       DEALLOCATE(sinlat_obs, STAT=status)
       IF (status /= 0) CALL die("Could not allocate memory")
       DEALLOCATE(ts_obs_axis(1)%data, STAT=status)
       IF (status /= 0) CALL die("Could not allocate memory")
       DEALLOCATE(ts_obs_axis(2)%data, STAT=status)
       IF (status /= 0) CALL die("Could not allocate memory")
       DEALLOCATE(ts_obs_axis(3)%data, STAT=status)
       IF (status /= 0) CALL die("Could not allocate memory")
       DEALLOCATE(ts_obs(1)%data, STAT=status)
       IF (status /= 0) CALL die("Could not allocate memory")
       CALL closeNetCDF(ncid_in)

       DO j = 1, maxj
          DO i = 1, maxi
             DO k = 1, maxk
                obsdata(i,j,k) = obsdata(i,j,k) / datascaling - dataoffset
             END DO
          END DO
       END DO
    END IF
  END SUBROUTINE read_gold_target_field

END MODULE goldstein_diag
