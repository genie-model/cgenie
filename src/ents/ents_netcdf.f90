MODULE ents_netcdf

  USE ents_lib
  IMPLICIT NONE

CONTAINS

  ! Write ENTS NetCDF with 2D fields
  SUBROUTINE netcdf_ents(fname, var_data, label, myday)
    USE netcdf
    IMPLICIT NONE
    CHARACTER(LEN=*) :: fname, label
    REAL, DIMENSION(:,:,:), INTENT(IN) :: var_data
    INTEGER, INTENT(IN) :: myday

    CHARACTER(LEN=200) :: dim1_name, dim2_name, dim3_name, &
         & var_name, var_att(2,2)
    INTEGER :: ncid, status, var_id, dims(3), jj, &
         & vardim_id1, vardim_id2, vardim_id3, time_id, lon_id, lat_id
    LOGICAL :: fexist
    REAL :: londata(maxj)

    CALL netcdf_db_ents(label, var_name, var_att)
    dim1_name = 'time'
    dim2_name = 'latitude'
    dim3_name = 'longitude'

    INQUIRE(FILE=fname,EXIST=fexist)

    IF (fexist) THEN
       CALL check_err(NF90_OPEN(fname, NF90_WRITE, ncid))
       CALL check_err(NF90_REDEF(ncid))
       CALL check_err(NF90_INQ_DIMID(ncid, dim1_name, vardim_id1))
       CALL check_err(NF90_INQ_DIMID(ncid, dim2_name, vardim_id2))
       CALL check_err(NF90_INQ_DIMID(ncid, dim3_name, vardim_id3))
       dims(1) = vardim_id1
       dims(2) = vardim_id2
       dims(3) = vardim_id3
    ELSE
       CALL check_err(NF90_CREATE(fname, NF90_CLOBBER, ncid))
       CALL check_err(NF90_DEF_DIM(ncid, dim1_name, 1, vardim_id1))
       CALL check_err(NF90_DEF_DIM(ncid, dim2_name, maxj, vardim_id2))
       CALL check_err(NF90_DEF_DIM(ncid, dim3_name, maxi, vardim_id3))
       dims(1) = vardim_id1
       dims(2) = vardim_id2
       dims(3) = vardim_id3
       CALL check_err(NF90_DEF_VAR(ncid, dim1_name, NF90_INT, &
            & (/ dims(1) /), time_id))
       CALL check_err(NF90_PUT_ATT(ncid, time_id, &
            & 'units', 'day from the start of the run'))
       CALL check_err(NF90_DEF_VAR(ncid, dim2_name, NF90_FLOAT, &
            & (/ dims(2) /), lat_id))
       CALL check_err(NF90_PUT_ATT(ncid, lat_id, 'units', 'degrees_north'))
       CALL check_err(NF90_DEF_VAR(ncid, dim3_name, NF90_FLOAT, &
            & (/ dims(3) /), lon_id))
       CALL check_err(NF90_PUT_ATT(ncid, lon_id, 'units', 'degrees_east'))
    END IF

    status = NF90_INQ_VARID(ncid, var_name, var_id)
    IF (status /= NF90_NOERR) THEN
       CALL check_err(NF90_DEF_VAR(ncid, var_name, NF90_DOUBLE, dims, var_id))
       CALL check_err(NF90_PUT_ATT(ncid, var_id, &
            & TRIM(var_att(1,1)), TRIM(var_att(1,2))))
       CALL check_err(NF90_PUT_ATT(ncid, var_id, &
            & TRIM(var_att(2,1)), TRIM(var_att(2,2))))
    END IF

    CALL check_err(NF90_ENDDEF(ncid))
    CALL check_err(NF90_INQ_VARID(ncid, dim1_name, time_id))
    CALL check_err(NF90_PUT_VAR(ncid, time_id, myday))
    CALL check_err(NF90_INQ_VARID(ncid,dim2_name,lat_id))
    CALL check_err(NF90_PUT_VAR(ncid, lat_id, ents_lat))
    CALL check_err(NF90_INQ_VARID(ncid,dim3_name,lon_id))
    DO jj = 1, maxj
       londata(jj) = -255 + (jj - 1) * (360 / maxj)
    END DO
    CALL check_err(NF90_PUT_VAR(ncid, lon_id, londata))
    CALL check_err(NF90_PUT_VAR(ncid, var_id, var_data))
    CALL check_err(NF90_CLOSE(ncid))
  END SUBROUTINE netcdf_ents


  ! ENTS variable names and attributes (common database)
  SUBROUTINE netcdf_db_ents(label, var_name, var_att)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: label
    CHARACTER(LEN=*), INTENT(OUT) :: var_name, var_att(2,2)

    SELECT CASE (label)
    ! For final annual 2d averages
    CASE ('lqavg')
       var_name = 'water'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'average amount of water'
       var_att(2,1) = 'units'
       var_att(2,2) = 'metres'
    CASE ('ltavg')
       var_name = 'land_temp'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'average temperature of land'
       var_att(2,1) = 'units'
       var_att(2,2) = 'C degrees'
    CASE ('palbavg')
       var_name = 'effective_albedo'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'average effective albedo'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('snowavg')
       var_name = 'snow'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'average fractional snow cover'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('albsavg')
       var_name = 'surface_albedo'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'average surface albedo'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('runavg')
       var_name = 'runoff'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'average runoff'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm/s'
    CASE ('pptnavg')
       var_name = 'precipitation'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'average precipitation'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm/s'
    CASE ('relhavg')
       var_name = 'humidity'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'average relative humidity'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('z0avg')
       var_name = 'roughness'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'average roughness length'
       var_att(2,1) = 'units'
       var_att(2,2) = 'metres'
    CASE ('evaplavg')
       var_name = 'evaporation'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'average evaporation'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm/s'
    CASE ('bcapavg')
       var_name = 'bcapavg'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'average bucket capacity'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm'

    ! For 2D restarts
    CASE ('photo')
       var_name = 'photosynthesis'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'photosynthesis'
       var_att(2,1) = 'units'
       var_att(2,2) = 'kgC/m2/yr'
    CASE ('respveg')
       var_name = 'veg_resp'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'vegetation respiration'
       var_att(2,1) = 'units'
       var_att(2,2) = 'kgC/m2/yr'
    CASE ('leaf')
       var_name = 'leaf'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'leaf litter'
       var_att(2,1) = 'units'
       var_att(2,2) = 'kgC/m2/yr'
    CASE ('respsoil')
       var_name = 'soil_resp'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'soil respiration'
       var_att(2,1) = 'units'
       var_att(2,2) = 'kgC/m2/yr'
    CASE ('Cveg')
       var_name = 'vegC'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'vegetation carbon'
       var_att(2,1) = 'units'
       var_att(2,2) = 'kgC/m2/yr'
    CASE ('Csoil')
       var_name = 'soilC'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'soil carbon'
       var_att(2,1) = 'units'
       var_att(2,2) = 'kgC/m2/yr'
    CASE ('fv')
       var_name = 'fv'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'vegetation fraction'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('tqld1')
       var_name = 'land_temp'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'land temperature'
       var_att(2,1) = 'units'
       var_att(2,2) = 'C degrees'
    CASE ('tqld2')
       var_name = 'land_water'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'land water'
       var_att(2,1) = 'units'
       var_att(2,2) = 'metres'
    CASE ('snow')
       var_name = 'snow'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'snow fraction'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'

    ! For 2D restart averages
    CASE ('sphoto')
       var_name = 'photosynthesis'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'averaged photosynthesis'
       var_att(2,1) = 'units'
       var_att(2,2) = 'kgC/m2/yr'
    CASE ('srespveg')
       var_name = 'veg_resp'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'averaged vegetation respiration'
       var_att(2,1) = 'units'
       var_att(2,2) = 'kgC/m2/yr'
    CASE ('sleaf')
       var_name = 'leaf'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'averaged leaf litter'
       var_att(2,1) = 'units'
       var_att(2,2) = 'kgC/m2/yr'
    CASE ('srespsoil')
       var_name = 'soil_resp'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'averaged soil respiration'
       var_att(2,1) = 'units'
       var_att(2,2) = 'kgC/m2/yr'
    CASE ('sCveg')
       var_name = 'vegC'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'averaged vegetation carbon'
       var_att(2,1) = 'units'
       var_att(2,2) = 'kgC/m2/yr'
    CASE ('sCsoil')
       var_name = 'soilC'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'averaged soil carbon'
       var_att(2,1) = 'units'
       var_att(2,2) = 'kgC/m2/yr'
    CASE ('sfv')
       var_name = 'fv'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'averaged vegetation fraction'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('sepsv')
       var_name = 'land_epsv'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'averaged epsv'
       var_att(2,1) = 'units'
       var_att(2,2) = 'C degrees'

    ! For .slandt time series
    CASE ('diagtime1')
       var_name = 'time'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'years from the start of the run'
       var_att(2,1) = 'units'
       var_att(2,2) = 'years'
    CASE ('Gtveg')
       var_name = 'veg_carb'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'vegetation carbon'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC'
    CASE ('Gtsoil')
       var_name = 'soil_carb'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'soil carbon'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC'
    CASE ('Gtatm')
       var_name = 'atm_carb'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'atmospheric carbon'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC'
    CASE ('Gfv')
       var_name = 'veg_frac'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'vegetation fraction'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('pco2ld')
       var_name = 'CO2'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'CO2 concentration'
       var_att(2,1) = 'units'
       var_att(2,2) = 'ppm'
    CASE ('Gtphoto')
       var_name = 'photosynthesis'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'photosynthesis'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC/yr'
    CASE ('Gtrveg')
       var_name = 'veg_resp'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'vegetation respiration'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC/yr'
    CASE ('Gtleaf')
       var_name = 'leaf'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'leaf litter'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC/yr'
    CASE ('Gtrsoil')
       var_name = 'soil_resp'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'soil respiration'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC/year'

    ! For .pslandt time series
    CASE ('diagtime2')
       var_name = 'time2'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'years from the start of the run'
       var_att(2,1) = 'units'
       var_att(2,2) = 'years'
    CASE ('avgs1')
       var_name = 'land_temp'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'land temperature'
       var_att(2,1) = 'units'
       var_att(2,2) = 'C degrees'
    CASE ('avgs2')
       var_name = 'land_water'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'land water'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm'
    CASE ('avgs3')
       var_name = 'rad_flux_atm'
       var_att(1,1) = 'long_name'
       var_att(1,2) = "radiation flux to the atmosphere over land"
       var_att(2,1) = 'units'
       var_att(2,2) = 'W/m2'
    CASE ('avgs4')
       var_name = 'rad_flux_land'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'radiation flux to the land'
       var_att(2,1) = 'units'
       var_att(2,2) = 'W/m2'
    CASE ('avgs5')
       var_name = 'sens_heat'
       var_att(1,1) = 'long_name'
       var_att(1,2) = "sensible heat flux to the  atmosphere over land"
       var_att(2,1) = 'units'
       var_att(2,2) = 'W/m2'
    CASE ('avgs6')
       var_name = 'net_long'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'net long wave heat flux to the atmosphere over land'
       var_att(2,1) = 'units'
       var_att(2,2) = 'W/m2'
    CASE ('avgs7')
       var_name = 'evap'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'total evaporation'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm/s'
    CASE ('avgs8')
       var_name = 'precip'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'precipitation over land'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm/s'
    CASE ('avgs9')
       var_name = 'humidity'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'relative humidity'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('avgs10')
       var_name = 'soil_field_cap'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'soil field capacity'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm'
    CASE ('avgs11')
       var_name = 'land_alb'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'land surface albedo'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('avgs12')
       var_name = 'snow'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'fractional snow cover'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('avgs13')
       var_name = 'roughness'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'roughness length'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm'

    ! For .sealevel time series
    CASE ('diagtime3')
       var_name = 'time3'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'years from the start of the run'
       var_att(2,1) = 'units'
       var_att(2,2) = 'years'
    CASE ('deltah')
       var_name = 'height_change'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'change in sea-level height relative to rhoref'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm'
    CASE ('avrho')
       var_name = 'ocean_density'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'gloval average ocean density'
       var_att(2,1) = 'units'
       var_att(2,2) = 'kg/m3'

    ! For .slavgt time series
    CASE ('year1')
       var_name = 'time'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'years from the start of the run'
       var_att(2,1) = 'units'
       var_att(2,2) = 'years'
    CASE ('avgsl1')
       var_name = 'net_photo'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average net photosynthesis'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC/yr'
    CASE ('avgsl2')
       var_name = 'veg_resp'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average vegetation respiration'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC/yr'
    CASE ('avgsl3')
       var_name = 'leaf_litter'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average leaf litter'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC/yr'
    CASE ('avgsl4')
       var_name = 'soil_resp'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average soil respiration'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC/yr'
    CASE ('avgsl5')
       var_name = 'veg_carbon'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average vegetation carbon'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC/yr'
    CASE ('avgsl6')
       var_name = 'soil_carbon'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average soil carbon'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC/yr'
    CASE ('avgsl7')
       var_name = 'veg_frac_cover'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average vegetation fractional cover'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('avgsl8')
       var_name = 'self_shading'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average self shading fraction'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('Gtatm2')
       var_name = 'atm_carbon'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average atmospheric carbon'
       var_att(2,1) = 'units'
       var_att(2,2) = 'GtC/yr'

    ! For .pslavgt time series
    CASE ('year2')
       var_name = 'time'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'years from the start of the run'
       var_att(2,1) = 'units'
       var_att(2,2) = 'years'
    CASE ('1avgs1')
       var_name = 'land_temp'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average land temperature'
       var_att(2,1) = 'units'
       var_att(2,2) = 'C degrees'
    CASE ('2avgs2')
       var_name = 'land_water'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average land water'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm'
    CASE ('3avgs3')
       var_name = 'rad_flux_atm'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average radiation ' // &
            & 'flux to the atmosphere over land'
       var_att(2,1) = 'units'
       var_att(2,2) = 'W/m2'
    CASE ('4avgs4')
       var_name = 'rad_flux_land'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average radiation flux to the land'
       var_att(2,1) = 'units'
       var_att(2,2) = 'W/m2'
    CASE ('5avgs5')
       var_name = 'sens_heat'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average sensible heat ' // &
            & 'flux to the atmosphere over land'
       var_att(2,1) = 'units'
       var_att(2,2) = 'W/m2'
    CASE ('6avgs6')
       var_name = 'net_long'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average net long wave heat ' // &
            & 'flux to the atmosphere over land'
       var_att(2,1) = 'units'
       var_att(2,2) = 'W/m2'
    CASE ('7avgs7')
       var_name = 'evap'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average  evaporation'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm/s'
    CASE ('8avgs8')
       var_name = 'precip'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average precipitation over land'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm/s'
    CASE ('9avgs9')
       var_name = 'humidity'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average relative humidity'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('10avgs10')
       var_name = 'soil_field_cap'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average soil field capacity'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm'
    CASE ('11avgs11')
       var_name = 'land_alb'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average land surface albedo'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('12avgs12')
       var_name = 'snow'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average fractional snow cover'
       var_att(2,1) = 'units'
       var_att(2,2) = 'dimensionless'
    CASE ('13avgs13')
       var_name = 'roughness'
       var_att(1,1) = 'long_name'
       var_att(1,2) = 'global annual average roughness length'
       var_att(2,1) = 'units'
       var_att(2,2) = 'm'
    END SELECT
  END SUBROUTINE netcdf_db_ents


  ! Write ENTS NetCDF with 1D fields (time series).
  SUBROUTINE netcdf_ts_ents(fname, var_value, label, myday)
    USE netcdf
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: fname, label
    REAL, INTENT(IN) :: var_value
    INTEGER, INTENT(IN) :: myday

    INTEGER :: var_id, ncid, status, time_dim, time_id
    CHARACTER(LEN=200) :: var_name
    CHARACTER(LEN=100) :: var_att(2,2)
    LOGICAL :: fexist
    INTEGER :: mystart, timedim_id, timedim_len
    INTEGER, DIMENSION(:), ALLOCATABLE :: temptime

    CALL netcdf_db_ents(label, var_name, var_att)

    INQUIRE(FILE=fname,EXIST=fexist)
    IF (fexist) THEN
       CALL check_err(NF90_OPEN(fname, NF90_WRITE, ncid))
       CALL check_err(NF90_REDEF(ncid))
       CALL check_err(NF90_INQ_DIMID(ncid, 'time', time_dim))
    ELSE
       CALL check_err(NF90_CREATE(fname, NF90_CLOBBER, ncid))
       CALL check_err(NF90_DEF_DIM(ncid, 'time', NF90_UNLIMITED, time_dim))
       CALL check_err(NF90_DEF_VAR(ncid, 'time', NF90_INT, &
            & (/ time_dim /), time_id))
       CALL check_err(NF90_PUT_ATT(ncid, time_id, &
            & 'long_name', 'day from the start of the run'))
    END IF

    status = NF90_INQ_VARID(ncid, var_name, var_id)
    IF (status /= NF90_NOERR) THEN
       CALL check_err(NF90_DEF_VAR(ncid, var_name, &
            & NF90_DOUBLE, (/ time_dim /), var_id))
       CALL check_err(NF90_PUT_ATT(ncid, var_id, &
            & TRIM(var_att(1,1)), TRIM(var_att(1,2))))
       CALL check_err(NF90_PUT_ATT(ncid, var_id, &
            & TRIM(var_att(2,1)), TRIM(var_att(2,2))))
    END IF

    CALL check_err(NF90_ENDDEF(ncid))
    CALL check_err(NF90_INQ_VARID(ncid, var_name, var_id))
    CALL check_err(NF90_INQ_VARID(ncid, 'time', time_id))
    CALL check_err(NF90_INQ_DIMID(ncid, 'time', timedim_id))
    CALL check_err(NF90_INQUIRE_DIMENSION(ncid, timedim_id, len=timedim_len))

    ALLOCATE(temptime(timedim_len))
    CALL check_err(NF90_GET_VAR(ncid, time_id, temptime))

    IF (timedim_len /= 0 .AND. myday == temptime(timedim_len)) THEN
       mystart = timedim_len
    ELSE
       mystart = timedim_len + 1
    END IF
    DEALLOCATE(temptime)

    CALL check_err(NF90_PUT_VAR(ncid, time_id, myday, start=(/ mystart /)))
    CALL check_err(NF90_PUT_VAR(ncid, var_id, var_value, start=(/ mystart /)))
    CALL check_err(NF90_CLOSE(ncid))
  END SUBROUTINE netcdf_ts_ents

END MODULE ents_netcdf
