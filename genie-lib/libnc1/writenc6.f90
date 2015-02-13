SUBROUTINE ininc(fname, nmaxdims, ndim, nvar, natts, nattsvar, &
     & vdims, vadims, ndims, dimname, varname, &
     & attdimname, attvarname, ncid, iddim, idvar)
  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN) :: fname
  INTEGER, INTENT(IN) :: nmaxdims, ndim, nvar
  INTEGER, DIMENSION(1:ndim), INTENT(IN) :: ndims, natts
  INTEGER, DIMENSION(1:nvar), INTENT(IN) :: vdims, nattsvar
  INTEGER, DIMENSION(1:nmaxdims, 1:nvar), INTENT(IN) :: vadims
  CHARACTER(LEN=200), DIMENSION(1:ndim), INTENT(IN) :: dimname
  CHARACTER(LEN=200), DIMENSION(1:nvar), INTENT(IN) :: varname
  CHARACTER(LEN=200), DIMENSION(2, 1:nmaxdims, 1:ndim), INTENT(IN) :: attdimname
  CHARACTER(LEN=200), DIMENSION(2, 1:nmaxdims, 1:nvar), INTENT(IN) :: attvarname
  INTEGER, INTENT(OUT) :: ncid
  INTEGER, DIMENSION(1:ndim), INTENT(OUT) :: iddim
  INTEGER, DIMENSION(1:nvar), INTENT(OUT) :: idvar

  INCLUDE 'netcdf.inc'
  INTEGER :: iflen, lnsig, ii, itlen, iii, itlen1, itlen2

  INTEGER :: iret, idebug
  INTEGER :: tempdims(200), dimdim(200)
  CHARACTER(LEN=200) :: tname, tname1, tname2
  REAL :: realval(1)
  INTEGER :: realkind

  realkind = KIND(realval)
  idebug = 0
  iflen = lnsig(fname)

  ! Enter define mode
  if (idebug.eq.1) then
     print*,' opening file = ',fname(1:iflen)
  end if
  iret = nf_create(fname(1:iflen), NF_CLOBBER, ncid)
  call check_err(iret)
  !
  ! define dimensions
  !
  if (idebug.eq.1) then
     print*,' defining dimensions ',ndim
  end if
  do ii=1,ndim
     tname=dimname(ii)
     itlen=lnsig(tname)
     iret = nf_def_dim(ncid,tname(1:itlen),ndims(ii),dimdim(ii))
  end do
  !
  ! define coordinates etc
  !
  if (idebug.eq.1) then
     print*,' defining coordinates ',ndim
  end if
  do ii=1,ndim
     tname=dimname(ii)
     itlen=lnsig(tname)
     tempdims(1) = dimdim(ii)
     iret = nf_def_var(ncid,tname(1:itlen), NF_REAL, 1, tempdims,iddim(ii))
     call check_err(iret)
  end do
  !
  ! and the real data
  !
  if (idebug.eq.1) then
     print*,' defining variables ',nvar
  end if
  do ii=1,nvar
     tname=varname(ii)
     itlen=lnsig(tname)
     if (idebug.eq.1) then
        print*,'       variables ',ii,vdims(ii),tname(1:itlen)
     end if
     do iii=1,vdims(ii)
        tempdims(iii)=dimdim(vadims(iii,ii))
        if (idebug.eq.1) then
           print*,'       variables ',ii,iii,tempdims(iii)
        end if
     end do
     iret = nf_def_var(ncid,tname(1:itlen), NF_REAL,vdims(ii), &
          & tempdims,idvar(ii))
     call check_err(iret)
  end do
  !
  ! assign attributes for coordinates
  !
  if (idebug.eq.1) then
     print*,' Defining attibutes of coordinates ',ndim
  end if
  do ii=1,ndim
     if (idebug.eq.1) then
        print*,' Number of attributes = ',ii,natts(ii)
     end if
     do iii=1,natts(ii)
        tname1=attdimname(1,iii,ii)
        tname2=attdimname(2,iii,ii)
        itlen1=lnsig(tname1)
        itlen2=lnsig(tname2)
        if (idebug.eq.1) then
           print*,ii,iii,tname1(1:itlen1)
           print*,ii,iii,tname2(1:itlen2)
        end if
        iret = nf_put_att_text(ncid,iddim(ii), &
             & tname1(1:itlen1),itlen2, &
             & tname2(1:itlen2))
        call check_err(iret)
     end do
     realval(1) = -99999.0
     if (realkind.eq.4) then
        iret = nf_put_att_real(ncid,iddim(ii), 'missing_value', &
             & NF_REAL, 1, realval)
     elseif (realkind.eq.8) then
        iret = nf_put_att_double(ncid,iddim(ii), 'missing_value', &
             & NF_REAL, 1, realval)
     else
        print*,'precision problem in writenc6'
        stop
     endif
     call check_err(iret)
  end do
  !
  !  assign attributes for variables
  !
  if (idebug.eq.1) then
     print*,' Defining attibutes of variables ',nvar
  end if
  do ii=1,nvar
     if (idebug.eq.1) then
        print*,' Number of attributes = ',ii,nattsvar(ii)
     end if
     do iii=1,nattsvar(ii)
        tname1=attvarname(1,iii,ii)
        tname2=attvarname(2,iii,ii)
        itlen1=lnsig(tname1)
        itlen2=lnsig(tname2)
        if (idebug.eq.1) then
           print*,ii,iii,tname1(1:itlen1)
           print*,ii,iii,tname2(1:itlen2)
        end if
        iret = nf_put_att_text(ncid,idvar(ii), &
             & tname1(1:itlen1),itlen2, &
             & tname2(1:itlen2))
        call check_err(iret)
     end do
     realval(1) = -99999.0
     if (realkind.eq.4) then
        iret = nf_put_att_real(ncid,idvar(ii), 'missing_value', &
             & NF_REAL, 1, realval)
     elseif (realkind.eq.8) then
        iret = nf_put_att_double(ncid,idvar(ii), 'missing_value', &
             & NF_REAL, 1, realval)
     else
        print*,'precision problem in writenc6'
        stop
     endif
     call check_err(iret)
  end do
  !
  ! global attribute
  !
  if (idebug.eq.1) then
     print*,' Writing global attribute '
  end if
  tname='Produced using writenc6 program by PJV'
  itlen=lnsig(tname)
  iret = nf_put_att_text(ncid, NF_GLOBAL,'title', itlen,tname(1:itlen))
  call check_err(iret)
  !
  ! leave define mode
  !
  iret = nf_enddef(ncid)
  call check_err(iret)

  if (idebug.eq.1) then
     print*,' Finished ininc '
  end if

  return
end subroutine ininc


subroutine writedim(ncid,iddim,data)
  implicit none
  include 'netcdf.inc'
  integer ncid,iddim
  real data(*)

  integer iret

  integer realkind

  realkind=kind(data(1))

  ! write coordinates
  !
  if (realkind.eq.4) then
     iret = nf_put_var_real(ncid,iddim,data)
  else if (realkind.eq.8) then
     iret = nf_put_var_double(ncid,iddim,data)
  else
     print*,'precision problem in writedim'
     stop
  endif
  call check_err(iret)

  return
end subroutine writedim


subroutine writevar(ncid,idvar,data)
  implicit none
  include 'netcdf.inc'
  integer ncid,idvar
  real data(*)

  integer iret

  integer realkind

  ! write data
  realkind=kind(data(1))


  if (realkind.eq.4) then
     iret = nf_put_var_real(ncid,idvar,data)
  else if (realkind.eq.8) then
     iret = nf_put_var_double(ncid,idvar,data)
  else
     print*,'precision problem in writevar'
     stop
  endif
  call check_err(iret)

  return
end subroutine writevar


subroutine writevar2(ncid,idvar,data,ix1,ix2,iy1,iy2,iz1,iz2,it1,it2)
  implicit none
  include 'netcdf.inc'
  integer ncid,idvar
  integer ix1,ix2,iy1,iy2,iz1,iz2,it1,it2
  real data(*)
  integer start(4),count(4)

  integer iret

  integer realkind

  ! write data
  realkind=kind(data(1))

  start(1)=ix1
  start(2)=iy1
  start(3)=iz1
  if (it1.gt.0) then
     start(4)=it1
  end if
  start(4)=it1
  count(1)=ix2-ix1+1
  count(2)=iy2-iy1+1
  count(3)=iz2-iz1+1
  if (it1.gt.0) then
     count(4)=it2-it1+1
  end if

  if (realkind.eq.4) then
     iret = nf_put_vara_real(ncid,idvar,start,count,data)
  else if (realkind.eq.8) then
     iret = nf_put_vara_double(ncid,idvar,start,count,data)
  else
     print*,'precision problem in writevar2'
     stop
  endif

  call check_err(iret)

  return
end subroutine writevar2


!
! close file
!
subroutine closenc(ncid)
  implicit none
  include 'netcdf.inc'
  integer ncid,iret
  iret = nf_close(ncid)
  call check_err(iret)

  return
end subroutine closenc
