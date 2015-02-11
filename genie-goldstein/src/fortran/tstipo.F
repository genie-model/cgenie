*
* timestep subroutine for program goldstein
*
* JGS iterative implicit version 2/10/00 with
* nii iterations of linearised version of timestep.
* Coeffs for iterative implicit scheme are defined at cell faces.
* eg flux (gradient component) due to east face = cie(i)*T(i+1) + ciw(i)*T(i)
* NRE 2-step version, 2nd attempt 2/11/01
* iterative scheme now used as a predictor step, followed by an
* optional 'corrector' step if(correct=.true.)
* lmax.gt.2 allowed 22/5/2
* coefficient of implicitness cimp included 27/5/2
* cimp=1 fully implicit, cimp=0 gives an explicit step, but
* not the original one unless the upstream weighting is removed and
* the diffusive convection is removed by setting cofc=0.0 and co is called.
* This explicit
* scheme will also do unnecessary recomputation unless either nii=0 and
* correct=.true. or nii=1 and correct=.false. (benign backward do loops in
* former case). Not setting -Ddimp in the Makefile gives the original
* explicit step by calling a different subroutine.
* Isoneutral diffusion and implicit iteration combined code 4/6/2
* version with recalculation of coeffs to improve isoneutral properties
* in time-dep't case. Not very logical to recalculate velocities (see readme).
* Variable timestep option prevents convergence and obscures instabilities;
* very dangerous and untested.
* variable ds version 6/12/4 nre
*
* AY (12/07/05) : removed surplus input argument to function
*
      subroutine tstipo

#include "ocean.cmn"

      real dtmin, dtmax, cimp, centre

      real cie(0:maxi,0:maxj,0:maxk),ciw(0:maxi,0:maxj,0:maxk),
     &     cin(0:maxi,0:maxj,0:maxk),cis(0:maxi,0:maxj,0:maxk),
     &     cia(0:maxi,0:maxj,0:maxk),cib(0:maxi,0:maxj,0:maxk)
      real ts2(maxl,0:maxi+1,0:maxj+1,0:maxk+1)
      common /coeffs/cie,ciw,cin,cis,cia,cib
      real ciae(maxi,maxj,0:maxk),ciaw(maxi,maxj,0:maxk),
     &     cian(maxi,maxj,0:maxk),cias(maxi,maxj,0:maxk)
      real cibe(maxi,maxj,0:maxk),cibw(maxi,maxj,0:maxk),
     &     cibn(maxi,maxj,0:maxk),cibs(maxi,maxj,0:maxk)
      common /coeiso/ciae,ciaw,cian,cias
     &              ,cibe,cibw,cibn,cibs

      integer i, j, k, l, iits, nii

      logical vdt, correct, recalc
c
c     locals
      integer mldpk(2,maxi,maxj)
c
c standard implicit options
      parameter (nii=4, cimp=0.5, correct=.true. ,recalc=.true. )

      parameter(vdt=.false.,dtmin=1e-4,dtmax=1.)


      call coeff(ts1)

c iterate to solve timestep

      do l=1,lmax
         do iits=1,nii
            do k=0,kmax+1
               do j=0,jmax+1
                  do i=0,imax+1
                     ts2(l,i,j,k) = cimp*ts(l,i,j,k)
     1                            + (1.0 - cimp)*ts1(l,i,j,k)
                  enddo
               enddo
            enddo

            do k=1,kmax
               do j=1,jmax
                  do i=1,imax
                     if(k.ge.k1(i,j))then
                        centre = dt(k)*(ciw(i,j,k) - cie(i-1,j,k)
     1                         + rds(j)*(cis(i,j,k) - cin(i,j-1,k))
     2                         + rdz(k)*(cib(i,j,k) - cia(i,j,k-1)))
                        ts(l,i,j,k) = ts1(l,i,j,k)*(1.0 - (1.0-cimp)
     1                              *centre) - dt(k)*(
     2                                cie(i,j,k)  *ts2(l,i+1,j,k)
     3                              - ciw(i-1,j,k)*ts2(l,i-1,j,k)
     4                      + rds(j)*(cin(i,j,k)  *ts2(l,i,j+1,k)
     5                              - cis(i,j-1,k)*ts2(l,i,j-1,k))
     6                      + rdz(k)*(cia(i,j,k)  *ts2(l,i,j,k+1)
     7                              - cib(i,j,k-1)*ts2(l,i,j,k-1)))

                        if(diso)then
                           ts(l,i,j,k) = ts(l,i,j,k) - dt(k)*rdz(k)*(
     1                          ciae(i,j,k)  *ts2(l,i+1,j,k+1)
     2                          + ciaw(i,j,k)  *ts2(l,i-1,j,k+1)
     3                          + (cibe(i,j,k) - ciae(i,j,k-1))
     &                             *ts2(l,i+1,j,k)
     4                          + (cibw(i,j,k) - ciaw(i,j,k-1))
     &                             *ts2(l,i-1,j,k)
     5                          - cibe(i,j,k-1)*ts2(l,i+1,j,k-1)
     6                          - cibw(i,j,k-1)*ts2(l,i-1,j,k-1))

                           ts(l,i,j,k) = ts(l,i,j,k) - dt(k)*rdz(k)*(
     1                          cian(i,j,k)  *ts2(l,i,j+1,k+1)
     2                          + cias(i,j,k)  *ts2(l,i,j-1,k+1)
     3                          + (cibn(i,j,k) - cian(i,j,k-1))
     &                             *ts2(l,i,j+1,k)
     4                          + (cibs(i,j,k) - cias(i,j,k-1))
     &                             *ts2(l,i,j-1,k)
     5                          - cibn(i,j,k-1)*ts2(l,i,j+1,k-1)
     6                          - cibs(i,j,k-1)*ts2(l,i,j-1,k-1))
                        endif
c
                        ts(l,i,j,k) = ts(l,i,j,k)/(1 + cimp*centre)
c
                     endif
                  enddo
               enddo
            enddo
c set periodic boundary conditions inside implicit iteration
            do k=1,kmax
               do j=1,jmax
                  ts(l,0,j,k) = ts(l,imax,j,k)
                  ts(l,imax+1,j,k) = ts(l,1,j,k)
               enddo
            enddo
         enddo
      enddo

      if(correct)then

         do l=1,lmax
            do k=0,kmax+1
               do j=0,jmax+1
                  do i=0,imax+1
                     ts2(l,i,j,k) = 0.5*(ts2(l,i,j,k) + cimp*ts(l,i,j,k)
     1                            + (1.0 - cimp)*ts1(l,i,j,k))
                  enddo
               enddo
            enddo
         enddo

         if(recalc)then
c recalc all coeffs rho, co, bc must be right.
            do i=1,imax
               do j=1,jmax
                  do k=k1(i,j),kmax
                     rho(i,j,k) = ec(1)*ts2(1,i,j,k)+ec(2)*ts2(2,i,j,k)
     1                 + ec(3)*ts2(1,i,j,k)**2 + ec(4)*ts2(1,i,j,k)**3
                  enddo
               enddo
            enddo

            call co(ts2,mldpk)

c periodic boundary condition for rho and ts2

            do k=1,kmax
               do j=1,jmax
                  rho(0,j,k) = rho(imax,j,k)
                  rho(imax+1,j,k) = rho(1,j,k)
                  do l=1,lmax
                     ts2(l,0,j,k) = ts2(l,imax,j,k)
                     ts2(l,imax+1,j,k) = ts2(l,1,j,k)
                  enddo
               enddo
            enddo

            call coeff(ts2)
         endif

         do l=1,lmax
            do k=1,kmax
               do j=1,jmax
                  do i=1,imax
                     if(k.ge.k1(i,j))then
c
c explicit and conservative corrector step
c
                        ts(l,i,j,k) =  ts1(l,i,j,k) - dt(k)*(
     1                             cie(i,j,k)  *ts2(l,i+1,j,k)
     2                           - ciw(i-1,j,k)*ts2(l,i-1,j,k)
     3                   + rds(j)*(cin(i,j,k)  *ts2(l,i,j+1,k)
     4                           - cis(i,j-1,k)*ts2(l,i,j-1,k))
     5                   + rdz(k)*(cia(i,j,k)  *ts2(l,i,j,k+1)
     6                           - cib(i,j,k-1)*ts2(l,i,j,k-1)))
     7                                - dt(k)*ts2(l,i,j,k)*(
     8                           ciw(i,j,k) - cie(i-1,j,k)
     9                   + rds(j)*(cis(i,j,k) - cin(i,j-1,k))
     1                   + rdz(k)*(cib(i,j,k) - cia(i,j,k-1)))

                        if(diso)then
                           ts(l,i,j,k) = ts(l,i,j,k) - dt(k)*rdz(k)*(
     1                          ciae(i,j,k)  *ts2(l,i+1,j,k+1)
     2                          + ciaw(i,j,k)  *ts2(l,i-1,j,k+1)
     3                          + (cibe(i,j,k) - ciae(i,j,k-1))
     &                             *ts2(l,i+1,j,k)
     4                          + (cibw(i,j,k) - ciaw(i,j,k-1))
     &                             *ts2(l,i-1,j,k)
     5                          - cibe(i,j,k-1)*ts2(l,i+1,j,k-1)
     6                          - cibw(i,j,k-1)*ts2(l,i-1,j,k-1))
                           ts(l,i,j,k) = ts(l,i,j,k) - dt(k)*rdz(k)*(
     1                          cian(i,j,k)  *ts2(l,i,j+1,k+1)
     2                          + cias(i,j,k)  *ts2(l,i,j-1,k+1)
     3                          + (cibn(i,j,k) - cian(i,j,k-1))
     &                             *ts2(l,i,j+1,k)
     4                          + (cibs(i,j,k) - cias(i,j,k-1))
     &                             *ts2(l,i,j-1,k)
     5                          - cibn(i,j,k-1)*ts2(l,i,j+1,k-1)
     6                          - cibs(i,j,k-1)*ts2(l,i,j-1,k-1))
                        endif
c
                     endif
                  enddo
               enddo
            enddo
         enddo
      endif

c reset rho

      do i=1,imax
         do j=1,jmax
            do k=k1(i,j),kmax
               rho(i,j,k) = ec(1)*ts(1,i,j,k) + ec(2)*ts(2,i,j,k)
     1                    + ec(3)*ts(1,i,j,k)**2 + ec(4)*ts(1,i,j,k)**3
            enddo
         enddo
      enddo

c optional extra convection for corrector step
c to recover old explicit must have this call
      call co(ts,mldpk)

c periodic boundary condition for rho (also for ts if call co above)

      do k=1,kmax
         do j=1,jmax
            rho(0,j,k) = rho(imax,j,k)
            rho(imax+1,j,k) = rho(1,j,k)
            do l=1,lmax
               ts(l,0,j,k) = ts(l,imax,j,k)
               ts(l,imax+1,j,k) = ts(l,1,j,k)
            enddo
         enddo
      enddo

c update

c
c extended ts1 needed for cimp.ne.1 or isoneutral cases only
c
      do 10 j=1,jmax
         do 10 i=0,imax+1
            do 10 k=k1(i,j),kmax
               do 10 l=1,lmax
                  ts1(l,i,j,k) = ts(l,i,j,k)
   10 continue

      end

      subroutine coeff(ts2)
c
c subroutine to calculate coeffs in linearised version of timestep scheme
c
#include "ocean.cmn"
      real tv, ups, ups0, pec, cof, cofc

      real ts2(maxl,0:maxi+1,0:maxj+1,0:maxk+1)
      real cie(0:maxi,0:maxj,0:maxk),ciw(0:maxi,0:maxj,0:maxk),
     &     cin(0:maxi,0:maxj,0:maxk),cis(0:maxi,0:maxj,0:maxk),
     &     cia(0:maxi,0:maxj,0:maxk),cib(0:maxi,0:maxj,0:maxk)
      common /coeffs/cie,ciw,cin,cis,cia,cib
c dxts is only used to calculate rho thus dimensioned to 2 not maxl
      real tec, scc, tatw, dzrho, rdzrho, tv1, slim
      real dxrho(4), dxts(2,4), dyrho(4), dyts(2,4)
      real ciae(maxi,maxj,0:maxk),ciaw(maxi,maxj,0:maxk),
     &     cian(maxi,maxj,0:maxk),cias(maxi,maxj,0:maxk)
      real cibe(maxi,maxj,0:maxk),cibw(maxi,maxj,0:maxk),
     &     cibn(maxi,maxj,0:maxk),cibs(maxi,maxj,0:maxk)
      common /coeiso/ciae,ciaw,cian,cias
     &              ,cibe,cibw,cibn,cibs
      integer ina, nnp, knp

c implicit
      parameter (cofc=9.0 , ups0=999.)

      integer i, j, k, l

c ------------------------------------------------------------ c
c INITIALIZE VARIABLES
c ------------------------------------------------------------ c
      scc = 0.0
      rdzrho = 0.0
c ------------------------------------------------------------ c

      if(diso)then
         scc = ec(2)
         limps = 0
      endif

c calculate coeffs, periodic b.c. on u must be set elsewhere beforehand

      do k=0,kmax
         do j=0,jmax
            do i=0,imax
c flux to east
               if(k.lt.max(k1(i,j),k1(i+1,j)))then
                  cie(i,j,k) = 0
                  ciw(i,j,k) = 0
               else
c note reference is made to u(i=0)
                  cie(i,j,k) = u(1,i,j,k)*rc(j)*0.5*rdphi
                  tv = rc2(j)*diff(1)*rdphi
                  pec = u(1,i,j,k)*dphi/diff(1)
                  ups = pec / (2.0 + abs(pec))
                  ciw(i,j,k) = cie(i,j,k)*(1+ups) + tv
                  cie(i,j,k) = cie(i,j,k)*(1-ups) - tv
               endif
c flux to north
               if(k.lt.max(k1(i,j),k1(i,j+1)))then
                  cin(i,j,k) = 0
                  cis(i,j,k) = 0
               else
                  cin(i,j,k) = cv(j)*u(2,i,j,k)*0.5
                  tv = cv2(j)*diff(1)
                  pec = u(2,i,j,k)*dsv(j)/diff(1)
                  ups = pec / (2.0 + abs(pec))
                  cis(i,j,k) = cin(i,j,k)*(1+ups) + tv
                  cin(i,j,k) = cin(i,j,k)*(1-ups) - tv
               endif
c flux above
               if(k.lt.k1(i,j))then
                  cia(i,j,k) = 0
                  cib(i,j,k) = 0
               elseif(k.eq.kmax)then
c first set up T eqn then reset for S. Relax T towards T(k+1)
c EMBM for fixed flux source term is stored in ts(...,kmax+1)
                  cia(i,j,kmax) = 1
                  cib(i,j,kmax) = 0
               else
                  cia(i,j,k) = u(3,i,j,k)*0.5
c
c convection by large diffusion
c
                  cof = 1.0 + cofc*0.5*(1.0 + sign(1.0,rho(i,j,k+1)
     1                                             - rho(i,j,k)))
                  tv = rdza(k)*diff(2)*cof
                  pec = u(3,i,j,k)*dza(k)/(diff(2)*cof)
                  ups = pec / (2.0 + abs(pec))
                  cib(i,j,k) = cia(i,j,k)*(1+ups) + tv
                  cia(i,j,k) = cia(i,j,k)*(1-ups) - tv
               endif
            enddo
         enddo
      enddo

      if(diso)then
c isoneutral diffusion
c initialisation not strictly needed at every point
         do k=0,kmax
            do j=1,jmax
               do i=1,imax
                  ciae(i,j,k) = 0.0
                  ciaw(i,j,k) = 0.0
                  cian(i,j,k) = 0.0
                  cias(i,j,k) = 0.0
                  cibe(i,j,k) = 0.0
                  cibw(i,j,k) = 0.0
                  cibn(i,j,k) = 0.0
                  cibs(i,j,k) = 0.0
               enddo
            enddo
         enddo
         dmax = 0.0
         do k=1,kmax-1
            do j=1,jmax
               do i=1,imax
                  if(k.ge.k1(i,j))then
                     tatw = 0.5*(ts2(1,i,j,k) + ts2(1,i,j,k+1))
                     tec = - ec(1) - ec(3)*tatw*2 - ec(4)*tatw*tatw*3
c moved to above  scc = ec(2)
                     dzrho = (scc*(ts2(2,i,j,k+1) - ts2(2,i,j,k))
     1                    - tec*(ts2(1,i,j,k+1) - ts2(1,i,j,k)))*rdza(k)
                     if(dzrho.lt.-1e-12)then
                        rdzrho = 1.0/dzrho
                        tv1 = 0.0
                        do knp=0,1
                           do nnp=0,1
                              ina = 1+nnp + 2*knp
c phi derivatives
                              do l=1,2
                                 if(k+knp.ge.k1(i-1+2*nnp,j))then
                                    dxts(l,ina) = (ts2(l,i+nnp,j,k+knp)
     2                                   - ts2(l,i+nnp-1,j,k+knp))*rc(j)
     &                                      *rdphi
                                 else
                                    dxts(l,ina) = 0.
                                 endif
c s-derivatives
                                 if(k+knp.ge.k1(i,j-1+2*nnp))then
                                    dyts(l,ina) = (ts2(l,i,j+nnp,k+knp)
     2                                   - ts2(l,i,j+nnp-1,k+knp))
     &                                   *cv(j-1+nnp)*rdsv(j+nnp-1)
                                 else
                                    dyts(l,ina) = 0.
                                 endif
                              enddo
                              dxrho(ina) = scc*dxts(2,ina)
     1                             -tec*dxts(1,ina)
                              dyrho(ina) = scc*dyts(2,ina)
     1                             -tec*dyts(1,ina)
c calculate diagonal part
                              tv1 = tv1 + dxrho(ina)*dxrho(ina)
     1                             + dyrho(ina)*dyrho(ina)
                           enddo
                        enddo
                        tv1 = 0.25*tv1*rdzrho*rdzrho
c limit flux by factor slim for large slope
                        if(tv1.gt.ssmax(k))then
                           slim = ssmax(k)*ssmax(k)/(tv1*tv1)
c count flux-limited points
                           limps = limps + 1
                        else
                           slim = 1.0
                        endif
                        tv1 = tv1*slim*diff(1)*rdza(k)
c test vertical diffusion number
                        tv = tv1*dt(k)*rdza(k)
                        if(tv.gt.dmax)then
                           dmax = tv
                        endif
c could reduce 8 to 4 lines by defining another array
                        tv = 0.5*slim*diff(1)*rdzrho
                        ciae(i,j,k) =   tv*rc(j)*dxrho(4)*rdphi
                        ciaw(i,j,k) = - tv*rc(j)*dxrho(3)*rdphi
                        cibe(i,j,k) =   tv*rc(j)*dxrho(2)*rdphi
                        cibw(i,j,k) = - tv*rc(j)*dxrho(1)*rdphi
c masking needed for varlat version
                        if(k+1.ge.k1(i,j+1))then
                           cian(i,j,k) =   tv*cv(j)*dyrho(4)*rdsv(j)
                        else
                           cian(i,j,k) = 0.
                        endif
                        if(k+1.ge.k1(i,j-1))then
                           cias(i,j,k) = - tv*cv(j-1)*dyrho(3)*rdsv(j-1)
                        else
                           cias(i,j,k) = 0.
                        endif
                        if(k.ge.k1(i,j+1))then
                           cibn(i,j,k) =   tv*cv(j)*dyrho(2)*rdsv(j)
                        else
                           cibn(i,j,k) = 0.
                        endif
                        if(k.ge.k1(i,j-1))then
                           cibs(i,j,k) = - tv*cv(j-1)*dyrho(1)*rdsv(j-1)
                        else
                           cibs(i,j,k) = 0.
                        endif

                        cia(i,j,k) = cia(i,j,k) - ciae(i,j,k)
     &                       - ciaw(i,j,k)
     &                       - cian(i,j,k) - cias(i,j,k) - tv1
                        cib(i,j,k) = cib(i,j,k) - cibe(i,j,k)
     &                       - cibw(i,j,k)
     &                       - cibn(i,j,k) - cibs(i,j,k) + tv1
                     endif
                  endif
               enddo
            enddo
         enddo
      endif

      end
