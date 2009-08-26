!! Paramètres pour les CL
! Pression nulle inconditionnellement en haut (comme lequere). Sinon, dépend du signe de v (comme benchmark desrayauds)
#define PRESSION_HAUT_0   0
! Global bernoulli pour les CL en bas
#define GB                0
! U = 0 au lieu de dU/dz = 0 en bas
#define CL_U_DIRICHLET    0

! Pondérer par la distance centre-interface
#define USE_PONDERATION   1
! Utiliser la stabilisation par méthode des clusters. Sinon, stabilisation globale Brezzi-Pitkaranta
#define STABILISATION_CLUSTERS 1

! ajustement du deltaT : adapter aux variations pour avoir un delta_obj, mais ne pas descendre en dessous de dt_i et ne pas monter au dessus de DT_MAX
#define DT_MAX (dt_i * 10)
#define DELTA_OBJ 1

! fonction du programme. à mettre en nutest dans donnees.dat
#define CHEMINEE 110
#define CHEMINEE_ETENDUE 400

! paramètres pour CHEMINEE_ETENDUE
! largeur et longueur relative (entre 0 et 1) de la cheminée dans sa boite, en supposant la cheminée centrée
#define LARGEUR_CHEMINEE 0.8
#define LONGUEUR_CHEMINEE 0.8
#define LARGEUR_COTE ((1 - LARGEUR_CHEMINEE) / 2)
#define LONGUEUR_COTE ((1 - LONGUEUR_CHEMINEE) / 2)

! constantes de représentation interne de CL
! extérieur du domaine
#define CL_EXTERIEUR -1
! paroi chauffée
#define CL_PAROI_GAUCHE -2
#define CL_PAROI_DROITE -3
#define CL_PAROI_EXTERIEUR -4
program nvstks
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! D'APRES CODE et ARTICLE R. EYMARD
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none

  logical,parameter::direct=.true.
  real(kind=8),parameter::test=1e-3
  integer,parameter::nordre=5
  integer,parameter::itbcgsmax=500
  real(kind=8),parameter::epsilon_newton=1e-10
  


  logical::mode_gb=.false.
  integer::nbsom,ndiv,maillage
  integer::ncv0,nbsom0
  integer::ncv,nptvois,nbc,nlin
  integer::i,j,k,i0,i1,j0,k0,l,l0
  integer::laa
  integer::iter
  integer::nutest,aff,cvgce,reprise
  real(kind=8)::t,ts,duree,dt,dt_i,&
       vtest,ucentt,centt,delobj,delpourc,&
       residu,vnorme,vxnorme,vynorme,tnorme,residu_old,&
       comu,&
       residobj,thetamax,theta,&
       diffusion,&
       delobtu,delobtt,&
       coefdu,coefdt,coefb,&
       ra,pr
  real(kind=8),dimension(:),allocatable::&
       xs,ys,&    ! coordonnées des sommets
       xcv,ycv,&  ! coordonnées des centres de mailles
       mcv,&
       A,bx,by,As,&
!!$       Ass,&
       vx,vy,tp,vxd,vyd,p,tpd,&
       sx,sy,st,sp,&
       potent

  character(len=20)::c_maillage

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real(kind=8),dimension(:),allocatable::xs0,ys0
  integer::ii,jj
  integer,parameter::neq=4
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer,dimension(:,:),allocatable::nusom0
  integer,dimension(:),allocatable::nusom,ptvois,ptsc,nusc,&
       nuvois

!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  integer,dimension(:),allocatable::ptbi,ptbo,ptmi,ptmo
!!$  integer::nptbi,nptbo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real(kind=8)::eta,ray
  real(kind=8)::cr,ct
  integer::nr,nt

  integer,dimension(10)::i_pos
  real(kind=8),dimension(10)::x_pos,y_pos
  real(kind=8)::distance,nui,nuo,debit,debit_in,debit_out,debit_prec,&
       ALy,ALx

  real(kind=4)::echelle,decalage
  integer::coul


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer,dimension(:),allocatable::numat,ptmat
  integer,dimension(:),allocatable::renum,denum
  integer,dimension(:),allocatable::nuv,ptv,ptd

  real(kind=8),dimension(:,:,:),allocatable::aa,dd,aaf,&
       aa1,dd1
  real(kind=8),dimension(:,:),allocatable::bb
  real(kind=8),dimension(:),allocatable::ui,rri,r0,vi,pi,aux1,aux2

  real(kind=8)::lambda

  real(kind=8)::norm1,norm2,norm3,norm4

  integer::itbcgs,nordre1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer,dimension(:),allocatable::pt_d,l_bg,l_bd
  real(kind=8),dimension(:,:,:),allocatable::aal
  real(kind=8),dimension(:,:),allocatable::bbf




  real(kind=8)::eta_old,cr_old,ct_old
  integer::nr_old,nt_old,ncv_old,nbsom_old,nptvois_old
  real(kind=8),dimension(:),allocatable::p_old,vx_old,vy_old,tp_old
  real(kind=8),dimension(:),allocatable::xcv_old,ycv_old



  residu_old=1D40
  debit=0

  !Lecture
  call lecture
  write(*,*)"Precision de Newton : ",epsilon_newton
  if (reprise==1) then
     write(*,*)"Changement de GRILLE !!!!!!!!!"
     call changement_grille_excentrique
  end if


  call geometrie
  call cls

!!$!Extraction des macromailles, des mailles de bord
!!$       write(*,*)"----> Les mailles de bord sont exclues des macro-mailles"
!!$       do i=1,ncv
!!$          iter1 : do j=ptvois(i)+1,ptvois(i+1)
!!$             j0=nuvois(j)
!!$             if (j0<=0) then !maille de bord
!!$                do k=ptvois(i)+1,ptvois(i+1) !Sortie des macromailles
!!$                   As(k)=0
!!$                   k0=nuvois(k)
!!$                   if (k0>0) then
!!$                      iter2 : do l=ptvois(k0)+1,ptvois(k0+1) !recherche de la connexion reciproque
!!$                         l0=nuvois(l)
!!$                         if (l0==i) then !connexion réciproque  trouvée
!!$                            As(l)=0
!!$                            exit iter2
!!$                         end if
!!$                      end do iter2
!!$                   end if
!!$                end do
!!$                !exit iter1
!!$             end if
!!$          end do iter1
!!$       end do
!!$!Fin extraction




#if STABILISATION_CLUSTERS
#else
As = A * lambda
#endif



!!$  coefdu=sqrt(pr/ra)   !pr        !1._8
!!$  coefdt=1/sqrt(ra*pr) !1         !1._8/pr
!!$  coefb=1              !ra*pr      !ra/pr

  coefdu=pr        !1._8
  coefdt=1         !1._8/pr
  coefb=ra*pr      !ra/pr



  !###################################################################
  !Renumérotation + définition des largeurs de bande
  !l_bg : largeur bande gauche
  !l_bd : largeur bande droite
  !pt_d : pointeur du terme diagonal
  !###################################################################
  nlin=neq*ncv



  !###################################################################
  !Avance temporelle
  !###################################################################





  !  allocate(aa(laa),bb(nlin))
  if (reprise==0) allocate(vx(ncv),vy(ncv),tp(ncv),p(ncv))
  allocate(vxd(ncv),vyd(ncv),tpd(ncv))
  allocate(sx(ncv),sy(ncv),sp(ncv),st(ncv))
  !  allocate(press(ncv))


  !Sauvegarde mes macromailles
  echelle=6000.
  decalage=10.
  open(unit=15, file="macro.fig", status="unknown")
  write(15,"(A)")"#FIG 3.2"
  write(15,"(A)")"Landscape"
  write(15,"(A)")"Center"
  write(15,"(A)")"Metric"
  write(15,"(A)")"A4"
  write(15,"(A)")"100.00"
  write(15,"(A)")"Single"
  write(15,"(A)")"-2"
  write(15,"(A)")"1200 2"
  do i=1,ncv
     i0=nusom(ptvois(i+1))
     do j=ptvois(i)+1,ptvois(i+1)
        jj=nuvois(j)
        i1=nusom(j)
        coul=0
        select case (nutest)
        case (1)
           if (jj==-1) coul=4 !rouge
           if (jj==-2) coul=1 !bleu
           if (As(j)<=0) call line( xs(i0),ys(i0),xs(i1),ys(i1),decalage,echelle,coul)
           !TODO : cheminée étendue
        case(100,110,210)
           if (jj==-1) then
              coul=4
           end if
           if (jj==-2) then
              coul=1
           end if
           if (jj==-3) then
              coul=3
           end if
           if (jj==-4) then
              coul=2 !vert
           end if
           if (As(j)<=0) call line( xs(i0),ys(i0),xs(i1),ys(i1),decalage,echelle,coul)

        case default
           if (As(j)<=0) call line( xs(i0),ys(i0),xs(i1),ys(i1),decalage,echelle,coul)
        end select

        i0=i1
     end do
  end do
  close(15)
  !Fin Sauvegarde mes macromailles




  if (direct) then
     call predir
  else
     nordre1=nordre
     write(*,*)"******BCGS*******"
     write(*,*)"Ordre de la factorisation incomplète :",nordre1
     write(*,*)"Résidu/(Résidu initial)=",test
     write(*,*)"Nombre d'itérations maximal :",itbcgsmax
     write(*,*)"*****************"
     call preilu(nordre1)
  end if


!!!!!!!!!!!!!!!
  !Instationnaire
!!!!!!!!!!!!!!!
  vxd(:)=0._8
  vyd(:)=0._8
  tpd(:)=0._8
  if (reprise==0) then
     vx(:)=0._8
     vy(:)=0._8
     tp(:)=0._8
     p(:)=0._8
  end if

  t=0._8
  delobj=delpourc !??????????,
  ucentt=1._8/centt
  ts=0._8
  if (duree>0) then
     write(*,*)"==========================="
     write(*,*)"=======INSTATIONNAIRE======"
     write(*,*)"==========================="

     dt_i=dt

     x_pos(1)=.01_8
     y_pos(1)=.5_8

     x_pos(2)=.25_8/2._8
     y_pos(2)=.5_8

     x_pos(3)=.25_8-.01_8
     y_pos(3)=.5_8

     do k=1,3
        distance=1000000
        do i=1,ncv
           if (sqrt((xcv(i)-x_pos(k))**2+(ycv(i)-y_pos(k))**2)<distance) then
              distance=sqrt((xcv(i)-x_pos(k))**2+(ycv(i)-y_pos(k))**2)
              i_pos(k)=i
           end if
        end do
     end do
  end if

  debit_prec = 0
  do while(t<duree)
     t=t+dt
     iter=0
     cvgce=0
     vxd(:)=vx(:)
     vyd(:)=vy(:)
     tpd(:)=tp(:)

     do while (cvgce==0)
        iter=iter+1
        if (iter>10) then
           write(*,*)"Pas de convergence en 10 itérations"
           goto 100
        end if

        call calculsl(&
             xcv,ycv,mcv,xs,ys,A,bx,by,&
             vxd,vyd,tpd,vx,vy,p,tp,&
             ptvois,nuvois,nusom,&
             ncv,&
             ucentt,dt,&
             aa,bb,dd,&
             nutest,vtest,&
             comu,&
             diffusion,&
             coefdt,coefdu,coefb,&
             eta)

        if (direct) then
           call resdir(residu)

        else
           call bicgstab(itbcgs,nordre1,residu,residu_old)
        end if

        !Remise dans les tableaux naturels
        do i=1,ncv
           st(i)=bb(i,4)
           sp(i)=bb(i,3)
           sy(i)=bb(i,2)
           sx(i)=bb(i,1)
        end do

        vxnorme=maxval(abs(vx(:)))
        vynorme=maxval(abs(vy(:)))
        vnorme=max(vxnorme,vynorme)
        tnorme=maxval(abs(tp(:)))

        theta=residobj/residu
        if (theta>thetamax) theta=thetamax !
        tp(:)=tp(:)+st(:)*theta
        vx(:)=vx(:)+sx(:)*theta
        vy(:)=vy(:)+sy(:)*theta
        p(:) =p(:) +sp(:)*theta

        if (residu<(vnorme+tnorme)*epsilon_newton) then
           cvgce=1
        else
           cvgce=0
        end if
        if (aff==1) write(*,10)residu,theta,diffusion,itbcgs
     end do
10   format("Residu : ",1pe6.0," theta : ",1pe6.0," diffusion : ",1pe8.1," (ItBcgs=",i3,&
          ")")

!!$     do i=1,ncv
!!$        press(i)=p(i)-.5_8*(vx(i)*vx(i)+vy(i)*vy(i))
!!$     end do

     !Remise des vitesses moyennes de fin
     delobtt=0._8
     delobtu=0._8
     do i=1,ncv
        tp(i)=(tp(i)-tpd(i)*(1-centt))*ucentt
        vx(i)=(vx(i)-vxd(i)*(1-centt))*ucentt
        vy(i)=(vy(i)-vyd(i)*(1-centt))*ucentt
        if (abs(tp(i)-tpd(i))>delobtt) delobtt=abs(tp(i)-tpd(i))
        if (abs(vx(i)-vxd(i))>delobtu) delobtu=abs(vx(i)-vxd(i))
        if (abs(vy(i)-vyd(i))>delobtu) delobtu=abs(vy(i)-vyd(i))
     end do



     write(*,20)t/duree*100,t,dt,iter,&
          vnorme,delobtu/dt/vnorme,&
          tnorme,delobtt/dt/tnorme,debit_in
20   format("% Temps eff :",1pe7.1," | t=",1pe10.4," | dt=",1pe7.1," || ",i3," iter ",&
          " | max(u)=",1pe7.1," | max(du/dt)/max(u)=",1pe7.1,&
          " | max(T)=",1pe7.1," | max(dT/dt)/max(T)=",1pe7.1,&
          " | debit=",1pe10.4)
     ! if (delobj>0) dt=max(dt_i,dt*delobj/max(delobtu/vnorme,delobtt/tnorme))

!!$     call nusselt(A,&
!!$       tp,&
!!$       nutest,&
!!$       eta,&
!!$       nui,nuo)


     write(10,*)t,tp(i_pos(1)),vx(i_pos(1)),vy(i_pos(1)), nui,nuo
     write(11,*)t,tp(i_pos(2)),vx(i_pos(2)),vy(i_pos(2)), nui,nuo
     write(12,*)t,tp(i_pos(3)),vx(i_pos(3)),vy(i_pos(3)), nui,nuo

     debit_prec = debit_in

     dt = dt * DELTA_OBJ / delobtu
     dt = max(dt, dt_i)
     dt = min(dt, DT_MAX)
  end do

!!!!!!!!!!!!!!!
  !Stationnaire
!!!!!!!!!!!!!!!
  ucentt=0._8
  iter=0
  cvgce=0
  if (duree<=0) then
     write(*,*)"==========================="
     write(*,*)"=======STATIONNAIRE========"
     write(*,*)"==========================="
  end if
  do while (cvgce==0)
     iter=iter+1
     if (iter>1000) then
        write(*,*)"Pas de convergence en 1000 itérations"
        stop
     end if

     call calculsl(&
          xcv,ycv,mcv,xs,ys,A,bx,by,&
          vxd,vyd,tpd,vx,vy,p,tp,&
          ptvois,nuvois,nusom,&
          ncv,&
          ucentt,dt,&
          aa,bb,dd,&
          nutest,vtest,&
          comu,&
          diffusion,&
          coefdt,coefdu,coefb,&
          eta)

     if (direct) then
        residu_old=1D40
        call resdir(residu)
     else
        call bicgstab(itbcgs,nordre1,residu,residu_old)
     end if

     !Remise dans les tableaux naturels
     do i=1,ncv
        st(i)=bb(i,4)
        sp(i)=bb(i,3)
        sy(i)=bb(i,2)
        sx(i)=bb(i,1)
     end do

     vxnorme=maxval(abs(vx(:)))
     vynorme=maxval(abs(vy(:)))
     vnorme=max(vxnorme,vynorme)
     tnorme=maxval(abs(tp(:)))

     theta=residobj/residu
     if (theta>thetamax) theta=thetamax !
     tp(:)=tp(:)+st(:)*theta
     vx(:)=vx(:)+sx(:)*theta
     vy(:)=vy(:)+sy(:)*theta
     p(:) =p(:) +sp(:)*theta


     if (residu < (vnorme+tnorme)*epsilon_newton) then
        cvgce=1
     else
        cvgce=0
     end if
     write(*,30)iter,residu,max(&
          maxval(abs(st(:))),&
          maxval(abs(sx(:))),&
          maxval(abs(sy(:))),&
          maxval(abs(sp(:)))),&
          theta,diffusion,itbcgs

  end do

  vnorme=0._8
  tnorme=0._8
  do i=1,ncv
     if (abs(vx(i))>vnorme) vnorme=abs(vx(i)) !max des composantes de vitesse
     if (abs(vy(i))>vnorme) vnorme=abs(vy(i))
     if (abs(tp(i))>tnorme) tnorme=abs(tp(i))
  end do

  write(*,*)"Max(u)=",maxval(abs(vx))
  write(*,*)"Max(v)=",maxval(abs(vy))
  write(*,*)"Max(T)=",tnorme


30 format("Iter=",i3," Residu : ",1pe8.2," dx : ",1pe8.2," theta : ",1pe8.2," diffusion : ",1pe9.2," (ItBcgs=",i3,&
       ")")
!!$  if (duree<=0) then
!!$     do i=1,ncv
!!$        press(i)=p(i)-.5_8*(vx(i)*vx(i)+vy(i)*vy(i))
!!$     end do
!!$  end if

!!!!!!!!!!!!!!!
  !Sortie
!!!!!!!!!!!!!!!


100 call ligncour(&
       mcv,A,bx,by,&
       xs,ys,&
       vx,vy,p,&
       ptvois,nusom,nuvois,&
       ncv,nbsom&
       )


  write(*,*)"Max psi =",maxval(potent)
  write(*,*)"Min psi =",minval(potent)

  call nusselt(A,&
       tp,&
       nutest,&
       eta,&
       nui,nuo)

  call sortie(&
       vx,vy,p,tp,mcv,&
       potent,&
       xcv,ycv,xs,ys,&
       ptvois,nusom,&
       ncv,nbsom,nptvois)


contains


  !###################################################################
  !Initialisation de la taille des tableaux
  !pour chaque maille, 1-qqmvt/x
  !                    2-qqmvt/y
  !                    3-divergence
  !                    4-NRJ
  !Variables (p,u,v)
  !*******************************

  !###################################################################
  !
  subroutine calculsl(&
       xcv,ycv,mcv,xs,ys,A,bx,by,&
       vxd,vyd,tpd,vx,vy,p,tp,&
       ptvois,nuvois,nusom,&
       ncv,&
       ucentt,dt,&
       aa,bb,dd,&
       nutest,vtest,&
       comu,&
       diffusion,&
       coefdt,coefdu,coefb,&
       eta)
    implicit none

    real(kind=8),dimension(:),intent(in)::xcv,ycv,mcv,xs,ys,A,bx,by
    integer,dimension(:),intent(in)::ptvois,nuvois,nusom
    real(kind=8),dimension(:,:,:),intent(out)::aa,dd
    real(kind=8),dimension(:,:),intent(out)::bb
    real(kind=8),dimension(:),intent(in)::vxd,vyd,tpd,vx,vy,p,tp
    integer,intent(in)::ncv,nutest
    real(kind=8),intent(in)::ucentt,dt,vtest,comu,&
         coefdt,coefdu,coefb,&
         eta
    real(kind=8),intent(out)::diffusion

    integer::i0,i1
    integer::i
    integer::k,kk
    integer::j,jj,j1,j0,k0
    real(kind=8)::u_lim,v_lim,p_lim,t_lim,sx_lim,sy_lim,st_lim,x_lim,y_lim
    integer::neumann_t,neumann_u,neumann_v,neumann_p,ind,&
         ipress,ipresslocal
    real(kind=8)::xxi,xxj,bxa,bya
    integer::flagu,flagv,flagt

    real(kind=8)::test_maille,pe_maille,re_maille,coef,erx,ery,y

    diffusion=0
    ipress=0
    ipresslocal=0

    pe_maille=0
    re_maille=0

    aa=0
    bb=0
    dd=0

    debit_out=0
    debit_in=0
    ! calcul du débit
    do i=1,ncv
       i0=nusom(ptvois(i+1))
       do j=ptvois(i)+1,ptvois(i+1)
          jj=nuvois(j)
          i1=nusom(j)
          if (nuvois(j)<=0) then
             if (jj==-4) then !Bord haut
                debit_out=debit_out+(xs(i1)-xs(i0))*vy(i)
             end if
             if (jj==-3) then !Bord bas
                debit_in=debit_in+(xs(i1)-xs(i0))*vy(i)
             end if
          end if
       end do
       i0 = i1
    end do
    debit_out = abs(debit_out)
    debit_in = abs(debit_in)
    debit = debit_in
    
    do i=1,ncv

       bb(i,1)=bb(i,1)+mcv(i)*(-ucentt*(vx(i)-vxd(i))/dt)
       dd(i,1,1)=dd(i,1,1)+ucentt*mcv(i)/dt

       bb(i,2)=bb(i,2)+mcv(i)*(-ucentt*(vy(i)-vyd(i))/dt + coefb*tp(i))
       dd(i,2,2)=dd(i,2,2)+ucentt*mcv(i)/dt
       dd(i,2,4)=dd(i,2,4)-mcv(i)*coefb

       bb(i,4)=bb(i,4)+mcv(i)*(-ucentt*(tp(i)-tpd(i))/dt)
       dd(i,4,4)=dd(i,4,4)+ucentt*mcv(i)/dt


       i0=nusom(ptvois(i+1))
       do j=ptvois(i)+1,ptvois(i+1)
          jj=nuvois(j)
          i1=nusom(j)
          !calcul des flux diffusifs
          if (nuvois(j)<=0) then !Bord
             call testsol(&
                  u_lim,v_lim,p_lim,t_lim,&
                  sx_lim,sy_lim,st_lim,&
                  neumann_t,neumann_u,neumann_v,neumann_p,&
                  nutest,vtest,eta,&
                  i,j,i0,i1)

!!$UX
             flagu=1-neumann_u
             bb(i,1)=bb(i,1)+A(j)*(u_lim-vx(i))*coefdu*flagu
             dd(i,1,1)=dd(i,1,1)+A(j)*coefdu*flagu

!!$UY
             flagv=1-neumann_v
             bb(i,2)=bb(i,2)+A(j)*(v_lim-vy(i))*coefdu*flagv
             dd(i,2,2)=dd(i,2,2)+A(j)*coefdu*flagv

!!$DIV
             !**L
             bb(i,3)=bb(i,3)-bx(j)*u_lim*flagu-by(j)*v_lim*flagv

             if (neumann_p==0) then
                !Pénalisation par le laplacien
                ipress=1
                ipresslocal=1
             end if

!!$TEMP
             flagt=1-neumann_t
             bb(i,4)=bb(i,4)+A(j)*(t_lim-tp(i))*coefdt*flagt+&
                  t_lim*coefdt*neumann_t
             dd(i,4,4)=dd(i,4,4)+A(j)*coefdt*flagt

          else !intérieur
             !trouver le numéro de voisinage de i par rapport à j, pour accéder à son {bx,by}
             ind = ptvois(jj)+1
             do while (nuvois(ind) /= i)
                ind = ind+1
             end do
             !les bx,by vus de l'autre côté. on change le signe pour que les formules d'après soient plus lisibles
             bxa = -bx(ind)
             bya = -by(ind)

             k=ptmat(i)+1
             do while (jj/=numat(k))
                k=k+1
             end do

             xxi = bx(j)*vx(i) + bxa*vx(jj)&
                  +by(j)*vy(i) + bya*vy(jj)&
                  -As(j)*(p(jj)-p(i))

             !Calcul Péclet et Reynolds de maille
             test_maille=xxi/A(j)
             pe_maille=max(pe_maille,test_maille/coefdt)
             re_maille=max(re_maille,test_maille/coefdu)





!!$UX
             !**L
             bb(i,1)=bb(i,1)+A(j)*(vx(jj)-vx(i))*coefdu
             dd(i,1,1)=dd(i,1,1)+A(j)*coefdu
             aa(k,1,1)=aa(k,1,1)-A(j)*coefdu
             !Termes en pression pour les aretes intérieures
             bb(i,1)=bb(i,1)-bx(j)*(p(jj)-p(i))
             dd(i,1,3)=dd(i,1,3)-bx(j)
             aa(k,1,3)=aa(k,1,3)+bx(j)
             !**NL
             !ajout des termes non-linéaires de transport
             xxj=(vx(jj)-vx(i))*.5_8
             bb(i,1)=bb(i,1)-xxi*xxj
             dd(i,1,1)=dd(i,1,1)+bx(j)*xxj-xxi*.5_8
             dd(i,1,2)=dd(i,1,2)+by(j)*xxj
             dd(i,1,3)=dd(i,1,3)+As(j)*xxj
             aa(k,1,1)=aa(k,1,1)+bxa*xxj+xxi*.5_8
             aa(k,1,2)=aa(k,1,2)+bya*xxj
             aa(k,1,3)=aa(k,1,3)-As(j)*xxj
!!$UY
             !**L
             bb(i,2)=bb(i,2)+A(j)*(vy(jj)-vy(i))*coefdu
             dd(i,2,2)=dd(i,2,2)+A(j)*coefdu
             aa(k,2,2)=aa(k,2,2)-A(j)*coefdu
             !Termes en pression pour les aretes intérieures
             bb(i,2)=bb(i,2)-by(j)*(p(jj)-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)
             aa(k,2,3)=aa(k,2,3)+by(j)
             !**NL
             !ajout des termes non-linéaires de transport
             xxj=(vy(jj)-vy(i))*.5_8
             bb(i,2)=bb(i,2)-xxi*xxj
             dd(i,2,2)=dd(i,2,2)+by(j)*xxj-xxi*.5_8
             dd(i,2,1)=dd(i,2,1)+bx(j)*xxj
             dd(i,2,3)=dd(i,2,3)+As(j)*xxj
             aa(k,2,1)=aa(k,2,1)+bxa*xxj
             aa(k,2,2)=aa(k,2,2)+bya*xxj+xxi*.5_8
             aa(k,2,3)=aa(k,2,3)-As(j)*xxj

!!$TEMP
             !**L
             bb(i,4)=bb(i,4)+A(j)*(tp(jj)-tp(i))*coefdt
             dd(i,4,4)=dd(i,4,4)+A(j)*coefdt
             aa(k,4,4)=aa(k,4,4)-A(j)*coefdt
             !**NL
             !ajout des termes non-linéaires de transport
             xxj=(tp(jj)-tp(i))*.5_8
             bb(i,4)=bb(i,4)-xxi*xxj
             dd(i,4,1)=dd(i,4,1)+bx(j)*xxj
             dd(i,4,2)=dd(i,4,2)+by(j)*xxj
             dd(i,4,3)=dd(i,4,3)+As(j)*xxj
             dd(i,4,4)=dd(i,4,4)-xxi*.5_8
             aa(k,4,1)=aa(k,4,1)+bxa*xxj
             aa(k,4,2)=aa(k,4,2)+bya*xxj
             aa(k,4,3)=aa(k,4,3)-As(j)*xxj
             aa(k,4,4)=aa(k,4,4)+xxi*.5_8




!!$DIV
             !**L
             bb(i,3)=bb(i,3)-xxi

             dd(i,3,1)=dd(i,3,1)+bx(j)
             dd(i,3,2)=dd(i,3,2)+by(j)
             aa(k,3,1)=aa(k,3,1)+bxa
             aa(k,3,2)=aa(k,3,2)+bya
             !Pénalisation par le laplacien
             dd(i,3,3)=dd(i,3,3)+As(j)
             aa(k,3,3)=aa(k,3,3)-As(j)


!!$DIFFUSION
!!$             xxi=.5_8*((bx(j)*(vx(i)+vx(jj))+by(j)*(vy(i)+vy(jj)))-As(j)*(p(jj)-p(i)) )
!!$             diffusion=diffusion+xxi*( vx(jj)*vx(i)+vy(jj)*vy(i) )
          end if
          i0=i1
       end do
    end do


    !ipresslocal=1

    if (nutest>=100) ipresslocal=1

    if (ipresslocal==0) then
       print*,'ipresslocal=',ipresslocal
       !stop
       i=ncv/2
       do k=ptmat(i)+1,ptmat(i+1)
          aa(k,3,1)=0
          aa(k,3,2)=0
          aa(k,3,3)=0
          aa(k,3,4)=0
       end do
       dd(i,3,1)=0
       dd(i,3,2)=0
       dd(i,3,3)=1
       dd(i,3,4)=0
       bb(i,3)=(0-p(i))
    end if


    if (dt<=0)    then
       print*,'Debit entrant',debit_in
       print*,'Peclet de maille :',pe_maille
    end if
    if (nutest>=100) then
       ! if (dt<=0)  print*,'Débit sortant :',debit_out,'      Débit entrant :',debit_in
       ! print*,'Débit entrant',debit_in
!!$      print*,'NUSSELT maximal (flux imposé) :',1/maxval(tp)
       debit=debit_out
    end if
  end subroutine calculsl





  !6969696969696969696969696969696969696969696969696969696969696969696969
  !6969696969696969696969696969696969696969696969696969696969696969696969
  !6969696969696969696969696969696969696969696969696969696969696969696969
  !69696969696969696969                   9696969696969696969696969696969
  !6969696969696969696   Routine générales  96969696969696969696969696969
  !69696969696969696969                   9696969696969696969696969696969
  !6969696969696969696969696969696969696969696969696969696969696969696969
  !6969696969696969696969696969696969696969696969696969696969696969696969
  !6969696969696969696969696969696969696969696969696969696969696969696969
  !6969696969696969696969696969696969696969696969696969696969696969696969


  !###################################################################
  !***************
  !Calcul du facteur de régularité et de l'aire des volumes
  !Facteur de régularité = d(centre volume, centre arete) / lg de l'arete
  !(triangle rectangle : 0.5)
  !***************
  subroutine regularite(&
       xs,ys,xcv,ycv,ptvois,nusom,ncv,&
       mcv&
       )
    implicit none

    real(kind=8),dimension(:),intent(in)::xs,ys,xcv,ycv
    integer,dimension(:),intent(in)::ptvois,nusom
    integer,intent(in)::ncv

    real(kind=8),dimension(:),intent(out)::mcv

    real(kind=8)::a11,a12,a21,a22,aire,ax,ay,diam,xx
    real(kind=8)::zeta
    integer::i,i1,i2,j

    zeta=1._8
    do i=1,ncv
       mcv(i)=0._8
       i1=nusom(ptvois(i+1)) !pointe sur le dernier noeud du triangle i
       a11=xs(i1)-xcv(i)
       a12=ys(i1)-ycv(i)
       do j=ptvois(i)+1,ptvois(i+1) !boucle sur tous les 3 noeuds consécutifs
          i2=nusom(j)
          a21=xs(i2)-xcv(i)
          a22=ys(i2)-ycv(i)
          aire=a11*a22-a12*a21 !aire du losange ICI2 \times ICI1
          !longueur de l'arete:
          ax=xs(i2)-xs(i1)
          ay=ys(i2)-ys(i1)
          diam=ax*ax+ay*ay ! I1I2.I1I2
          xx=aire/diam
          if (aire<0) then
             if (diam>1e-8)&
                  write(*,*)"Aire négative : aire=0 !!!!, diam=",diam
             aire=0
             xx=1
             stop
          end if
          if (xx<zeta) zeta=xx ! recherche du rapport le plus petit (facteur de régularité)
          mcv(i)=mcv(i)+aire*0.5_8 !contribution à l'aire du triangle
          !on modifie le noeud de départ I1
          i1=i2
          a11=a21
          a12=a22
       end do
    end do
    write(*,*)"Facteur de régularité : ",zeta
  end subroutine regularite
  !###################################################################
  !***************
  !Calcul des pointeurs
  !ptsc : pointeur contenant le cumul du nombre des aretes
  !nusc : numéro sommet de l'arete dans un tableau cumulé
  !***************
  subroutine pointeur(&
       ptvois,ncv,nbsom,nusom,&
       ptsc&
       )
    implicit none

    integer,dimension(:),intent(in)::ptvois,nusom
    integer::ncv,nbsom
    integer,dimension(:),intent(inout)::ptsc

    integer::i,j,k,is,js,nbc,i0


    !comptage du nombre maximum de sommets connectés pour chaque noeud
    do i=1,ncv
       do j=ptvois(i)+1,ptvois(i+1)
          is=nusom(j)
          ptsc(is)=ptsc(is)+2 ! décompte par noeud des sommets connectés
       end do
    end do

    nbc=0
    do i=1,nbsom
       j=ptsc(i)
       ptsc(i)=ptsc(i)+nbc !ptcs(i) cumul le nb de sommets connectés
       nbc=nbc+j !calcul du nombre total de sommets connectés
    end do
    ptsc(nbsom+1)=nbc+1

  end subroutine pointeur


  !###################################################################
  !***************
  !Calcul des pointeurs
  !ptsc : pointeur contenant le cumul du nombre des aretes
  !nusc : numéro sommet de l'arete dans un tableau cumulé
  !***************
  subroutine pre_num_voisin_volume(&
       ptvois,nusom,ncv,nbsom,nusc,&
       ptsc&
       )
    implicit none

    integer,dimension(:),intent(in)::ptvois,nusom
    integer,intent(in)::ncv,nbsom
    integer,dimension(:),intent(inout)::ptsc
    integer,dimension(:),intent(out)::nusc

    integer::i,j,k,is,js,nbc,i0


    !définition du numéro de l'arete : même numéro que le noeud connecté
    do i=1,ncv
       is=nusom(ptvois(i+1)) ! pointe sur 3ième noeud du triangle i
       do j=ptvois(i)+1,ptvois(i+1) !boucle sur tous les noeuds du triangle i
          js=nusom(j) !numéro du sommet          nusc(ptsc(is))=js !numéro de l'arete = noeud connecté
          nusc(ptsc(is))=js ! le pointer pointe sur le sommet connecté (au noeud is) précédent
          ptsc(is)=ptsc(is)-1
          nusc(ptsc(js))=is
          ptsc(js)=ptsc(js)-1! pour le noeud is, un sommet connecté traité :
          is=js !noeuds suivants pour décrire tous les noeuds du triangle.
       end do
    end do

    !remise des pointeurs au début de chaque séquence de sommets connectés
    do i=1,nbsom
       ptsc(i)=ptsc(i)+1 !ajout de 1 sinon ptsc(1)=0 !!!!
    end do

    !remise dans l'ordre des numéros des aretes
    do i=1,nbsom
       call trie(nusc(ptsc(i):ptsc(i+1)-1) , ptsc(i+1)-ptsc(i) ) !ptsc(i+1)-ptsc(i)= nb aretes
    end do

    !Elimination des aretes en doublon (de part et d'autre d'une arete commune à 2 triangles)
    !et
    !Modification du pointeur
    nbc=1
    do i=1,nbsom
       i0=ptsc(i) !pointe sur le nombre de sommets connectés
       ptsc(i)=nbc !nouveau pointeur
!!$     if (ptsc(i+1)>ptsc(i)) then !le nombre de sommets connecté au noeud i : ptsc(i+1)-ptsc(i)
       !
       !
       !
!!$       if (ptsc(i+1)>i0) then !le nombre de sommets connecté au noeud i : ptsc(i+1)-ptsc(i)
       if (ptsc(i+1)>ptsc(i)) then !le nombre de sommets connecté au noeud i : ptsc(i+1)-ptsc(i)
          !
          !
          !
          k=nusc(i0)
          nusc(nbc)=k !stock le numéro arete
          nbc=nbc+1
          do j=i0+1, ptsc(i+1)-1
             !si le numéro de l'arete (classé) est différent du précédent, on conserve
             if (nusc(j) /= k) then
                k=nusc(j)
                nusc(nbc)=k
                nbc=nbc+1
             end if
          end do
       end if
    end do

    ptsc(nbsom+1)=nbc

    nbc=nbc-1
  end subroutine pre_num_voisin_volume

  !###################################################################
  !Détermination des voisins :
  !voisind,voising définis sur la base des pointeurs
  !nuvois(i)=k correspond aux numéros entre voisins : arete nunero i voisine droite triangle k
  !*******************************
  subroutine num_voisin_volume(&
       ncv,nusom,ptsc,nusc,&
       ptvois,&
       nuvois,&
       nbsom)

    implicit none

    integer,intent(in)::ncv,nbsom
    integer,dimension(:),intent(in)::ptvois,nusom,ptsc,nusc
    integer,dimension(:),intent(out)::nuvois
    integer,dimension(:),allocatable::voisind,voising

    integer::i,j,k,is,js,nbc

    !preparation des voisin à l'aide du pointeur
    nbc=ptsc(nbsom+1)-1
    allocate(voisind(nbc),voising(nbc))
    voisind=0
    voising=0
    do i=1,ncv !Parcours de tous les triangles
       is=nusom(ptvois(i+1))
       do j=ptvois(i)+1,ptvois(i+1)
          js=nusom(j)
          !recherche du numéro de l'arete (le numéro correspond au noeud connecté)
          k=ptsc(is)
          do while (nusc(k)<js) !tant que le numéro de l'arete est inférieure au numéro du sommet
             k=k+1 !arete suivante
          end do
          if (nusc(k)/=js) then
             write(*,*)"Problème dans les voisins js"
             stop
          end if
          voising(k)=i ! voisin gauche de l'arete nusc(k) : triangle i
          !recherche du numéro de l'arete
          k=ptsc(js)
          do while (nusc(k)<is)
             k=k+1
          end do
          if (nusc(k)/=is) then
             write(*,*)"Problème dans les voisins is"
             stop
          end if
          voisind(k)=i
          is=js
       end do
    end do

    !Fabrication du tableau des voisins de chaque numéro d'arete j :
    do i=1,ncv
       is=nusom(ptvois(i+1))
       do j=ptvois(i)+1,ptvois(i+1)
          js=nusom(j)
          !recherche du pointeur de l'arete
          k=ptsc(is)
          do while (nusc(k)/=js)
             k=k+1
          end do
          nuvois(j)=voisind(k)
!!$          print*,'volume=',i,' arete=',js,' triangle  voisin : ',nuvois(j)
          is=js
       end do
    end do

    deallocate(voisind,voising)
  end subroutine num_voisin_volume

  !###################################################################
  !Calcul des coefficients A
  !coef vitesse-vitesse
  !longueur de l'arete divisée par la distance entre les centres des mailles
  !VOIR ARTICLE
  !*******************************
  subroutine calcul_coef(&
       nusom,ptvois,nuvois,&
       xs,ys,xcv,ycv,&
       A,bx,by,As,&
       ncv)
    implicit none

    integer,dimension(:),intent(in)::nusom,ptvois,nuvois
    real(kind=8),dimension(:),intent(out)::A,bx,by,As
    real(kind=8),dimension(:),intent(in)::xs,ys,xcv,ycv
    integer,intent(in)::ncv
    integer::i,j,i0,i1,ic,icc,jjj,j1,k,j0,k0,l,l0
    real(kind=8)::a11,a12,a21,a22,b0,x_lim,y_lim,diam, xj, yj, xi0, yi0, d_j_droite,ponderation

    integer,dimension(:),allocatable::denlcv

    integer,parameter::macromaille=2
    integer::n_cluster,n_som_in,&
         n_maille_a_rattacher,n_maille_rattacher,n_maille_attacher
    integer,dimension(:),allocatable::pt_som,num_maille_par_sommet,&
         cluster,maille_utilise,nb_attach_clus

    logical,dimension(:),allocatable::sommet_utilise



    As(:)=0

    ic=0
    icc=0
    do i=1,ncv
       i0=nusom(ptvois(i+1))
       do j=ptvois(i)+1,ptvois(i+1)
          i1=nusom(j)
          a11=xs(i1)-xs(i0) !I0I1
          a12=ys(i1)-ys(i0)
          if (nuvois(j)<=0) then !Triangle droit absent (bord du domaine)
             !I=insection(médiane,coté)
             !(I0IC.I0I1) =  d(I0,I) x d(IOI1)
             b0=((xcv(i)-xs(i0))*a11+(ycv(i)-ys(i0))*a12)/(a11*a11+a12*a12) !(I0IC.I0I1)/(I0I1.I0I1)
             x_lim=xs(i0)+b0*a11 ! coordonnée de l'intersection (médiane,coté) I
             y_lim=ys(i0)+b0*a12
             a21=xcv(i)-x_lim !IIIC
             a22=ycv(i)-y_lim

             diam=a11*a11+a12*a12
             if (diam<1e-10) then
                ic=ic+1
                !print*,"La maille ",i," a deux sommets identiques sur le bord"
                A(j)=0
                bx(j)=0
                by(j)=0
             else
                A(j)=(a11*a11+a12*a12)/(a11*a22-a12*a21) ! d(I0,I1)/d(C,I)
                bx(j)=-A(j)*a21 !-d(I1,I0)IIIC =d(I1,I0)ICII
                by(j)=-A(j)*a22 !
             end if
          else
             a21=xcv(i)-xcv(nuvois(j)) !ICj ICi
             a22=ycv(i)-ycv(nuvois(j))
             diam=a11*a11+a12*a12
             if (diam<1e-10) then
                icc=icc+1
                !print*,"La maille ",i," a deux sommets identiques sur le bord"
                A(j)=0
                bx(j)=0
                by(j)=0
             else
                A(j)=(a11*a11+a12*a12)/(a11*a22-a12*a21) ! d(I0,I1)/d(C,I)
#if PONDERATION
                ! pondération par la distance de j à la droite I0, I1
                ! distance de j à la droite I0, I1 : d = sqrt(i0j^2 - ((i0i1 scal i0j)/i0i1)^2)
                xj = xcv(nuvois(j))
                yj = ycv(nuvois(j))
                xi0 = xs(i0);
                yi0 = ys(i0);
                d_j_droite = sqrt((xj - xi0)*(xj - xi0) + (yj - yi0)*(yj - yi0) - &
                                  (a11 * (xi0 - xj) + a12 * (yi0 - yj))**2 / (a11*a11 + a12 * a12))
                ponderation = 1 - d_j_droite / sqrt(a21*a21 + a22*a22) !entre 0 et 1
                bx(j) = -A(j) * a21 * ponderation
                by(j) = -A(j) * a22 * ponderation
#else
                !ponderation par 1/2
                bx(j)=-A(j)*a21*.5_8
                by(j)=-A(j)*a22*.5_8
#endif
             end if
          end if
          if (A(j)<0) then
             write(*,*)"Conditions non remplies sur le maillage ...",i,j,A(j)
             stop
          end if
          i0=i1
       end do
    end do
    print*,"Nombre de bords éliminés : ",ic
    print*,"Nombre de faces éliminées : ",icc/2


    if (macromaille==1) then

       write(*,*)"Macro-mailles par mailles, macromaille=",macromaille
!!!
!!!Amas de mailles par blocs contigus
!!!
       k=0 !compteur des macro-mailles
       allocate(denlcv(ncv))
       denlcv(:)=0
       As=0

       !Constitution des amas de base
       do i=1,ncv
          !recherche si les voisins de i appartiennentà une macro-maille
          jjj=denlcv(i)
          do j=ptvois(i)+1,ptvois(i+1)
             i1=nuvois(j)
             if (i1>0) then
                if (denlcv(i1)/=0) jjj=1
             end if
          end do

          !Si les voisins n'appartiennent pas à une macro maille
          if (jjj==0) then
             k=k+1
             denlcv(i)=k !la maille i appartient à la macro-maille k
             do j=ptvois(i)+1,ptvois(i+1)
                i1=nuvois(j)
                if (i1>0) then
                   As(j)=As(j)+A(j)*lambda
                   denlcv(i1)=k !la maille i1 appartient à la macro-maille k
                   !recherche de la face vue par i1 (et non par i)
                   j1=ptvois(i1)+1
                   do while (nuvois(j1)/=i)
                      j1=j1+1
                   end do
                   As(j1)=As(j)
                end if
             end do
          end if
       end do

       write(*,*)"Nombre de macro-mailles :",k

       !accrochage virtuel des mailles isolées aux macro-mailles déjà constituées
       do i=1,ncv
          if (denlcv(i)==0) then
             do j=ptvois(i)+1,ptvois(i+1)
                i1=nuvois(j)
                if (i1>0) then
                   if (denlcv(i)==0) then !Raccrochement d'une face
                      if (denlcv(i1)>0) then !une macro-maille voisine
                         denlcv(i)=-denlcv(i1) !valeur négative temporaire pour éviter des rattachement abusifs
                         As(j)=As(j)+A(j)*lambda
                         j1=ptvois(i1)+1
                         do while (nuvois(j1)/=i)
                            j1=j1+1
                         end do
                         As(j1)=As(j)
                      end if
                   else
                      if (denlcv(i1)==-denlcv(i)) then !Raccrochement des autres faces à la même macro-maille (si nécessaire)
                         As(j)=As(j)+A(j)*lambda
                         j1=ptvois(i1)+1
                         do while (nuvois(j1)/=i)
                            j1=j1+1
                         end do
                         As(j1)=As(j)
                      end if
                   end if
                end if
             end do

             if (denlcv(i)==0) then !la raccrochement a échoué !!!!
                write(*,*)"Problème dans la construction des macro-mailles : mailles isolées !!!"
                stop
             end if
          end if
       end do

       !Accrochage effectif des mailles isolées
       do i=1,ncv
          if (denlcv(i)<0) denlcv(i)=-denlcv(i)
       end do

       deallocate(denlcv)



    elseif (macromaille==2) then
       write(*,*)"Macro-mailles par sommets, macromaille=",macromaille


       !Décompte des sommets intérieurs
       allocate(sommet_utilise(nbsom))
       sommet_utilise(:)=.false.
       l=0
       do i=1,ncv
          j0=ptvois(i+1)
          do j=ptvois(i)+1,ptvois(i+1)
             k0=nusom(j0)
             if (nuvois(j0)>0.and.nuvois(j)>0.and.(.not.sommet_utilise(k0))) then
                sommet_utilise(k0)=.true.
                l=l+1
             end if
             j0=j
          end do
       end do

       n_som_in=l
       write(*,*)"Nombre de sommets intérieurs :",n_som_in


       allocate(pt_som(nbsom+1))

!!!
!!!comptage du nombre de mailles par sommet intérieur
!!!
       pt_som(1:nbsom+1)=0
       do i=1,ncv
          do j=ptvois(i)+1,ptvois(i+1)
             j0=nusom(j)
             if (sommet_utilise(j0)) pt_som(j0)=pt_som(j0)+1
          end do
       end do

       do i=1,nbsom
          pt_som(i+1)=pt_som(i)+pt_som(i+1)
       end do

!!!
!!!On associe les mailles aux noeuds (cluster)
!!!
       allocate(num_maille_par_sommet(pt_som(nbsom+1)))
       do i=1,ncv
          do j=ptvois(i)+1,ptvois(i+1)
             j0=nusom(j)
             k0=pt_som(j0)
             if (sommet_utilise(j0)) then
                num_maille_par_sommet(k0)=i
                pt_som(j0)=k0-1
             end if
          end do
       end do

!!!
!!!Construction des clusters de base : contient les numéros des noeuds
!!!
       allocate(maille_utilise(ncv))
       maille_utilise(:)=0
       n_cluster=0
       do i=1,nbsom
          if (sommet_utilise(i)) then
             k=0
             do j=pt_som(i)+1,pt_som(i+1)
                j0=num_maille_par_sommet(j)
                if (maille_utilise(j0)/=0) then
                   k=1 !La maille appartient deja à un cluster
                   exit
                end if
             end do
             if (k==0) then
                n_cluster=n_cluster+1
                do j=pt_som(i)+1,pt_som(i+1)
                   j0=num_maille_par_sommet(j)
                   maille_utilise(j0)=n_cluster
                end do
             end if
          end if
       end do

       allocate(cluster(n_cluster))


       maille_utilise(:)=0
       n_cluster=0
       do i=1,nbsom
          if (sommet_utilise(i)) then
             k=0
             do j=pt_som(i)+1,pt_som(i+1)
                j0=num_maille_par_sommet(j)
                if (maille_utilise(j0)/=0) then
                   k=1 !La maille appartient deja à un cluster
                   exit
                end if
             end do
             if (k==0) then
                n_cluster=n_cluster+1
                cluster(n_cluster)=i
                do j=pt_som(i)+1,pt_som(i+1)
                   l=j-pt_som(i)
                   j0=num_maille_par_sommet(j)
                   maille_utilise(j0)=n_cluster
                end do
             end if
          end if
       end do

       write(*,*)"Nombre de cluster de base :",n_cluster

!!$       do i=1,n_cluster
!!$          write(*,*)'cluster ',i, " mailles ",num_maille_par_sommet(pt_som(cluster(i))+1:pt_som(cluster(i)+1))
!!$       end do


       do i=1,n_cluster !Pour tous les cluster
          do j=pt_som(cluster(i))+1,pt_som(cluster(i)+1)
             j0=num_maille_par_sommet(j) !On étudie la maille j0 du cluster i
             do k=ptvois(j0)+1,ptvois(j0+1) !on regarde les voisins de j0
                k0=nuvois(k)
                do l=pt_som(cluster(i))+1,pt_som(cluster(i)+1) !si voisin dans cluster i
                   l0=num_maille_par_sommet(l)
                   if (k0==l0) then
!!$                      write(*,*)"maille j0=",j0," communique avec k0=",k0
                      As(k)=As(k)+A(k)*lambda
                   end if
                end do
             end do
          end do
       end do

!!!
!!!Mailles isolées
!!!

       n_maille_a_rattacher=0
       n_maille_rattacher=0
       n_maille_attacher=0
       do i=1,ncv
          if (maille_utilise(i)==0) then !Rattachement à un cluster !!!
             n_maille_a_rattacher=n_maille_a_rattacher+1

             boucle0 : do j=ptvois(i)+1,ptvois(i+1) !voisin de la maille à rattacher
                j0=nuvois(j)
                if (j0>0) then
                   !Definition du Rattachement principal
                   if (maille_utilise(j0)>0) then !si le voisin appartient à un cluster de base, on rattache au cluster
                      maille_utilise(i)=-maille_utilise(j0)
                      n_maille_rattacher=n_maille_rattacher+1
                      exit boucle0
                   end if
                end if
             end do boucle0

             if (maille_utilise(i)/=0) then !Si la maille est rattachée ...
                boucle1 : do j=ptvois(i)+1,ptvois(i+1) !Construction des connexions au cluster de base
                   j0=nuvois(j)
                   if (j0>0) then
                      if (abs(maille_utilise(j0))==-maille_utilise(i)) then !recherche des connexions à effectuer dans le cluster
                         As(j)=As(j)+A(j)*lambda
                         boucle2 : do k=ptvois(j0)+1,ptvois(j0+1) !recherche de la connexion reciproque
                            k0=nuvois(k)
                            if (k0==i) then !connexion réciproque  trouvée
                               As(k)=As(k)+A(k)*lambda
                               exit boucle2
                            end if
                         end do boucle2
                      end if
                   end if
                end do boucle1
             end if
          else
             n_maille_attacher=n_maille_attacher+1
          end if
       end do

       write(*,*)"Nombre de mailles à rattacher :",n_maille_a_rattacher
       write(*,*)"Nombre de mailles rattachées :",n_maille_rattacher
       write(*,*)"Nombre de mailles attachées :",n_maille_attacher
       if (n_maille_rattacher+n_maille_attacher/=ncv) then
          write(*,*)"Toutes les mailles ne sont pas attachées !!!"
          stop
       end if
    elseif (macromaille==3) then
       write(*,*)"Macro-mailles par sommets, macromaille=",macromaille


       !Décompte des sommets intérieurs
       allocate(sommet_utilise(nbsom))
       sommet_utilise(:)=.false.
       l=0
       do i=1,ncv
          j0=ptvois(i+1)
          do j=ptvois(i)+1,ptvois(i+1)
             k0=nusom(j0)
             if (nuvois(j0)>0.and.nuvois(j)>0.and.(.not.sommet_utilise(k0))) then
                sommet_utilise(k0)=.true.
                l=l+1
             end if
             j0=j
          end do
       end do

       n_som_in=l
       write(*,*)"Nombre de sommets intérieurs :",n_som_in


       allocate(pt_som(nbsom+1))

!!!
!!!comptage du nombre de mailles par sommet intérieur
!!!
       pt_som(1:nbsom+1)=0
       do i=1,ncv
          do j=ptvois(i)+1,ptvois(i+1)
             j0=nusom(j)
             if (sommet_utilise(j0)) pt_som(j0)=pt_som(j0)+1
          end do
       end do

       do i=1,nbsom
          pt_som(i+1)=pt_som(i)+pt_som(i+1)
       end do

!!!
!!!On associe les mailles aux noeuds (cluster)
!!!
       allocate(num_maille_par_sommet(pt_som(nbsom+1)))
       do i=1,ncv
          do j=ptvois(i)+1,ptvois(i+1)
             j0=nusom(j)
             k0=pt_som(j0)
             if (sommet_utilise(j0)) then
                num_maille_par_sommet(k0)=i
                pt_som(j0)=k0-1
             end if
          end do
       end do

!!!
!!!Construction des clusters de base : contient les numéros des noeuds
!!!
       allocate(maille_utilise(ncv))
       maille_utilise(:)=0
       n_cluster=0
       do i=1,nbsom
          if (sommet_utilise(i)) then
             k=0
             do j=pt_som(i)+1,pt_som(i+1)
                j0=num_maille_par_sommet(j)
                if (maille_utilise(j0)/=0) then
                   k=1 !La maille appartient deja à un cluster
                   exit
                end if
             end do
             if (k==0) then
                n_cluster=n_cluster+1
                do j=pt_som(i)+1,pt_som(i+1)
                   j0=num_maille_par_sommet(j)
                   maille_utilise(j0)=n_cluster
                end do
             end if
          end if
       end do

       allocate(cluster(n_cluster))


       maille_utilise(:)=0
       n_cluster=0
       do i=1,nbsom
          if (sommet_utilise(i)) then
             k=0
             do j=pt_som(i)+1,pt_som(i+1)
                j0=num_maille_par_sommet(j)
                if (maille_utilise(j0)/=0) then
                   k=1 !La maille appartient deja à un cluster
                   exit
                end if
             end do
             if (k==0) then
                n_cluster=n_cluster+1
                cluster(n_cluster)=i
                do j=pt_som(i)+1,pt_som(i+1)
                   l=j-pt_som(i)
                   j0=num_maille_par_sommet(j)
                   maille_utilise(j0)=n_cluster
                end do
             end if
          end if
       end do

       write(*,*)"Nombre de cluster de base :",n_cluster

!!$       do i=1,n_cluster
!!$          write(*,*)'cluster ',i, " mailles ",num_maille_par_sommet(pt_som(cluster(i))+1:pt_som(cluster(i)+1))
!!$       end do


       do i=1,n_cluster !Pour tous les cluster
          do j=pt_som(cluster(i))+1,pt_som(cluster(i)+1)
             j0=num_maille_par_sommet(j) !On étudie la maille j0 du cluster i
             do k=ptvois(j0)+1,ptvois(j0+1) !on regarde les voisins de j0
                k0=nuvois(k)
                do l=pt_som(cluster(i))+1,pt_som(cluster(i)+1) !si voisin dans cluster i
                   l0=num_maille_par_sommet(l)
                   if (k0==l0) then
!!$                      write(*,*)"maille j0=",j0," communique avec k0=",k0
                      As(k)=As(k)+A(k)*lambda
                   end if
                end do
             end do
          end do
       end do

!!!
!!!Mailles isolées
!!!

       n_maille_a_rattacher=0
       n_maille_rattacher=0
       n_maille_attacher=0
       do i=1,ncv
          if (maille_utilise(i)==0) then !Rattachement à un cluster !!!
             n_maille_a_rattacher=n_maille_a_rattacher+1
             allocate(nb_attach_clus(ptvois(i+1)-ptvois(i)))
             nb_attach_clus=0
             do j=ptvois(i)+1,ptvois(i+1) !voisin de la maille à rattacher
                j0=nuvois(j)
                if (j0>0) then
                   !Definition du Rattachement principal
                   if (maille_utilise(j0)>0) then !si le voisin appartient à un cluster de base, on rattache au cluster
                      nb_attach_clus(j-ptvois(i))=nb_attach_clus(j-ptvois(i))+1
                   end if
                end if
             end do
             j0=nuvois(maxval(nb_attach_clus))
             deallocate(nb_attach_clus)
             maille_utilise(i)=-maille_utilise(j0)
             n_maille_rattacher=n_maille_rattacher+1

             if (maille_utilise(i)/=0) then !Si la maille est rattachée ...
                boucle1b : do j=ptvois(i)+1,ptvois(i+1) !Construction des connexions au cluster de base
                   j0=nuvois(j)
                   if (j0>0) then
                      if (abs(maille_utilise(j0))==-maille_utilise(i)) then !recherche des connexions à effectuer dans le cluster
                         As(j)=As(j)+A(j)*lambda
                         boucle2b : do k=ptvois(j0)+1,ptvois(j0+1) !recherche de la connexion reciproque
                            k0=nuvois(k)
                            if (k0==i) then !connexion réciproque  trouvée
                               As(k)=As(k)+A(k)*lambda
                               exit boucle2b
                            end if
                         end do boucle2b
                      end if
                   end if
                end do boucle1b
             end if
          else
             n_maille_attacher=n_maille_attacher+1
          end if
       end do

       write(*,*)"Nombre de mailles à rattacher :",n_maille_a_rattacher
       write(*,*)"Nombre de mailles rattachées :",n_maille_rattacher
       write(*,*)"Nombre de mailles attachées :",n_maille_attacher
       if (n_maille_rattacher+n_maille_attacher/=ncv) then
          write(*,*)"Toutes les mailles ne sont pas attachées !!!"
          stop
       end if
    end if

  end subroutine calcul_coef

  !###################################################################
  !Trie à bulles
  subroutine trie(numer,long)
    implicit none
    integer,intent(in)::long
    integer,dimension(long),intent(inout)::numer

    integer::i,ip,j,k

    do j=long,2,-1
       do i=1,j-1
          ip=i+1
          if (numer(i)>numer(ip)) then
             !permutation
             k=numer(ip)
             numer(ip)=numer(i)
             numer(i)=k
          end if
       end do
    end do
  end subroutine trie


  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~       ROUTINES DE RéSOLUTION    ~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  !###################################################################
  subroutine preilu(nordre1)
    implicit none

    integer,intent(in)::nordre1

    integer::i,j,k,ncvv,icv,jcv,nnuv,kcompt,kk,jj,nnuvp
    integer::iord

    integer,dimension(:),allocatable::ptvp,nuvp,denlcv,renlcv



!!!
!!!renumérotation
!!!

    if (.not.allocated(ptmat)) then
       allocate(ptmat(ncv+1),numat(ptvois(ncv+1)))
       k=0
       ptmat(1)=0
       do i=1,ncv
          do j=ptvois(i)+1,ptvois(i+1)
             if (nuvois(j)>0) then
                k=k+1
                numat(k)=nuvois(j)
             end if
          end do
          ptmat(i+1)=k
          call trie(numat(ptmat(i)+1:ptmat(i+1)),ptmat(i+1)-ptmat(i))
       end do

       ncvv=ptmat(ncv+1)

       allocate(aa(ncvv,neq,neq),bb(ncv,neq),dd(ncv,neq,neq))

!!$       allocate(aa1(ncvv,3:4,3:3),dd1(ncvv,3:4,3:3))
    end if


    allocate(renum(ncv),denum(ncv))
    renum(:)=0

    renum(1)=1
    denum(1)=1
    j=0 !nouvelle numérotation
    i=1 !nouvelle numérotation
    do while (j<ncv)
       j=j+1
       icv=denum(j)
       do k=ptmat(icv)+1,ptmat(icv+1)
          jcv=numat(k)
          if (jcv/=0) then
             if (renum(jcv)==0) then
                i=i+1
                denum(i)=jcv
                renum(jcv)=i
             end if
          end if
       end do
    end do

    if (i/=ncv) then
       write(*,*)"Problème dans la renumérotation (subroutine preilu)",i,ncv
       stop
    end if


!!!
!!!Initialisation de nuv et ptv et ptd
!!!      nuv=numéro du bloc dans la nouvelle numérotation
!!!      ptv=pointeur de bloc dans la nouvelle numérotation
!!!      ptd=pointeur de bloc diagonal dans la nouvelle numérotation
!!!
    ncvv=ptmat(ncv+1)
    nnuv=ncvv+ncv !on ajoute la diagonale, donc ncv termes
    allocate(nuv(nnuv),ptv(ncv+1),ptd(ncv))

    ptv(1)=0
    do i=1,ncv
       icv=denum(i)
       j=ptv(i)-ptmat(icv)
       ptv(i+1)=j+ptmat(icv+1)
       do k=ptmat(icv)+1,ptmat(icv+1)
          nuv(j+k)=renum(numat(k))
       end do
       ptv(i+1)=ptv(i+1)+1 !ajout de la diagonale
       nuv(ptv(i+1))=i !numéro du bloc diagonal pour la maille i
       call trie(nuv(ptv(i)+1:ptv(i+1)),ptv(i+1)-ptv(i))

       !Recherche du terme diagonal après la renumérotation
       k=ptv(i)+1
       do while (nuv(k)<i)
          k=k+1
       end do
       ptd(i)=k
    end do
    deallocate(denum)

!!!
!!!Decomposition ILU
!!!
    allocate(denlcv(ncv),renlcv(ncv))
    do iord=1,nordre1
!!$       write(*,*)"iord=",iord
       !
       ! Elimination symbolique pour dimensionner la taille de la matrice
       !
       allocate(ptvp(ncv+1))
       denlcv=0

       !Decompte des blocs explorés pour chaque maille
       !si denlcv(n° bloc)=1, déjà compté
       !renlcv(décompte bloc)=n° bloc
       do i=1,ncv

          !décompte nombre de blocs sur une ligne
          kcompt=0
          do j=ptv(i)+1,ptv(i+1)
             kk=nuv(j)
             kcompt=kcompt+1
             denlcv(kk)=1
             renlcv(kcompt)=kk
          end do

          !Ajout des voisins des voisins (décomposition LU)
          do j=ptv(i)+1,ptd(i)-1
             jj=nuv(j)
             do k=ptd(jj)+1,ptv(jj+1)
                kk=nuv(k)
                if (denlcv(kk)==0) then !bloc non encore considéré
                   kcompt=kcompt+1
                   denlcv(kk)=1
                   renlcv(kcompt)=kk
                end if
             end do
          end do

          ptvp(i)=kcompt !nouveau pointeur de décomposition iLU

          do kk=1,kcompt !remise à zéro du pointeur de bloc de décompte
             denlcv(renlcv(kk))=0
          end do

       end do

       do i=2,ncv !Cumul
          ptvp(i)=ptvp(i)+ptvp(i-1)
       end do

       nnuvp=ptvp(ncv)
       ptvp(ncv+1)=nnuvp

       !
       ! Allocation et construction des pointeurs d'élimination
       !
       allocate(nuvp(nnuvp))

       do i=1,ncv
          kcompt=0
          do j=ptv(i)+1,ptv(i+1)
             kk=nuv(j)
             nuvp(ptvp(i))=kk
             ptvp(i)=ptvp(i)-1 !numerotation decroissante car ptvp(i) initial = nombre de bloc total
             kcompt=kcompt+1
             denlcv(kk)=1
             renlcv(kcompt)=kk
          end do

          do j=ptv(i)+1,ptd(i)-1
             jj=nuv(j)
             do k=ptd(jj)+1,ptv(jj+1)
                kk=nuv(k)
                if (denlcv(kk)==0) then !bloc non encore considéré
                   nuvp(ptvp(i))=kk
                   ptvp(i)=ptvp(i)-1
                   kcompt=kcompt+1
                   denlcv(kk)=1
                   renlcv(kcompt)=kk
                end if
             end do
          end do

          do kk=1,kcompt !remise à zéro du pointeur de bloc de décompte
             denlcv(renlcv(kk))=0
          end do

       end do

       !
       !Classement, recherche du pointeur diagonal et recopie des tableaux pour une nouvelle décomposition LU
       !
       k=0
       do i=1,ncv
          call trie(nuvp(ptvp(i)+1:ptvp(i+1)),ptvp(i+1)-ptvp(i))
          ptv(i+1)=ptvp(i+1)

          j=ptv(i)+1
          do while (nuvp(j)<i)
             j=j+1
          end do
          ptd(i)=j
       end do

       nnuv=ptv(ncv+1)

       deallocate(nuv)
       allocate(nuv(nnuv))
       nuv(1:nnuv)=nuvp(1:nnuv)
       deallocate(nuvp,ptvp)

    end do
    deallocate(denlcv,renlcv)

    allocate(aaf(nnuv,neq,neq))

    allocate(ui(nlin),rri(nlin),r0(nlin),vi(nlin),pi(nlin))
    allocate(aux1(nlin),aux2(nlin))

!!$    write(*,*)"Fin preiLU"

  end subroutine preilu

  !###################################################################
  subroutine bicgstab(it,nordre1,residu,residu_old)
    implicit none

    integer,intent(out)::it
    integer,intent(inout)::nordre1
    real(kind=8),intent(inout)::residu_old,residu

    integer::k1
    real(kind=8)::rho,resini,alpha,omega,beta,rhom1,psca,test1,&
         residu1,residu2

    real(kind=8),dimension(:,:),allocatable::bb_old


    test1=test
    allocate(bb_old(ncv,4))
    bb_old=bb

    retour : do
!!$       if (residu_old>1) then
!!$          call normal
!!$       else
       norm1=1
       norm2=1
       norm3=1
       norm4=1
!!$       end if

       !print*,norm1,norm2,norm3,norm4



       call facilu

       ui(1:nlin)=0
       call residilu(ui,r0)



       call derilu(r0)

       resini=maxval(abs(r0(1:nlin)))

!!$       write(*,*)"resi(u,v)",maxval(abs(r0(1:2*ncv)))
!!$       write(*,*)"resi(p)",maxval(abs(r0(2*ncv+1:3*ncv)))
!!$       write(*,*)"resi(T)",maxval(abs(r0(3*ncv+1:4*ncv)))
!!$    write(*,*)"Residu initial :",resini

       rri(1:nlin)=r0(1:nlin)
       vi(1:nlin)=0
       pi(1:nlin)=0

       rho=1
       alpha=1
       omega=1

!!!
!!!Demarrage des itérations
!!!
       iterations : do it=1,itbcgsmax
          rhom1=rho

          rho=dot_product(r0(1:nlin),rri(1:nlin))
          beta=(rho/rhom1)*(alpha/omega)
          pi(1:nlin)=rri(1:nlin)+beta*(pi(1:nlin)-omega*vi(1:nlin))

          call promat(pi,aux1)
          call derilu(aux1)

          vi(1:nlin)=aux1(1:nlin)

          psca=dot_product(r0(1:nlin),vi(1:nlin))
          alpha=rho/psca

          aux1(1:nlin)=rri(1:nlin)-alpha*vi(1:nlin)

          call promat(aux1,aux2)
          call derilu(aux2)

          omega=dot_product(aux1(1:nlin),aux2(1:nlin))
          psca=dot_product(aux2(1:nlin),aux2(1:nlin))
          omega=omega/psca
          ui(1:nlin)=ui(1:nlin)+alpha*pi(1:nlin)+omega*aux1(1:nlin)
          rri(1:nlin)=aux1(1:nlin)-omega*aux2(1:nlin)

          psca=maxval(abs(rri(1:nlin)))
          psca=psca/resini

          if (mod(it,100)==0) write(*,*)"It=",it," psca=",psca," rho=",rho

          if (psca<=test1.and.it/=1) exit iterations
       end do iterations

!!!
!!!Remise à l'échelle
!!!

       do ii=1,ncv
          jj=renum(ii)
          k1=jj
          bb(ii,1)=ui(k1)*norm1
          k1=k1+ncv
          bb(ii,2)=ui(k1)*norm2
          k1=k1+ncv
          bb(ii,3)=ui(k1)*norm3
          k1=k1+ncv
          bb(ii,4)=ui(k1)*norm4
       end do
       if (neq/=4) then
          write(*,*)"Calcul prévu pour neq=4 !!!"
          stop
       end if


       residu1=maxval(abs(bb(:,1:2)))
       residu2=maxval(abs(bb(:,4)))
       residu=max(residu1,residu2)

       if (residu/residu_old>1) then

!!$          if (it<10) then
!!$             test1=test1/10
!!$             write(*,*)"Nombre d'itérations insuffisant :",it
!!$             write(*,*)"critère d'arrêt abaissé : test=",test1
!!$             cycle retour
!!$          end if

!!$         if (it<10.or.it>itbcgsmax) then
          if (it>itbcgsmax) then
             nordre1=nordre1+1
             write(*,*)"residu_old=",residu_old
             write(*,*)"residu=",residu
             write(*,*)"Le résidu augmente !!!  Ordre iLU=",nordre1
             deallocate(renum)
             deallocate(aaf)
             deallocate(nuv,ptv,ptd)
             deallocate(ui,rri,r0,vi,pi)
             deallocate(aux1,aux2)
             call preilu(nordre1)
             bb=bb_old
             cycle retour
          end if
       end if
       exit retour
    end do retour

    residu_old=residu
    deallocate(bb_old)


  end subroutine bicgstab

  !###################################################################
  subroutine normal
    implicit none

    integer::i,j
    real(kind=8)::unorm1,unorm2,unorm3,unorm4


!!!
!!! Normalisation de la diagonale
!!!

    do i=1,ncv
!!!Matrice U, de diagonale unité
       dd(i,1,1)=1._8/dd(i,1,1)
       dd(i,1,2)=dd(i,1,2)*dd(i,1,1)
       dd(i,1,3)=dd(i,1,3)*dd(i,1,1)
       dd(i,1,4)=dd(i,1,4)*dd(i,1,1)
       !
       dd(i,2,2)=1._8/(dd(i,2,2)-dd(i,2,1)*dd(i,1,2))
       dd(i,2,3)=(dd(i,2,3)-dd(i,2,1)*dd(i,1,3))*dd(i,2,2)
       dd(i,2,4)=(dd(i,2,4)-dd(i,2,1)*dd(i,1,4))*dd(i,2,2)
       !
       dd(i,3,2)=dd(i,3,2)-dd(i,3,1)*dd(i,1,2)
       dd(i,3,3)=1._8/(dd(i,3,3)-dd(i,3,1)*dd(i,1,3)-dd(i,3,2)*dd(i,2,3))
       dd(i,3,4)=(dd(i,3,4)-dd(i,3,1)*dd(i,1,4)-dd(i,3,2)*dd(i,2,4))*dd(i,3,3)
       !
       dd(i,4,2)=dd(i,4,2)-dd(i,4,1)*dd(i,1,2)
       dd(i,4,3)=dd(i,4,3)-dd(i,4,1)*dd(i,1,3)-dd(i,4,2)*dd(i,2,3)
       dd(i,4,4)=1._8/(dd(i,4,4)-dd(i,4,1)*dd(i,1,4)-dd(i,4,2)*dd(i,2,4)-dd(i,4,3)*dd(i,3,4))
!!!Construction puis résolution : d^{-1}b
       bb(i,1)=bb(i,1)*dd(i,1,1)
       bb(i,2)=(bb(i,2)-dd(i,2,1)*bb(i,1))*dd(i,2,2)
       bb(i,3)=(bb(i,3)-dd(i,3,1)*bb(i,1)-dd(i,3,2)*bb(i,2))*dd(i,3,3)
       bb(i,4)=(bb(i,4)-dd(i,4,1)*bb(i,1)-dd(i,4,2)*bb(i,2)-dd(i,4,3)*bb(i,3))*dd(i,4,4)
       !
       bb(i,3)=bb(i,3)-dd(i,3,4)*bb(i,4)
       bb(i,2)=bb(i,2)-dd(i,2,4)*bb(i,4)-dd(i,2,3)*bb(i,3)
       bb(i,1)=bb(i,1)-dd(i,1,4)*bb(i,4)-dd(i,1,3)*bb(i,3)-dd(i,1,2)*bb(i,2)
!!!Construction puis résolution : d^{-1}a
       do j=ptmat(i)+1,ptmat(i+1)
          aa(j,1,1)=aa(j,1,1)                                                              *dd(i,1,1)
          aa(j,2,1)=(aa(j,2,1)-dd(i,2,1)*aa(j,1,1))                                        *dd(i,2,2)
          aa(j,3,1)=(aa(j,3,1)-dd(i,3,1)*aa(j,1,1)-dd(i,3,2)*aa(j,2,1))                    *dd(i,3,3)
          aa(j,4,1)=(aa(j,4,1)-dd(i,4,1)*aa(j,1,1)-dd(i,4,2)*aa(j,2,1)-dd(i,4,3)*aa(j,3,1))*dd(i,4,4)
          aa(j,3,1)=aa(j,3,1)-dd(i,3,4)*aa(j,4,1)
          aa(j,2,1)=aa(j,2,1)-dd(i,2,4)*aa(j,4,1)-dd(i,2,3)*aa(j,3,1)
          aa(j,1,1)=aa(j,1,1)-dd(i,1,4)*aa(j,4,1)-dd(i,1,3)*aa(j,3,1)-dd(i,1,2)*aa(j,2,1)
          !
          aa(j,1,2)=aa(j,1,2)                                                              *dd(i,1,1)
          aa(j,2,2)=(aa(j,2,2)-dd(i,2,1)*aa(j,1,2))                                        *dd(i,2,2)
          aa(j,3,2)=(aa(j,3,2)-dd(i,3,1)*aa(j,1,2)-dd(i,3,2)*aa(j,2,2))                    *dd(i,3,3)
          aa(j,4,2)=(aa(j,4,2)-dd(i,4,1)*aa(j,1,2)-dd(i,4,2)*aa(j,2,2)-dd(i,4,3)*aa(j,3,2))*dd(i,4,4)
          aa(j,3,2)=aa(j,3,2)-dd(i,3,4)*aa(j,4,2)
          aa(j,2,2)=aa(j,2,2)-dd(i,2,4)*aa(j,4,2)-dd(i,2,3)*aa(j,3,2)
          aa(j,1,2)=aa(j,1,2)-dd(i,1,4)*aa(j,4,2)-dd(i,1,3)*aa(j,3,2)-dd(i,1,2)*aa(j,2,2)
          !
          aa(j,1,3)=aa(j,1,3)                                                              *dd(i,1,1)
          aa(j,2,3)=(aa(j,2,3)-dd(i,2,1)*aa(j,1,3))                                        *dd(i,2,2)
          aa(j,3,3)=(aa(j,3,3)-dd(i,3,1)*aa(j,1,3)-dd(i,3,2)*aa(j,2,3))                    *dd(i,3,3)
          aa(j,4,3)=(aa(j,4,3)-dd(i,4,1)*aa(j,1,3)-dd(i,4,2)*aa(j,2,3)-dd(i,4,3)*aa(j,3,3))*dd(i,4,4)
          aa(j,3,3)=aa(j,3,3)-dd(i,3,4)*aa(j,4,3)
          aa(j,2,3)=aa(j,2,3)-dd(i,2,4)*aa(j,4,3)-dd(i,2,3)*aa(j,3,3)
          aa(j,1,3)=aa(j,1,3)-dd(i,1,4)*aa(j,4,3)-dd(i,1,3)*aa(j,3,3)-dd(i,1,2)*aa(j,2,3)
          !
          aa(j,1,4)=aa(j,1,4)                                                              *dd(i,1,1)
          aa(j,2,4)=(aa(j,2,4)-dd(i,2,1)*aa(j,1,4))                                        *dd(i,2,2)
          aa(j,3,4)=(aa(j,3,4)-dd(i,3,1)*aa(j,1,4)-dd(i,3,2)*aa(j,2,4))                    *dd(i,3,3)
          aa(j,4,4)=(aa(j,4,4)-dd(i,4,1)*aa(j,1,4)-dd(i,4,2)*aa(j,2,4)-dd(i,4,3)*aa(j,3,4))*dd(i,4,4)
          aa(j,3,4)=aa(j,3,4)-dd(i,3,4)*aa(j,4,4)
          aa(j,2,4)=aa(j,2,4)-dd(i,2,4)*aa(j,4,4)-dd(i,2,3)*aa(j,3,4)
          aa(j,1,4)=aa(j,1,4)-dd(i,1,4)*aa(j,4,4)-dd(i,1,3)*aa(j,3,4)-dd(i,1,2)*aa(j,2,4)
       end do
    end do

!!!Etape de normalisation des inconnues
    norm1=maxval(abs(bb(:,1)))
    norm2=maxval(abs(bb(:,2)))
    norm3=maxval(abs(bb(:,3)))
    norm4=maxval(abs(bb(:,4)))

    unorm1=1._8/norm1
    unorm2=1._8/norm2
    unorm3=1._8/norm3
    unorm4=1._8/norm4

    do i=1,ncv
       dd(i,1,1)=1
       dd(i,1,2)=0
       dd(i,1,3)=0
       dd(i,1,4)=0
       dd(i,2,1)=0
       dd(i,2,2)=1
       dd(i,2,3)=0
       dd(i,2,4)=0
       dd(i,3,1)=0
       dd(i,3,2)=0
       dd(i,3,3)=1
       dd(i,3,4)=0
       dd(i,4,1)=0
       dd(i,4,2)=0
       dd(i,4,3)=0
       dd(i,4,4)=1

       !Changement d'échelle des inconnues
       bb(i,1)=bb(i,1)*unorm1
       bb(i,2)=bb(i,2)*unorm2
       bb(i,3)=bb(i,3)*unorm3
       bb(i,4)=bb(i,4)*unorm4

       do j=ptmat(i)+1,ptmat(i+1)
          aa(j,1,2)=aa(j,1,2)*unorm1*norm2
          aa(j,1,3)=aa(j,1,3)*unorm1*norm3
          aa(j,1,4)=aa(j,1,4)*unorm1*norm4
          aa(j,2,1)=aa(j,2,1)*unorm2*norm1
          aa(j,2,3)=aa(j,2,3)*unorm2*norm3
          aa(j,2,4)=aa(j,2,4)*unorm2*norm4
          aa(j,3,1)=aa(j,3,1)*unorm3*norm1
          aa(j,3,2)=aa(j,3,2)*unorm3*norm2
          aa(j,3,4)=aa(j,3,4)*unorm3*norm4
          aa(j,4,1)=aa(j,4,1)*unorm4*norm1
          aa(j,4,2)=aa(j,4,2)*unorm4*norm2
          aa(j,4,3)=aa(j,4,3)*unorm4*norm3
       end do
    end do

    write(*,*)"norme1=",norm1," norme2=",norm2," norme3=",norm3," norme4=",norm4

  end subroutine normal

  !###################################################################
  subroutine facilu
    implicit none

    integer::i,j,k,ii,jj,kk,kkd


!!!
!!!Remplissage de la matrice de base
!!!
    aaf(:,:,:)=0

    do i=1,ncv
       ii=renum(i)
       kkd=ptd(ii)

       aaf(kkd,1,1)=dd(i,1,1)
       aaf(kkd,1,2)=dd(i,1,2)
       aaf(kkd,1,3)=dd(i,1,3)
       aaf(kkd,1,4)=dd(i,1,4)
       aaf(kkd,2,1)=dd(i,2,1)
       aaf(kkd,2,2)=dd(i,2,2)
       aaf(kkd,2,3)=dd(i,2,3)
       aaf(kkd,2,4)=dd(i,2,4)
       aaf(kkd,3,1)=dd(i,3,1)
       aaf(kkd,3,2)=dd(i,3,2)
       aaf(kkd,3,3)=dd(i,3,3)
!!$       aaf(kkd,3,3)=dd1(i,3,3)
       aaf(kkd,3,4)=dd(i,3,4)
       aaf(kkd,4,1)=dd(i,4,1)
       aaf(kkd,4,2)=dd(i,4,2)
       aaf(kkd,4,3)=dd(i,4,3)
       aaf(kkd,4,4)=dd(i,4,4)

       do k=ptmat(i)+1,ptmat(i+1)
          jj=renum(numat(k))
          kk=ptv(ii)+1
          do while (jj/=nuv(kk))
             kk=kk+1
          end do
          aaf(kk,1,1)=aa(k,1,1)
          aaf(kk,1,2)=aa(k,1,2)
          aaf(kk,1,3)=aa(k,1,3)
          aaf(kk,1,4)=aa(k,1,4)
          aaf(kk,2,1)=aa(k,2,1)
          aaf(kk,2,2)=aa(k,2,2)
          aaf(kk,2,3)=aa(k,2,3)
          aaf(kk,2,4)=aa(k,2,4)
          aaf(kk,3,1)=aa(k,3,1)
          aaf(kk,3,2)=aa(k,3,2)
          aaf(kk,3,3)=aa(k,3,3)
!!$          aaf(kk,3,3)=aa1(k,3,3)
          aaf(kk,3,4)=aa(k,3,4)
          aaf(kk,4,1)=aa(k,4,1)
          aaf(kk,4,2)=aa(k,4,2)
          aaf(kk,4,3)=aa(k,4,3)
          aaf(kk,4,4)=aa(k,4,4)
       end do
    end do

!!!
!!!FACTORISATION INCOMPLETE
!!!

    do i=1,ncv
       !Elimination de la partie gauche
       do j=ptv(i)+1,ptd(i)-1
          jj=nuv(j)
          do k=ptd(jj)+1,ptv(jj+1) !voisins des voisins
             ii=nuv(k) !numéro du bloc candidat à la combinaison linéaire d'élimination
             kk=j+1 !pointeur  des termes à éliminer
             do while (nuv(kk)<ii.and.kk<ptv(i+1)) !recherche des bloc utilisés dans la décomposition  iLU
                kk=kk+1
             end do
             if (nuv(kk)==ii) then
                aaf(kk,1,1)=aaf(kk,1,1)-aaf(j,1,1)*aaf(k,1,1)-aaf(j,1,2)*aaf(k,2,1)-&
                     aaf(j,1,3)*aaf(k,3,1)-aaf(j,1,4)*aaf(k,4,1)
                aaf(kk,2,1)=aaf(kk,2,1)-aaf(j,2,1)*aaf(k,1,1)-aaf(j,2,2)*aaf(k,2,1)-&
                     aaf(j,2,3)*aaf(k,3,1)-aaf(j,2,4)*aaf(k,4,1)
                aaf(kk,3,1)=aaf(kk,3,1)-aaf(j,3,1)*aaf(k,1,1)-aaf(j,3,2)*aaf(k,2,1)-&
                     aaf(j,3,3)*aaf(k,3,1)-aaf(j,3,4)*aaf(k,4,1)
                aaf(kk,4,1)=aaf(kk,4,1)-aaf(j,4,1)*aaf(k,1,1)-aaf(j,4,2)*aaf(k,2,1)-&
                     aaf(j,4,3)*aaf(k,3,1)-aaf(j,4,4)*aaf(k,4,1)
                aaf(kk,1,2)=aaf(kk,1,2)-aaf(j,1,1)*aaf(k,1,2)-aaf(j,1,2)*aaf(k,2,2)-&
                     aaf(j,1,3)*aaf(k,3,2)-aaf(j,1,4)*aaf(k,4,2)
                aaf(kk,2,2)=aaf(kk,2,2)-aaf(j,2,1)*aaf(k,1,2)-aaf(j,2,2)*aaf(k,2,2)-&
                     aaf(j,2,3)*aaf(k,3,2)-aaf(j,2,4)*aaf(k,4,2)
                aaf(kk,3,2)=aaf(kk,3,2)-aaf(j,3,1)*aaf(k,1,2)-aaf(j,3,2)*aaf(k,2,2)-&
                     aaf(j,3,3)*aaf(k,3,2)-aaf(j,3,4)*aaf(k,4,2)
                aaf(kk,4,2)=aaf(kk,4,2)-aaf(j,4,1)*aaf(k,1,2)-aaf(j,4,2)*aaf(k,2,2)-&
                     aaf(j,4,3)*aaf(k,3,2)-aaf(j,4,4)*aaf(k,4,2)
                aaf(kk,1,3)=aaf(kk,1,3)-aaf(j,1,1)*aaf(k,1,3)-aaf(j,1,2)*aaf(k,2,3)-&
                     aaf(j,1,3)*aaf(k,3,3)-aaf(j,1,4)*aaf(k,4,3)
                aaf(kk,2,3)=aaf(kk,2,3)-aaf(j,2,1)*aaf(k,1,3)-aaf(j,2,2)*aaf(k,2,3)-&
                     aaf(j,2,3)*aaf(k,3,3)-aaf(j,2,4)*aaf(k,4,3)
                aaf(kk,3,3)=aaf(kk,3,3)-aaf(j,3,1)*aaf(k,1,3)-aaf(j,3,2)*aaf(k,2,3)-&
                     aaf(j,3,3)*aaf(k,3,3)-aaf(j,3,4)*aaf(k,4,3)
                aaf(kk,4,3)=aaf(kk,4,3)-aaf(j,4,1)*aaf(k,1,3)-aaf(j,4,2)*aaf(k,2,3)-&
                     aaf(j,4,3)*aaf(k,3,3)-aaf(j,4,4)*aaf(k,4,3)
                aaf(kk,1,4)=aaf(kk,1,4)-aaf(j,1,1)*aaf(k,1,4)-aaf(j,1,2)*aaf(k,2,4)-&
                     aaf(j,1,3)*aaf(k,3,4)-aaf(j,1,4)*aaf(k,4,4)
                aaf(kk,2,4)=aaf(kk,2,4)-aaf(j,2,1)*aaf(k,1,4)-aaf(j,2,2)*aaf(k,2,4)-&
                     aaf(j,2,3)*aaf(k,3,4)-aaf(j,2,4)*aaf(k,4,4)
                aaf(kk,3,4)=aaf(kk,3,4)-aaf(j,3,1)*aaf(k,1,4)-aaf(j,3,2)*aaf(k,2,4)-&
                     aaf(j,3,3)*aaf(k,3,4)-aaf(j,3,4)*aaf(k,4,4)
                aaf(kk,4,4)=aaf(kk,4,4)-aaf(j,4,1)*aaf(k,1,4)-aaf(j,4,2)*aaf(k,2,4)-&
                     aaf(j,4,3)*aaf(k,3,4)-aaf(j,4,4)*aaf(k,4,4)
             end if
          end do
       end do

!!!
!!!Normalisation de la diagonale
!!!

       k=ptd(i)
       aaf(k,1,1)=1._8/aaf(k,1,1)
       aaf(k,1,2)=aaf(k,1,2)*aaf(k,1,1)
       aaf(k,1,3)=aaf(k,1,3)*aaf(k,1,1)
       aaf(k,1,4)=aaf(k,1,4)*aaf(k,1,1)
       !
       aaf(k,2,2)=1._8/(aaf(k,2,2)-aaf(k,2,1)*aaf(k,1,2))
       aaf(k,2,3)=(aaf(k,2,3)-aaf(k,2,1)*aaf(k,1,3))*aaf(k,2,2)
       aaf(k,2,4)=(aaf(k,2,4)-aaf(k,2,1)*aaf(k,1,4))*aaf(k,2,2)
       !
       aaf(k,3,2)=aaf(k,3,2)-aaf(k,3,1)*aaf(k,1,2)
       aaf(k,3,3)=1._8/(aaf(k,3,3)-aaf(k,3,1)*aaf(k,1,3)-aaf(k,3,2)*aaf(k,2,3))
       aaf(k,3,4)=(aaf(k,3,4)-aaf(k,3,1)*aaf(k,1,4)-aaf(k,3,2)*aaf(k,2,3))*aaf(k,3,3)
       !
       aaf(k,4,2)=aaf(k,4,2)-aaf(k,4,1)*aaf(k,1,2)
       aaf(k,4,3)=aaf(k,4,3)-aaf(k,4,1)*aaf(k,1,3)-aaf(k,4,2)*aaf(k,2,3)
       aaf(k,4,4)=1._8/(aaf(k,4,4)-aaf(k,4,1)*aaf(k,1,4)-aaf(k,4,2)*aaf(k,2,4)-aaf(k,4,3)*aaf(k,3,4))

       do j=k+1,ptv(i+1)
          aaf(j,1,1)=aaf(j,1,1)*aaf(k,1,1)
          aaf(j,2,1)=(aaf(j,2,1)-aaf(k,2,1)*aaf(j,1,1))*aaf(k,2,2)
          aaf(j,3,1)=(aaf(j,3,1)-aaf(k,3,1)*aaf(j,1,1)-aaf(k,3,2)*aaf(j,2,1))*aaf(k,3,3)
          aaf(j,4,1)=(aaf(j,4,1)-aaf(k,4,1)*aaf(j,1,1)-aaf(k,4,2)*aaf(j,2,1)-aaf(k,4,3)*aaf(j,3,1))*aaf(k,4,4)
          aaf(j,3,1)=aaf(j,3,1)-aaf(k,3,4)*aaf(j,4,1)
          aaf(j,2,1)=aaf(j,2,1)-aaf(k,2,4)*aaf(j,4,1)-aaf(k,2,3)*aaf(j,3,1)
          aaf(j,1,1)=aaf(j,1,1)-aaf(k,1,4)*aaf(j,4,1)-aaf(k,1,3)*aaf(j,3,1)-aaf(k,1,2)*aaf(j,2,1)
          !
          aaf(j,1,2)=aaf(j,1,2)*aaf(k,1,1)
          aaf(j,2,2)=(aaf(j,2,2)-aaf(k,2,1)*aaf(j,1,2))*aaf(k,2,2)
          aaf(j,3,2)=(aaf(j,3,2)-aaf(k,3,1)*aaf(j,1,2)-aaf(k,3,2)*aaf(j,2,2))*aaf(k,3,3)
          aaf(j,4,2)=(aaf(j,4,2)-aaf(k,4,1)*aaf(j,1,2)-aaf(k,4,2)*aaf(j,2,2)-aaf(k,4,3)*aaf(j,3,2))*aaf(k,4,4)
          aaf(j,3,2)=aaf(j,3,2)-aaf(k,3,4)*aaf(j,4,2)
          aaf(j,2,2)=aaf(j,2,2)-aaf(k,2,4)*aaf(j,4,2)-aaf(k,2,3)*aaf(j,3,2)
          aaf(j,1,2)=aaf(j,1,2)-aaf(k,1,4)*aaf(j,4,2)-aaf(k,1,3)*aaf(j,3,2)-aaf(k,1,2)*aaf(j,2,2)
          !
          aaf(j,1,3)=aaf(j,1,3)*aaf(k,1,1)
          aaf(j,2,3)=(aaf(j,2,3)-aaf(k,2,1)*aaf(j,1,3))*aaf(k,2,2)
          aaf(j,3,3)=(aaf(j,3,3)-aaf(k,3,1)*aaf(j,1,3)-aaf(k,3,2)*aaf(j,2,3))*aaf(k,3,3)
          aaf(j,4,3)=(aaf(j,4,3)-aaf(k,4,1)*aaf(j,1,3)-aaf(k,4,2)*aaf(j,2,3)-aaf(k,4,3)*aaf(j,3,3))*aaf(k,4,4)
          aaf(j,3,3)=aaf(j,3,3)-aaf(k,3,4)*aaf(j,4,3)
          aaf(j,2,3)=aaf(j,2,3)-aaf(k,2,4)*aaf(j,4,3)-aaf(k,2,3)*aaf(j,3,3)
          aaf(j,1,3)=aaf(j,1,3)-aaf(k,1,4)*aaf(j,4,3)-aaf(k,1,3)*aaf(j,3,3)-aaf(k,1,2)*aaf(j,2,3)
          !
          aaf(j,1,4)=aaf(j,1,4)*aaf(k,1,1)
          aaf(j,2,4)=(aaf(j,2,4)-aaf(k,2,1)*aaf(j,1,4))*aaf(k,2,2)
          aaf(j,3,4)=(aaf(j,3,4)-aaf(k,3,1)*aaf(j,1,4)-aaf(k,3,2)*aaf(j,2,4))*aaf(k,3,3)
          aaf(j,4,4)=(aaf(j,4,4)-aaf(k,4,1)*aaf(j,1,4)-aaf(k,4,2)*aaf(j,2,4)-aaf(k,4,3)*aaf(j,3,4))*aaf(k,4,4)
          aaf(j,3,4)=aaf(j,3,4)-aaf(k,3,4)*aaf(j,4,4)
          aaf(j,2,4)=aaf(j,2,4)-aaf(k,2,4)*aaf(j,4,4)-aaf(k,2,3)*aaf(j,3,4)
          aaf(j,1,4)=aaf(j,1,4)-aaf(k,1,4)*aaf(j,4,4)-aaf(k,1,3)*aaf(j,3,4)-aaf(k,1,2)*aaf(j,2,4)
       end do
    end do
  end subroutine facilu


  !###################################################################
  subroutine promat(xxx,yyy)
    implicit none

    real(kind=8),dimension(ncv,4),intent(in)::xxx
    real(kind=8),dimension(ncv,4),intent(out)::yyy

    integer::i,ii,j,jj

    do i=1,ncv
       ii=renum(i)
       yyy(ii,1)=dd(i,1,1)*xxx(ii,1)+dd(i,1,2)*xxx(ii,2)+dd(i,1,3)*xxx(ii,3)+dd(i,1,4)*xxx(ii,4)
       yyy(ii,2)=dd(i,2,1)*xxx(ii,1)+dd(i,2,2)*xxx(ii,2)+dd(i,2,3)*xxx(ii,3)+dd(i,2,4)*xxx(ii,4)
       yyy(ii,3)=dd(i,3,1)*xxx(ii,1)+dd(i,3,2)*xxx(ii,2)+dd(i,3,3)*xxx(ii,3)+dd(i,3,4)*xxx(ii,4)
       yyy(ii,4)=dd(i,4,1)*xxx(ii,1)+dd(i,4,2)*xxx(ii,2)+dd(i,4,3)*xxx(ii,3)+dd(i,4,4)*xxx(ii,4)
       do j=ptmat(i)+1,ptmat(i+1)
          jj=renum(numat(j))
          yyy(ii,1)=yyy(ii,1)+aa(j,1,1)*xxx(jj,1)+aa(j,1,2)*xxx(jj,2)+aa(j,1,3)*xxx(jj,3)+aa(j,1,4)*xxx(jj,4)
          yyy(ii,2)=yyy(ii,2)+aa(j,2,1)*xxx(jj,1)+aa(j,2,2)*xxx(jj,2)+aa(j,2,3)*xxx(jj,3)+aa(j,2,4)*xxx(jj,4)
          yyy(ii,3)=yyy(ii,3)+aa(j,3,1)*xxx(jj,1)+aa(j,3,2)*xxx(jj,2)+aa(j,3,3)*xxx(jj,3)+aa(j,3,4)*xxx(jj,4)
          yyy(ii,4)=yyy(ii,4)+aa(j,4,1)*xxx(jj,1)+aa(j,4,2)*xxx(jj,2)+aa(j,4,3)*xxx(jj,3)+aa(j,4,4)*xxx(jj,4)
       end do
    end do
    !write(*,*)"fin promat"
  end subroutine promat

  !###################################################################
  subroutine derilu(bbb)
    implicit none

    real(kind=8),dimension(ncv,4),intent(inout)::bbb

    integer::i,j,jj

!!!
!!!Descente
!!!
    do i=1,ncv
       do j=ptv(i)+1,ptd(i)-1
          jj=nuv(j)
          bbb(i,1)=bbb(i,1)-aaf(j,1,1)*bbb(jj,1)-aaf(j,1,2)*bbb(jj,2)-aaf(j,1,3)*bbb(jj,3)-aaf(j,1,4)*bbb(jj,4)
          bbb(i,2)=bbb(i,2)-aaf(j,2,1)*bbb(jj,1)-aaf(j,2,2)*bbb(jj,2)-aaf(j,2,3)*bbb(jj,3)-aaf(j,2,4)*bbb(jj,4)
          bbb(i,3)=bbb(i,3)-aaf(j,3,1)*bbb(jj,1)-aaf(j,3,2)*bbb(jj,2)-aaf(j,3,3)*bbb(jj,3)-aaf(j,3,4)*bbb(jj,4)
          bbb(i,4)=bbb(i,4)-aaf(j,4,1)*bbb(jj,1)-aaf(j,4,2)*bbb(jj,2)-aaf(j,4,3)*bbb(jj,3)-aaf(j,4,4)*bbb(jj,4)
       end do

!!!
!!!Normalisation de la diagonale
!!!
       jj=ptd(i)
       bbb(i,1)=bbb(i,1)*aaf(jj,1,1)
       bbb(i,2)=(bbb(i,2)-aaf(jj,2,1)*bbb(i,1))*aaf(jj,2,2)
       bbb(i,3)=(bbb(i,3)-aaf(jj,3,1)*bbb(i,1)-aaf(jj,3,2)*bbb(i,2))*aaf(jj,3,3)
       bbb(i,4)=(bbb(i,4)-aaf(jj,4,1)*bbb(i,1)-aaf(jj,4,2)*bbb(i,2)-aaf(jj,4,3)*bbb(i,3))*aaf(jj,4,4)
       bbb(i,3)=bbb(i,3)-aaf(jj,3,4)*bbb(i,4)
       bbb(i,2)=bbb(i,2)-aaf(jj,2,4)*bbb(i,4)-aaf(jj,2,3)*bbb(i,3)
       bbb(i,1)=bbb(i,1)-aaf(jj,1,4)*bbb(i,4)-aaf(jj,1,3)*bbb(i,3)-aaf(jj,1,2)*bbb(i,2)
    end do

!!!
!!!Remonté
!!!
    do i=ncv,1,-1
       do j=ptd(i)+1,ptv(i+1)
          jj=nuv(j)
          bbb(i,1)=bbb(i,1)-aaf(j,1,1)*bbb(jj,1)-aaf(j,1,2)*bbb(jj,2)-aaf(j,1,3)*bbb(jj,3)-aaf(j,1,4)*bbb(jj,4)
          bbb(i,2)=bbb(i,2)-aaf(j,2,1)*bbb(jj,1)-aaf(j,2,2)*bbb(jj,2)-aaf(j,2,3)*bbb(jj,3)-aaf(j,2,4)*bbb(jj,4)
          bbb(i,3)=bbb(i,3)-aaf(j,3,1)*bbb(jj,1)-aaf(j,3,2)*bbb(jj,2)-aaf(j,3,3)*bbb(jj,3)-aaf(j,3,4)*bbb(jj,4)
          bbb(i,4)=bbb(i,4)-aaf(j,4,1)*bbb(jj,1)-aaf(j,4,2)*bbb(jj,2)-aaf(j,4,3)*bbb(jj,3)-aaf(j,4,4)*bbb(jj,4)
       end do
    end do

  end subroutine derilu

  !###################################################################
  subroutine residilu(xxx,yyy)
    implicit none

    real(kind=8),dimension(ncv,4),intent(in)::xxx
    real(kind=8),dimension(ncv,4),intent(out)::yyy

    integer::i,ii,j



    do i=1,ncv
       ii=renum(i)
       yyy(ii,1)=bb(i,1)-dd(i,1,1)*xxx(ii,1)-dd(i,1,2)*xxx(ii,2)-dd(i,1,3)*xxx(ii,3)-dd(i,1,4)*xxx(ii,4)
       yyy(ii,2)=bb(i,2)-dd(i,2,1)*xxx(ii,1)-dd(i,2,2)*xxx(ii,2)-dd(i,2,3)*xxx(ii,3)-dd(i,2,4)*xxx(ii,4)
       yyy(ii,3)=bb(i,3)-dd(i,3,1)*xxx(ii,1)-dd(i,3,2)*xxx(ii,2)-dd(i,3,3)*xxx(ii,3)-dd(i,3,4)*xxx(ii,4)
       yyy(ii,4)=bb(i,4)-dd(i,4,1)*xxx(ii,1)-dd(i,4,2)*xxx(ii,2)-dd(i,4,3)*xxx(ii,3)-dd(i,4,4)*xxx(ii,4)
       do j=ptmat(i)+1,ptmat(i+1)
          jj=renum(numat(j))
          yyy(ii,1)= yyy(ii,1)-aa(j,1,1)*xxx(jj,1)-aa(j,1,2)*xxx(jj,2)-aa(j,1,3)*xxx(jj,3)-aa(j,1,4)*xxx(jj,4)
          yyy(ii,2)= yyy(ii,2)-aa(j,2,1)*xxx(jj,1)-aa(j,2,2)*xxx(jj,2)-aa(j,2,3)*xxx(jj,3)-aa(j,2,4)*xxx(jj,4)
          yyy(ii,3)= yyy(ii,3)-aa(j,3,1)*xxx(jj,1)-aa(j,3,2)*xxx(jj,2)-aa(j,3,3)*xxx(jj,3)-aa(j,3,4)*xxx(jj,4)
          yyy(ii,4)= yyy(ii,4)-aa(j,4,1)*xxx(jj,1)-aa(j,4,2)*xxx(jj,2)-aa(j,4,3)*xxx(jj,3)-aa(j,4,4)*xxx(jj,4)
       end do
    end do
  end subroutine residilu

  !###################################################################
  subroutine predir
    implicit none

    integer::i,j,k,ncvv,icv,jcv,jj,jjj,lmax,laa,i0


    !renumérotation
    allocate(ptmat(ncv+1),numat(ptvois(ncv+1)))

    k=0
    ptmat(1)=0
    do i=1,ncv
       do j=ptvois(i)+1,ptvois(i+1)
          if (nuvois(j)>0) then
             k=k+1
             numat(k)=nuvois(j)
          end if
       end do
       ptmat(i+1)=k
       call trie(numat(ptmat(i)+1:ptmat(i+1)),ptmat(i+1)-ptmat(i))
    end do
    ncvv=ptmat(ncv+1)
    allocate(aa(ncvv,4,4),bb(ncv,4),dd(ncv,4,4))

    allocate(renum(ncv),denum(ncv))
    renum(:)=0

    renum(1)=1
    denum(1)=1
    j=0
    i=1

    do while(j<ncv)
       j=j+1
       icv=denum(j)
       do k=ptmat(icv)+1,ptmat(icv+1)
          jcv=numat(k)
          if (jcv/=0) then
             if (renum(jcv)==0) then
                i=i+1
                denum(i)=jcv
                renum(jcv)=i
             end if
          end if
       end do
    end do

    if (i/=ncv) then
       write(*,*)"Pas assez d'inconnues (subroutine predir)"
       stop
    end if

!!!
!!!On simule le remplissage pour calculer les largeurs de bande
!!!
    allocate(l_bd(ncv),l_bg(ncv),pt_d(ncv))
    do i=1,ncv
       jj=renum(i)
       l_bg(jj)=0
       l_bd(jj)=0
       do i0=ptmat(i)+1,ptmat(i+1)
          j=numat(i0)
          jjj=renum(j)
          k=jj-jjj
          if (k>0) then
             if (l_bg(jj)<k) l_bg(jj)=k
          else
             if (l_bd(jj)<-k) l_bd(jj)=-k
          end if
       end do
    end do
!!!
!!!On ajuste la ligne de ciel pour eviter des l_bd non-concordantes lors de l'élimination
!!!
    k=0
    lmax=0
    do i=1,ncv
       if (l_bd(i)<k) then
          l_bd(i)=k
       else
          k=l_bd(i)
       end if

       k=k-1
       if (k<0) k=0

       j=l_bg(i)+1+l_bd(i)
       if (lmax<j) lmax=j

    end do

!!!
!!!Position du terme diagonal
!!!
    pt_d(1)=0
    do i=2,ncv
       pt_d(i)=pt_d(i-1)+l_bd(i-1)
    end do

    laa=pt_d(ncv)

    write(*,*)"Taille de la matrice :",laa
    write(*,*)"Taille de la ligne :",lmax

    allocate(aaf(laa,neq,neq),aal(lmax,neq,neq),bbf(ncv,neq))

  end subroutine predir

  !###################################################################
  subroutine resdir(residu)
    implicit none

    real(kind=8),intent(out)::residu


    integer::i,j,k,icv,kdia,jj,jdia,kd,kk,kkk

    real(kind=8)::residu1,residu2


!!!
!!!Méthode de Gauss
!!!

    do i=1,ncv
       icv=denum(i)

       bbf(i,1)=bb(icv,1)
       bbf(i,2)=bb(icv,2)
       bbf(i,3)=bb(icv,3)
       bbf(i,4)=bb(icv,4)

       do k=1,l_bg(i)+1+l_bd(i)
          aal(k,:,:)=0
       end do

       kdia=l_bg(i)+1

       aal(kdia,:,:)=dd(icv,:,:)
       do j=ptmat(icv)+1,ptmat(icv+1)
          jj=renum(numat(j))-i+kdia
          aal(jj,:,:)=aa(j,:,:)
       end do

!!!
!!!Elimination des termes sous la diagonale
!!!
       do k=l_bg(i),1,-1
          j=i-k
          jdia=kdia-k

          bbf(i,1)=bbf(i,1)-aal(jdia,1,1)*bbf(j,1)-aal(jdia,1,2)*bbf(j,2)&
               -aal(jdia,1,3)*bbf(j,3)-aal(jdia,1,4)*bbf(j,4)
          bbf(i,2)=bbf(i,2)-aal(jdia,2,1)*bbf(j,1)-aal(jdia,2,2)*bbf(j,2)&
               -aal(jdia,2,3)*bbf(j,3)-aal(jdia,2,4)*bbf(j,4)
          bbf(i,3)=bbf(i,3)-aal(jdia,3,1)*bbf(j,1)-aal(jdia,3,2)*bbf(j,2)&
               -aal(jdia,3,3)*bbf(j,3)-aal(jdia,3,4)*bbf(j,4)
          bbf(i,4)=bbf(i,4)-aal(jdia,4,1)*bbf(j,1)-aal(jdia,4,2)*bbf(j,2)&
               -aal(jdia,4,3)*bbf(j,3)-aal(jdia,4,4)*bbf(j,4)

          kd=pt_d(j)-jdia
          do kk=jdia+1,jdia+l_bd(j)
             kkk=kk+kd

             aal(kk,1,1)=aal(kk,1,1)-aal(jdia,1,1)*aaf(kkk,1,1)-aal(jdia,1,2)*aaf(kkk,2,1)&
                  -aal(jdia,1,3)*aaf(kkk,3,1)-aal(jdia,1,4)*aaf(kkk,4,1)
             aal(kk,2,1)=aal(kk,2,1)-aal(jdia,2,1)*aaf(kkk,1,1)-aal(jdia,2,2)*aaf(kkk,2,1)&
                  -aal(jdia,2,3)*aaf(kkk,3,1)-aal(jdia,2,4)*aaf(kkk,4,1)
             aal(kk,3,1)=aal(kk,3,1)-aal(jdia,3,1)*aaf(kkk,1,1)-aal(jdia,3,2)*aaf(kkk,2,1)&
                  -aal(jdia,3,3)*aaf(kkk,3,1)-aal(jdia,3,4)*aaf(kkk,4,1)
             aal(kk,4,1)=aal(kk,4,1)-aal(jdia,4,1)*aaf(kkk,1,1)-aal(jdia,4,2)*aaf(kkk,2,1)&
                  -aal(jdia,4,3)*aaf(kkk,3,1)-aal(jdia,4,4)*aaf(kkk,4,1)


             aal(kk,1,2)=aal(kk,1,2)-aal(jdia,1,1)*aaf(kkk,1,2)-aal(jdia,1,2)*aaf(kkk,2,2)&
                  -aal(jdia,1,3)*aaf(kkk,3,2)-aal(jdia,1,4)*aaf(kkk,4,2)
             aal(kk,2,2)=aal(kk,2,2)-aal(jdia,2,1)*aaf(kkk,1,2)-aal(jdia,2,2)*aaf(kkk,2,2)&
                  -aal(jdia,2,3)*aaf(kkk,3,2)-aal(jdia,2,4)*aaf(kkk,4,2)
             aal(kk,3,2)=aal(kk,3,2)-aal(jdia,3,1)*aaf(kkk,1,2)-aal(jdia,3,2)*aaf(kkk,2,2)&
                  -aal(jdia,3,3)*aaf(kkk,3,2)-aal(jdia,3,4)*aaf(kkk,4,2)
             aal(kk,4,2)=aal(kk,4,2)-aal(jdia,4,1)*aaf(kkk,1,2)-aal(jdia,4,2)*aaf(kkk,2,2)&
                  -aal(jdia,4,3)*aaf(kkk,3,2)-aal(jdia,4,4)*aaf(kkk,4,2)

             aal(kk,1,3)=aal(kk,1,3)-aal(jdia,1,1)*aaf(kkk,1,3)-aal(jdia,1,2)*aaf(kkk,2,3)&
                  -aal(jdia,1,3)*aaf(kkk,3,3)-aal(jdia,1,4)*aaf(kkk,4,3)
             aal(kk,2,3)=aal(kk,2,3)-aal(jdia,2,1)*aaf(kkk,1,3)-aal(jdia,2,2)*aaf(kkk,2,3)&
                  -aal(jdia,2,3)*aaf(kkk,3,3)-aal(jdia,2,4)*aaf(kkk,4,3)
             aal(kk,3,3)=aal(kk,3,3)-aal(jdia,3,1)*aaf(kkk,1,3)-aal(jdia,3,2)*aaf(kkk,2,3)&
                  -aal(jdia,3,3)*aaf(kkk,3,3)-aal(jdia,3,4)*aaf(kkk,4,3)
             aal(kk,4,3)=aal(kk,4,3)-aal(jdia,4,1)*aaf(kkk,1,3)-aal(jdia,4,2)*aaf(kkk,2,3)&
                  -aal(jdia,4,3)*aaf(kkk,3,3)-aal(jdia,4,4)*aaf(kkk,4,3)

             aal(kk,1,4)=aal(kk,1,4)-aal(jdia,1,1)*aaf(kkk,1,4)-aal(jdia,1,2)*aaf(kkk,2,4)&
                  -aal(jdia,1,3)*aaf(kkk,3,4)-aal(jdia,1,4)*aaf(kkk,4,4)
             aal(kk,2,4)=aal(kk,2,4)-aal(jdia,2,1)*aaf(kkk,1,4)-aal(jdia,2,2)*aaf(kkk,2,4)&
                  -aal(jdia,2,3)*aaf(kkk,3,4)-aal(jdia,2,4)*aaf(kkk,4,4)
             aal(kk,3,4)=aal(kk,3,4)-aal(jdia,3,1)*aaf(kkk,1,4)-aal(jdia,3,2)*aaf(kkk,2,4)&
                  -aal(jdia,3,3)*aaf(kkk,3,4)-aal(jdia,3,4)*aaf(kkk,4,4)
             aal(kk,4,4)=aal(kk,4,4)-aal(jdia,4,1)*aaf(kkk,1,4)-aal(jdia,4,2)*aaf(kkk,2,4)&
                  -aal(jdia,4,3)*aaf(kkk,3,4)-aal(jdia,4,4)*aaf(kkk,4,4)

          end do
       end do

!!!
!!!Inversion du terme diagonal
!!!

       aal(kdia,1,1)=1._8/aal(kdia,1,1)
       aal(kdia,1,2)=aal(kdia,1,1)*aal(kdia,1,2)
       aal(kdia,1,3)=aal(kdia,1,1)*aal(kdia,1,3)
       aal(kdia,1,4)=aal(kdia,1,1)*aal(kdia,1,4)

       aal(kdia,2,2)=1._8/(aal(kdia,2,2)-aal(kdia,2,1)*aal(kdia,1,2))
       aal(kdia,2,3)=aal(kdia,2,2)*(aal(kdia,2,3)-aal(kdia,2,1)*aal(kdia,1,3))
       aal(kdia,2,4)=aal(kdia,2,2)*(aal(kdia,2,4)-aal(kdia,2,1)*aal(kdia,1,4))

       aal(kdia,3,2)=aal(kdia,3,2)-aal(kdia,3,1)*aal(kdia,1,2)
       aal(kdia,3,3)=1._8/(aal(kdia,3,3)-aal(kdia,3,1)*aal(kdia,1,3)-aal(kdia,3,2)*aal(kdia,2,3))
       aal(kdia,3,4)=aal(kdia,3,3)*(aal(kdia,3,4)-aal(kdia,3,1)*aal(kdia,1,4)-aal(kdia,3,2)*aal(kdia,2,4))

       aal(kdia,4,2)=aal(kdia,4,2)-aal(kdia,4,1)*aal(kdia,1,2)
       aal(kdia,4,3)=aal(kdia,4,3)-aal(kdia,4,1)*aal(kdia,1,3)-aal(kdia,4,2)*aal(kdia,2,3)
       aal(kdia,4,4)=1._8/(aal(kdia,4,4)-aal(kdia,4,1)*aal(kdia,1,4)-aal(kdia,4,2)*aal(kdia,2,4)-aal(kdia,4,3)*aal(kdia,3,4))

       bbf(i,1)=bbf(i,1)*aal(kdia,1,1)
       bbf(i,2)=(bbf(i,2)-aal(kdia,2,1)*bbf(i,1))*aal(kdia,2,2)
       bbf(i,3)=(bbf(i,3)-aal(kdia,3,1)*bbf(i,1)-aal(kdia,3,2)*bbf(i,2))*aal(kdia,3,3)
       bbf(i,4)=(bbf(i,4)-aal(kdia,4,1)*bbf(i,1)-aal(kdia,4,2)*bbf(i,2)-aal(kdia,4,3)*bbf(i,3))*aal(kdia,4,4)
       bbf(i,3)=bbf(i,3)-aal(kdia,3,4)*bbf(i,4)
       bbf(i,2)=bbf(i,2)-aal(kdia,2,4)*bbf(i,4)-aal(kdia,2,3)*bbf(i,3)
       bbf(i,1)=bbf(i,1)-aal(kdia,1,4)*bbf(i,4)-aal(kdia,1,3)*bbf(i,3)-aal(kdia,1,2)*bbf(i,2)

       do k=kdia+1,kdia+l_bd(i)
          aal(k,1,1)=aal(k,1,1)*aal(kdia,1,1)
          aal(k,2,1)=(aal(k,2,1)-aal(kdia,2,1)*aal(k,1,1))*aal(kdia,2,2)
          aal(k,3,1)=(aal(k,3,1)-aal(kdia,3,1)*aal(k,1,1)-aal(kdia,3,2)*aal(k,2,1))*aal(kdia,3,3)
          aal(k,4,1)=(aal(k,4,1)-aal(kdia,4,1)*aal(k,1,1)-aal(kdia,4,2)*aal(k,2,1)-aal(kdia,4,3)*aal(k,3,1))*aal(kdia,4,4)
          aal(k,3,1)=aal(k,3,1)-aal(kdia,3,4)*aal(k,4,1)
          aal(k,2,1)=aal(k,2,1)-aal(kdia,2,4)*aal(k,4,1)-aal(kdia,2,3)*aal(k,3,1)
          aal(k,1,1)=aal(k,1,1)-aal(kdia,1,4)*aal(k,4,1)-aal(kdia,1,3)*aal(k,3,1)-aal(kdia,1,2)*aal(k,2,1)

          aal(k,1,2)=aal(k,1,2)*aal(kdia,1,1)
          aal(k,2,2)=(aal(k,2,2)-aal(kdia,2,1)*aal(k,1,2))*aal(kdia,2,2)
          aal(k,3,2)=(aal(k,3,2)-aal(kdia,3,1)*aal(k,1,2)-aal(kdia,3,2)*aal(k,2,2))*aal(kdia,3,3)
          aal(k,4,2)=(aal(k,4,2)-aal(kdia,4,1)*aal(k,1,2)-aal(kdia,4,2)*aal(k,2,2)-aal(kdia,4,3)*aal(k,3,2))*aal(kdia,4,4)
          aal(k,3,2)=aal(k,3,2)-aal(kdia,3,4)*aal(k,4,2)
          aal(k,2,2)=aal(k,2,2)-aal(kdia,2,4)*aal(k,4,2)-aal(kdia,2,3)*aal(k,3,2)
          aal(k,1,2)=aal(k,1,2)-aal(kdia,1,4)*aal(k,4,2)-aal(kdia,1,3)*aal(k,3,2)-aal(kdia,1,2)*aal(k,2,2)

          aal(k,1,3)=aal(k,1,3)*aal(kdia,1,1)
          aal(k,2,3)=(aal(k,2,3)-aal(kdia,2,1)*aal(k,1,3))*aal(kdia,2,2)
          aal(k,3,3)=(aal(k,3,3)-aal(kdia,3,1)*aal(k,1,3)-aal(kdia,3,2)*aal(k,2,3))*aal(kdia,3,3)
          aal(k,4,3)=(aal(k,4,3)-aal(kdia,4,1)*aal(k,1,3)-aal(kdia,4,2)*aal(k,2,3)-aal(kdia,4,3)*aal(k,3,3))*aal(kdia,4,4)
          aal(k,3,3)=aal(k,3,3)-aal(kdia,3,4)*aal(k,4,3)
          aal(k,2,3)=aal(k,2,3)-aal(kdia,2,4)*aal(k,4,3)-aal(kdia,2,3)*aal(k,3,3)
          aal(k,1,3)=aal(k,1,3)-aal(kdia,1,4)*aal(k,4,3)-aal(kdia,1,3)*aal(k,3,3)-aal(kdia,1,2)*aal(k,2,3)

          aal(k,1,4)=aal(k,1,4)*aal(kdia,1,1)
          aal(k,2,4)=(aal(k,2,4)-aal(kdia,2,1)*aal(k,1,4))*aal(kdia,2,2)
          aal(k,3,4)=(aal(k,3,4)-aal(kdia,3,1)*aal(k,1,4)-aal(kdia,3,2)*aal(k,2,4))*aal(kdia,3,3)
          aal(k,4,4)=(aal(k,4,4)-aal(kdia,4,1)*aal(k,1,4)-aal(kdia,4,2)*aal(k,2,4)-aal(kdia,4,3)*aal(k,3,4))*aal(kdia,4,4)
          aal(k,3,4)=aal(k,3,4)-aal(kdia,3,4)*aal(k,4,4)
          aal(k,2,4)=aal(k,2,4)-aal(kdia,2,4)*aal(k,4,4)-aal(kdia,2,3)*aal(k,3,4)
          aal(k,1,4)=aal(k,1,4)-aal(kdia,1,4)*aal(k,4,4)-aal(kdia,1,3)*aal(k,3,4)-aal(kdia,1,2)*aal(k,2,4)

          kkk=k-kdia+pt_d(i)

          aaf(kkk,:,:)=aal(k,:,:)
       end do
    end do

!!!
!!!Remonté
!!!
    do i=ncv,1,-1
       kd=pt_d(i)-i
       do j=i+1,i+l_bd(i)
          k=j+kd
          bbf(i,1)=bbf(i,1)-aaf(k,1,1)*bbf(j,1)-aaf(k,1,2)*bbf(j,2)-aaf(k,1,3)*bbf(j,3)-aaf(k,1,4)*bbf(j,4)
          bbf(i,2)=bbf(i,2)-aaf(k,2,1)*bbf(j,1)-aaf(k,2,2)*bbf(j,2)-aaf(k,2,3)*bbf(j,3)-aaf(k,2,4)*bbf(j,4)
          bbf(i,3)=bbf(i,3)-aaf(k,3,1)*bbf(j,1)-aaf(k,3,2)*bbf(j,2)-aaf(k,3,3)*bbf(j,3)-aaf(k,3,4)*bbf(j,4)
          bbf(i,4)=bbf(i,4)-aaf(k,4,1)*bbf(j,1)-aaf(k,4,2)*bbf(j,2)-aaf(k,4,3)*bbf(j,3)-aaf(k,4,4)*bbf(j,4)
       end do
       icv=denum(i)
       bb(icv,:)=bbf(i,:)
    end do


    residu1=maxval(abs(bb(:,1:2)))
    residu2=maxval(abs(bb(:,4)))
    residu=max(residu1,residu2)

  end subroutine resdir

  !6969696969696969696969696969696969696969696969696969696969696969696969
  !6969696969696969696969696969696969696969696969696969696969696969696969
  !6969696969696969696969696969696969696969696969696969696969696969696969
  !69696969                               9696969696969696969696969696969
  !69696969   FIN        Routine générales  96969696969696969696969696969
  !69696969                               9696969696969696969696969696969
  !6969696969696969696969696969696969696969696969696969696969696969696969
  !6969696969696969696969696969696969696969696969696969696969696969696969
  !6969696969696969696969696969696969696969696969696969696969696969696969
  !6969696969696969696969696969696969696969696969696969696969696969696969



  !###################################################################
  !###################################################################
  !###################################################################
  !GEOMETRIE
  !###################################################################
  !###################################################################
  !###################################################################
  subroutine geometrie
    implicit none

    !###################################################################
    !Calcul du facteur de régularité et de l'aire des volumes
    !###################################################################
    allocate(mcv(ncv))

    call regularite(&
         xs,ys,xcv,ycv,ptvois,nusom,ncv,&
         mcv&
         )
    !###################################################################
    !Définition du pointeur
    !###################################################################
    allocate(ptsc(nbsom+1)) !pointeur du nombre de sommet connecté cumulé
    ptsc=0
    call pointeur(&
         ptvois,ncv,nbsom,nusom,&
         ptsc&
         )


    !###################################################################
    !Détermination des sommets connecté
    !Numérotation des aretes
    !###################################################################
    nbc=ptsc(nbsom+1)-1
    allocate(nusc(nbc)) !numéro des sommets cumulés=numero sommet connecté
    call pre_num_voisin_volume(&
         ptvois,nusom,ncv,nbsom,nusc,&
         ptsc&
         )
    !###################################################################
    !Détermination des voisins :
    !voisind,voising définis sur la base des pointeurs
    !nuvois(i)=k correspond aux numéros entre voisins : arete nunero i voisine droite triangle k
    !###################################################################
    nbc=ptsc(nbsom+1)-1 ! le nombre d'aretes diférentes
    allocate(nuvois(nptvois))
    nuvois=0
    call num_voisin_volume(&
         ncv,nusom,ptsc,nusc,&
         ptvois,&
         nuvois,&
         nbsom)
    deallocate(nusc,ptsc)
    !
    !Impression avant la renumérotation
    !
    if (.true.) then
       call imprime(&
            ptvois,nusom,xs,ys,xcv,ycv,ncv,&
            neq)
    end if

    !###################################################################
    !Article Robert Eymart:
    !                      A  = \tau
    !                      bx = A(k,l) selon x
    !                      by = A(k,l) selon y
    !###################################################################
    allocate(A(nptvois),bx(nptvois),by(nptvois),As(nptvois))
!!$    allocate(Ass(nptvois))
    call calcul_coef(&
         nusom,ptvois,nuvois,&
         xs,ys,xcv,ycv,&
         A,bx,by,As,&
         ncv)

  end subroutine geometrie

  !###################################################################
  !###################################################################
  !###################################################################
  !MAILLAGE TRIANGULAIRE
  !###################################################################
  !###################################################################
  !###################################################################
  subroutine triangle_element
    implicit none


    !##############################################################
    !division de triangles : nvc0->ncv
    !création des nouveaux noeuds : nbsom0->nbsom
    !définitions des numéros : nusom
    !définition des coordonnée : (xs,ys)
    !##############################################################
    !calcul du nombre exacte de triangle après subdivision
    ncv=ncv0*ndiv*ndiv !calcul du nombre exacte de triangle après subdivision
    nptvois=3*ncv ! nombre total de points pour tous les triangles
    allocate(nusom(nptvois),ptvois(ncv+1))
    do k=1,ncv+1
       ptvois(k)=3*k-3   !pointe sur les 3 points voisins au triangle k : ptvoisin(k)+1 à ptvoisin(k+1)
    end do
    nbsom=ncv0*(ndiv+1)*(ndiv+2)/2 ! nombre de sommet apres division (vrai pour chaque triangle)
    allocate(xs(nbsom),ys(nbsom))
    call div_triangle(&
         nusom ,xs ,ys ,nbsom ,ncv ,&
         nusom0,xs0,ys0,nbsom0,ncv0,&
         ndiv)
    deallocate(xs0,ys0,nusom0)

    !###################################################################
    !Détermination des centres des volumes : xcv et ycv
    !###################################################################
    allocate(xcv(ncv),ycv(ncv))
    call centre_volume(&
         xcv,ycv,&
         nusom,xs,ys,ncv&
         )

  end subroutine triangle_element



  !###################################################################
  !division de triangles : nvc0->ncv
  !création des nouveaux noeuds : nbsom0->nbsom
  !définitions des numéro : nusom
  !définition des coordonnée : (xs,ys)
  subroutine div_triangle(&
       nusom ,xs ,ys ,nbsom ,ncv ,&
       nusom0,xs0,ys0,nbsom0,ncv0,&
       ndiv)
    implicit none

    integer,dimension(:),intent(out)::nusom
    real(kind=8),dimension(:),intent(out)::xs,ys
    integer,intent(inout)::nbsom,ncv
    integer,intent(in)::ndiv

    integer,dimension(:,:),intent(in)::nusom0
    real(kind=8),dimension(:),intent(in)::xs0,ys0
    integer,intent(in)::nbsom0,ncv0

    integer,dimension(:,:,:),allocatable::arete0
    integer,dimension(:),allocatable::lim,lip
    real(kind=8)::cf0,cf1,a0,a1,a2,ds
    integer::is,icv,i0,i1,i2
    integer::i,j,k


    !compteur du nombre de noeuds
    is=nbsom0
    !compteur du nombre de triangles
    icv=ncv0
    !Allocation maxi
    allocate(arete0(nbsom0,nbsom0,ndiv+1),lim(ndiv+1),lip(ndiv+1))
    !Création des nouveau sommets sur les aretes principales
    arete0=0._8
    !Initialisation des sommets par le maillage de base
    xs(1:nbsom0)=xs0(1:nbsom0)
    ys(1:nbsom0)=ys0(1:nbsom0)
    do i=1,ncv0 !pour chaque volume
       do j=1,3 !pour chaque sommet
          k=j+1
          if (k>3) k=1
          i0=nusom0(i,j) !premier sommet de l'arete
          i1=nusom0(i,k) !second sommet de l'arete
          if (arete0(i0,i1,1) == 0) then
             arete0(i0,i1,1)=i0      !premier sommet de l'arete
             arete0(i0,i1,ndiv+1)=i1 !dernier sommet de l'arete
             arete0(i1,i0,1)=i1      !premier sommet de l'arete
             arete0(i1,i0,ndiv+1)=i0 !dernier sommet de l'arete
             !debut de la subdivision interne sur l'arete
             do k=1,ndiv-1
                cf0=dfloat(k)/dfloat(ndiv)
                cf1=1._8-cf0
                !coordonnées des nouveaux noeuds
                is=is+1
                xs(is)=cf0*xs0(i0)+cf1*xs0(i1)
                ys(is)=cf0*ys0(i0)+cf1*ys0(i1)
!!$                !****Dépalcement du noeud aléatoire
!!$                call random_number(ds)
!!$                xs(is)=xs(is)*(1._8+5e-2_8*(ds-.5_8))
!!$                ys(is)=ys(is)*(1._8+5e-2_8*(ds-.5_8))
!!$                !****
                !définition des noeuds de l'arete (numéro de i1 vers i0)
                arete0(i1,i0,k+1)=is
                arete0(i0,i1,ndiv-k+1)=is
             end do
          end if
       end do
    end do
    !création des triangles internes par subdivision
    do i=1,ncv0 !définition numéro des sommets du triangle i
       i0=nusom0(i,1)
       i1=nusom0(i,2)
       i2=nusom0(i,3)
       !parcours des bandes de triangles entre 2 lignes parallèles
       !lim : tableau des sommets de la ligne inférieure
       !lip :  tableau des sommets de la ligne supérieure
       !Initialisation de lim sur coté 1-2
       do j=1,ndiv+1
          lim(j)=arete0(i2,i1,j)
       end do
       do j=1,ndiv-1
          !initialisation des extrémité de lip (parallèle à 1-2)
          lip(1)       =arete0(i2,i0,j+1)
          lip(ndiv-j+1)=arete0(i1,i0,j+1)
          a0=float(j)/float(ndiv) !valeur du pas sur le coté I2I0
          do k=1,ndiv-j-1 ! nombre de division sur la parallèle j
             a1=dfloat(k)/dfloat(ndiv) !pas sur la parallèle j
             a2=1._8-a0-a1
             is=is+1
             xs(is)=a0*xs0(i0)+a1*xs0(i1)+a2*xs0(i2) !somme vecteur a0*I2I0 +a1*I2I1 (origine en I2)
             ys(is)=a0*ys0(i0)+a1*ys0(i1)+a2*ys0(i2) ! (suite)
             !****Dépalcement du noeud aléatoire
!!$             call random_number(ds)
!!$             xs(is)=xs(is)+0.e-2_8*(ds-.5_8)
!!$             ys(is)=ys(is)+0.e-2_8*(ds-.5_8)
             !****
             lip(k+1)=is ! numéro du nouveau noeud
          end do
          !création des triangles pointe en haut
          !numérotation consécutive des 3 sommets des triangles
          do k=1,ndiv-j+1
             icv=icv+1 !nouveau triangle
             nusom(3*icv-2)=lim(k)
             nusom(3*icv-1)=lip(k)
             nusom(3*icv)  =lim(k+1)
          enddo
          !création des triangles pointe en bas
          do k=1,ndiv-j ! 1 de moins que le nombre triangles hauts
             icv=icv+1 !nouveau triangle
             nusom(3*icv-2)=lip(k)
             nusom(3*icv-1)=lip(k+1)
             nusom(3*icv)  =lim(k+1)
          enddo
          !passage à la parallèle suivante
          !recopie de lip dans lim
          do k=1,ndiv-j+1
             lim(k)=lip(k)
          end do
       end do
       !Numérotation du dernier triangle
       nusom(3*i-2)=lim(1)
       nusom(3*i-1)=i0
       nusom(3*i)  =lim(2)
    end do
    if (icv /= ncv) then
       write(*,*)"La division donne ",icv," triangles au lieu de ",ncv
       stop
    end if
    !NOUVEAU NOMBRE DE SOMMETS ET DE TRIANGLES :
    nbsom=is
    ncv=icv
    write(*,*)"ndiv=",ndiv," nb sommets=",nbsom," nb triangles=",ncv
    deallocate(arete0)
    deallocate(lim)
    deallocate(lip)
  end subroutine div_triangle

  !###################################################################
  !Détermination des centres des volumes : xcv et ycv
  !2 équations à deux inconnues (2 produits scalaires) :
  !             [Milieu(I0I1)X].IOI1=0
  !             [Milieu(I0I2)X].IOI2=0
  !***************
  subroutine centre_volume(&
       xcv,ycv,&
       nusom,xs,ys,ncv&
       )
    implicit none

    real(kind=8),dimension(:),intent(out)::xcv,ycv
    real(kind=8),dimension(:),intent(in)::xs,ys
    integer,dimension(:),intent(in)::nusom
    integer,intent(in)::ncv

    integer::i,i0,i1,i2
    real(kind=8)::a11,a12,a21,a22,b1,b2
    real(kind=8)::aire,deter

    do i=1,ncv
       i0=nusom(3*i-2)
       i1=nusom(3*i-1)
       i2=nusom(3*i)
       a11=xs(i1)-xs(i0) !Vecteur I0I1
       a12=ys(i1)-ys(i0) ! (suite)
       a21=xs(i2)-xs(i0) !Vecteur I0I2
       a22=ys(i2)-ys(i0) ! (suite)
       b1=a11*(xs(i0)+xs(i1))+a12*(ys(i0)+ys(i1))
       b2=a21*(xs(i0)+xs(i2))+a22*(ys(i0)+ys(i2))
       !aire du losange : aire (produit vectoriel I0I1 x I0I2)
       aire= a11*a22-a12*a21
       if (aire<0) then !si centre du cercle hors du triangle
          write(*,*)"L'aire du volume ",i," est négative"
          stop
       end if
       deter=0.5_8/aire
       xcv(i)=deter*(b1*a22-b2*a12)
       ycv(i)=deter*(a11*b2-a21*b1)
    end do
  end subroutine centre_volume


  !###################################################################
  !###################################################################
  !###################################################################
  !MAILLAGE RECTANGULAIRE
  !###################################################################
  !###################################################################
  !###################################################################
!!$  subroutine rectangle_element
!!$    implicit none
!!$
!!$    real(kind=8),dimension(:),allocatable::xs0,ys0
!!$    integer,dimension(:,:),allocatable::nusom0
!!$     real(kind=8),dimension(:,:),allocatable::rais0
!!$
!!$    integer::ncv0,nbsom0
!!$
!!$    ncv=ncv0*ndiv*ndiv
!!$    nptvois=4*ncv
!!$    allocate(nusom(nptvois),ptvois(ncv+1))
!!$    do k=1,ncv+1
!!$       ptvois(k)=4*k-4
!!$    end do
!!$    nbsom=ncv0*(ndiv+1)*(ndiv+1)
!!$    allocate(xs(nbsom),ys(nbsom))
!!$    call div_rectangle(&
!!$         nusom ,xs ,ys ,nbsom ,ncv ,&
!!$         nusom0,xs0,ys0,nbsom0,ncv0,&
!!$         ndiv,rais0)
!!$    deallocate(xs0,ys0,nusom0)
!!$  end subroutine rectangle_element

  !###################################################################


  subroutine div_rectangle(&
       nusom ,xs ,ys ,nbsom ,ncv ,&
       nusom0,xs0,ys0,nbsom0,ncv0,&
       ndiv,rais_0,raff)

    implicit none

    integer,dimension(:),intent(out)::nusom
    real(kind=8),dimension(:,:),intent(inout)::rais_0
    real(kind=8),dimension(:),intent(out)::xs,ys
    integer,intent(inout)::nbsom,ncv
    integer,intent(in)::ndiv
    integer,intent(in)::raff

    integer,dimension(:,:),intent(in)::nusom0
    real(kind=8),dimension(:),intent(in)::xs0,ys0
    integer,intent(in)::nbsom0,ncv0

    integer,dimension(:,:,:),allocatable::arete0
    integer,dimension(:),allocatable::lim,lip
    real(kind=8),dimension(:,:),allocatable::rais0
    real(kind=8)::a0,a1,a2,ds,long0
    integer::is,icv,i0,i1,i2,i3
    integer::i,j,k

    select case(raff)
    case(100)
       write(*,*)"Maillage REGULIER en x et y"
    case(101)
       write(*,*)"Maillage en 'cos' dans la direction x"
    case(110,210)
       write(*,*)"Maillage en 'cos' dans la direction y"
    case(111)
       write(*,*)"Maillage en 'cos' dans les directions x et y"
    case(1000)
       write(*,*)"Utilisation de la raison"
       allocate(rais0(ncv0,4))
       rais0(1:ncv0,1:2)=rais_0(1:ncv0,1:2)
       rais0(1:ncv0,3:4)=1._8/rais_0(1:ncv0,1:2)
    case default
       write(*,*)"raff inconnu, raff=",raff
    end select

    !compteur du nombre de noeuds
    is=nbsom0
    !compteur du nombre de volume
    icv=ncv0
    !Allocation maxi
    allocate(arete0(nbsom0,nbsom0,ndiv+1),lim(ndiv+1),lip(ndiv+1))
    !Création des nouveau sommets sur les aretes principales
    arete0=0._8
    !Initialisation des sommets par le maillage de base
    xs(1:nbsom0)=xs0(1:nbsom0)
    ys(1:nbsom0)=ys0(1:nbsom0)
    do i=1,ncv0 !pour chaque volume
       do j=1,4 !pour chaque sommet
          k=j+1
          if (k>4) k=1
          i0=nusom0(i,j) !premier sommet de l'arete
          i1=nusom0(i,k) !second sommet de l'arete


!!$          if (raff==1000) then
!!$             if (xs0(i1)>xs0(i0).or.ys0(i1)>ys0(i0)) rais0(i,j)=1/rais0(i,j) !Modification de la raison pour débuter de la fin !!!
!!$          end if

          if (arete0(i0,i1,1) == 0) then
             arete0(i0,i1,1)=i0      !premier sommet de l'arete
             arete0(i0,i1,ndiv+1)=i1 !dernier sommet de l'arete
             arete0(i1,i0,1)=i1      !premier sommet de l'arete
             arete0(i1,i0,ndiv+1)=i0 !dernier sommet de l'arete
             !debut de la subdivision interne sur l'arete
             do k=1,ndiv-1
                if (raff>=100.and.raff<=111) then
                   a0=dfloat(k)/dfloat(ndiv)
                   if (raff==101.or.raff==111) a0=.5_8*(1._8-dcos(a0*dacos(-1._8)))
                   a1=dfloat(k)/dfloat(ndiv)
                   if (raff==110.or.raff==111) a1=.5_8*(1._8-dcos(a1*dacos(-1._8)))
                   !coordonnées des nouveaux noeuds
                   is=is+1
                   xs(is)=a0*xs0(i0)+(1._8-a0)*xs0(i1)
                   ys(is)=a1*ys0(i0)+(1._8-a1)*ys0(i1)
                   arete0(i1,i0,k+1)=is
                   arete0(i0,i1,ndiv-k+1)=is
                else if (raff==1000) then
                   is=is+1
                   if (rais0(i,j) /=1) then
                      a0=(1._8-rais0(i,j)**k)/(1._8-rais0(i,j)**ndiv)  !!!rapport de deux mailles consécutives
                      xs(is)=xs0(i1)+a0*(xs0(i0)-xs0(i1))
                      ys(is)=ys0(i1)+a0*(ys0(i0)-ys0(i1))
                   else
                      a0=dfloat(k)/dfloat(ndiv)
                      xs(is)=a0*xs0(i0)+(1._8-a0)*xs0(i1)
                      ys(is)=a0*ys0(i0)+(1._8-a0)*ys0(i1)
                   end if
                   arete0(i1,i0,k+1)=is
                   arete0(i0,i1,ndiv-k+1)=is
                end if
                !définition des noeuds de l'arete (numéro de i1 vers i0)
             end do
          end if
       end do
    end do

    !création des rectangles internes par subdivision
    do i=1,ncv0 !définition numéro des sommets du triangle i
       i0=nusom0(i,1)
       i1=nusom0(i,2)
       i2=nusom0(i,3)
       i3=nusom0(i,4)
       !parcours des bandes de triangles entre 2 lignes parallèles
       !lim : tableau des sommets de la ligne inférieure
       !lip :  tableau des sommets de la ligne supérieure
       !Initialisation de lim sur coté 1-2
       do j=1,ndiv+1
          lim(j)=arete0(i2,i1,j)
       end do
       do j=1,ndiv-1
          !initialisation des extrémité de lip (parallèle à 1-2)
          lip(1)       =arete0(i2,i3,j+1)
          lip(ndiv+1)  =arete0(i1,i0,j+1)


          if (raff>=100.and.raff<=111) then
             a0=float(j)/float(ndiv) !valeur du pas sur le coté I2I0
             if (raff==101.or.raff==111) a0=.5_8*(1._8-dcos(a0*dacos(-1._8)))
             do k=1,ndiv-1 ! nombre de division sur la parallèle j
                a1=dfloat(k)/dfloat(ndiv) !pas sur la parallèle j
                if (raff==110.or.raff==111) a1=.5_8*(1._8-dcos(a1*dacos(-1._8)))
                is=is+1
                xs(is)=a0*xs0(i0)+(1._8-a0)*xs0(i1) !
                ys(is)=a1*ys0(i1)+(1._8-a1)*ys0(i2) ! (suite)
                lip(k+1)=is ! numéro du nouveau noeud
             end do
          else if (raff==1000) then
             do k=1,ndiv-1 ! nombre de division sur la parallèle j
                is=is+1
                if (rais0(i,1) /=1) then
                   a0=(1._8-rais0(i,1)**j)/(1._8-rais0(i,1)**ndiv)
                   xs(is)=xs0(i1)+a0*(xs0(i0)-xs0(i1))
                else
                   a0=float(j)/float(ndiv)
                   xs(is)=a0*xs0(i0)+(1._8-a0)*xs0(i1)
                end if
                if (rais0(i,2) /=1) then
                   a0=(1._8-rais0(i,2)**k)/(1._8-rais0(i,2)**ndiv)
                   ys(is)=ys0(i2)+a0*(ys0(i1)-ys0(i2))
                else
                   a1=dfloat(k)/dfloat(ndiv)
                   ys(is)=a1*ys0(i1)+(1._8-a1)*ys0(i2)
                end if
                lip(k+1)=is ! numéro du nouveau noeud
             end do
          end if
          !
          do k=1,ndiv
             icv=icv+1 !nouveau rectangle
             nusom(4*icv-3)=lim(k+1)
             nusom(4*icv-2)=lim(k)
             nusom(4*icv-1)=lip(k)
             nusom(4*icv)  =lip(k+1)
          enddo
          !
          !recopie de lip dans lim
          do k=1,ndiv+1
             lim(k)=lip(k)
          end do
       end do
       !Numérotation de la derniere ligne de  rectangles
       do j=1,ndiv+1
          lip(j)=arete0(i3,i0,j)
       end do
       do k=1,ndiv-1
          icv=icv+1 !nouveau rectangle
          nusom(4*icv-3)=lim(k+1)
          nusom(4*icv-2)=lim(k)
          nusom(4*icv-1)=lip(k)
          nusom(4*icv)  =lip(k+1)
       enddo
       !Numérotation du dernier rectangle (ancien numéro)
       k=ndiv
       nusom(4*i-3)=lim(k+1)
       nusom(4*i-2)=lim(k)
       nusom(4*i-1)=lip(k)
       nusom(4*i)  =lip(k+1)
    end do

    if (icv /= ncv) then
       write(*,*)"La division donne ",icv," rectangles au lieu de ",ncv
       stop
    end if
    !NOUVEAU NOMBRE DE SOMMETS ET DE RECTANGLES :
    nbsom=is
    ncv=icv
    write(*,*)"ndiv=",ndiv," nb sommets=",nbsom," nb rectangles=",ncv
    deallocate(arete0)
    deallocate(lim)
    deallocate(lip)
    if (allocated(rais0)) deallocate(rais0)

    !###################################################################
    !Détermination des centres des volumes : xcv et ycv
    !###################################################################
    allocate(xcv(ncv),ycv(ncv))
    do i=1,ncv
       i0=nusom(4*i-3)
       i1=nusom(4*i-2)
       i2=nusom(4*i-1)
       i3=nusom(4*i  )
       xcv(i)=.25_8*(xs(i0)+xs(i1)+xs(i2)+xs(i3))
       ycv(i)=.25_8*(ys(i0)+ys(i1)+ys(i2)+ys(i3))
    end do

  end subroutine div_rectangle









!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  EN COURS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  subroutine div_srectangle(&
       ndx,ndy,&
       ndivx,ndivy,&
       maillage_x,maillage_y,&
       xs0,ys0)

    implicit none
    integer,intent(in)::ndx,ndy
    integer,dimension(:),intent(in)::ndivx,ndivy
    character(len=*),dimension(:),intent(in)::maillage_x,maillage_y
    real(kind=8),dimension(:),intent(in)::xs0,ys0

    integer::ix,iy,i,j,is,k1,k2,k3,k4,i0,i1,i2,i3
    integer::nxc,nyc
    integer::nxt,nyt

    real(kind=8)::pi,ax,ay,xm,ym,xl,yl


    nxt=sum(ndivx(:))+1
    nyt=sum(ndivy(:))+1

    pi=dacos(-1._8)


    allocate(xs(nbsom),ys(nbsom))

    nxc=0
    xm=0
    ym=0
    do ix=1,ndx
       if (ix/=1) then
          xl=xs0(ix)-xs0(ix-1)
       else
          xl=xs0(ix)
       end if
       nyc=0
       do iy=1,ndy
          if (iy/=1) then
             yl=ys0(iy)-ys0(iy-1)
          else
             yl=ys0(iy)
          end if
          do i=0,ndivx(ix)
             ax=dfloat(i)/dfloat(ndivx(ix))
             do j=0,ndivy(iy)
                ay=dfloat(j)/dfloat(ndivy(iy))

                !Nouvelle maille
                is=i+1+(nxc)+(j+nyc)*nxt
                select case(maillage_x(ix))
                case('srect_cos_x', 'srect_cos_xy')
!!$                   xs(is)=xm+.5_8*(1._8-dcos(ax*pi))*xl
                   xs(is)=xm+(1-dcos(ax*pi/2))*xl
                case('regulier')
                   xs(is)=xm+ax*xl
                case default
                   write(*,*)'pas de maillage approprié'
                   stop
                end select

                select case(maillage_y(iy))
                case('srect_cos_y', 'srect_cos_xy')
                   ys(is)=ym+.5_8*(1._8-dcos(ay*pi))*yl
                case('regulier')
                   ys(is)=ym+ay*yl
                case default
                   write(*,*)'pas de maillage approprié'
                   stop
                end select
             end do
          end do
          nyc=nyc+ndivy(iy)
          ym=ys0(iy)
       end do
       ym=0
       nxc=nxc+ndivx(ix)
       xm=xs0(ix)
    end do


    is=0
    do i=1,nxt-1
       do j=1,nyt-1
          k1=i+(j-1)*nxt
          k2=i+1+(j-1)*nxt
          k3=i+1+(j)*nxt
          k4=i+(j)*nxt

!!$          print*,k1,k2,k3,k4
!!$          print*,xs(k1),xs(k2),xs(k3),xs(k4)
!!$          print*,ys(k1),ys(k2),ys(k3),ys(k4)

          nusom(is+1)=k1
          nusom(is+2)=k2
          nusom(is+3)=k3
          nusom(is+4)=k4
          is=is+4
       end do
    end do




    !###################################################################
    !Détermination des centres des volumes : xcv et ycv
    !###################################################################
    allocate(xcv(ncv),ycv(ncv))
    do i=1,ncv
       i0=nusom(4*i-3)
       i1=nusom(4*i-2)
       i2=nusom(4*i-1)
       i3=nusom(4*i  )
       xcv(i)=.25_8*(xs(i0)+xs(i1)+xs(i2)+xs(i3))
       ycv(i)=.25_8*(ys(i0)+ys(i1)+ys(i2)+ys(i3))
    end do

    !    print*,xcv
    !    print*,ycv

    !    do i=1,ncv
    !       ii=nusom(i)
    !       print*,xs(ii),ys(ii)
    !       ii=nusom(i+1)
    !       print*,xs(ii),ys(ii)
    ! ii=nusom(i+2)
    !       print*,xs(ii),ys(ii)
    ! ii=nusom(i+3)
    !       print*,xs(ii),ys(ii)
    !    end do


  end subroutine div_srectangle







































  !###################################################################
  !###################################################################
  !###################################################################
  !POLAIRE 104
  !###################################################################
  !###################################################################
  !###################################################################
  subroutine polaire(nr,nt,cr,ct)
    implicit none

    integer,intent(in)::nr,nt
    real(kind=8),intent(in)::cr,ct

    real(kind=8),dimension(:),allocatable::r,t
    real(kind=8)::deter,aire,a11,a12,a21,a22,b1,b2,pi
    integer::i0,i1,i2,i,ii,is

    integer::ip,jp
    real(kind=8)::angle,rayon_m,rayon_p


    write(*,*)"Nombre de mailles de R : ",nr
    write(*,*)"Nombre de mailles de THETA : ",nt
    if (cr==0) then
       write(*,*)"Maillage RADIAL régulier"
    else
       write(*,*)"R(J)=(atan(cr*(2*r-1))+atan(cr))/(2*atan(cr))"
       write(*,*)"cr=",cr
    end if
    if (ct==0) then
       write(*,*)"Maillage AZIMUTAL régulier"
    else
       write(*,*)"THETA(J)=(exp(ct*2*t(j)*pi)-1)"
       write(*,*)"         ------------------------"
       write(*,*)"          (exp(ct*pi)-1)/2"
       write(*,*)"ct=",ct
    end if

    pi=acos(-1._8)

    ncv=nr*nt
    nbsom=(nr+1)*nt
    nptvois=ncv*4

    write(*,*)"Nombre de volumes :",ncv
    write(*,*)"Nombre de sommets :",nbsom

    allocate(r(nr),t(nt))
    allocate(xs(nbsom),ys(nbsom))
    allocate(xcv(ncv),ycv(ncv))
    allocate(nusom(nptvois))

    do i=1,nr
       r(i)=(dfloat(i)/dfloat(nr+1))
    end do

    if (cr/=0) then
       r(:)=(atan(cr*(2*r(:)-1))+atan(cr))/(2*atan(cr))
    end if

    r(:)=r(:)+1._8/eta

    do i=1,nt
       t(i)=dfloat(i)/dfloat(nt+1)
    end do

    if (ct/=0) then
       do j=1,nt/2
          t(j)=(dexp(ct*2_8*t(j)*pi)-1_8)/(dexp(ct*pi)-1_8)/2._8
       end do
    end if
    t(nt/2+1:nt)=1._8-t(nt/2:1:-1)

    t(:)=2._8*pi*t(:)-pi*.5_8

    allocate(ptvois(ncv+1))
    do i=1,ncv+1
       ptvois(i)=4*i-4
    end do

    is=0
    do i=1,nr
       do j=1,nt
          is=is+1
          xcv(is)=r(i)*cos(t(j))
          ycv(is)=r(i)*sin(t(j))
       end do
    end do

    ii=0
    do i=1,nr
       do j=1,nt

          if (j==1) then
             angle=(t(nt)-2*pi+t(1))/2
          else
             angle=(t(j-1)+t(j))/2
          end if
          !
          if (i==1) then
             rayon_m=1/eta/( cos(t(j)-angle) )
          else
             rayon_m=(r(i-1)+r(i))/2/( cos(t(j)-angle) )
          end if
          if (i==nr) then
             rayon_p=1/eta+1/( cos(t(j)-angle) )
          else
             rayon_p=(r(i+1)+r(i))/2/( cos(t(j)-angle) )
          end if
          !
          jp=j+1
          ip=i+1
          if (j==nt) then
             jp=1
          end if
          !
          ii=ii+1
          nusom(ii)=jp+(i-1)*nt

          ii=ii+1
          nusom(ii)=j+(i-1)*nt
          xs(nusom(ii))=rayon_m*cos(angle)
          ys(nusom(ii))=rayon_m*sin(angle)

          ii=ii+1
          nusom(ii)=j+(ip-1)*nt
          xs(nusom(ii))=rayon_p*cos(angle)
          ys(nusom(ii))=rayon_p*sin(angle)

          ii=ii+1
          nusom(ii)=jp+(ip-1)*nt
       end do
    end do

    deallocate(r,t)

  end subroutine polaire


  !###################################################################


  subroutine polaire_old(nr,nt,cr,ct)
    implicit none

    integer,intent(in)::nr,nt
    real(kind=8),intent(in)::cr,ct

    real(kind=8),dimension(:),allocatable::r,t
    real(kind=8)::deter,aire,a11,a12,a21,a22,b1,b2,pi
    integer::i0,i1,i2,i,ii,is


    write(*,*)"Nombre de mailles de R : ",nr
    write(*,*)"Nombre de mailles de THETA : ",nt
    if (cr==0) then
       write(*,*)"Maillage RADIAL régulier"
    else
       write(*,*)"R(J)=(atan(cr*(2*r-1))+atan(cr))/(2*atan(cr))"
       write(*,*)"cr=",cr
    end if
    if (ct==0) then
       write(*,*)"Maillage AZIMUTAL régulier"
    else
       write(*,*)"THETA(J)=(exp(ct*2*t(j)*pi)-1)"
       write(*,*)"         ------------------------"
       write(*,*)"          (exp(ct*pi)-1)/2"
       write(*,*)"ct=",ct
    end if

    pi=acos(-1._8)

    ncv=nr*nt
    nbsom=(nr+1)*nt
    nptvois=ncv*4

    write(*,*)"Nombre de volumes :",ncv
    write(*,*)"Nombre de sommets :",nbsom

    allocate(r(nr+1),t(nt+1))
    allocate(xs(nbsom),ys(nbsom))
    allocate(xcv(ncv),ycv(ncv))
    allocate(nusom(nptvois))

    do i=1,nr+1
       r(i)=(dfloat(i-1)/dfloat(nr))
    end do

    if (cr/=0) then
       r(:)=(atan(cr*(2*r(:)-1))+atan(cr))/(2*atan(cr))
    end if

    r(:)=r(:)+1._8/eta

    do i=1,nt+1
       t(i)=dfloat(i-1)/dfloat(nt)
    end do

    do j=1,nt/2+1
       t(j)=(dexp(ct*2_8*t(j)*pi)-1_8)/(dexp(ct*pi)-1_8)/2._8
    end do
    t(nt/2+1:nt+1)=1._8-t(nt/2+1:1:-1)

    t(:)=2._8*pi*t(:)-pi*.5_8

    allocate(ptvois(ncv+1))
    do i=1,ncv+1
       ptvois(i)=4*i-4
    end do

    ii=0
    is=0
    do i=1,nr
       do j=1,nt
          is=is+1
          xs(is)=r(i)*cos(t(j))
          ys(is)=r(i)*sin(t(j))

          ii=ii+1
          nusom(ii)=j+(i-1)*nt
          ii=ii+1
          nusom(ii)=j+(i)*nt
          if (j==nt) then
             ii=ii+1
             nusom(ii)=(1)+(i)*nt
             ii=ii+1
             nusom(ii)=(1)+(i-1)*nt
          else
             ii=ii+1
             nusom(ii)=(j+1)+(i)*nt
             ii=ii+1
             nusom(ii)=(j+1)+(i-1)*nt
          end if
       end do
    end do

    i=nr+1
    do j=1,nt
       is=is+1
       xs(is)=r(i)*cos(t(j))
       ys(is)=r(i)*sin(t(j))
    end do

    !CENTRE VOLUMES
    do i=1,ncv
       i0=nusom(4*i)
       i1=nusom(4*i-3)
       i2=nusom(4*i-1)
       a11=xs(i1)-xs(i0) !Vecteur I0I1
       a12=ys(i1)-ys(i0) ! (suite)
       a21=xs(i2)-xs(i0) !Vecteur I0I2
       a22=ys(i2)-ys(i0) ! (suite)
       b1=a11*(xs(i0)+xs(i1))+a12*(ys(i0)+ys(i1))
       b2=a21*(xs(i0)+xs(i2))+a22*(ys(i0)+ys(i2))
       !aire du losange : aire (produit vectoriel I0I1 x I0I2)
       aire= a11*a22-a12*a21
       if (aire<0) then
          write(*,*)"Aire <0 dans polaire"
          stop
       end if
       deter=0.5_8/aire
       xcv(i)=deter*(b1*a22-b2*a12)
       ycv(i)=deter*(a11*b2-a21*b1)
    end do

    deallocate(r,t)


  end subroutine polaire_old

  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°                        °°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°    Lecture et écriture   °°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°                        °°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

  !###################################################################
  !lecture
  subroutine lecture
    implicit none

    character(len=1)::txt
    integer::test,n
    real(kind=8)::dec,nbr,nbt,para,cstethe,cster
    real(kind=8)::lg
    real(kind=8),dimension(:,:),allocatable::rais_0
    integer::raff
    logical::l_reprise

    integer,dimension(:),allocatable::ndivx,ndivy
    character(len=15),dimension(:),allocatable::maillage_x,maillage_y
    integer::ndx,ndy

    read(*,*)l_reprise
    read(*,*)c_maillage

    read(*,*)nutest,duree,dt,centt,delpourc
    read(*,*)vtest,residobj,thetamax,comu,aff
    read(*,*)ra,pr

    write(*,*)"Reprise",l_reprise
    write(*,*)"Maillage=",c_maillage
    write(*,*)"nutest=",nutest
    write(*,*)"durée=",duree
    write(*,*)"dt initial=",dt
    write(*,*)"centt=",centt
    write(*,*)"delpourc=",delpourc
    write(*,*)"Résidu objectif : ",residobj
    write(*,*)"Coefficient maximal sur l'incrément : ",thetamax
    write(*,*)"Coefficient de pénalisation",comu
    lambda=comu
    write(*,*)"Affichage des étapes de Newton=",aff
    write(*,*)"Re : Vtest=",vtest
    write(*,*)"Ra=",ra
    write(*,*)"Pr=",pr
    write(*,*)


!!!**********************************************
!!! Avec reprise
!!!**********************************************
    if (l_reprise) then
       reprise=1
       write(*,*)"--------------->Reprise"
       write(*,*)"Reprise"
       call lecture_maillage
       write(*,*)
    else
!!!**********************************************
!!! Sans reprise
!!!**********************************************
       reprise=0
    end if



    select case(c_maillage)
    case('triangle') !Maillage triangle : triangle.dat
       write(*,*)"---------->>> Lecture dans triangle.dat"
       open(20,file="triangle.dat",status="old")
       read(20,*)ndiv
       !Lecture du nombre de volumes de base
       read(20,*)ncv0
       !lecture du nombre de sommets maillage de base
       read(20,*)nbsom0
       !lecture des coordonnées des sommets
       allocate(xs0(nbsom0),ys0(nbsom0))
       do i=1,nbsom0
          read(20,*)xs0(i),ys0(i)
       end do
       allocate(nusom0(ncv0,3))
       do i=1,ncv0
          read(20,*)(nusom0(i,j),j=1,3)
       end do
       close(20)
       call triangle_element

    case('triangle_polaire')
       write(*,*)"---------->>> Lecture dans triangle_eccentrique.dat"
       !nbse=3
       open(20,file="triangle_eccentrique.dat",status="old")
       read(20,*)ndiv
       read(20,*) eta
       !Décentrage
       read(20,*) ray
       !Angle de décentrage
       read(20,*) dec
       !Nombre de mailles en r
       read(20,*) nbr
       !Nombre de mailles en théta
       read(20,*) nbt
       !Lecture du nombre de volumes de base
       read(20,*) ncv0
       !lecture du nombre de sommets maillage de base
       read(20,*) nbsom0
       !Paramètre de subdivision des mailles
       read(20,*) para
       !Coefficient pour la répartition tangente hyperbolique en théta
       read(20,*) cstethe
       !Coeff réparition des centres des cercles en r
       read(20,*) cster
       !lecture des coordonnées des sommets
       print*,'Nombre de sommets : ',nbsom0,' De mailles : ',ncv0,' eta=',eta
       allocate(xs0(nbsom0),ys0(nbsom0))
       do i=1,nbsom0
          read(20,*) xs0(i),ys0(i)
       enddo
       write(*,*) 'Bornes en x : ',maxval(xs0),minval(xs0)
       write(*,*) 'Bornes en y : ',maxval(ys0),minval(ys0)
       !lecture des numéros des noeuds pour chaque volume
       allocate(nusom0(ncv0,3))
       do i=1,ncv0
          read(20,*)(nusom0(i,j),j=1,3)
       end do
       close(20)
       write(*,*) '---------------- Fin lecture maillage'
       write(*,*) 'Excentricite : ', ray, ' Angle : ', dec
       write(*,*) 'Nombre de mailles initiales : ', 'nr=',nbr,' ntheta=',nbt
       write(*,*) 'Parametre de subdivision des mailles triangulaires : ', para
       write(*,*) 'Coefficient de repartition des mailles en theta : ', cstethe
       write(*,*) 'Coefficient de repartition des centres des cercles en r : ', cster
       call triangle_element

    case('voronoi_polaire')
       write(*,*)"---------->>> Lecture dans voronoi.dat"
       open(20,file="voronoi.dat",status="old")
       open(21,file="voronoi_donnees.dat",status="old")
       read(21,*) eta
       read(21,*) ray
       read(21,*) dec
       read(21,*) ncv
       read(21,*) nbsom
       read(21,*) nptvois
       close(21)
       print*,'Nombre de sommets : ',nbsom,' De mailles : ',ncv,' eta=',eta
       write(*,*) 'Excentricite : ', ray, ' Angle : ', dec
       allocate(xs(nbsom),ys(nbsom))

       do i=1,nbsom
          read(20,*) xs(i),ys(i)
       enddo
       write(*,*) 'Bornes en x : ',maxval(xs),minval(xs)
       write(*,*) 'Bornes en y : ',maxval(ys),minval(ys)
       allocate(nusom(nptvois))
       allocate(ptvois(ncv+1))
       allocate(xcv(ncv),ycv(ncv))
       ptvois(1)=0
       do i=1,ncv
          read(20,*) n,(nusom(j),j=ptvois(i)+1,ptvois(i)+n)
          ptvois(i+1)=ptvois(i)+n
       enddo
       do i=1,ncv
          read(20,*) xcv(i), ycv(i)
       enddo
       write(*,*) 'Excentricite : ', ray, ' Angle : ', dec
       write(*,*) '---------------- Fin lecture maillage'
       close(20)

    case('voronoi')
       write(*,*)"---------->>> Lecture dans voronoi.dat"
       open(20,file="voronoi.dat",status="old")
       open(21,file="voronoi_donnees.dat",status="old")
       read(21,*) ncv
       read(21,*) nbsom
       read(21,*) nptvois
       close(21)
       print*,'Nombre de sommets : ',nbsom,' De mailles : ',ncv, ' De voisins :',nptvois
       allocate(xs(nbsom),ys(nbsom))

       do i=1,nbsom
          read(20,*) xs(i),ys(i)
       enddo
       write(*,*) 'Bornes en x : ',maxval(xs),minval(xs)
       write(*,*) 'Bornes en y : ',maxval(ys),minval(ys)
       allocate(nusom(nptvois))
       allocate(ptvois(ncv+1))
       allocate(xcv(ncv),ycv(ncv))
       ptvois(1)=0
       do i=1,ncv
          read(20,*) n,(nusom(j),j=ptvois(i)+1,ptvois(i)+n)
          ptvois(i+1)=ptvois(i)+n
       enddo
       do i=1,ncv
          read(20,*) xcv(i), ycv(i)
       enddo
       write(*,*) '---------------- Fin lecture maillage'
       close(20)




    case('rect_cos_x', 'rect_cos_y', 'rect_cos_xy', 'rect_raison')
       write(*,*)"---------->>> Lecture dans rectangle.dat"
       open(20,file="rectangle.dat",status="old")
       read(20,*)ndiv
       !Lecture du nombre de volumes de base
       read(20,*)ncv0
       !lecture du nombre de sommets maillage de base
       read(20,*)nbsom0
       !lecture des coordonnées des sommets
       allocate(xs0(nbsom0),ys0(nbsom0))
       do i=1,nbsom0
          read(20,*)xs0(i),ys0(i)
       end do
       allocate(nusom0(ncv0,4))
       if (c_maillage=='rect_raison') then
          allocate(rais_0(ncv0,2))
          do i=1,ncv0
             read(20,*)(nusom0(i,j),j=1,4),(rais_0(i,j),j=1,2)
             do j=1,2
                if (rais_0(i,j)<0) rais_0(i,j)=1/abs(rais_0(i,j))
                if (rais_0(i,j)/=1) then
                   write(*,*)"~~~~~~~~~~~~~~~ MAILLE",i," ~~~~~~~~~~~~~~~"
                   select case(j)
                   case (1)
                      write(*,*)"X"
                      lg=abs(xs0(nusom0(i,2))-xs0(nusom0(i,1)))
                   case (2)
                      write(*,*)"Y"
                      lg=abs(ys0(nusom0(i,3))-ys0(nusom0(i,2)))
                   end select
                   write(*,*)"Taille de maille /1:",&
                        lg*min(&
                        (1._8-rais_0(i,j)**1)/(1._8-rais_0(i,j)**ndiv),&
                        rais_0(i,j)**(ndiv-1)*(1._8-rais_0(i,j)**1)/(1._8-rais_0(i,j)**ndiv)),&
                        lg*max(&
                        (1._8-rais_0(i,j)**1)/(1._8-rais_0(i,j)**ndiv),&
                        rais_0(i,j)**(ndiv-1)*(1._8-rais_0(i,j)**1)/(1._8-rais_0(i,j)**ndiv))
                end if
             end do
          end do
       else
          allocate(rais_0(1,1))
          do i=1,ncv0
             read(20,*)(nusom0(i,j),j=1,4)
          end do
       end if
       close(20)
       ncv=ncv0*ndiv*ndiv
       nptvois=4*ncv
       allocate(nusom(nptvois),ptvois(ncv+1))
       do k=1,ncv+1
          ptvois(k)=4*k-4
       end do
       nbsom=ncv0*(ndiv+1)*(ndiv+1)
       allocate(xs(nbsom),ys(nbsom))
       if (c_maillage=='rect_cos_x') raff=101
       if (c_maillage=='rect_cos_y') raff=110
       if (c_maillage=='rect_cos_xy') raff=111
       if (c_maillage=='rect_raison') raff=1000
       call div_rectangle(&
            nusom ,xs ,ys ,nbsom ,ncv ,&
            nusom0,xs0,ys0,nbsom0,ncv0,&
            ndiv,rais_0,raff)
       deallocate(xs0,ys0,nusom0)
       if (allocated(rais_0)) deallocate(rais_0)

    case ('polaire')
       open(20,file="polaire.dat",status="old")
       read(20,*)eta
       eta=eta-1
       write(*,*)"ETA=",eta
       read(20,*)nr,nt
       read(20,*)cr,ct
       close(20)
       call polaire(nr,nt,cr,ct)

    case('structure')
       write(*,*)"---------->>> Lecture dans srectangle.dat"
       open(20,file="srectangle.dat",status="old")
       !Lecture du nombre de volumes en x et y
       read(20,*)ndx,ndy
       allocate(ndivx(ndx),ndivy(ndy),maillage_x(ndx),maillage_y(ndy))
       do i=1,ndx
          read(20,*)ndivx(i),maillage_x(i)
       end do
       do i=1,ndy
          read(20,*)ndivy(i),maillage_y(i)
       end do
       !lecture des coordonnées des sommets
       allocate(xs0(ndx),ys0(ndy))
       do i=1,ndx
          read(20,*)xs0(i)
       end do
       do i=1,ndy
          read(20,*)ys0(i)
       end do

!!$          if (nutest==100) then
!!$             ALy=ys0(ndy)
!!$             write(*,*)"Cavité ouverte de longueur : ",ALy
!!$             write(*,*)"Valeur de Ra_m=",ra
!!$             ra=ra*ALy
!!$             write(*,*)"Valeur de Ra calculé :",ra
!!$          end if

       ncv=0
       nbsom=0
       do i=1,ndx
          do j=1,ndy
             ncv=ncv+ndivx(i)*ndivy(j)
          end do
       end do

       nbsom=(sum(ndivx(:))+1)*(sum(ndivy(:))+1)

       nptvois=4*ncv
       allocate(nusom(nptvois),ptvois(ncv+1))
       do k=1,ncv+1
          ptvois(k)=4*k-4
       end do

       call div_srectangle(&
            ndx,ndy,&
            ndivx,ndivy,&
            maillage_x,maillage_y,&
            xs0,ys0)
       deallocate(ndivx,ndivy)
    case default
       write(*,*)'pas de lecture appropriée'
       stop
    end select



    select case(nutest)
    case(2,3,4,100,102,110,210, CHEMINEE_ETENDUE)
       ALy=maxval(ys)
       ALx=maxval(xs)
       write(*,*)"Cavité de hauteur : ",ALy
       write(*,*)"Cavité de largeur : ",ALx
       !TODO : étendue ?
       if (nutest==100.or.nutest==110.or.nutest==210) then
          write(*,*)"Valeur de Ra_m=",ra
          ra=ra*ALy/2
          write(*,*)"Valeur de Ra calculé :",ra
       end if
    case(101)
       ALy=maxval(ys)-3
       ALx=maxval(xs)-3
       write(*,*)"Cavité de hauteur : ",ALy
       write(*,*)"Cavité de largeur : ",ALx

       write(*,*)"Valeur de Ra_m=",ra
       ra=ra*ALy/2
       write(*,*)"Valeur de Ra calculé :",ra


    end select





  end subroutine lecture

  !###################################################################
  !Sortie
  subroutine sortie(&
       vx,vy,press,tp,mcv,&
       potent,&
       xcv,ycv,xs,ys,&
       ptvois,nusom,&
       ncv,nbsom,nptvois)
    implicit none


    real(kind=8),dimension(:),intent(in)::vx,vy,press,tp,mcv,potent
    real(kind=8),dimension(:),intent(in)::xcv,ycv,xs,ys
    integer,dimension(:),intent(in)::ptvois,nusom
    integer,intent(in)::ncv,nbsom,nptvois

    real(kind=8),dimension(:),allocatable::vxs,vys,ps,tps,ssom,potentc
    integer::i,j,is,compt,test

    allocate(vxs(nbsom),vys(nbsom),ps(nbsom),tps(nbsom),ssom(nbsom),potentc(ncv))
    vxs=0
    vys=0
    ps=0
    tps=0
    ssom=0
    potentc=0

    !Valeurs aux sommets
    do i=1,ncv
       compt=0
       do j=ptvois(i)+1,ptvois(i+1)
          is=nusom(j)
          tps(is)=tps(is)+mcv(i)*tp(i)
          vxs(is)=vxs(is)+mcv(i)*vx(i)
          vys(is)=vys(is)+mcv(i)*vy(i)
          ps(is)=ps(is)+mcv(i)*press(i)
          ssom(is)=ssom(is)+mcv(i)
          potentc(i)=potentc(i)+potent(is)
          compt=compt+1
       end do
       potentc(i)=potentc(i)/compt
    end do

    do is=1,nbsom
       tps(is)=tps(is)/ssom(is)
       vxs(is)=vxs(is)/ssom(is)
       vys(is)=vys(is)/ssom(is)
       ps(is)=ps(is)/ssom(is)
    end do

    open(unit=15,file='uvpt_donnees.plt',status='unknown')
    write(15,*)ncv,nbsom,nptvois
    select case(c_maillage)
    case('voronoi_polaire')
       write(*,*)"°°°°°°°°°°°°°°°°°ETA"
       write(15,*)eta
    case('triangle_polaire')
       write(*,*)"°°°°°°°°°°°°°°°°°ETA"
       write(15,*)eta
       write(*,*)"°°°°°°°°°°°°°°°°°NR,NT,CR,CT"
       write(15,*)nr,nt
       write(15,*)cr,ct
    end select
    close(15)

    open(unit=15,file='uvpt.plt',status='unknown')
    write(15,*)"TITLE = Nombre de sommets et mailles "
    write(15,*)"VARIABLES = x y p u v T psi"
    write(15,*)"ZONE T=p F=FEPOINT N=",nbsom+ncv," E=",nptvois," ET=TRIANGLE"
    do i=1,ncv
       write(15,*)xcv(i),ycv(i),press(i),vx(i),vy(i),tp(i),potentc(i)
    end do
    do i=1,nbsom
       write(15,*)xs(i),ys(i),ps(i),vxs(i),vys(i),tps(i),potent(i)
    enddo
    do i=1,ncv
       is=ptvois(i+1)
       do j=ptvois(i)+1,ptvois(i+1)
          write(15,*)i,ncv+nusom(is),ncv+nusom(j)
          is=j
       end do
    end do
    close(15)

    deallocate(vxs,vys,ps,tps,ssom,potentc)

  end subroutine sortie

  !###################################################################
  !Lecture maillage
  subroutine lecture_maillage
    implicit none


    real(kind=8)::vxs,vys,ps,tps,potent,potentc
    integer::i,j,k,is,ncv_plus_nusom_is,ncv_plus_nusom_j
    integer::test



    character(len=1)::txt

    open(unit=15,file='uvpt_donnees.plt',status='unknown')
    read(15,*)ncv_old,nbsom_old,nptvois_old
    select case(c_maillage)
    case('voronoi_polaire')
       read(15,*)eta_old
       write(*,*)"eta_old=",eta_old
    case('triangle_polaire')
       read(15,*)eta_old
       write(*,*)"eta_old=",eta_old
       read(15,*)nr_old,nt_old
       read(15,*)cr_old,ct_old
       write(*,*)"nr_old=",nr_old," nt_old=",nt_old
       write(*,*)"cr_old=",cr_old," ct_old=",ct_old
    end select

    open(unit=15,file='uvpt.plt',status='unknown')
    read(15,"(A)")txt
    read(15,"(A)")txt
    read(15,"(A)")txt

    allocate(xcv_old(ncv_old),ycv_old(ncv_old))
    allocate(p_old(ncv_old),vx_old(ncv_old),vy_old(ncv_old),tp_old(ncv_old))

    do i=1,ncv_old
       read(15,*)xcv_old(i),ycv_old(i),p_old(i),vx_old(i),vy_old(i),tp_old(i),potentc

    end do


  end subroutine lecture_maillage
  !###################################################################
!!! Passage des variables OLD aux NEW
  subroutine changement_grille_excentrique
    implicit none
    integer,dimension(:),allocatable::compt
    integer,dimension(:,:),allocatable::secteur_angl
    integer::n_angle,maille
    real(kind=8)::pi,angle,delta_angle,angle_min,angle_max
    real(kind=8)::lg,lg_min


    !Comptage du nombre de mailles par secteur angulaire
    write(*,*)"Comptage du nombre de mailles par secteur angulaire"
    pi=acos(-1._8)
    n_angle=100
    allocate(compt(n_angle))
    delta_angle=2*pi/n_angle
    compt(:)=0
    do i=1,ncv_old
       if (ycv_old(i)>=0.and.xcv_old(i)==0) angle=pi/2
       if (ycv_old(i)<0.and.xcv_old(i)==0)  angle=3*pi/2
       if (ycv_old(i)>=0.and.xcv_old(i)>0)  angle=atan(ycv_old(i)/xcv_old(i))
       if (ycv_old(i)<0 .and.xcv_old(i)>0)  angle=atan(ycv_old(i)/xcv_old(i))+2*pi
       if (ycv_old(i)>=0.and.xcv_old(i)<0)  angle=atan(ycv_old(i)/xcv_old(i))+pi
       if (ycv_old(i)<0 .and.xcv_old(i)<0)  angle=atan(ycv_old(i)/xcv_old(i))+pi
       angle_min=0
       angle_max=delta_angle
       do j=1,n_angle
          if (angle>=angle_min.and.angle<=angle_max) then
             compt(j)=compt(j)+1
             exit
          end if
          angle_min=angle_min+delta_angle
          angle_max=angle_max+delta_angle
       end do
    end do

    allocate(secteur_angl(n_angle,maxval(compt(:))))

    !Definition des secteurs angulaires
    write(*,*)"Definition des secteurs angulaires"
    compt(:)=0
    do i=1,ncv_old
       if (ycv_old(i)>=0.and.xcv_old(i)==0) angle=pi/2
       if (ycv_old(i)<0.and.xcv_old(i)==0)  angle=3*pi/2
       if (ycv_old(i)>=0.and.xcv_old(i)>0)  angle=atan(ycv_old(i)/xcv_old(i))
       if (ycv_old(i)<0 .and.xcv_old(i)>0)  angle=atan(ycv_old(i)/xcv_old(i))+2*pi
       if (ycv_old(i)>=0.and.xcv_old(i)<0)  angle=atan(ycv_old(i)/xcv_old(i))+pi
       if (ycv_old(i)<0 .and.xcv_old(i)<0)  angle=atan(ycv_old(i)/xcv_old(i))+pi
       angle_min=0
       angle_max=delta_angle
       do j=1,n_angle
          if (angle>=angle_min.and.angle<=angle_max) then
             compt(j)=compt(j)+1
             secteur_angl(j,compt(j))=i
             exit
          end if
          angle_min=angle_min+delta_angle
          angle_max=angle_max+delta_angle
       end do
    end do

    !Nouvelle grille
    write(*,*)"Nouvelle grille"
    allocate(vx(ncv),vy(ncv),tp(ncv),p(ncv))
    do i=1,ncv
       lg_min=1000
       if (ycv(i)>=0.and.xcv(i)==0) angle=pi/2
       if (ycv(i)<0 .and.xcv(i)==0) angle=3*pi/2
       if (ycv(i)>=0.and.xcv(i)>0)  angle=atan(ycv(i)/xcv(i))
       if (ycv(i)<0 .and.xcv(i)>0)  angle=atan(ycv(i)/xcv(i))+2*pi
       if (ycv(i)>=0.and.xcv(i)<0)  angle=atan(ycv(i)/xcv(i))+pi
       if (ycv(i)<0 .and.xcv(i)<0)  angle=atan(ycv(i)/xcv(i))+pi
       angle_min=0
       angle_max=delta_angle
       do j=1,n_angle
          if (angle>=angle_min.and.angle<=angle_max) then
             do k=1,compt(j)
                l=secteur_angl(j,k)
                lg=(xcv_old(l)-xcv(i))**2+(ycv_old(l)-ycv(i))**2
                if (lg<lg_min) then
                   lg_min=lg
                   maille=l
                end if
             end do
             exit
          end if
          angle_min=angle_min+delta_angle
          angle_max=angle_max+delta_angle
       end do
       vx(i)=vx_old(maille)
       vy(i)=vy_old(maille)
       tp(i)=tp_old(maille)
       p(i)=p_old(maille)
    end do

    deallocate(xcv_old,ycv_old,vx_old,vy_old,tp_old,p_old,compt,secteur_angl)
    write(*,*)"Deallocation des tableaux de changement de maillage"

  end subroutine changement_grille_excentrique

  !###################################################################
  !Impression
  subroutine imprime(&
       ptvois,nusom,xs,ys,xcv,ycv,ncv,&
       neq)
    implicit none

    integer,dimension(:),intent(in)::ptvois,nusom
    real(kind=8),dimension(:),intent(in)::xs,ys,xcv,ycv
    integer,intent(in)::ncv,neq
    integer::i,j,i0,i1
    integer::grille=1,volume=0,noeud=0,taille_n=10,taille_v=14
    real(kind=4)::decalage,echelle,trans

    if (grille ==1) then
       echelle=6000.
       decalage=10.
       open(unit=15, file="grid.fig", status="unknown")
       write(15,"(A)")"#FIG 3.2"
       write(15,"(A)")"Landscape"
       write(15,"(A)")"Center"
       write(15,"(A)")"Metric"
       write(15,"(A)")"A4"
       write(15,"(A)")"100.00"
       write(15,"(A)")"Single"
       write(15,"(A)")"-2"
       write(15,"(A)")"1200 2"
       do i=1,ncv
          if (volume==1) call numero(xcv(i),ycv(i),i,decalage,echelle,taille_v)
          i0=nusom(ptvois(i+1))
          do j=ptvois(i)+1,ptvois(i+1)
             i1=nusom(j)
             call line( xs(i0),ys(i0),xs(i1),ys(i1),decalage,echelle,0)
             if (noeud==1) call numero(xs(i1),ys(i1),i1,decalage,echelle,taille_n)
             i0=i1
          end do
       end do
       close(15)
    end if
  end subroutine imprime

  !###################################################################
  !ligne
  subroutine line(x1,y1,x2,y2,decalage,echelle,coul)
    implicit none
    real(kind=8),intent(in)::x1,y1,x2,y2
    integer::i1,i2,j1,j2,coul,epais
    real(kind=4),intent(in)::decalage,echelle

    i1=x1*echelle+decalage
    j1=(3-y1)*echelle+decalage
    i2=x2*echelle+decalage
    j2=(3-y2)*echelle+decalage

    epais=1
    if (coul/=0) epais=3

    write(15,*)"2 1 0 ",epais,"  ",coul," 7 50 -1 -1 0.000 0 0 -1 0 0 2"
    write(15,*)"         ",i1,j1,i2,j2
  end subroutine line

  !###################################################################
  !noeud
  subroutine numero(x1,y1,nusom1,decalage,echelle,taille)
    implicit none
    real(kind=8),intent(in)::x1,y1
    integer,intent(in)::nusom1,taille
    integer::i1,j1
    real(kind=4),intent(in)::decalage,echelle

    i1=x1*echelle+decalage
    j1=(3-y1)*echelle+decalage
    write(15,10)taille,i1,j1,nusom1
10  format("4 0 0 50 -1 0 ",i4," 0.0000 4 135 90 ",i5,1x,i5,1x,i5,"\001")

  end subroutine numero

  !###################################################################
  !Lignes de courant
  subroutine ligncour(&
       mcv,A,bx,by,&
       xs,ys,&
       vx,vy,p,&
       ptvois,nusom,nuvois,&
       ncv,nbsom&
       )
    implicit none


    real(kind=8),dimension(:),intent(in)::mcv,A,bx,by,xs,ys
    real(kind=8),dimension(:),intent(in)::vx,vy,p
    integer,dimension(:),intent(in)::ptvois,nusom,nuvois
    integer,intent(in)::ncv,nbsom


    integer::j,jj,jjj,icv,i0,i1,i0x,jx,i1x,jy
    integer::neumann_t,neumann_u,neumann_v,neumann_p,flagu,flagv,flagu2,flagv2,ind

    real(kind=8)::x_lim,y_lim,u_lim,v_lim,p_lim,t_lim,sx_lim,sy_lim,st_lim,bxa,bya
    integer,dimension(:),allocatable::valpo,renlcv,denlcv


    write(*,*)"Début ligncour"


    !Initialisation générale
    allocate(valpo(nbsom),renlcv(ncv),denlcv(ncv),potent(nbsom))
    valpo(1:nbsom)=0
    renlcv(1:ncv)=0

    renlcv(1)=1
    denlcv(1)=1
    i=1 !compteur des mailles rencontrées

    !Initialisation du premier point
    icv=1
    j=ptvois(icv)+1
    i0=nusom(j)
    potent(i0)=0
    valpo(i0)=1

    do k=1,ncv

       !Recherche d'un sommet connu
       icv=denlcv(k)
       j=ptvois(icv)+1
       i0=nusom(j)
       do while (valpo(i0) == 0)
          j=j+1
          if (j>ptvois(icv+1)) then
             write(*,*)"Problème dans ligncour"
             stop
          end if
          i0=nusom(j)
       end do

       !Une fois le sommet connu, calcul des sommets suivants
       do jjj=ptvois(icv)+1,ptvois(icv+1) !boucle sur tous les sommets de icv
          j=j+1
          if (j>ptvois(icv+1)) j=ptvois(icv)+1
          i1=nusom(j)
          valpo(i1)=1 !sommet i1 en cours de calcul

          jj=nuvois(j)
          if (jj<=0) then
             call testsol(&
                  u_lim,v_lim,p_lim,t_lim,&
                  sx_lim,sy_lim,st_lim,&
                  neumann_t,neumann_u,neumann_v,neumann_p,&
                  nutest,vtest,eta,&
                  icv,j,i0,i1)
             flagu=1-neumann_u
             flagv=1-neumann_v
             potent(i1)=potent(i0)+bx(j)*u_lim*flagu-by(j)*v_lim*flagv

             if (neumann_u==1.or.neumann_v==1) then !Reconstruction du flux :u1+u2+u3=0 (u3=-u2-u1)
                i0x=nusom(ptvois(icv+1))
                do jx=ptvois(icv)+1,ptvois(icv+1)
                   i1x=nusom(jx)
                   jy=nuvois(jx)
                   if (jy<=0.and.jx/=j) then
                      call testsol(&
                           u_lim,v_lim,p_lim,t_lim,&
                           sx_lim,sy_lim,st_lim,&
                           neumann_t,neumann_u,neumann_v,neumann_p,&
                           nutest,vtest,eta,&
                           icv,jx,i0x,i1x)
                      if (neumann_u==1.or.neumann_v==1) then
                         write(*,*)"Une maille a deux faces NEUMANN (dans ligncour)"
                         !stop
                      end if
                      flagu2=(1-neumann_u)*(1-flagu)
                      flagv2=(1-neumann_v)*(1-flagv)
                      potent(i1)=potent(i0)-&
                           bx(j)*u_lim*flagu2-by(j)*v_lim*flagv2
                   else
                      flagu2=(1-flagu)
                      flagv2=(1-flagv)
                      potent(i1)=potent(i0)-&
                           (bx(j)*vx(jy) + bxa*vx(icv))*flagu2&
                           -(by(j)*vy(jy) + bya*vy(icv))*flagv2
                   end if
                   i0x=i1x
                end do
             end if
          else
             ind = ptvois(jj)
             do while (nuvois(ind) /= icv)
                ind = ind+1
             end do
             !les bx,by vus de l'autre côté
             bxa = bx(ind)
             bya = by(ind)

             if (renlcv(jj)==0) then !Nouvelle maille à considérer
                i=i+1
                renlcv(jj)=i
                denlcv(i)=jj
             end if
             potent(i1) = potent(i0) + bx(j)*vx(jj) + bxa*vx(icv)&
                  +by(j)*vy(jj)+bya*vy(icv)&
                  -As(j)*(p(jj) - p(icv))
          end if
          i0=i1
       end do
    end do

    write(*,*)"Fin ligncour"
  end subroutine ligncour

  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°                               °°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°   FIN     Lecture et écriture   °°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°                               °°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  !°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !++++++++++++++++++++                  ++++++++++++++++++++++++++
  !++++++++++++++++++  Conditions de bord   +++++++++++++++++++++++
  !++++++++++++++++++++                  ++++++++++++++++++++++++++
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



  !NUSSELT moyen

  subroutine nusselt(A,&
       tp,&
       nutest,&
       eta,&
       nui,nuo)
    implicit none

    real(kind=8),dimension(:),intent(in)::A,tp
    integer,intent(in)::nutest
    real(kind=8),intent(in)::eta
    real(kind=8),intent(out)::nui,nuo

    integer::n,i,j,k,nbi,nbo
    real(kind=8)::psi_i,psi_o,psi_mi,psi_mo
    real(kind=8)::urmax,utmax,theta,ur,ut
    real(kind=8)::psii,psio

    real(kind=8)::long,longo,longi,er_psii,er_psio

    !
    integer,dimension(:,:),allocatable::n_coupe_x,n_coupe_y
    integer,dimension(3)::compt_coupe_x,compt_coupe_y
    real(kind=8)::tp1,nu_1,nu_2,nu_3
    integer::kk,j1,j2,icv
    real(kind=8)::psi,Tbulk,db

    nui=0
    nuo=0
    nbi=0
    nbo=0
    psii=0
    psio=0
    longi=0
    longo=0


    select case(nutest)
    case(1)
       do i=1,ncv
          i0=nusom(ptvois(i+1))
          do j=ptvois(i)+1,ptvois(i+1)
             i1=nusom(j)
             jj=nuvois(j)
             if (jj==-1) then
                nbi=nbi+1
                nui=nui+(.5_8-tp(i))*A(j)
                long=dsqrt((xs(i1)-xs(i0))**2+(ys(i1)-ys(i0))**2)
                psii= psii+long*(potent(i0)+potent(i1))/2
                longi=longi+long
             end if
             if (jj==-2) then
                nbo=nbo+1
                nuo=nuo+(tp(i)+.5_8)*A(j)
                long=dsqrt((xs(i1)-xs(i0))**2+(ys(i1)-ys(i0))**2)
                psio= psio+long*(potent(i0)+potent(i1))/2
                longo=longo+long
             end if
          end do
       end do

       psii=psii/longi
       psio=psio/longo
       er_psii=0
       er_psio=0

       do i=1,ncv
          i0=nusom(ptvois(i+1))
          do j=ptvois(i)+1,ptvois(i+1)
             i1=nusom(j)
             jj=nuvois(j)
             if (jj==-1) then
                long=dsqrt((xs(i1)-xs(i0))**2+(ys(i1)-ys(i0))**2)
                er_psii=er_psii+  long*(psii - (potent(i0)+potent(i1))/2 )**2
             end if
             if (jj==-2) then
                long=dsqrt((xs(i1)-xs(i0))**2+(ys(i1)-ys(i0))**2)
                er_psio= er_psio+  long*(psio - (potent(i0)+potent(i1))/2 )**2
             end if
          end do
       end do

       er_psii=sqrt(er_psii)/longi
       er_psio=sqrt(er_psio)/longo


       nui=nui/(2*acos(-1._8)*1) !Flux moyen convectif
       nui=nui*1*log(eta+1._8)      !Nusselt
       nuo=nuo/(2*acos(-1._8)*(1+eta))!Flux moyen convectif
       nuo=nuo*(1+eta)*log(eta+1)!Nusselt
       write(*,*)"eta/log(eta+1)=",eta/log(eta+1)
       write(*,10)nui,nuo

       urmax=0
       utmax=0
       do i=1,ncv
          theta=atan(ycv(i)/(xcv(i)+.0000001))
          ur=vx(i)*cos(theta)+vy(i)*sin(theta)
          ut=-vx(i)*sin(theta)+vy(i)*cos(theta)
          if (abs(ur)>urmax) urmax=abs(ur)
          if (abs(ut)>utmax) utmax=abs(ut)
       end do
       write(*,*)"Max(ur)=",urmax
       write(*,*)"Max(ut)=",utmax
       write(*,*)"PSI inner =", psii," pm RMS= ",er_psii
       write(*,*)"PSI outer =", psio," pm RMS= ",er_psio


!!$       psi_i=0._8
!!$       psi_mi=0._8
!!$       n=size(ptbi)
!!$       do k=1,n
!!$          j=nusom(ptbi(k))
!!$          if (abs(potent(j))>psi_i) psi_i=abs(potent(j))
!!$          psi_mi=psi_mi+potent(j)
!!$       end do
!!$
!!$       psi_o=0._8
!!$       psi_mo=0._8
!!$       n=size(ptbo)
!!$       do k=1,n
!!$          j=nusom(ptbo(k))
!!$          if (abs(potent(j))>psi_o) psi_o=abs(potent(j))
!!$          psi_mo=psi_mo+potent(j)
!!$       end do
!!$
!!$       write(*,*)"Max de Psi et Psi moyen"
!!$       write(*,*)"Cylindre interne :",psi_i,psi_mi
!!$       write(*,*)"Cylindre externe :",psi_o,psi_mo

    case(2,3)
       do i=1,ncv
          do j=ptvois(i)+1,ptvois(i+1)
             jj=nuvois(j)
             if (jj==-1) nui=nui+(.5_8-tp(i))*A(j)
             if (jj==-2) nuo=nuo+(tp(i)+.5_8)*A(j)
          end do
       end do
       nui=nui/1._8
       nuo=nuo/1._8
       write(*,10)nui,nuo
       !


    case(110,210)
!!!Détermination de la taille des tableaux
       compt_coupe_x(:)=0
       compt_coupe_y(:)=0
       do i=1,ncv
          i0=nusom(ptvois(i+1))
          do j=ptvois(i)+1,ptvois(i+1)
             i1=nusom(j)
             jj=nuvois(j)
!!! Détermination des coupes horizontales
             if ((ys(i0)>=Aly/4+1e-8).and.(ys(i1)<Aly/4+1e-8)) then
                compt_coupe_x(1)=compt_coupe_x(1)+1
                exit
             elseif ((ys(i0)>=Aly/2+1e-8).and.(ys(i1)<Aly/2+1e-8)) then
                compt_coupe_x(2)=compt_coupe_x(2)+1
                exit
             elseif ((ys(i0)>=.75_8*Aly+1e-8).and.(ys(i1)<.75_8*Aly+1e-8)) then
                compt_coupe_x(3)=compt_coupe_x(3)+1
                exit
             end if
!!! Détermination des coupes verticales
             if ((xs(i0)<=0).and.(xs(i1)>0)) then
                compt_coupe_y(1)=compt_coupe_y(1)+1
                exit
             elseif ((xs(i0)<=0.5_8+1e-8).and.(xs(i1)>0.5_8+1e-8))  then
                compt_coupe_y(2)=compt_coupe_y(2)+1
                exit
             elseif ((xs(i0)<1).and.(xs(i1)>=1-1e-6))    then
                compt_coupe_y(3)=compt_coupe_y(3)+1
                exit
             end if
             i0=i1
          end do
       end do

       print*,compt_coupe_y,maxval(xs)
       !stop

!!! CALCUL des numéros des mailles des coupes

       n=maxval(compt_coupe_x(:))
       allocate(n_coupe_x(n,3))
       n=maxval(compt_coupe_y(:))
       allocate(n_coupe_y(n,3))

       write(*,*)"--------------------------"
       write(*,*)"-- Resultats thermiques --"
       write(*,*)"--------------------------"
       long=0
       nui=0
       compt_coupe_x(:)=0
       compt_coupe_y(:)=0

       do i=1,ncv
          i0=nusom(ptvois(i+1))
          do j=ptvois(i)+1,ptvois(i+1)
             i1=nusom(j)
             jj=nuvois(j)
!!! Détermination des Nusselt .5, .9 et .97
             if (jj==-1) then
                !A(j)*(tp-t(j))=1*(ys(i1)-ys(i0))
                tp1=(ys(i1)-ys(i0))/A(j)+tp(i)
                nui=nui+(1/tp1)*(ys(i1)-ys(i0))
                long=long+(ys(i1)-ys(i0))
                if ((ys(i0)>=Aly*.5_8).and.(ys(i1)<=Aly*.5_8))  then
                   nu_1=1/tp1
                   write(*,*)"Nu(0.5)= ",nu_1," ---->ycv=",ycv(i), "/",Aly*.5_8
                   write(*,*)"REF Webb and Hill : Nu(0.5)=",.58*(Ra*2/Aly)**.206_8
                   write(*,*)"REF Aung : Nu(0.5)=",.60*(Ra*2/Aly)**.2_8
                   write(*,*)
                end if
                if ((ys(i0)>=Aly*(.25_8+.5_8*.9_8)).and.(ys(i1)<=Aly*(.25_8+.5_8*.9_8)))  then
                   nu_2=1/tp1
                   write(*,*)"Nu(0.9)= ",nu_2," ---->ycv=",ycv(i), "/",Aly*(.25_8+.5_8*.9_8)
                   write(*,*)"REF Webb and Hill : Nu(0.9)=",.61*(Ra*2/Aly)**.205_8
                   write(*,*)
                end if
                if ((ys(i0)>=Aly*(.25_8+.5_8*.97_8)).and.(ys(i1)<=Aly*(.25_8+.5_8*.97_8)))  then
                   nu_3=1/tp1
                   write(*,*)"Nu(0.97)=",nu_3," ---->ycv=",ycv(i), "/",Aly*(.25_8+.5_8*.97_8)
                   write(*,*)"REF Webb and Hill : Nu(0.97)=",.63*(Ra*2/Aly)**.209_8
                   write(*,*)
                end if
             end if
!!! Détermination des coupes horizontales
             if ((ys(i0)>=Aly/4+1e-8).and.(ys(i1)<=Aly/4+1e-8))         then
                compt_coupe_x(1)=compt_coupe_x(1)+1
                n_coupe_x(compt_coupe_x(1),1)=i
                exit
             else if ((ys(i0)>=Aly/2+1e-8).and.(ys(i1)<=Aly/2+1e-8))    then
                compt_coupe_x(2)=compt_coupe_x(2)+1
                n_coupe_x(compt_coupe_x(2),2)=i
                exit
             else if ((ys(i0)>=.75_8*Aly+1e-8).and.(ys(i1)<=.75_8*Aly+1e-8)) then
                compt_coupe_x(3)=compt_coupe_x(3)+1
                n_coupe_x(compt_coupe_x(3),3)=i
                exit
             end if
!!! Détermination des coupes verticales
             if ((xs(i0)<=0).and.(xs(i1)>0))          then
                compt_coupe_y(1)=compt_coupe_y(1)+1
                n_coupe_y(compt_coupe_y(1),1)=i
                exit
             else if ((xs(i0)<=0.5_8+1e-8).and.(xs(i1)>0.5_8+1e-8)) then
                compt_coupe_y(2)=compt_coupe_y(2)+1
                n_coupe_y(compt_coupe_y(2),2)=i
                exit
             else if ((xs(i0)<1).and.(xs(i1)>=1-1e-6)) then
                compt_coupe_y(3)=compt_coupe_y(3)+1
                n_coupe_y(compt_coupe_y(3),3)=i
                exit
             end if
             i0=i1
          end do
       end do




       write(*,*)"Nusselt moyen sur la partie chauffée :",nui/long
       write(*,*)"REF Webb and Hill : Nu_m=",.82*(Ra*2/Aly)**.194_8
       write(*,*)

       write(*,*)"----------------------"
       write(*,*)"-- COUPES /x et /y  --"
       write(*,*)"----------------------"


       !Trie horizontal
       do k=1,3
          do kk=1,compt_coupe_x(k)
             do i=2,compt_coupe_x(k)-kk+1
                j1=n_coupe_x(i  ,k)
                j2=n_coupe_x(i-1,k)
                if (xcv(j1)<xcv(j2)) then
                   n_coupe_x(i  ,k)=j2
                   n_coupe_x(i-1,k)=j1
                end if
             end do
          end do
       end do



       write(*,*)"Fin trie horizontal"

       !Trie vertical
       do k=1,3
          do kk=1,compt_coupe_y(k)
             do i=2,compt_coupe_y(k)-kk+1
                j1=n_coupe_y(i  ,k)
                j2=n_coupe_y(i-1,k)
                if (ycv(j1)<ycv(j2)) then
                   n_coupe_y(i  ,k)=j2
                   n_coupe_y(i-1,k)=j1
                end if
             end do
          end do
       end do

       write(*,*)"Fin trie vertical"

       open(1001,file="profil_h_e.dat")
       do i=1,compt_coupe_x(1)
          ii=n_coupe_x(i,1)
          psi=0
          do j=ptvois(ii)+1,ptvois(ii+1)
             k=nusom(j)
             psi=psi+potent(k)
          end do
          psi=psi/(ptvois(ii+1)-ptvois(ii))
          write(1001,*)xcv(ii),vy(ii),vx(ii),psi,tp(ii),p(ii)
       end do
       close(1001)

       open(1001,file="profil_h_m.dat")
       do i=1,compt_coupe_x(2)
          ii=n_coupe_x(i,2)
          psi=0
          do j=ptvois(ii)+1,ptvois(ii+1)
             k=nusom(j)
             psi=psi+potent(k)
          end do
          psi=psi/(ptvois(ii+1)-ptvois(ii))
          write(1001,*)xcv(ii),vy(ii),vx(ii),psi,tp(ii),p(ii)
       end do
       close(1001)

       open(1001,file="profil_h_s.dat")
       do i=1,compt_coupe_x(3)
          ii=n_coupe_x(i,3)
          psi=0
          do j=ptvois(ii)+1,ptvois(ii+1)
             k=nusom(j)
             psi=psi+potent(k)
          end do
          psi=psi/(ptvois(ii+1)-ptvois(ii))
          write(1001,*)xcv(ii),vy(ii),vx(ii),psi,tp(ii),p(ii)
       end do
       close(1001)


       open(1001,file="profil_v_0.dat")
       do i=1,compt_coupe_y(1)
          ii=n_coupe_y(i,1)
          write(1001,*)ycv(ii),tp(ii)
       end do
       close(1001)

       open(1001,file="profil_v_1.dat")
       do i=1,compt_coupe_y(3)
          ii=n_coupe_y(i,3)
          write(1001,*)ycv(ii),tp(ii)
       end do
       close(1001)

       open(1001,file="profil_v_0.5.dat")
       do i=1,compt_coupe_y(2)
          ii=n_coupe_y(i,2)
          psi=0
          do j=ptvois(ii)+1,ptvois(ii+1)
             k=nusom(j)
             psi=psi+potent(k)
          end do
          psi=psi/(ptvois(ii+1)-ptvois(ii))
!!!Debut de la paroi gauche
          icv=n_coupe_y(i,1)
          Tbulk=tp(icv)*vy(icv)*mcv(icv)
          db=vy(icv)*mcv(icv)
          do
             i0=nusom(ptvois(icv+1))
             b1:do j=ptvois(icv)+1,ptvois(icv+1)
                i1=nusom(j)
                j1=nuvois(j)
                if ((ys(i0)<ycv(ii)).and.(ys(i1)>ycv(ii))) then
                   if (j1<0) goto 88
                   icv=j1
                   Tbulk=Tbulk+tp(icv)*vy(icv)*mcv(icv)
                   db=db+vy(icv)*mcv(icv)
                   exit b1
                end if
                i0=i1
             end do b1
          end do
88        Tbulk=Tbulk/db
          write(1001,*)ycv(ii),vy(ii),vx(ii),psi,tp(ii),Tbulk,p(ii)
       end do
       close(1001)



    case default
       write(*,*)"Nutest=",nutest," inconnu dans nusselt"
    end select


10  format("Nombres de Nusselt : interne =",1pe11.4," | externe =",1pe11.4)

  end subroutine nusselt



  !###################################################################
  !Test solutions
  subroutine cls
    implicit none


    real(kind=8)::x,y
    real(kind=8)::rayon

    real(kind=8)::erx,ery

    integer::i,i0,i1,j,jj


    do i=1,ncv
       i0=nusom(ptvois(i+1))
       do j=ptvois(i)+1,ptvois(i+1)
          i1=nusom(j)
          jj=nuvois(j)
          if (jj<=0) then !Definition des CLS

             select case(nutest)
             case(1) !Cylindres (test sur le rayon)
                x=xs(i1)
                y=ys(i1)
             case(2,3,4,100,102,110,210) !carrés (test sur le centre du coté)
                x=(xs(i1)+xs(i0))*.5_8
                y=(ys(i1)+ys(i0))*.5_8

                erx=abs(xcv(i)-x)/100
                ery=abs(ycv(i)-y)/100

             case default
                !write(*,*)"nutest inconnu :",nutest
                !stop
             end select


             select case(nutest)
             case(1)
                rayon=sqrt(x*x+y*y)
                nuvois(j)=-1
                !print*,eta,1/eta+1,rayon
                if (rayon> 1/eta+1) nuvois(j)=-2

             case(2,3,100,102)
                if (x-erx <0)   nuvois(j)=-1
                if (x+erx >ALx) nuvois(j)=-2
                if (y-ery <0)   nuvois(j)=-3
                if (y+ery >ALy) nuvois(j)=-4

             case(4)
                if (x-erx <0) then
                   nuvois(j)=-1 !y<1.94
                   if ( (y<=ALy).and.(y>=.94)) nuvois(j)=-2
                end if
                if (x+erx >ALx) nuvois(j)=-3
                if (y-ery <0)   nuvois(j)=-4
                if (y+ery >ALy) nuvois(j)=-5

             case(CHEMINEE,210)
                x=(xs(i1)+xs(i0))*.5_8
                y=(ys(i1)+ys(i0))*.5_8
                erx=abs(xcv(i)-x)/100
                ery=abs(ycv(i)-y)/100

                if (x-erx <0)   then
                   if ((y-ery <ALy/4).or.(y+ery>.75_8*ALy)) then
                      nuvois(j)=-2
                   else
                      nuvois(j)=-1
                   end if
                end if
                if (x+erx >ALx) nuvois(j)=-2

                if (y-ery <0)   nuvois(j)=-3
                if (y+ery >ALy) nuvois(j)=-4

             case(CHEMINEE_ETENDUE)
                ! coordonnées du centre du côté
                x=(xs(i1)+xs(i0))*.5_8
                y=(ys(i1)+ys(i0))*.5_8
                erx=abs(xcv(i)-x)/100
                ery=abs(ycv(i)-y)/100

                ! TODO : définir les CL ici
                ! les bords de la boite
                if ((x - erx < 0) .or. (x + erx > ALx) .or. (y+ery > ALy) .or. (y-ery < 0)) then
                   nuvois(j) = CL_EXTERIEUR
                end if
                ! les parois de la cheminée

             case default
                !write(*,*)"nutest inconnu dans cls, nutest=",nutest
             end select

          end if
          i0=i1
       end do
    end do

!!!CHEMINEE
    if (nutest==101) then


       do i=1,ncv
          i0=nusom(ptvois(i+1))
          do j=ptvois(i)+1,ptvois(i+1)
             i1=nusom(j)
             jj=nuvois(j)

             x=(xs(i1)+xs(i0))*.5_8
             y=(ys(i1)+ys(i0))*.5_8
             erx=abs(xcv(i)-x)/2
             ery=abs(ycv(i)-y)/2

             if (jj>0) then !Definition des CLS intérieures


                if (y<ALy.and.y>0) then
                   if (abs(x-0)-erx<0) then
                      if (xcv(i)<0) then
                         nuvois(j)=-101
                      elseif (xcv(i)>0) then
                         nuvois(j)=-102
                      else
                         write(*,*)"Pb 1 dans nutest==101, CLS"
                         stop
                      end if
                   end if

                   if (abs(x-Alx)-erx<0) then
                      if (xcv(i)-Alx<0) then
                         nuvois(j)=-103
                      elseif (xcv(i)-Alx>0) then
                         nuvois(j)=-104
                      else
                         write(*,*)"Pb 2 dans nutest==101, CLS"
                         stop
                      end if
                   end if

                end if
             else if (jj==0) then !Definition des CLS extérieures
                if (x-erx <-3)   nuvois(j)=-1
                if (x+erx >ALx+3) nuvois(j)=-2
                if (y-ery <-6)   nuvois(j)=-3
                if (y+ery >ALy+2) nuvois(j)=-4
                !print*,nuvois(j)
             end if
             i0=i1
          end do
       end do
    end if
  end subroutine cls



  !###################################################################
  !Test solutions
  subroutine testsol(&
       u,v,press,t_q,&
       sx,sy,st,&
       neumann_t,neumann_u,neumann_v,neumann_p,&
       nutest,vtest,eta,&
       i,j,i0,i1)
    implicit none
    real(kind=8),intent(in)::vtest,eta
    integer,intent(in)::nutest,j,i,i0,i1
    real(kind=8),intent(out)::u,v,press,t_q,sx,sy,st
    integer,intent(out)::neumann_t,neumann_u,neumann_v,neumann_p


    real(kind=8)::y
    real(kind=8)::coef


    sx=0
    sy=0
    st=0

    u=0
    v=0
    press=0
    t_q=0

    neumann_t=0
    neumann_u=0
    neumann_v=0
    neumann_p=1
    !
    jj=nuvois(j)
    !

    select case(nutest)
    case(1)
       t_q=.5_8
       if (jj==-2) t_q=-.5_8

    case(2,3)
       if (jj==-1) t_q=+.5_8
       if (jj==-2) t_q=-.5_8
       if (jj==-3) neumann_t=1
       if (jj==-4) then
          neumann_t=1
          if (nutest==3) u=vtest
       end if

    case(4)
       if (jj==-2) then
          y=(ys(i1)+ys(i0))*.5_8
          u=(y-.94_8)*(1.94_8-y)*vtest*4._8
       end if
       if (jj==-3)  then
          neumann_p=0
          neumann_u=1
          neumann_v=1
       end if

       !!%%%%%CHEMINEE
    case(100)
       if (jj==-1) then !GAUCHE
          neumann_t=1
          t_q=abs(ys(i1)-ys(i0))*1._8
       end if
       if (jj==-2) then !DROIT
          neumann_t=1
          t_q=abs(ys(i1)-ys(i0))*1._8
       end if
       if (jj==-3) then !BAS
          if (vy(i)>=0) then
             neumann_u=0
             u=0
             neumann_v=1
             v=vy(i)
             neumann_t=0
             t_q=0
             coef=1
             press=-.5_8*(vx(i)*vx(i)+vy(i)*vy(i))*coef
             dd(i,2,1)=dd(i,2,1)-by(j)*vx(i)*coef
             dd(i,2,2)=dd(i,2,2)-by(j)*vy(i)*coef
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          else
             neumann_u=1
             u=vx(i)
             neumann_v=1
             v=vy(i)
             neumann_t=1
             t_q=0
             press=0
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          end if
       end if
       if (jj==-4) then !HAUT
          if (vy(i)>=0) then
             neumann_u=1
             u=vx(i)
             neumann_v=1
             v=vy(i)
             neumann_t=1
             t_q=0
             press=0
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          else
             neumann_u=0
             u=0
             neumann_v=1
             v=vy(i)
             neumann_t=0
             t_q=0
             coef=1
             press=-.5*(vx(i)*vx(i)+vy(i)*vy(i))*coef
             dd(i,2,1)=dd(i,2,1)-by(j)*vx(i)*coef
             dd(i,2,2)=dd(i,2,2)-by(j)*vy(i)*coef
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          end if
       end if

       !!%%%%%CHEMINEE  DANS UN DOMAINE + VASTE
    case(101)
       if (jj==-1) then !GAUCHE
          if (vx(i)>=0) then
             neumann_u=1
             u=vx(i)
             neumann_v=0
             v=0
             neumann_t=0
             t_q=0
             coef=1
             press=-.5_8*(vx(i)*vx(i)+vy(i)*vy(i))*coef
             dd(i,1,1)=dd(i,1,1)-bx(j)*vx(i)*coef
             dd(i,1,2)=dd(i,1,2)-bx(j)*vy(i)*coef
             bb(i,1)=bb(i,1)-bx(j)*(press-p(i))
             dd(i,1,3)=dd(i,1,3)-bx(j)

             bb(i,3)=bb(i,3)-bx(j)*u
             dd(i,3,1)=dd(i,3,1)+bx(j)
          else
             neumann_u=1
             u=vx(i)
             neumann_v=1
             v=vy(i)
             neumann_t=1
             t_q=0
             press=0
             bb(i,1)=bb(i,1)-bx(j)*(press-p(i))
             dd(i,1,3)=dd(i,1,3)-bx(j)

             bb(i,3)=bb(i,3)-bx(j)*u
             dd(i,3,1)=dd(i,3,1)+bx(j)
          end if
       end if
       if (jj==-2) then !DROIT
          if (vx(i)<0) then
             neumann_u=1
             u=vx(i)
             neumann_v=0
             v=0
             neumann_t=0
             t_q=0
             coef=1
             press=-.5_8*(vx(i)*vx(i)+vy(i)*vy(i))*coef
             dd(i,1,1)=dd(i,1,1)-bx(j)*vx(i)*coef
             dd(i,1,2)=dd(i,1,2)-bx(j)*vy(i)*coef
             bb(i,1)=bb(i,1)-bx(j)*(press-p(i))
             dd(i,1,3)=dd(i,1,3)-bx(j)

             bb(i,3)=bb(i,3)-bx(j)*u
             dd(i,3,1)=dd(i,3,1)+bx(j)
          else
             neumann_u=1
             u=vx(i)
             neumann_v=1
             v=vy(i)
             neumann_t=1
             t_q=0
             press=0
             bb(i,1)=bb(i,1)-bx(j)*(press-p(i))
             dd(i,1,3)=dd(i,1,3)-bx(j)

             bb(i,3)=bb(i,3)-bx(j)*u
             dd(i,3,1)=dd(i,3,1)+bx(j)
          end if
       end if
       if (jj==-3) then !BAS
          if (vy(i)>=0) then
             neumann_u=0
             u=0
             neumann_v=1
             v=vy(i)
             neumann_t=0
             t_q=0
             coef=1
             press=-.5_8*(vx(i)*vx(i)+vy(i)*vy(i))*coef
             dd(i,2,1)=dd(i,2,1)-by(j)*vx(i)*coef
             dd(i,2,2)=dd(i,2,2)-by(j)*vy(i)*coef
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          else
             neumann_u=1
             u=vx(i)
             neumann_v=1
             v=vy(i)
             neumann_t=1
             t_q=0
             press=0
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          end if
       end if
       if (jj==-4) then !HAUT
          if (vy(i)<0) then
             neumann_u=0
             u=0
             neumann_v=1
             v=vy(i)
             neumann_t=0
             t_q=0
             coef=1
             press=-.5_8*(vx(i)*vx(i)+vy(i)*vy(i))*coef
             dd(i,2,1)=dd(i,2,1)-by(j)*vx(i)*coef
             dd(i,2,2)=dd(i,2,2)-by(j)*vy(i)*coef
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          else
             neumann_u=1
             u=vx(i)
             neumann_v=1
             v=vy(i)
             neumann_t=1
             t_q=0
             press=0
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          end if
       end if

       if (jj==-101) then
          neumann_t=1
          t_q=0
       end if
       if (jj==-102) then
          neumann_t=1
          t_q=abs(ys(i1)-ys(i0))*1._8
       end if
       if (jj==-103) then
          neumann_t=1
          t_q=abs(ys(i1)-ys(i0))*1._8
       end if
       if (jj==-104) then
          neumann_t=1
          t_q=0
       end if

!!!!!Ouverture par le bas uniquement
    case(102)
       if (jj==-1) then !GAUCHE
          neumann_t=0
          t_q=1
       end if
       if (jj==-2) then !DROIT
          neumann_t=0
          t_q=0
       end if
       if (jj==-3) then !BAS
          if (vy(i)>=0) then
             neumann_u=0
             u=0
             neumann_v=1
             v=vy(i)
             neumann_t=0
             t_q=0
             coef=1
             press=-.5_8*(vx(i)*vx(i)+vy(i)*vy(i))*coef
             dd(i,2,1)=dd(i,2,1)-by(j)*vx(i)*coef
             dd(i,2,2)=dd(i,2,2)-by(j)*vy(i)*coef
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          else
             neumann_u=1
             u=vx(i)
             neumann_v=1
             v=vy(i)
             neumann_t=1
             t_q=0
             press=0
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          end if
       end if
       if (jj==-4) then !HAUT
          neumann_u=0
          u=0
          neumann_v=0
          v=0
          neumann_t=1
          t_q=0
          press=0
       end if


       !TODO étendue
       !!%%%%%CHEMINEE BENCHMARK Webb et Hill
    case(110)
       if (jj==-1) then !GAUCHE
          neumann_t=1
          t_q=abs(ys(i1)-ys(i0))*1._8
       end if
       if (jj==-2) then !DROIT
          neumann_t=1
          t_q=0
       end if
       
       if (jj==-3) then !BAS
          if (vy(i)>=0) then
             neumann_v=1
             v=vy(i)
             neumann_t=0
             t_q=0
             coef=1.
#if CL_U_DIRICHLET
             neumann_u=0
             u=0
#else
             neumann_u=1
             u=vx(i)
#endif
             ! write(*,*) debit, sqrt(vx(i)*vx(i)+vy(i)*vy(i))
#if GB
             ! global bernoulli
             if (mode_gb .or. ((residu < 1e-2) .and. (residu > 1e-15))) then !première itération : residu = 0
                mode_gb = .true.
                ! si transitoire, on utilise le débit précédent
                if (duree > 0) then
                   press = -.5_8 * debit_prec * debit_prec
                else
                   press=-.5_8*debit*debit
                   dd(i,2,1)=dd(i,2,1)-by(j)*u*coef
                   dd(i,2,2)=dd(i,2,2)-by(j)*vy(i)*coef
                   !éventuellement : ajouter la dépendance sur les voisins aussi
                endif
             else
                press=-.5_8*(vx(i)*vx(i)+vy(i)*vy(i))*coef
                dd(i,2,1)=dd(i,2,1)-by(j)*u*coef
                dd(i,2,2)=dd(i,2,2)-by(j)*vy(i)*coef
             end if
#else
             ! local bernoulli
             press=-.5_8*(vx(i)*vx(i)+vy(i)*vy(i))*coef
             dd(i,2,1)=dd(i,2,1)-by(j)*u*coef
             dd(i,2,2)=dd(i,2,2)-by(j)*vy(i)*coef
#endif
             
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          else
             neumann_u=1
             u=vx(i)
             neumann_v=1
             v=vy(i)
             neumann_t=1
             t_q=0
             press=0
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          end if
       end if
       if (jj==-4) then !HAUT
          if (vy(i)>=0) then
             !fluide sortant
             neumann_u=1
             u=vx(i)
             neumann_v=1
             v=vy(i)
             neumann_t=1
             t_q=0
             press=0
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          else
             !fluide entrant
             neumann_v=1
             v=vy(i)
             neumann_t=0
             t_q=0

             coef=1.
             neumann_u=1
             u=vx(i)

#if PRESSION_HAUT_0
             press = 0

#else
             press=-.5_8*(vy(i)*vy(i))*coef
             dd(i,2,2)=dd(i,2,2)-by(j)*vy(i)*coef
#endif
             
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          end if
       end if

       !!%%%%%CHEMINEE BENCHMARK Webb et Hill avec Pression constante à l'entrée
    case(210)
       if (jj==-1) then !GAUCHE
          neumann_t=1
          t_q=abs(ys(i1)-ys(i0))*1._8
       end if
       if (jj==-2) then !DROIT
          neumann_t=1
          t_q=0
       end if
       if (jj==-3) then !BAS
          if (vy(i)>=0) then
             neumann_u=0
             u=0
             neumann_v=1
             v=vy(i)
             neumann_t=0
             t_q=0
             coef=1.
             press=-.5_8*(debit_out*debit_out)*coef
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

!!$             press=-.5_8*(vy(i)*vy(i))*coef!-.5_8*(vx(i)*vx(i)+vy(i)*vy(i))*coef
!!$             !dd(i,2,1)=dd(i,2,1)-by(j)*vx(i)*coef
!!$             dd(i,2,2)=dd(i,2,2)-by(j)*vy(i)*coef
!!$             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
!!$             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          else
             neumann_u=1
             u=vx(i)
             neumann_v=1
             v=vy(i)
             neumann_t=1
             t_q=0
             press=0
             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
             dd(i,2,3)=dd(i,2,3)-by(j)

             bb(i,3)=bb(i,3)-by(j)*v
             dd(i,3,2)=dd(i,3,2)+by(j)
          end if
       end if
       if (jj==-4) then !HAUT
!!$          if (vy(i)>=0) then
          neumann_u=1
          u=vx(i)
          neumann_v=1
          v=vy(i)
          neumann_t=1
          t_q=0
          press=0
          bb(i,2)=bb(i,2)-by(j)*(press-p(i))
          dd(i,2,3)=dd(i,2,3)-by(j)

          bb(i,3)=bb(i,3)-by(j)*v
          dd(i,3,2)=dd(i,3,2)+by(j)
!!$          else
!!$             neumann_u=0
!!$             u=0
!!$             neumann_v=1
!!$             v=vy(i)
!!$             neumann_t=0
!!$             t_q=0
!!$
!!$             coef=1.
!!$             press=-.5_8*(vy(i)*vy(i))*coef!-.5*(vx(i)*vx(i)+vy(i)*vy(i))*coef
!!$             !dd(i,2,1)=dd(i,2,1)-by(j)*vx(i)*coef
!!$             dd(i,2,2)=dd(i,2,2)-by(j)*vy(i)*coef
!!$             bb(i,2)=bb(i,2)-by(j)*(press-p(i))
!!$             dd(i,2,3)=dd(i,2,3)-by(j)
!!$
!!$             bb(i,3)=bb(i,3)-by(j)*v
!!$             dd(i,3,2)=dd(i,3,2)+by(j)
!!$          end if
       end if


    case default
       write(*,*)"Nutest=",nutest
       write(*,*)"Non implémenté dans testsol"
       stop
    end select

  end subroutine testsol

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !+++++++++++++                         ++++++++++++++++++++++++++
  !+++++++++++  FIN    Conditions de bord   +++++++++++++++++++++++
  !+++++++++++++                         ++++++++++++++++++++++++++
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



end program nvstks
