module maximum_hourly_diagnostics

   use machine, only: kind_phys

   implicit none

   private

   public maximum_hourly_diagnostics_init, maximum_hourly_diagnostics_run, maximum_hourly_diagnostics_finalize

   ! DH* TODO - THIS CAME FROM PHYSCONS.F90 BUT IS IT BETTER PLACED IN HERE?
   real(kind=kind_phys), parameter ::PQ0=379.90516E0, A2A=17.2693882, A3=273.16, A4=35.86, RHmin=1.0E-6
   ! *DH

contains

   subroutine maximum_hourly_diagnostics_init()
   end subroutine maximum_hourly_diagnostics_init

   subroutine maximum_hourly_diagnostics_finalize()
   end subroutine maximum_hourly_diagnostics_finalize

#if 0
!> \section arg_table_maximum_hourly_diagnostics_run Argument Table
!! | local_name           | standard_name                                                      | long_name                                                          | units      | rank | type      | kind      | intent | optional |
!! |----------------------|--------------------------------------------------------------------|--------------------------------------------------------------------|------------|------|-----------|-----------|--------|----------|
!! | im                   | horizontal_loop_extent                                             | horizontal loop extent                                             | count      |    0 | integer   |           | in     | F        |
!! | levs                 | vertical_dimension                                                 | number of vertical levels                                          | count      |    0 | integer   |           | in     | F        |
!! | kdt                  | index_of_time_step                                                 | current forecast iteration                                         | index      |    0 | integer   |           | in     | F        |
!! | nsteps_per_reset     | number_of_time_steps_per_maximum_hourly_time_interval              | number_of_time_steps_per_maximum_hourly_time_interval              | count      |    0 | integer   |           | in     | F        |
!! | lradar               | flag_for_radar_reflectivity                                        | flag for radar reflectivity                                        | flag       |    0 | logical   |           | in     | F        |
!! | imp_physics          | flag_for_microphysics_scheme                                       | choice of microphysics scheme                                      | flag       |    0 | integer   |           | in     | F        |
!! | imp_physics_gfdl     | flag_for_gfdl_microphysics_scheme                                  | choice of GFDL microphysics scheme                                 | flag       |    0 | integer   |           | in     | F        |
!! | imp_physics_thompson | flag_for_thompson_microphysics_scheme                              | choice of Thompson microphysics scheme                             | flag       |    0 | integer   |           | in     | F        |
!! | imp_physics_nssl     | flag_for_nssl_microphysics_scheme                                  | choice of NSSL microphysics scheme                                 | flag       |    0 | integer   |           | in     | F        |
!! | imp_physics_mg       | flag_for_morrison_gettelman_microphysics_scheme                    | choice of Morrison-Gettelman microphysics scheme                   | flag       |    0 | integer   |           | in     | F        |
!! | con_g                | gravitational_acceleration                                         | gravitational acceleration                                         | m s-2      |    0 | real      | kind_phys | in     | F        |
!! | con_rd               | gas_constant_dry_air                                               | ideal gas constant for dry air                                     | J kg-1 K-1 |    0 | real      | kind_phys | in     | F        |
!! | phil                 | geopotential                                                       | geopotential at model layer centers                                | m2 s-2     |    2 | real      | kind_phys | in     | F        |
!! | gt0                  | air_temperature_updated_by_physics                                 | temperature updated by physics                                     | K          |    2 | real      | kind_phys | in     | F        |
!! | prsl                 | air_pressure                                                       | mean layer pressure                                                | Pa         |    2 | real      | kind_phys | in     | F        |
!! | gqgl                 | graupel_mixing_ratio_updated_by_physics                            | moist mixing ratio of graupel updated by physics                   | kg kg-1    |    2 | real      | kind_phys | in     | F        |
!! | gqhl                 | hail_mixing_ratio_updated_by_physics                               | moist mixing ratio of hail updated by physics                      | kg kg-1    |    2 | real      | kind_phys | in     | F        |
!! | gngl                 | graupel_number_concentration_updated_by_physics                    | number concentration of graupel updated by physics                 | kg-1       |    2 | real      | kind_phys | in     | F        |
!! | gnhl                 | hail_number_concentration_updated_by_physics                       | number concentration of hail updated by physics                    | kg-1       |    2 | real      | kind_phys | in     | F        |
!! | vhl                  | hail_particle_volume                                               | volume of hail particles                                           | m3 kg-1    |    2 | real      | kind_phys | in     | F        |
!! | refl_10cm            | radar_reflectivity_10cm                                            | instantaneous refl_10cm                                            | dBZ        |    2 | real      | kind_phys | in     | F        |
!! | refdmax              | maximum_reflectivity_at_1km_agl_over_maximum_hourly_time_interval  | maximum reflectivity at 1km agl over maximum hourly time interval  | dBZ        |    1 | real      | kind_phys | inout  | F        |
!! | refdmax263k          | maximum_reflectivity_at_minus10c_over_maximum_hourly_time_interval | maximum reflectivity at minus10c over maximum hourly time interval | dBZ        |    1 | real      | kind_phys | inout  | F        |
!! | u10m                 | x_wind_at_10m                                                      | 10 meter u wind speed                                              | m s-1      |    1 | real      | kind_phys | in     | F        |
!! | v10m                 | y_wind_at_10m                                                      | 10 meter v wind speed                                              | m s-1      |    1 | real      | kind_phys | in     | F        |
!! | u10max               | maximum_u_wind_at_10m_over_maximum_hourly_time_interval            | maximum u wind at 10m over maximum hourly time interval            | m s-1      |    1 | real      | kind_phys | inout  | F        |
!! | v10max               | maximum_v_wind_at_10m_over_maximum_hourly_time_interval            | maximum v wind at 10m over maximum hourly time interval            | m s-1      |    1 | real      | kind_phys | inout  | F        |
!! | spd10max             | maximum_wind_at_10m_over_maximum_hourly_time_interval              | maximum wind at 10m over maximum hourly time interval              | m s-1      |    1 | real      | kind_phys | inout  | F        |
!! | meshmax              | maximum_maximum_expected_size_of_hail_over_maximum_hourly_time_interval    | hourly max maximum expected size of hail                           | mm         |    1 | real      | kind_phys | inout  | F        |
!! | hmflux01max          | maximum_hail_mass_flux_over_layer_0_1_km_over_maximum_hourly_time_interval | hourly max hail mass flux over the 0-1 km layer                    | kg m-2 s-1 |    1 | real      | kind_phys | inout  | F        |
!! | hske01max            | maximum_hail_sedimentation_kinetic_energy_over_layer_0_1_km_over_hour      | hourly max hail sedimentation kinetic energy over the 0-1 km layer | J m-3      |    1 | real      | kind_phys | inout  | F        |
!! | shcp01max            | maximum_severe_hail_composite_parameter_over_layer_0_1_km_over_hour        | hourly max severe hail composite parameter over the 0-1 km layer   |            |    1 | real      | kind_phys | inout  | F        |
!! | hm01max              | maximum_hail_mass_over_layer_0_1_km_over_maximum_hourly_time_interval      | hourly max hail mass over the 0-1 km layer                         | kg m-2     |    1 | real      | kind_phys | inout  | F        |
!! | hmflux03max          | maximum_hail_mass_flux_over_layer_0_3_km_over_maximum_hourly_time_interval | hourly max hail mass flux over the 0-3 km layer                    | kg m-2 s-1 |    1 | real      | kind_phys | inout  | F        |
!! | hske03max            | maximum_hail_sedimentation_kinetic_energy_over_layer_0_3_km_over_hour      | hourly max hail sedimentation kinetic energy over the 0-3 km layer | J m-3      |    1 | real      | kind_phys | inout  | F        |
!! | shcp03max            | maximum_severe_hail_composite_parameter_over_layer_0_3_km_over_hour        | hourly max severe hail composite parameter over the 0-3 km layer   |            |    1 | real      | kind_phys | inout  | F        |
!! | hm03max              | maximum_hail_mass_over_layer_0_3_km_over_maximum_hourly_time_interval      | hourly max hail mass over the 0-3 km layer                         | kg m-2     |    1 | real      | kind_phys | inout  | F        |
!! | pgr                  | surface_air_pressure                                               | surface pressure                                                   | Pa         |    1 | real      | kind_phys | in     | F        |
!! | t2m                  | temperature_at_2m                                                  | 2 meter temperature                                                | K          |    1 | real      | kind_phys | in     | F        |
!! | q2m                  | specific_humidity_at_2m                                            | 2 meter specific humidity                                          | kg kg-1    |    1 | real      | kind_phys | in     | F        |
!! | t02max               | maximum_temperature_at_2m_over_maximum_hourly_time_interval        | maximum temperature at 2m over maximum hourly time interval        | K          |    1 | real      | kind_phys | inout  | F        |
!! | t02min               | minimum_temperature_at_2m_over_maximum_hourly_time_interval        | minumum temperature at 2m over maximum hourly time interval        | K          |    1 | real      | kind_phys | inout  | F        |
!! | rh02max              | maximum_relative_humidity_at_2m_over_maximum_hourly_time_interval  | maximum relative humidity at 2m over maximum hourly time interval  | %          |    1 | real      | kind_phys | inout  | F        |
!! | rh02min              | minimum_relative_humidity_at_2m_over_maximum_hourly_time_interval  | minumum relative humidity at 2m over maximum hourly time interval  | %          |    1 | real      | kind_phys | inout  | F        |
!! | errmsg               | ccpp_error_message                                                 | error message for error handling in CCPP                           | none       |    0 | character | len=*     | out    | F        |
!! | errflg               | ccpp_error_flag                                                    | error flag for error handling in CCPP                              | flag       |    0 | integer   |           | out    | F        |
!!
#endif
   subroutine maximum_hourly_diagnostics_run(im, levs, kdt, nsteps_per_reset, lradar, imp_physics, &
                                             imp_physics_gfdl, imp_physics_thompson,               &
                                             imp_physics_nssl, imp_physics_mg, con_g, con_rd,      &
                                             phil, gt0, prsl, gqgl, gqhl, gngl, gnhl, vhl, refl_10cm, &
                                             refdmax, refdmax263k, u10m, v10m, u10max, v10max,     &
                                             spd10max, meshmax, hmflux01max, hske01max, shcp01max, &
                                             hm01max, hmflux03max, hske03max, shcp03max, hm03max,  &
                                             pgr, t2m, q2m, t02max, t02min, rh02max, rh02min,      &
                                             errmsg, errflg)

       ! Interface variables
       integer, intent(in) :: im, levs, kdt, nsteps_per_reset
       logical, intent(in) :: lradar
       integer, intent(in) :: imp_physics, imp_physics_gfdl, imp_physics_thompson, imp_physics_nssl, imp_physics_mg
       real(kind_phys), intent(in   ) :: con_g
       real(kind_phys), intent(in   ) :: con_rd
       real(kind_phys), intent(in   ) :: phil(im,levs)
       real(kind_phys), intent(in   ) :: gt0(im,levs)
       real(kind_phys), intent(in   ) :: prsl(im,levs)
       real(kind_phys), intent(in   ) :: gqgl(im,levs)
       real(kind_phys), intent(in   ) :: gqhl(im,levs)
       real(kind_phys), intent(in   ) :: gngl(im,levs)
       real(kind_phys), intent(in   ) :: gnhl(im,levs)
       real(kind_phys), intent(in   ) :: vhl(im,levs)
       real(kind_phys), intent(in   ) :: refl_10cm(im,levs)
       real(kind_phys), intent(inout) :: refdmax(im)
       real(kind_phys), intent(inout) :: refdmax263k(im)
       real(kind_phys), intent(in   ) :: u10m(im)
       real(kind_phys), intent(in   ) :: v10m(im)
       real(kind_phys), intent(inout) :: u10max(im)
       real(kind_phys), intent(inout) :: v10max(im)
       real(kind_phys), intent(inout) :: spd10max(im)
       real(kind_phys), intent(inout) :: meshmax(im)
       real(kind_phys), intent(inout) :: hmflux01max(im)
       real(kind_phys), intent(inout) :: hske01max(im)
       real(kind_phys), intent(inout) :: shcp01max(im)
       real(kind_phys), intent(inout) :: hm01max(im)
       real(kind_phys), intent(inout) :: hmflux03max(im)
       real(kind_phys), intent(inout) :: hske03max(im)
       real(kind_phys), intent(inout) :: shcp03max(im)
       real(kind_phys), intent(inout) :: hm03max(im)
       real(kind_phys), intent(in   ) :: pgr(im)
       real(kind_phys), intent(in   ) :: t2m(im)
       real(kind_phys), intent(in   ) :: q2m(im)
       real(kind_phys), intent(inout) :: t02max(im)
       real(kind_phys), intent(inout) :: t02min(im)
       real(kind_phys), intent(inout) :: rh02max(im)
       real(kind_phys), intent(inout) :: rh02min(im)
       character(len=*), intent(out)  :: errmsg
       integer, intent(out)           :: errflg

       ! Local variables
       real(kind_phys), dimension(:), allocatable :: refd, refd263k, mesh
       real(kind_phys), dimension(:), allocatable :: hmflux01, hske01, shcp01, hm01
       real(kind_phys), dimension(:), allocatable :: hmflux03, hske03, shcp03, hm03
       real(kind_phys) :: tem, pshltr, QCQ, rh02
       integer :: kdtminus1, i

       ! Initialize CCPP error handling variables
       errmsg = ''
       errflg = 0

       kdtminus1 = kdt-1

!Calculate hourly max 1-km agl and -10C reflectivity
       if (lradar .and. (imp_physics == imp_physics_gfdl .or. imp_physics == imp_physics_thompson &
                    .or. imp_physics == imp_physics_mg .or. imp_physics == imp_physics_nssl)) then
          allocate(refd(im))
          allocate(refd263k(im))

          allocate(mesh(im))
          allocate(hmflux01(im))
          allocate(hske01(im))
          allocate(shcp01(im))
          allocate(hm01(im))
          allocate(hmflux03(im))
          allocate(hske03(im))
          allocate(shcp03(im))
          allocate(hm03(im))

          call max_fields(phil,refl_10cm,con_g,im,levs,refd,gt0,refd263k)
          call hail_diagnostics(im, levs, imp_physics, imp_physics_thompson, imp_physics_nssl, imp_physics_mg,  &
                                gt0, prsl, gqgl, gqhl, gngl, gnhl, vhl, phil / con_g, refl_10cm, con_g, con_rd, &
                                mesh, hmflux01, hske01, shcp01, hm01, hmflux03, hske03, shcp03, hm03)

          if(mod(kdtminus1,nsteps_per_reset)==0)then
             do i=1,im
               refdmax(i) = -35.
               refdmax263k(i) = -35.

               meshmax(i) = 0.
               hmflux01max(i) = 0.
               hske01max(i) = 0.
               shcp01max(i) = 0.
               hm01max(i) = 0.
               hmflux03max(i) = 0.
               hske03max(i) = 0.
               shcp03max(i) = 0.
               hm03max(i) = 0.
             enddo
          endif
          do i=1,im
             !if(mod(kdtminus1,nsteps_per_reset)==0)then
             !  refdmax(I) = -35.
             !  refdmax263k(I) = -35.
             !endif
             refdmax(i) = max(refdmax(i),refd(i))
             refdmax263k(i) = max(refdmax263k(i),refd263k(i))

             meshmax(i) = max(meshmax(i),mesh(i))
             hmflux01max(i) = max(hmflux01max(i),hmflux01(i))
             hske01max(i) = max(hske01max(i),hske01(i))
             shcp01max(i) = max(shcp01max(i),shcp01(i))
             hm01max(i) = max(hm01max(i),hm01(i))
             hmflux03max(i) = max(hmflux03max(i),hmflux03(i))
             hske03max(i) = max(hske03max(i),hske03(i))
             shcp03max(i) = max(shcp03max(i),shcp03(i))
             hm03max(i) = max(hm03max(i),hm03(i))
          enddo

          deallocate (refd) 
          deallocate (refd263k)
          deallocate (mesh)
          deallocate (hmflux01)
          deallocate (hske01)
          deallocate (shcp01)
          deallocate (hm01)
          deallocate (hmflux03)
          deallocate (hske03)
          deallocate (shcp03)
          deallocate (hm03)
       endif
!
       if(mod(kdtminus1,nsteps_per_reset)==0)then
          do i=1,im
             spd10max(i) = -999.
             u10max(i)   = -999.
             v10max(i)   = -999.
             t02max(i)   = -999.
             t02min(i)   = 999.
             rh02max(i)  = -999.
             rh02min(i)  = 999.
          enddo
       endif
       do i=1,im
! find max hourly wind speed then decompose
          tem = sqrt(u10m(i)*u10m(i) + v10m(i)*v10m(i))
          !if(mod(kdtminus1,nsteps_per_reset)==0)then
          !   spd10max(i) = -999.
          !   u10max(i)   = -999.
          !   v10max(i)   = -999.
          !   t02max(i)   = -999.
          !   t02min(i)   = 999.
          !   rh02max(i)  = -999.
          !   rh02min(i)  = 999.
          !endif
          if (tem > spd10max(i)) then
             spd10max(i) = tem
             u10max(i)   = u10m(i)
             v10max(i)   = v10m(i)
          endif
          pshltr=pgr(i)*exp(-0.068283/gt0(i,1))
          QCQ=PQ0/pshltr*EXP(A2A*(t2m(i)-A3)/(t2m(i)-A4))
          rh02=q2m(i)/QCQ
          IF (rh02.GT.1.0) THEN
             rh02=1.0
          ENDIF
          IF (rh02.LT.RHmin) THEN !use smaller RH limit for stratosphere
             rh02=RHmin
          ENDIF
          rh02max(i)=max(rh02max(i),rh02)
          rh02min(i)=min(rh02min(i),rh02)
          t02max(i)=max(t02max(i),t2m(i))  !<--- hourly max 2m t
          t02min(i)=min(t02min(i),t2m(i))  !<--- hourly min 2m t
       enddo

   end subroutine maximum_hourly_diagnostics_run

   subroutine max_fields(phil,ref3D,grav,im,levs,refd,tk,refd263k)
      integer, intent(in)               :: im,levs
      real (kind=kind_phys), intent(in) :: grav
      real (kind=kind_phys), intent(in),dimension(im,levs)  :: phil,ref3D,tk
      integer               :: i,k,ll,ipt,kpt
      real :: dbz1avg,zmidp1,zmidloc,refl,fact
      real, dimension(im,levs) :: z
      real, dimension(im) :: zintsfc
      real, dimension(im), intent(inout) :: refd,refd263k
      REAL :: dbz1(2),dbzk,dbzk1
      logical :: counter
      do i=1,im
         do k=1,levs
            z(i,k)=phil(i,k)/grav
         enddo
      enddo
      do i=1,im
         refd(I) = -35.
         vloop:  do k=1,levs-1
            if ( (z(i,k+1)) .ge. 1000.     &
             .and.(z(i,k))   .le. 1000.)  then
               zmidp1=z(i,k+1)
               zmidLOC=z(i,k)
               dbz1(1)=ref3d(i,k+1)   !- dBZ (not Z) values
               dbz1(2)=ref3d(i,k) !- dBZ values
               exit vloop
            endif
         enddo vloop

!!! Initial curefl value without reduction above freezing level
!
!         curefl=0.
!         if (cprate(i,j)>0.) then
!           cuprate=rdtphs*cprate(i,j)
!           curefl=cu_a*cuprate**cu_b
!         endif
         do ll=1,2
           refl=0.
           if (dbz1(ll)>-35.) refl=10.**(0.1*dbz1(ll))
!           dbz1(l)=curefl+refl    !- in Z units
             dbz1(ll)=refl
         enddo
!-- Vertical interpolation of Z (units of mm**6/m**3)
         fact=(1000.-zmidloc)/(zmidloc-zmidp1)
         dbz1avg=dbz1(2)+(dbz1(2)-dbz1(1))*fact
!-- Convert to dBZ (10*logZ) as the last step
         if (dbz1avg>0.01) then
           dbz1avg=10.*alog10(dbz1avg)
         else
           dbz1avg=-35.
         endif
         refd(I)=max(refd(I),dbz1avg)
      enddo

!-- refl at -10C
      do i=1,im
         dbz1(1) = -35.
         dbz1(2) = -35.
         vloopm10:  do k=1,levs-1
            if (tk(i,k+1) .le. 263.15 .and. tk(i,k) .ge. 263.15)  then
               dbz1(1)=ref3d(i,k+1)   !- dBZ (not Z) values
               dbz1(2)=ref3d(i,k) !- dBZ values
               exit vloopm10
            endif
         enddo vloopm10

         do ll=1,2
           refl=0.
           if (dbz1(ll)>-35.) refl=10.**(0.1*dbz1(ll))
!           dbz1(l)=curefl+refl    !- in Z units
             dbz1(ll)=refl
         enddo
!-- Take max of bounding reflectivity values 
         dbz1avg=maxval(dbz1)
!-- Convert to dBZ (10*logZ) as the last step
         if (dbz1avg>0.01) then
           dbz1avg=10.*alog10(dbz1avg)
         else
           dbz1avg=-35.
         endif
         refd263K(I)=dbz1avg
      enddo
   end subroutine max_fields

   subroutine hail_diagnostics(im, levs, imp_physics, imp_physics_thompson, imp_physics_nssl, imp_physics_mg, &
                               temperature, pressure, qg, qh, ng, nh, vh, zh, refl_10cm, g, r_d, mesh,  &
                               hmflux01, hske01, shcp01, hm01, hmflux03, hske03, shcp03, hm03)

     use module_mp_radar, only : xam_g, xbm_g, xmu_g
     use micro_mg_utils, only : rhog_mg => rhog

     integer, intent(in) :: im, levs, imp_physics, imp_physics_thompson, imp_physics_nssl, imp_physics_mg
     real(kind=kind_phys), intent(in) :: g, r_d
     real(kind=kind_phys), dimension(im, levs), intent(in) :: temperature, pressure, zh, refl_10cm
     real(kind=kind_phys), dimension(im, levs), intent(in) :: qg, qh, ng, nh, vh
     real(kind=kind_phys), dimension(im), intent(out) :: mesh
     real(kind=kind_phys), dimension(im), intent(out) :: hmflux01, hske01, shcp01, hm01
     real(kind=kind_phys), dimension(im), intent(out) :: hmflux03, hske03, shcp03, hm03

     real :: temp_hmflux, temp_hske, temp_shcp, temp_hm, mmdiam, fallspd
     real :: xrho_g, rhoa, mh, dz, temp_qg, temp_ng

     ! Temporary variables for Thompson ng calculation
     real :: zans1, N0exp, lam_exp, lamg, N0_g, cge(3), cgg(3)

     integer :: i, k, n
     logical :: output_01_hail

     call calc_mesh(im, levs, zh, temperature, refl_10cm, mesh)

     hmflux01 = 0.0
     hske01 = 0.0
     shcp01 = 0.0
     hm01 = 0.0
     hmflux03 = 0.0
     hske03 = 0.0
     shcp03 = 0.0
     hm03 = 0.0

     if (imp_physics == imp_physics_thompson) then
!      xmu_g = 1.
       cge(1) = xbm_g + 1.
       cge(2) = xmu_g + 1.
       cge(3) = xbm_g + xmu_g + 1.
       do n = 1, 3
          cgg(n) = WGAMMA(cge(n))
       enddo
     endif

     do i=1,im
       temp_hmflux = 0
       temp_hske = 0
       temp_shcp = 0
       temp_hm = 0

       output_01_hail = .true.

       do k=1,levs
         if (zh(i,k) - zh(i,1) > 3000) then
           exit
         endif

         if (zh(i,k) - zh(i,1) > 1000 .and. output_01_hail) then
           hmflux01(i) = temp_hmflux
           hske01(i) = temp_hske
           shcp01(i) = temp_shcp
           hm01(i) = temp_hm

           output_01_hail = .false.
         endif

         rhoa = pressure(i,k) / (temperature(i,k) * r_d)

         if (imp_physics == imp_physics_thompson) then
           temp_qg = qg(i,k)
           mh = temp_qg * rhoa

           if (temp_qg < 1e-6) cycle

           xrho_g = 500.

           zans1 = (2.5 + 2./7. * (alog10(mh)+7.))
           zans1 = max(2., min(zans1, 7.))
           N0exp = 10.**zans1
           lam_exp = (N0exp*xam_g*cgg(1)/mh)**(1./cge(1))
           lamg = lam_exp * (cgg(3)/cgg(2)/cgg(1))**(1./xbm_g)
           N0_g = N0exp/(cgg(2)*lam_exp) * lamg**cge(2)
           temp_ng = N0_g*cgg(2)*lamg**(-cge(2))

         else if (imp_physics == imp_physics_nssl) then
           temp_qg = qh(i,k)
           temp_ng = nh(i,k)

           if (temp_qg < 1e-6) cycle

           xrho_g = qh(i,k) / vh(i,k)
         else if (imp_physics == imp_physics_mg) then
           temp_qg = qg(i,k)
           temp_ng = ng(i,k)

           if (temp_qg < 1e-6) cycle

           xrho_g = rhog_mg
         endif

         call mmdi(rhoa, temp_qg, temp_ng, xrho_g, mmdiam)
         call ferrier_fallspd(rhoa, temp_qg, temp_ng, 0.0, xrho_g, fallspd)

         mh = temp_qg * rhoa
         dz = zh(i,k+1) - zh(i,k)

         ! Compute parameters and integrate over a layer
         temp_hmflux = temp_hmflux + dz * mh * fallspd
         temp_hske = temp_hske + dz * mh * fallspd * fallspd * 0.5
         temp_shcp = temp_shcp + dz * mh * fallspd * mmdiam / 1000.0 * g
         temp_hm = temp_hm + dz * mh
       enddo

       hmflux03(i) = temp_hmflux
       hske03(i) = temp_hske
       shcp03(i) = temp_shcp
       hm03(i) = temp_hm
     enddo

   end subroutine hail_diagnostics

!+---+-----------------------------------------------------------------+ 
      REAL FUNCTION GAMMLN(XX)
!     --- RETURNS THE VALUE LN(GAMMA(XX)) FOR XX > 0.
      IMPLICIT NONE
      REAL, INTENT(IN):: XX
      DOUBLE PRECISION, PARAMETER:: STP = 2.5066282746310005D0
      DOUBLE PRECISION, DIMENSION(6), PARAMETER:: &
               COF = (/76.18009172947146D0, -86.50532032941677D0, &
                       24.01409824083091D0, -1.231739572450155D0, &
                      .1208650973866179D-2, -.5395239384953D-5/)
      DOUBLE PRECISION:: SER,TMP,X,Y
      INTEGER:: J

      X=XX
      Y=X
      TMP=X+5.5D0
      TMP=(X+0.5D0)*LOG(TMP)-TMP
      SER=1.000000000190015D0
      DO 11 J=1,6
        Y=Y+1.D0
        SER=SER+COF(J)/Y
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER/X)
      END FUNCTION GAMMLN
!  (C) Copr. 1986-92 Numerical Recipes Software 2.02
!+---+-----------------------------------------------------------------+ 
      REAL FUNCTION WGAMMA(y)

      IMPLICIT NONE
      REAL, INTENT(IN):: y

      WGAMMA = EXP(GAMMLN(y))

      END FUNCTION WGAMMA
!+---+-----------------------------------------------------------------+ 

  SUBROUTINE calc_mesh(im, levs, zh, temperature, refl, mesh)
 
  !
  !-----------------------------------------------------------------------
  !
  !  PURPOSE:
  !
  !  Compute Maximum Estimated Size of Hail (MESH) following Witt et al.
  !  (1998) 
  !
  !-----------------------------------------------------------------------
  !
  !  AUTHOR: N. Snook
  !  4/20/2017
  !  Ported to FV3 by T. Supinie
  !  4/23/2019
  !
  !-----------------------------------------------------------------------
  !
  !  INPUT:
  !    nx       Number of grid points in the x-direction (east/west)
  !    ny       Number of grid points in the y-direction (north/south)
  !    nz       Number of grid points in the vertical
  !    
  !    zps      Physical height (z-coord) of scalar points (m)
  !
  !    ptprt    Perturbation potential temperature (K)
  !    ptbar    Base-state potential temperature (K)
  !    pprt     Perturbation pressure (Pa)
  !    pbar     Base-state pressure (Pa)
  !
  !  OUTPUT:
  !    mesh     Maximum Estimated Size of Hail, in mm
  
  !-------------------------------------
  !  Variable Declarations
  !-------------------------------------

  IMPLICIT NONE

  !Inputs
  INTEGER, INTENT(IN) :: im, levs

  REAL(kind=kind_phys), INTENT(IN) :: zh(im, levs) ! Height

  REAL(kind=kind_phys), INTENT(IN) :: refl(im, levs) !Radar reflectivity (dBZ)
  REAL(kind=kind_phys), INTENT(IN) :: temperature(im, levs) !Air temperature (K)
  

  ! TAS: Could optimize somewhat by making these scalars, but maybe not worth it.
  REAL :: hgt_0(im), hgt_neg20(im) !Height of 0C and -20C layers (m)
  REAL :: wgt_refl(levs), wgt_temp(levs) !reflectivity and temperature weighting functions
  REAL :: hke(levs)   !hail kinetic energy (Witt et al. 1998)
  REAL :: shi       !severe hail index (Witt et al. 1998)
  REAL :: zps, last_zps

  REAL(kind=kind_phys), INTENT(OUT) :: mesh(im) !Maximum estimated size of hail (in mm)

  !Needed for logical and loop operations
  INTEGER :: ix, kz  !loop iterators for x, y, and z dimensions
  REAL :: colmax  !storage for maximum value in a column
  LOGICAL :: found_h0, found_hneg20  !flags for freezing level and -20C level

  !-------------------------------------
  !  Beginning of executable code
  !-------------------------------------

  !Calculation of MESH is done column by column
  DO ix=1, im
      !(Re)set freezing and -20C layer flags
      found_h0 = .FALSE.
      found_hneg20 = .FALSE.
  
      !If maximum Z in a column is less than 40 dBZ, MESH = 0
      colmax = 0.0
      DO kz=1, levs
          colmax = MAX(refl(ix, kz), colmax)
      END DO
      IF (colmax < 40.0) THEN
          mesh(ix) = 0.0
      !Otherwise, calculate MESH from temperature and reflectivity using weight functions
      ELSE
          DO kz=2, levs
              zps = zh(ix, kz)

              !Calculate height of freezing layer and -20C isotherm
              IF ((temperature(ix, kz) <= 273.15) .AND. (found_h0 .EQV. .FALSE.)) THEN
                  hgt_0(ix) = zps
                  found_h0 = .TRUE.
              END IF
              IF ((temperature(ix, kz) <= 253.15) .AND. (found_hneg20 .EQV. .FALSE.)) THEN
                  hgt_neg20(ix) = zps
                  found_hneg20 = .TRUE.
              END IF
          END DO

          shi = 0.0
          last_zps = zh(ix, 1)

          DO kz=2, levs

              !Calculate reflectivity weighting
              IF (refl(ix, kz) < 40.0) THEN
                  wgt_refl(kz) = 0.0
              ELSE IF (refl(ix, kz) > 50.0) THEN
                  wgt_refl(kz) = 1.0
              ELSE
                  wgt_refl(kz) = (refl(ix, kz) - 40.0) / 10.0
              END IF
            
              !Calculate HKE
              IF (refl(ix, kz) < 40.0) THEN
                  hke(kz) = 0.0
              ELSE
                  hke(kz) = 0.000005 * (10.0 ** (0.084 * refl(ix, kz))) * wgt_refl(kz)
              END IF

              zps = zh(ix, kz)

              !Calculate temperature weighting function wgt_temp
              IF (zps < hgt_0(ix)) THEN
                  wgt_temp(kz) = 0.0
              ELSE IF (zps >= hgt_neg20(ix)) THEN
                  wgt_temp(kz) = 1.0
              ELSE
                  wgt_temp(kz) = (zps - hgt_0(ix)) /                      &
                                 (hgt_neg20(ix) - hgt_0(ix))
              END IF
              !Calculate severe hail index (shi) contribution for this level
              shi = shi + 0.1 * (wgt_temp(kz) * hke(kz) * (zps - last_zps))

              last_zps = zps
          END DO

          !Calculate MESH (units are mm) using Witt. et al. (1998) formula:
          IF (shi <= 0.0) THEN
              mesh(ix) = 0.0
          ELSE
              mesh(ix) = 2.54 * (shi ** 0.5)
          END IF
      END IF
  END DO

  END SUBROUTINE calc_mesh

  SUBROUTINE mmdi(rho_air, qx, nx, rho_x, mm_diam)
    !Calculates mean-mass diameter from pressure, potential temperature, and microphysical fields 
    !Required arguments --  rho_air (air density)
    !                       qx (mixing ratio of the hydrometeor species (e.g. qh))
    !                       nx (number concentration of the hydrometeor species (e.g. nh))
    !                       rho_x (density of the hydrometeor species (rho_x = 913.0 for hail))

    !Returns -- mmdi (mean mass diameter in mm) 

    IMPLICIT NONE

    REAL, INTENT(IN) :: rho_air, qx, nx, rho_x
    REAL, INTENT(OUT) :: mm_diam

    REAL :: mx, cx

    IF (nx < 1e-6) THEN
      mm_diam = 0
      RETURN
    END IF

    mx = qx * rho_air 
    cx = (3.1415926536 / 6.) * rho_x

    mm_diam = (mx / (cx * nx)) ** (1. / 3.)
    mm_diam = mm_diam * 1000

  END SUBROUTINE mmdi


  SUBROUTINE ferrier_fallspd(rho_air, qx, nx, alpha, rho_x, fallspd)
    ! *** Module ferrier_fallspeed *** !
    !Calculates hydrometeor fallspeed following Ferrier (1994).
    !Author: Jon Labriola
    !Date Created: 15 Dec. 2017

    !Modification History:
    !  Nate Snook, 17 Dec. 2017:
    !     Incorporated code into ns_modules.py, added documentation and defaults 
    !     suitable for hail, folded cx calculation into module so that only hydrometeor
    !     density must be provided in function call, added line to remove NaN values.
    !  Tim Supinie, 15 Jan. 2017:
    !     Ported from Python to WRF

    !Optional arguments --  rho_x (density of the hydrometeor species in kg/m^3; default is 913.0 for hail)
    !                       alpha (shape parameter of the gamma distribution; default 0.0 for MY2)

    !Required arguments --  rho_air (air density as a 3D field)
    !                       qx (mixing ratio of the hydrometeor species (e.g. qh, qg).)
    !                       nx (number concentration of the hydrometeor species (e.g. nh, ng).)

    !Returns -- fallspd (hydrometeor fallspeed; m/s) 

    !---------------------------!
    !References:
    !  Ferrier, S. B., 1994: A Double-Moment Multiple-Phase Four-Class Bulk Ice Scheme. Part I: Description. 
    !      J. Atmos. Sci., 51, 249-280
    !---------------------------!

    IMPLICIT NONE

    REAL, INTENT(IN) :: rho_air, qx, nx, alpha, rho_x
    REAL, INTENT(OUT) :: fallspd

    REAL :: cx, GX5, GX2, gammfact, iLAMx, iLAMxB, ckQx1, tmpdp1
    REAL, PARAMETER :: afh = 206.890, bfh = 0.6384, dmh = 3.0

    IF (nx < 1e-6) THEN
      fallspd = 0
      RETURN
    END IF

    cx = (3.1415926536 / 6.0) * rho_x  !Density-related constant cx from Ferrier (1994).

    GX5 = WGAMMA(1.+alpha+dmh)
    GX2 = 1./WGAMMA(1.+alpha)
    gammfact = (1.225/rho_air)**0.5  !Ferrier (1994) gamma factor "gammfact"

    ! Calculate stuff related to slope parameter lambda
    iLAMx  = ((qx*rho_air/nx)/(cx*GX5*GX2))**0.333333333
    iLAMxB = iLAMx**bfh

    ckQx1 = afh*WGAMMA(1.+alpha+dmh+bfh)/GX5
    tmpdp1 = gammfact*iLAMxB

    fallspd = tmpdp1*ckQx1
    IF (fallspd > 50.) THEN
      fallspd = 50.
    END IF

  END SUBROUTINE ferrier_fallspd

  SUBROUTINE calc_dmax(lamda, n0, alpha, mu, dmax)
    !
    !-----------------------------------------------------------------------
    !  PURPOSE:  Calculates the maximum particle size based off PSD
    !-----------------------------------------------------------------------
    !
    !  AUTHOR: Jon Labriola
    !  (06/09/2017)
    !
    !  MODIFICATION HISTORY:
    !  Tim Supinie (1-17-2018)
    !     Ported to WRF
    !-----------------------------------------------------------------------
    !  Variable Declarations:
    !-----------------------------------------------------------------------

    IMPLICIT NONE

    REAL, INTENT(IN) :: lamda, n0, alpha, mu
    REAL, INTENT(OUT) :: dmax

    REAL :: bin, conc, conc_new
    REAL :: min_size = 0.001   ! smallest hail size (m)
    REAL :: increment = 0.001  ! The size bin (m) 
    REAL :: min_conc = 0.0001  ! m^-3 mm^-1

    bin = min_size

    IF (alpha > 1e-12) THEN
      conc = 0.d0
      conc_new = 1.d0

      ! For non-zero shape parameters, find the maximum in the PSD and start the search after that
      DO WHILE (conc_new > conc)
        bin = bin + increment
        conc_new = N0 * (bin**alpha) * EXP(-lamda*(bin**(3.0 * mu)))
        conc = N0 * ((bin - increment)**alpha) * EXP(-lamda * ((bin - increment)**(3.0 * mu)))
      END DO
    ELSE
      conc = N0 * (bin**alpha) * EXP(-lamda * (bin**(3.0 * mu)))
    ENDIF

    bin = bin - increment

    ! Now do the actual PSD search
    DO WHILE(conc > min_conc)
       bin = bin + increment
       conc = N0 * (bin**alpha) * EXP(-lamda * (bin**(3.0 * mu)))
    END DO
    dmax = bin

  END SUBROUTINE calc_dmax

end module maximum_hourly_diagnostics
