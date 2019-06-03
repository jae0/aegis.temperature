  # 1. stmv interpolations assuming some seasonal pattern
  # twostep:  ~ 160+ hrs


  if (!exists("year.assessment")) {
    year.assessment=lubridate::year(Sys.Date())
    year.assessment=lubridate::year(Sys.Date()) - 1
  }

  p = aegis.temperature::temperature_parameters( yrs=1950:year.assessment )  # these are default years

    scale_ram_required_main_process = 0.8 # GB twostep / fft
    scale_ram_required_per_process  = 1.25 # twostep / fft /fields vario ..  (mostly 0.5 GB, but up to 5 GB) -- 20 hrs
    scale_ncpus = min( parallel::detectCores(), floor( (ram_local()- scale_ram_required_main_process) / scale_ram_required_per_process ) )

    interpolate_ram_required_main_process = 24 # GB twostep / fft
    interpolate_ram_required_per_process  = 1.25 # 1 GB seems enough for twostep / fft /fields vario .. but make 2 in case
    interpolate_ncpus = min( parallel::detectCores(), floor( (ram_local()- interpolate_ram_required_main_process) / interpolate_ram_required_per_process ) )

    nyrs = year.assessment-1950

    p = aegis.temperature::temperature_parameters(
      data_root = project.datadirectory( "aegis", "temperature" ),
      spatial.domain = "canada.east", # default
      DATA = 'temperature.db( p=p, DS="stmv.inputs" )',
      additional.data=c("groundfish", "snowcrab", "USSurvey_NEFSC", "lobster"),
      pres_discretization_temperature = 1 / 20, # 1==p$pres; controls resolution of data prior to modelling (km .. ie 20 linear units smaller than the final discretization pres)
      yrs = 1950:year.assessment,
      stmv_dimensionality="space-year-season",
      stmv_global_modelengine = "none",
      stmv_global_modelformula = "none",  # only marginally useful .. consider removing it and use "none",
      stmv_global_family ="none",
      stmv_local_modelengine = "twostep" ,
      stmv_local_modelformula_time = formula( paste(
        't',
        '~ s( yr, k=20, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts")  ',
        '+ s( yr, cos.w, sin.w, k=30, bs="ts") ',
        '+ s( log(z), k=3, bs="ts") + s( plon, k=3, bs="ts") + s( plat, k=3, bs="ts")  ',
        '+ s( log(z), plon, plat, k=30, bs="ts")  '
       ) ),
      stmv_twostep_time = "gam",
      stmv_twostep_space = "fft",  # everything else is too slow ...
      stmv_fft_filter="lowpass_matern_tapered",  #  matern, krige (very slow), lowpass, lowpass_matern
      stmv_range_correlation_fft_taper = 0.5,  # in local smoothing convolutions occur of this correlation scale
      stmv_local_model_distanceweighted = TRUE,
      stmv_rsquared_threshold = 0, # lower threshold .. not used if twostep method
      stmv_distance_statsgrid = 5, # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
      stmv_distance_scale = c( 20, 25, 30, 35, 40, 45, 50 ), # km ... approx guess of 95% AC range
      stmv_distance_prediction_fraction = 4/5, # i.e. 4/5 * 5 = 4 km
      stmv_clusters = list( scale=rep("localhost", scale_ncpus), interpolate=rep("localhost", interpolate_ncpus) ),  # ncpus for each runmode
      stmv_nmin = 16*nyrs,  # ~ 1000 min number of data points req before attempting to model timeseries in a localized space .. control no error in local model
      stmv_nmax = 25*nyrs # no real upper bound.. just speed / RAM limits  .. can go up to 10 GB / core if too large
    )


    stmv( p=p, runmode=c("scale", "interpolate")  )  #700 MB (main); 500 MB child .. 2 days for scaling and 2 days for interpolation


    if (debugging) {
      stmv( p=p, runmode=c("debug", "scale", "interpolate"), force_complete_solution=TRUE )
      stmv_db( p=p, DS="stmv.results" ) # save to disk for use outside stmv*, returning to user scale
      if (really.finished) {
        stmv_db( p=p, DS="cleanup.all" )
      }
    }


  # 2.  collect predictions from stmv and warp/break into sub-areas defined by
  #     p$spatial.domain.subareas = c( "SSE", "SSE.mpa", "snowcrab" )
  temperature.db( p=p, DS="predictions.redo" ) # ~1hr
  temperature.db( p=p, DS="stmv.stats.redo" ) # warp to sub grids

  # 3. extract relevant statistics .. including climatologies all defined by p$yrs
  temperature.db( p=p, DS="bottom.statistics.annual.redo" )

  # 4. all time slices in array format
  temperature.db( p=p,  DS="spatial.annual.seasonal.redo" )

  # 5. time slice at prediction time of year
  temperature.db( p=p,  DS="timeslice.redo" )

  # 6. complete statistics and warp/regrid database ... ~ 2 min :: only for  default grid . TODO might as well do for each subregion/subgrid
  temperature.db( p=p, DS="complete.redo")


# 7. maps
  # run only on local cores ... file swapping seem to reduce efficiency
  # p = aegis.temperature::temperature_parameters( yrs=1950:year.assessment )


  temperature.map( p=p, DS="all", yr=p$yrs ) # default is to do all

  # just redo a couple maps for ResDoc in the  SSE domain
  #p$bstats = "tmean"
  p = spatial_parameters( p=p, spatial.domain = "SSE.mpa" )  # default grid and resolution
  p$corners = data.frame(plon=c(150, 1022), plat=c(4600, 5320) )  # alter corners ..
  temperature.map( p=p, DS='climatology' )
  temperature.map( p=p, DS='stmv.stats' )
  temperature.map( p=p, DS='annual' )
  temperature.map( p=p, DS='seasonal' )


# finished
