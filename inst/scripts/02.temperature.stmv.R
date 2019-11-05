# 1. stmv interpolations assuming some seasonal pattern
# twostep:  ~ 160+ hrs


if (!exists("year.assessment")) {
  year.assessment=lubridate::year(Sys.Date())
  year.assessment=lubridate::year(Sys.Date()) - 1
}

scale_ram_required_main_process = 27 # GB twostep / fft
scale_ram_required_per_process  = 1.5 # twostep / fft /fields vario ..  (mostly 0.5 GB, but up to 5 GB) -- 20 hrs
scale_ncpus = min( parallel::detectCores(), floor( (ram_local()- scale_ram_required_main_process) / scale_ram_required_per_process ) )

interpolate_ram_required_main_process = 24 # GB twostep / fft
interpolate_ram_required_per_process  = 3  # 1 GB seems enough for twostep / fft /fields vario .. but make 2 in case
interpolate_ncpus = min( parallel::detectCores(), floor( (ram_local()- interpolate_ram_required_main_process) / interpolate_ram_required_per_process ) )

p = aegis.temperature::temperature_parameters(
  project_class="stmv",
  data_root = project.datadirectory( "aegis", "temperature" ),
  DATA = 'temperature.db( p=p, DS="stmv_inputs" )',
  spatial_domain = "canada.east", # default
  inputdata_spatial_discretization_planar_km = 1 / 20, # 1==p$pres; controls resolution of data prior to modelling (km .. ie 20 linear units smaller than the final discretization pres)
  additional.data=c("groundfish", "snowcrab", "USSurvey_NEFSC", "lobster"),
  yrs = 1950:year.assessment,
  aegis_dimensionality="space-year-season",
  stmv_global_modelengine = "none",  # only marginally useful
  stmv_local_modelengine = "twostep" ,
  stmv_local_modelformula_time = formula( paste(
    't',
    '~ s( yr, k=30, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts")  ',
    '+ s( yr, cos.w, sin.w, k=20, bs="ts") ',
    '+ s( log(z), k=3, bs="ts") + s( plon, k=3, bs="ts") + s( plat, k=3, bs="ts")  ',
    '+ s( log(z), plon, plat, k=20, bs="ts")  '
    ) ),
  stmv_twostep_time = "gam",
  stmv_twostep_space = "fft",  # everything else is too slow ...
  stmv_fft_filter="matern_tapered_modelled",  #  matern, krige (very slow), lowpass, lowpass_matern
  # stmv_lowpass_nu = 0.1,
  # stmv_lowpass_phi = stmv::matern_distance2phi( distance=0.25, nu=0.1, cor=0.5 ), # default p$res = 0.5;
  stmv_autocorrelation_fft_taper = 0.5,  # benchmark from which to taper
  stmv_autocorrelation_localrange=0.1,
  stmv_autocorrelation_basis_interpolation = c( 0.2, 0.1, 0.05, 0.01 ),
  stmv_variogram_method = "fft",
  stmv_filter_depth_m = 5, # the depth covariate is input as  depth (m)
  stmv_local_model_distanceweighted = TRUE,
  stmv_rsquared_threshold = 0.25, # lower threshold .. not used if twostep method
  stmv_distance_statsgrid = 5, # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
  stmv_distance_scale = c( 5, 10, 20, 30, 40 ), # km ... approx guess of 95% AC range
  stmv_nmin = 100, # min number of unique spatial data points req before attempting to model timeseries in a localized space .. control no error in local model
  stmv_nmax = 400, # no real upper bound.. just speed / RAM limits  .. can go up to 10 GB / core if too large
  stmv_tmin = round( 1.25 * (year.assessment-1950) ),  # min no of time slices for timeseries part
  stmv_force_complete_method = "linear",
  stmv_runmode = list(
    globalmodel = FALSE,
    # scale = rep("localhost", scale_ncpus),
    interpolate = list(
      cor_0.2 = rep("localhost", interpolate_ncpus),
      cor_0.1 = rep("localhost", interpolate_ncpus),
      cor_0.05 = rep("localhost", interpolate_ncpus ),
      cor_0.01 = rep("localhost", max(1, interpolate_ncpus-1) )
    ),  # ncpus for each runmode
    interpolate_force_complete = rep("localhost", max(1, interpolate_ncpus/2)),
    restart_load = FALSE,
    save_intermediate_results = FALSE,
    save_completed_data = TRUE # just a dummy variable with the correct name
  )  # ncpus for each runmode
)


    stmv( p=p)  #700 MB (main); 500 MB child .. 2 days for scaling and 2 days for interpolation


    if (debugging) {
      stmv( p=p, runmode=c("debug", "scale", "interpolate"), force_complete_solution=TRUE )
      stmv_db( p=p, DS="stmv.results" ) # save to disk for use outside stmv*, returning to user scale
      if (really.finished) {
        stmv_db( p=p, DS="cleanup.all" )
      }
    }


# quick view
  predictions = stmv_db( p=p, DS="stmv.prediction", ret="mean", yr=2000 )
  statistics  = stmv_db( p=p, DS="stmv.stats" )

  locations = bathymetry.db( spatial_domain=p$spatial_domain, DS="baseline") # these are the prediction locations

  # comparisons
  dev.new(); surface( as.image( Z=rowMeans(predictions), x=locations, nx=p$nplons, ny=p$nplats, na.rm=TRUE) )

  # stats
  (p$statsvars)
  dev.new(); levelplot( predictions[,1] ~ locations[,1] + locations[,2], aspect="iso" )
  dev.new(); levelplot( statistics[,match("nu", p$statsvars)]  ~ locations[,1] + locations[,2], aspect="iso" ) # nu
  dev.new(); levelplot( statistics[,match("sdTotal", p$statsvars)]  ~ locations[,1] + locations[,2], aspect="iso" ) #sd total
  dev.new(); levelplot( statistics[,match("localrange", p$statsvars)]  ~ locations[,1] + locations[,2], aspect="iso" ) #localrange


  # 2.  collect predictions from stmv and warp/break into sub-areas defined by
  #     p$spatial_domain_subareas = c( "SSE", "SSE.mpa", "snowcrab" )
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
  p = spatial_parameters( p=p, spatial_domain = "SSE.mpa" )  # default grid and resolution
  p$corners = data.frame(plon=c(150, 1022), plat=c(4600, 5320) )  # alter corners ..
  temperature.map( p=p, DS='climatology' )
  temperature.map( p=p, DS='stmv.stats' )
  temperature.map( p=p, DS='annual' )
  temperature.map( p=p, DS='seasonal' )


# finished
