# 1. stmv interpolations assuming some seasonal pattern
# twostep:  ~ 1000+ hrs (40 days+)

year.assessment = 2020
year.start = year.assessment - 30
year.start = 1950

nyrs = year.assessment - year.start


p = aegis.temperature::temperature_parameters( project_class="stmv")

if (0) {
  p$stmv_nmin = 80  # min number of unit spatial locations req before attempting to model in a localized space .. control no error in local model
  p$stmv_nmax = 80*(nyrs/2) # no real upper bound.. just speed / RAM limits  .. can go up to 10 GB / core if too large
  p$stmv_tmin = floor( nyrs * 1.25 )
)



if (0) {
    # default is serial mode .. to enable multicore:
    scale_ncpus = ram_local( "ncores", ram_main=27, ram_process=1.5 ) # in GB; about 24 hr
    interpolate_ncpus = ram_local( "ncores", ram_main=27, ram_process=2.5 ) # nn hrs

    if (!exists("stmv_runmode", p) ) p$stmv_runmode = list()

    p$stmv_runmode$globalmodel = FALSE
    
    p$stmv_runmode$scale =list(
          c1 = rep("localhost", scale_ncpus),
          c2 = rep("localhost", scale_ncpus),
          c3 = rep("localhost", scale_ncpus),
          c4 = rep("localhost", scale_ncpus),
          c5 = rep("localhost", scale_ncpus),
          c6 = rep("localhost", scale_ncpus),
          c7 = rep("localhost", scale_ncpus)
        )

    p$stmv_runmode$interpolate_correlation_basis = list(
          cor_0.25 = rep("localhost", interpolate_ncpus),
          cor_0.1  = rep("localhost", interpolate_ncpus),
          cor_0.05 = rep("localhost", interpolate_ncpus),
          cor_0.01 = rep("localhost", interpolate_ncpus)
        )

    p$stmv_runmode$interpolate_predictions = list(
          c1 = rep("localhost", interpolate_ncpus),
          c2 = rep("localhost", interpolate_ncpus),
          c3 = rep("localhost", interpolate_ncpus),
          c4 = rep("localhost", interpolate_ncpus),
          c5 = rep("localhost", interpolate_ncpus),
          c6 = rep("localhost", interpolate_ncpus),
          c7 = rep("localhost", interpolate_ncpus)
        )

    p$stmv_runmode$save_intermediate_results = TRUE

    p$stmv_runmode$save_completed_data = TRUE # just a dummy variable with the correct name

    if(0)  {
      p$stmv_runmode$restart_load = TRUE
      p$restart_load = "interpolate_correlation_basis"
      p$stmv_runmode$scale = FALSE

    } 

}


stmv( p=p)  #700 MB (main); 500 MB child .. 2 days for scaling and 2 days for interpolation


    if (debugging) {
      stmv( p=p, runmode=c("debug", "scale", "interpolate_correlation_basis"), force_complete_solution=TRUE )
      stmv_db( p=p, DS="stmv.results" ) # save to disk for use outside stmv*, returning to user scale
      if (really.finished) {
        stmv_db( p=p, DS="cleanup.all" )
      }
    }


# quick view
  predictions = stmv_db( p=p, DS="stmv.prediction", ret="mean", yr=2000 )
  statistics  = stmv_db( p=p, DS="stmv.stats" )


# quick map
  locations =  bathymetry_db(p=bathymetry_parameters( spatial_domain=p$spatial_domain, project_class="stmv"  ), DS="baseline")
  # comparisons
  dev.new(); surface( as.image( Z=rowMeans(predictions), x=locations, nx=p$nplons, ny=p$nplats, na.rm=TRUE) )

  # stats
  # statsvars = c( "sdTotal", "rsquared", "ndata", "sdSpatial", "sdObs", "phi", "nu", "localrange" )
  statsvars = dimnames(statistics)[[2]]

  dev.new(); levelplot( predictions[,1] ~ locations[,1] + locations[,2], aspect="iso" )
  dev.new(); levelplot( statistics[,match("nu", statsvars)]  ~ locations[,1] + locations[,2], aspect="iso" ) # nu
  dev.new(); levelplot( statistics[,match("sdTotal", statsvars)]  ~ locations[,1] + locations[,2], aspect="iso" ) #sd total
  dev.new(); levelplot( statistics[,match("localrange", statsvars)]  ~ locations[,1] + locations[,2], aspect="iso" ) #localrange


  # 2.  collect predictions from stmv and warp/break into sub-areas defined by
  #     p$spatial_domain_subareas = c( "SSE", "SSE.mpa", "snowcrab" )
  temperature_db( p=p, DS="predictions.redo" ) # ~1hr
  temperature_db( p=p, DS="stmv.stats.redo" ) # warp to sub grids

  # 3. extract relevant statistics .. including climatologies all defined by p$yrs
  temperature_db( p=p, DS="bottom.statistics.annual.redo" )

  # 4. all time slices in array format
  temperature_db( p=p,  DS="spatial.annual.seasonal.redo" )

  # 5. time slice at prediction time of year
  temperature_db( p=p,  DS="timeslice.redo" )

  # 6. complete statistics and warp/regrid database ... ~ 2 min :: only for  default grid . TODO might as well do for each subregion/subgrid
  temperature_db( p=p, DS="complete.redo")


# 7. maps
  # run only on local cores ... file swapping seem to reduce efficiency
  # p = aegis.temperature::temperature_parameters( yrs=1950:year.assessment )


  temperature_map( p=p, DS="all", yr=p$yrs ) # default is to do all

  # just redo a couple maps for ResDoc in the  SSE domain
  #p$bstats = "tmean"
  p = spatial_parameters( p=p, spatial_domain = "SSE.mpa" )  # default grid and resolution
  p$corners = data.frame(plon=c(150, 1022), plat=c(4600, 5320) )  # alter corners ..
  temperature_map( p=p, DS='climatology' )
  temperature_map( p=p, DS='stmv.stats' )
  temperature_map( p=p, DS='annual' )
  temperature_map( p=p, DS='seasonal' )


# finished
