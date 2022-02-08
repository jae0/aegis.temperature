

  ### stmv/carstm hybrid:  slow
  
  ### generate a modelled surface using areal units placed on a lattice system (1km x 1 km grid)
  ### CARSTM-based, however, run within a range of influence of dimension defined by : p$stmv_distance_interpolation = 5
  ### i.e., the same size as the stats grid (this is 1/2 of the window so 5 km surround each stats node )

  p = aegis.temperature::temperature_parameters( project_class="hybrid" )  # "hybrid" uses the "best" interpolation method given data constraints

    # stmv_au_distance_reference="completely_inside_boundary",
    # stmv_au_buffer_links=1,
    # stmv_filter_depth_m = FALSE,  # need data above sea level to get coastline
    # stmv_distance_statsgrid = 1, # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
    # stmv_distance_prediction_limits =c( 5, 10 ), # range of permissible predictions km (i.e 1/2 stats grid to upper limit based upon data density)
    # stmv_nmin = 50,  # min number of data points req before attempting to model in a localized space
    # stmv_nmax = 600, # no real upper bound.. just speed /RAM


  ncores = ram_local( "ncores", ram_main=?, ram_process=? ) # in GB; 

  p$stmv_runmode$carstm = rep("localhost", ncores)


  if (redo_inouts) {
      temperature_db( p=p, DS="stmv_inputs_redo" )  # recreate fields for .. requires 60GB+
      temperature_db( p=p, DS="stmv_inputs_highres_redo" )  # recreate fields for .. requires 60GB+
  }

  stmv( p=p )  # This will take from 40-70 hrs, depending upon system
  # stmv_db( p=p, DS="cleanup.all" )
      
  # quick view
  predictions = stmv_db( p=p, DS="stmv.prediction", ret="mean" )
  statistics  = stmv_db( p=p, DS="stmv.stats" )
  # locations   = spatial_grid( p )
  locations =  bathymetry_db(p=bathymetry_parameters( spatial_domain=p$spatial_domain, project_class="stmv"  ), DS="baseline")
  # comparisons

  # comparison
  dev.new(); surface( as.image( Z=predictions, x=locations, nx=p$nplons, ny=p$nplats, na.rm=TRUE) )



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



  # end
