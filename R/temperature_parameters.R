

temperature_parameters = function( p=NULL, project_name=NULL, project_class="default", ... ) {

  # ---------------------
  # deal with additional passed parameters
  if ( is.null(p) ) p=list()
  p_add = list(...)
  if (length(p_add) > 0 ) p = c(p, p_add)
  i = which(duplicated(names(p), fromLast = TRUE ))
  if ( length(i) > 0 ) p = p[-i] # give any passed parameters a higher priority, overwriting pre-existing variable


  # ---------------------

  # create/update library list
  p$libs = unique( c( p$libs, RLibrary ( "colorspace",  "fields", "geosphere", "lubridate",  "lattice",
    "maps", "mapdata", "maptools", "parallel",  "rgdal", "rgeos",  "sp", "splancs", "GADMTools" ) ) )
  p$libs = unique( c( p$libs, project.library ( "aegis", "aegis.bathymetry", "aegis.coastline", "aegis.polygons", "aegis.substrate", "aegis.temperature" ) ) )

  p$project_name = ifelse ( !is.null(project_name), project_name, "temperature" )

  if ( !exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project_name )
  if ( !exists("datadir", p) )   p$datadir  = file.path( p$data_root, "data" )
  if ( !exists("modeldir", p) )  p$modeldir = file.path( p$data_root, "modelled" )

  if ( !file.exists(p$datadir) ) dir.create( p$datadir, showWarnings=F, recursive=T )
  if ( !file.exists(p$modeldir) ) dir.create( p$modeldir, showWarnings=F, recursive=T )

  if ( !exists("variabletomodel", p) ) p$variabletomodel = "t"

  if ( !exists("spatial_domain", p) ) p$spatial_domain = "canada.east" # canada.east.highres and canada.east.superhighres result in memory overflow
  if ( !exists("spatial_domain_subareas", p) )  p$spatial_domain_subareas = c( "SSE.mpa", "SSE", "snowcrab" ) # target domains and resolution for additional data subsets .. add here your are of interest

  p = spatial_parameters( p=p )  # default grid and resolution

  # define focal years
  if (!exists( "yrs", p)) p$yrs = 1950:lubridate::year(lubridate::now())  # default

  p = temporal_parameters(p=p, aegis_dimensionality="space-year-season")

  if ( !exists("additional.data", p) )  p$additional.data=c("groundfish", "snowcrab", "USSurvey_NEFSC", "lobster")



  if (project_class=="default") {
    if ( !exists("inputdata_spatial_discretization_planar_km", p) )  p$inputdata_spatial_discretization_planar_km = p$pres / 4 # controls resolution of data prior to modelling (km .. ie 100 linear units smaller than the final discretization pres)
    if ( !exists("inputdata_temporal_discretization_yr", p) )  p$inputdata_temporal_discretization_yr = 1/52  # ie., weekly .. controls resolution of data prior to modelling to reduce data set and speed up modelling
    return(p)
  }

  if (project_class=="carstm")  {
    if ( !exists("inputdata_spatial_discretization_planar_km", p) )  p$inputdata_spatial_discretization_planar_km = p$pres/3  # =3x3=9 units possible per location .. km controls resolution of data prior to modelling to reduce data set and speed up modelling
    if ( !exists("inputdata_temporal_discretization_yr", p) )  p$inputdata_temporal_discretization_yr = 1/12  # ie., monthly .. controls resolution of data prior to modelling to reduce data set and speed up modelling
    return(p)
}


  if (project_class=="stmv") {
    p$libs = unique( c( p$libs, project.library ( "stmv" ) ) )

    if (!exists("DATA", p) ) p$DATA = 'temperature.db( p=p, DS="stmv_inputs" )'

    if (!exists("stmv_variables", p)) p$stmv_variables = list()
    if (!exists("LOCS", p$stmv_variables)) p$stmv_variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$stmv_variables)) p$stmv_variables$TIME="tiyr"
    if (!exists("Y", p$stmv_variables)) p$stmv_variables$Y="t"
    if (!exists("COV", p$stmv_variables)) p$stmv_variables$COV="z"

    # increase resolution from defaults as we can with stmv
    if ( !exists("inputdata_spatial_discretization_planar_km", p) )  p$inputdata_spatial_discretization_planar_km = p$pres / 4 # controls resolution of data prior to modelling (km .. ie 100 linear units smaller than the final discretization pres)
    if ( !exists("inputdata_temporal_discretization_yr", p) )  p$inputdata_temporal_discretization_yr = 1/52  # ie., weekly .. controls resolution of data prior to modelling to reduce data set and speed up modelling

    if ( !exists("bstats", p) )  p$bstats = c("tmean", "tsd", "tmin", "tmax", "tamplitude", "degreedays" )

    # global model options
    # using covariates as a first pass essentially makes it ~ kriging with external drift
    # marginally useful .. consider removing it.
    if (!exists("stmv_global_modelengine", p)) p$stmv_global_modelengine = "none"
    # if (!exists("stmv_global_modelformula", p)) p$stmv_global_modelformula = formula( t ~ s(z, bs="ts" + s(s.localrange, bs="ts") + s(dZ, bs="ts") + s(ddZ, bs="ts") + s(log(substrate.grainsize), bs="ts")  ) )
    if (!exists("stmv_global_family", p))  p$stmv_global_family = gaussian()

    # local model options
    if (!exists("stmv_local_modelengine", p)) p$stmv_local_modelengine = "gam" # gam is the most flexible
    if (!exists("stmv_local_model_distanceweighted", p)) p$stmv_local_model_distanceweighted = TRUE

    if (exists("stmv_local_modelengine", p)) {

      if (p$stmv_local_modelengine =="gam") {
        if (!exists("stmv_gam_optimizer", p)) p$stmv_gam_optimizer=c("outer", "bfgs")
        if (!exists("stmv_local_modelformula", p)) p$stmv_local_modelformula = formula( paste(
          p$variabletomodel, ' ~ s(yr, k=25, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
            '+ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") ',
            '+ s(log(z), plon, plat, k=25, bs="ts") + s( cos.w, sin.w, yr, k=25, bs="ts")') )
          # more than 100 knots and it takes a very long time, 50 seems sufficient, given the large-scaled pattern outside of the prediction box
          # other possibilities:
          #     seasonal.basic = ' s(yr) + s(dyear, bs="cc") ',
          #     seasonal.smoothed = ' s(yr, dyear) + s(yr) + s(dyear, bs="cc")  ',
          #     seasonal.smoothed.depth.lonlat = ' s(yr, dyear) + s(yr, k=3) + s(dyear, bs="cc") +s(z) +s(plon) +s(plat) + s(plon, plat, by=yr), s(plon, plat, k=10, by=dyear ) ',

      }

      if (p$stmv_local_modelengine =="fft") {
        # no need for formulas, etc
        # lowpass seems a bit too noisy
        # matern and lowpass_matern are over-smooth
        # p$stmv_fft_filter = "lowpass" # only act as a low pass filter .. depth has enough data for this. Otherwise, use:
        # p$stmv_fft_filter = "matern" # to ~ unviersal krige with external drift
        # for fft-based methods that require lowpass:
        if (!exists("stmv_lowpass_phi", p))  p$stmv_lowpass_phi = p$pres / 5 # FFT-baed methods cov range parameter .. not required for "matern" ..
        if (!exists("stmv_lowpass_nu", p))  p$stmv_lowpass_nu = 0.5
      }

      if (p$stmv_local_modelengine == "gaussianprocess2Dt") {
        message( "NOTE:: The gaussianprocess2Dt method is really slow .. " )
      }

      if (p$stmv_local_modelengine =="twostep") {
        # similar to GAM model but no spatial interpolation .. space is handled via simple FFT-based kriging
        # if (!exists("stmv_local_modelformula", p))  p$stmv_local_modelformula = formula( paste(
        #   p$variabletomodel, ' ~ s(yr, k=5, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
        #     '+ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") ',
        #     '+ s(log(z), plon, plat, cos.w, sin.w, yr, k=100, bs="ts")') )

        if (!exists("stmv_twostep_time", p)) p$stmv_twostep_time = "gam"
        if (!exists("stmv_twostep_space", p))  p$stmv_twostep_space = "fft" #  matern, krige (very slow), lowpass, lowpass_matern

        if (p$stmv_twostep_time == "gam")  {
          if (!exists("stmv_local_modelformula_time", p))  p$stmv_local_modelformula_time = formula( paste(
            p$variabletomodel,  '~ s(yr, k=10, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts")  ',
            '+ s( log(z), k=3, bs="ts") + s( plon, k=3, bs="ts") + s( plat, k=3, bs="ts")  ',
            '+ s( cos.w, sin.w, yr, k=15, bs="ts") + s( log(z), plon, plat, k=15, bs="ts")  '
            ) )
        }

        if (p$stmv_twostep_space == "gam")  {
          # very resource intensive ..
          if (!exists("stmv_local_modelformula_space", p))  p$stmv_local_modelformula_space = formula( paste(
          p$variabletomodel, ' ~ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s( log(z), plon, plat, k=27, bs="ts")  ') )
        }

        if (p$stmv_twostep_space == "fft" ) {
          if (!exists("stmv_fft_filter", p)) p$stmv_fft_filter="matern"  #  matern, krige (very slow), lowpass, lowpass_matern
          # for fft-based methods that require lowpass:
          if (!exists("stmv_lowpass_phi", p))  p$stmv_lowpass_phi = p$pres / 5 # FFT-baed methods cov range parameter .. not required for "matern" ..
          if (!exists("stmv_lowpass_nu", p))  p$stmv_lowpass_nu = 0.5
        }

      }

      if (p$stmv_local_modelengine == "bayesx") {
        # bayesx families are specified as characters, this forces it to pass as is and
        # then the next does the transformation internal to the "stmv__bayesx"
        if (!exists("stmv_local_modelformula", p))  p$stmv_local_modelformula = formula( paste(
          p$variabletomodel, ' ~ sx(yr,   bs="ps") + sx(cos.w, bs="ps") + s(sin.w, bs="ps")',
            ' + sx(plon, bs="ps") + sx(plat, bs="ps") ',
            ' + sx(plon, plat, cos.w, sin.w, yr, bs="te")'  # te is tensor spline
        ))
        if (!exists("stmv_local_model_bayesxmethod", p))  p$stmv_local_model_bayesxmethod="MCMC"
        if (!exists("stmv_local_model_distanceweighted", p))  p$stmv_local_model_distanceweighted = FALSE
      }
    }

    if (!exists("stmv_global_modelformula", p)) p$stmv_global_modelformula = "none"


    # intervals of decimal years... fractional year breaks finer than the default 10 units (taking daily for now..)
    #.. need to close right side for "cut" .. controls resolution of data prior to modelling
    if (!exists("dyear_discretization_rawdata", p)) p$dyear_discretization_rawdata = c( {c(1:365)-1}/365, 1)

    return(p)

  }



}
