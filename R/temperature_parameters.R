

temperature_parameters = function( p=list(), project_name="temperature", project_class="core", workflow_decentralized=FALSE, ...) {


  p = parameters_add(p, list(...)) # add passed args to parameter list, priority to args


  # ---------------------

  # create/update library list
  p$libs = unique( c( p$libs, RLibrary ( "colorspace",  "fields", "geosphere", "lubridate",  "lattice",
    "maps", "mapdata", "maptools", "parallel",  "rgdal", "rgeos",  "sp", "splancs", "GADMTools" ) ) )
  p$libs = unique( c( p$libs, project.library ( "aegis", "aegis.bathymetry", "aegis.coastline", "aegis.polygons", "aegis.substrate", "aegis.temperature", "aegis.survey" ) ) )


  p = parameters_add_without_overwriting( p, project_name = project_name )
  p = parameters_add_without_overwriting( p, data_root = project.datadirectory( "aegis", p$project_name ) )
  p = parameters_add_without_overwriting( p, datadir  = file.path( p$data_root, "data" ) )
  p = parameters_add_without_overwriting( p, modeldir = file.path( p$data_root, "modelled" ) )


  # for projects that require access to default data and local data, a switch is needed to force use of default data
  if ( p$workflow_decentralized )  {
    if (exists( "modeldir_override", p)) p$modeldir = p$modeldir_override  # must also specify p$workflow_decentralized =TRUE  for override to work
  }

  if ( !file.exists(p$datadir) ) dir.create( p$datadir, showWarnings=FALSE, recursive=TRUE )
  if ( !file.exists(p$modeldir) ) dir.create( p$modeldir, showWarnings=FALSE, recursive=TRUE )


  p = parameters_add_without_overwriting( p,
    variabletomodel = "t",
    spatial_domain = "canada.east",  # canada.east.highres and canada.east.superhighres result in memory overflow
    spatial_domain_subareas = c( "SSE", "SSE.mpa" , "snowcrab"),  # this is for bathymetry_db, not stmv
    aegis_dimensionality="space-year-season"
  )

  p = spatial_parameters( p=p )  # default grid and resolution

  # define focal years
  p = parameters_add_without_overwriting( p, yrs = 1950:lubridate::year(lubridate::now()) )  # default
  p = temporal_parameters(p=p)

  p = parameters_add_without_overwriting( p,
    additional.data=c("groundfish", "snowcrab", "USSurvey_NEFSC", "lobster"),
    inputdata_spatial_discretization_planar_km = p$pres / 4, # controls resolution of data prior to modelling (km .. ie 100 linear units smaller than the final discretization pres)
    inputdata_temporal_discretization_yr = 1/52  # ie., weekly .. controls resolution of data prior to modelling to reduce data set and speed up modelling;; use 1/12 -- monthly or even 1/4.. if data density is low
  )


  # ---------------------

  if (project_class=="core") return(p)

  # ---------------------

  if (project_class %in% c("carstm") )  {
  # simple run of carstm. There are two types:
    #   one global, run directly from  polygons defined in aegis.bathymetry/inst/scripts/99.bathymetry.carstm.R
    #   and one that is called secondarily specific to a local project's polygons (eg. snow crab)
    p$libs = c( p$libs, project.library ( "carstm", "INLA"  ) )

    # defaults in case not provided ...
    p = parameters_add_without_overwriting( p,
      areal_units_source = "lattice", # "stmv_fields" to use ageis fields instead of carstm fields ... note variables are not the same
      areal_units_resolution_km = 5, # default in case not provided ... 25 km dim of lattice ~ 1 hr; 5km = 79hrs; 2km = ?? hrs
      areal_units_proj4string_planar_km = aegis::projection_proj4string("utm20"),  # coord system to use for areal estimation and gridding for carstm
      # areal_units_proj4string_planar_km = projection_proj4string("omerc_nova_scotia")  # coord system to use for areal estimation and gridding for carstm
      areal_units_overlay = "none",
      carstm_modelengine = "inla",  # {model engine}.{label to use to store}
      carstm_model_label = "default",
      carstm_inputs_aggregated = FALSE
    )

    if ( !exists("carstm_model_call", p)  ) {
      if ( grepl("inla", p$carstm_modelengine) ) {
        p$carstm_model_call = paste(
         'inla(
            formula = ', p$variabletomodel, ' ~ 1
              + f( dyri, model="ar1", hyper=H$ar1 )
              + f( inla.group( z, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
              + f( auid, model="bym2", graph=slot(sppoly, "nb"), group=year_factor, scale.model=TRUE, constr=TRUE, hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)),
            family = "normal",
            data= M,
            control.compute=list(dic=TRUE, waic=TRUE, config=TRUE),
            control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
            control.predictor=list(compute=FALSE, link=1 ),
            control.fixed=H$fixed,  # priors for fixed effects, generic is ok
            verbose=TRUE
          ) ' )
      }
    }

    p = carstm_parameters( p=p )  # fill in anything missing with defaults and do some checks

    return(p)
  }


  # ---------------------

  if (project_class %in% c("stmv") ) {

    p = parameters_add_without_overwriting( p,
      project_class="stmv",
      DATA = 'temperature_db( p=p, DS="stmv_inputs" )',
      stmv_model_label="default",
      stmv_variables = list(
        Y="t",
        LOCS=c("plon", "plat"),
        TIME="tiyr",
        COV="z"
      ),  # required as fft has no formulae
      inputdata_spatial_discretization_planar_km = p$pres / 4, # controls resolution of data prior to modelling (km .. ie 100 linear units smaller than the final discretization pres)
      inputdata_temporal_discretization_yr = 1/52,  # ie., weekly .. controls resolution of data prior to modelling to reduce data set and speed up modelling
      stmv_global_modelengine = "none",  # only marginally useful .. consider removing it and use "none",
      stmv_local_modelengine="fft",
      stmv_local_model_distanceweighted = TRUE,
      stmv_fft_filter = "matern tapered lowpass modelled fast_predictions", #  act as a low pass filter first before matern with taper .. depth has enough data for this. Otherwise, use:
      stmv_lowpass_nu = 0.5, # exp
      stmv_lowpass_phi = stmv::matern_distance2phi( distance=0.1, nu=0.5, cor=0.1 ),
      stmv_autocorrelation_fft_taper = 0.9,  # benchmark from which to taper
      stmv_autocorrelation_localrange = 0.1,  # for output to stats
      stmv_autocorrelation_basis_interpolation = c(0.25, 0.1, 0.05, 0.01),
      stmv_variogram_method = "fft",
      stmv_filter_depth_m = FALSE,  # need data above sea level to get coastline
      stmv_Y_transform =list(
        transf = function(x) {log10(x + 2500)} ,
        invers = function(x) {10^(x) - 2500}
      ), # data range is from -1667 to 5467 m: make all positive valued
      stmv_rsquared_threshold = 0.01, # lower threshold  .. ignore
      stmv_distance_statsgrid = 5, # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
      stmv_distance_prediction_limits =c( 3, 25 ), # range of permissible predictions km (i.e 1/2 stats grid to upper limit based upon data density)
      stmv_distance_scale = c( 5, 10, 20, 25, 40, 80, 150, 200), # km ... approx guesses of 95% AC range
      stmv_distance_basis_interpolation = c(  2.5 , 5, 10, 15, 20, 40, 80, 150, 200 ) , # range of permissible predictions km (i.e 1/2 stats grid to upper limit) .. in this case 5, 10, 20
      stmv_nmin = 90, # min number of data points req before attempting to model in a localized space
      stmv_nmax = 1000, # no real upper bound.. just speed /RAM
      stmv_force_complete_method = "linear_interp"
    )

    p$libs = c( p$libs, project.library ( "stmv" ) )

    if ( !exists("bstats", p) )  p$bstats = c("tmean", "tsd", "tmin", "tmax", "tamplitude", "degreedays" )

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


  # ---------------------


  if (project_class %in% c("hybrid", "default" ) ) {
    p = parameters_add_without_overwriting( p,
      project_class="stmv",
      DATA = 'temperature_db( p=p, DS="stmv_inputs" )',
      stmv_model_label="default",
      stmv_variables = list(
        Y="t",
        LOCS=c("plon", "plat"),
        TIME="tiyr",
        COV="z"
      ),  # required as fft has no formulae
      inputdata_spatial_discretization_planar_km = p$pres / 4, # controls resolution of data prior to modelling (km .. ie 100 linear units smaller than the final discretization pres)
      inputdata_temporal_discretization_yr = 1/52,  # ie., weekly .. controls resolution of data prior to modelling to reduce data set and speed up modelling
      stmv_global_modelengine = "none",  # only marginally useful .. consider removing it and use "none",
      stmv_local_modelengine="carstm",
      stmv_local_covariates_carstm = "",  # only model covariates
      stmv_local_all_carstm = "",  # ignoring au
      stmv_local_modelcall = paste(
        'inla(
          formula = z ~ 1
            + f(auid, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, constr=TRUE, hyper=H$bym2),
          family = "normal",
          data= dat,
          control.compute=list(dic=TRUE, waic=TRUE, cpo=FALSE, config=FALSE),  # config=TRUE if doing posterior simulations
          control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
          control.predictor=list(compute=FALSE, link=1 ),
          control.fixed=H$fixed,  # priors for fixed effects, generic is ok
          verbose=FALSE
        ) '
      ),   # NOTE:: this is a local model call
      stmv_filter_depth_m = TRUE,
      stmv_local_model_distanceweighted = TRUE,
      stmv_rsquared_threshold = 0.01, # lower threshold  .. ignore
      stmv_distance_statsgrid = 5, # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
      stmv_distance_prediction_limits =c( 3, 25 ), # range of permissible predictions km (i.e 1/2 stats grid to upper limit based upon data density)
      stmv_distance_basis_interpolation = c(  2.5 , 5, 10, 15, 20, 40, 80, 150, 200 ) , # range of permissible predictions km (i.e 1/2 stats grid to upper limit) .. in this case 5, 10, 20
      stmv_nmin = 90, # min number of data points req before attempting to model in a localized space
      stmv_nmax = 1000, # no real upper bound.. just speed /RAM
      stmv_runmode = list(
        carstm = rep("localhost", 1),
        globalmodel = FALSE,
        save_intermediate_results = TRUE,
        save_completed_data = TRUE
      )  # ncpus for each runmode
    )

    p$libs = c( p$libs, project.library ( "stmv" ) )
    p = aegis_parameters( p=p, DS="stmv" )  # get defaults

    if ( !exists("bstats", p) )  p$bstats = c("tmean", "tsd", "tmin", "tmax", "tamplitude", "degreedays" )

    # intervals of decimal years... fractional year breaks finer than the default 10 units (taking daily for now..)
    #.. need to close right side for "cut" .. controls resolution of data prior to modelling
    if (!exists("dyear_discretization_rawdata", p)) p$dyear_discretization_rawdata = c( {c(1:365)-1}/365, 1)
    if ( p$inputdata_spatial_discretization_planar_km >= p$pres ) {
      warning( "p$inputdata_spatial_discretization_planar_km >= p$pres " )
    }
    message ("p$stmv_distance_statsgrid: ", p$stmv_distance_statsgrid)

    return(p)

  }




}
