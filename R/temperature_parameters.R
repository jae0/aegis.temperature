
temperature_parameters = function( p=list(), project_name="temperature", project_class="core", ...) {

  p = parameters_add(p, list(...)) # add passed args to parameter list, priority to args

  # ---------------------

  # create/update library list
  p$libs = unique( c( p$libs, RLibrary ( "colorspace", "lubridate",  "lattice",
    "parallel", "sf", "GADMTools", "INLA" ) ) )

  p$libs = unique( c( p$libs, project.library ( "aegis", "aegis.bathymetry", "aegis.coastline",
    "aegis.polygons", "aegis.substrate", "aegis.temperature", "aegis.survey" ) ) )


  p = parameters_add_without_overwriting( p, project_name = project_name )
  p = parameters_add_without_overwriting( p, data_root = project.datadirectory( "aegis", p$project_name ) )
  p = parameters_add_without_overwriting( p, datadir  = file.path( p$data_root, "data" ) )
  p = parameters_add_without_overwriting( p, modeldir = file.path( p$data_root, "modelled" ) )

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
  yrs_default = 1950:lubridate::year(lubridate::now())
  p = parameters_add_without_overwriting( p, yrs = yrs_default )  # default unless already provided
  p = temporal_parameters(p=p)

  p = parameters_add_without_overwriting( p,
    additional.data=c("groundfish", "snowcrab", "USSurvey_NEFSC", "lobster"),
    inputdata_spatial_discretization_planar_km = p$pres / 4, # controls resolution of data prior to modelling (km .. ie 100 linear units smaller than the final discretization pres)
    inputdata_temporal_discretization_yr = 1/52,  # ie., weekly .. controls resolution of data prior to modelling to reduce data set and speed up modelling;; use 1/12 -- monthly or even 1/4.. if data density is low
    dyear_discretization_rawdata = c( {c(1:365)-1}/365, 1)
  )
  # dyear_discretization_rawdata :: intervals of decimal years... fractional year breaks finer than the default 10 units (taking daily for now..) .. need to close right side for "cut" .. controls resolution of data prior to modelling


  # ---------------------

  if (project_class=="core") {
    p$project_class = "core"
    return(p)
  }

  # ---------------------

  if (project_class %in% c("carstm") )  {
  # simple run of carstm. There are two types:
    #   one global, run directly from  polygons defined in aegis.bathymetry/inst/scripts/99.bathymetry.carstm.R
    #   and one that is called secondarily specific to a local project's polygons (eg. snow crab)
    p$libs = c( p$libs, project.library ( "carstm", "INLA"  ) )
    p$project_class = "carstm"

    # defaults in case not provided ...
    p = parameters_add_without_overwriting( p,
      areal_units_type = "lattice", # "stmv_fields" to use ageis fields instead of carstm fields ... note variables are not the same
      areal_units_resolution_km = 5, # default in case not provided ... 25 km dim of lattice ~ 1 hr; 5km = 79hrs; 2km = ?? hrs
      areal_units_proj4string_planar_km =  p$aegis_proj4string_planar_km,  # coord system to use for areal estimation and gridding for carstm
      # areal_units_proj4string_planar_km = projection_proj4string("omerc_nova_scotia")  # coord system to use for areal estimation and gridding for carstm
      areal_units_overlay = "none",
      carstm_modelengine = "inla",  # {model engine}.{label to use to store}
      carstm_model_label = "default",
      carstm_inputs_aggregated = TRUE
    )

    if ( !exists("carstm_inputadata_model_source", p))  p$carstm_inputadata_model_source = list()
    if ( !exists("bathymetry", p$carstm_inputadata_model_source ))  p$carstm_inputadata_model_source$bathymetry = "stmv"  # "stmv", "hybrid", "carstm"

    if ( !exists("carstm_model_call", p)  ) {
      if ( grepl("inla", p$carstm_modelengine) ) {
        p$carstm_model_call = paste(
         'inla(
            formula = ', p$variabletomodel, ' ~ 1',
              '+ f( dyri, model="ar1", hyper=H$ar1 )',
              '+ f( inla.group( z, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2)',
              '+ f( auid, model="bym2", graph=slot(sppoly, "nb"), group=year_factor, scale.model=TRUE, constr=TRUE, hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)),',
            'family = "normal",',
            'data= M,',
            'control.compute=list(dic=TRUE, waic=TRUE, config=TRUE),',
            'control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),',
            'control.predictor=list(compute=FALSE, link=1 ),',
            'control.fixed=H$fixed,',
            'verbose=TRUE
          ) ' )
      }
    }

    p = carstm_parameters( p=p )  # fill in anything missing with defaults and do some checks

    if ( p$inputdata_spatial_discretization_planar_km >= p$areal_units_resolution_km ) {
      warning( "p$inputdata_spatial_discretization_planar_km >= p$areal_units_resolution_km " )
    }
    message ("p$areal_units_resolution_km: ", p$areal_units_resolution_km)

    return(p)
  }


  # ---------------------

  if (project_class %in% c("stmv", "default") ) {

    p$libs = c( p$libs, project.library ( "stmv" ) )
    p$project_class = "stmv"

    p = parameters_add_without_overwriting( p,
      DATA = 'temperature_db( p=p, DS="stmv_inputs" )',
      stmv_model_label="default",
      stmv_variables = list(
        Y="t",
        LOCS=c("plon", "plat"),
        TIME="tiyr",
        COV="z"
      ),  # required as fft has no formulae
      stmv_global_modelengine = "none",  # only marginally useful .. consider removing it and use "none",
      stmv_local_modelengine = "twostep" ,
      stmv_local_modelformula_time = formula( paste(
        't',
        '~ s( yr, k=', floor(nyrs*0.3), ', bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts")  ',
        '+ s( yr, cos.w, sin.w, k=', floor(nyrs*0.3), ', bs="ts") ',
        '+ s( log(z), k=3, bs="ts") + s( plon, k=3, bs="ts") + s( plat, k=3, bs="ts")  ',
        '+ s( log(z), plon, plat, k=6, bs="ts")  '
        ) ),
      stmv_twostep_time = "gam",
      stmv_twostep_space = "fft",  # everything else is too slow ...
      stmv_variogram_method = "fft",
      stmv_filter_depth_m = 10,  # need data above sea level to get coastline
      stmv_rsquared_threshold = 0.01, # lower threshold  .. ignore
      stmv_distance_statsgrid = 5, # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
      stmv_nmin = 120, # min number of data points req before attempting to model in a localized space
      stmv_nmax = 120*(nyrs/2), # no real upper bound.. just speed /RAM
      stmv_tmin = floor( nyrs * 1.5 ),
      stmv_force_complete_method = "fft"
      # stmv_force_complete_method = "linear_interp"
    )



    p = parameters_add_without_overwriting( p,
      stmv_distance_prediction_limits = p$stmv_distance_statsgrid * c( 1/2, 2 ), # range of permissible predictions km (i.e 1/2 stats grid to upper limit based upon data density)
      stmv_distance_scale = p$stmv_distance_statsgrid * c( 1/2, 1, 2, 3, 4, 8, 16), # km ... approx guesses of 95% AC range
      stmv_distance_interpolation = p$stmv_distance_statsgrid * c( 1, 2, 3, 4, 8 ),  # range of permissible predictions km (i.e 1/2 stats grid to upper limit) .. in this case 5, 10, 20
      stmv_distance_interpolate_predictions = p$stmv_distance_statsgrid * c( 1/2, 1, 2, 3, 4, 8) # finalizing preds using linear interpolation
    )


    p = parameters_add_without_overwriting( p,
      bstats = c("tmean", "tsd", "tmin", "tmax", "tamplitude", "degreedays" )
    )

    if (exists("stmv_local_modelengine", p)) {

      if (p$stmv_local_modelengine =="twostep") {
        # similar to GAM model but no spatial interpolation .. space is handled via simple FFT-based kriging
        # if (!exists("stmv_local_modelformula", p))  p$stmv_local_modelformula = formula( paste(
        #   p$variabletomodel, ' ~ s(yr, k=5, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
        #     '+ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") ',
        #     '+ s(log(z), plon, plat, cos.w, sin.w, yr, k=100, bs="ts")') )

        p = parameters_add_without_overwriting( p,
          stmv_twostep_time = "gam",
          stmv_twostep_space = "fft" #  matern, krige (very slow), lowpass, lowpass_matern
        )

        if (p$stmv_twostep_time == "gam")  {
          p = parameters_add_without_overwriting( p,
            stmv_local_modelformula_time = formula( paste(
              p$variabletomodel,  '~ s(yr, k=10, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts")  ',
              '+ s( log(z), k=3, bs="ts") + s( plon, k=3, bs="ts") + s( plat, k=3, bs="ts")  ',
              '+ s( cos.w, sin.w, yr, k=15, bs="ts") + s( log(z), plon, plat, k=15, bs="ts")  '
              ) )
          )
        }

        if (p$stmv_twostep_space == "gam")  {
          # very resource intensive ..
          p = parameters_add_without_overwriting( p,
            stmv_local_modelformula_space = formula( paste(
              p$variabletomodel, ' ~ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s( log(z), plon, plat, k=27, bs="ts")  ') )
          )
        }

        if (p$stmv_twostep_space == "fft" ) {
          nu = 0.5  # exponential smoothing
          ac_local = 0.1  # ac at which to designate "effective range"
          p = parameters_add_without_overwriting( p,
            stmv_fft_filter = "matern tapered lowpass modelled exhaustive_predictions",  # exhaustive_predictions required as data density is variable and low
            # options for fft method: also matern, krige (very slow), lowpass, lowpass_matern, stmv_variogram_resolve_time,
            #stmv_fft_filter = "matern tapered lowpass modelled fast_predictions", #  act as a low pass filter first before matern with taper
            stmv_autocorrelation_fft_taper = 0.75,  # benchmark from which to taper
            stmv_autocorrelation_localrange = ac_local,  # for output to stats
            stmv_autocorrelation_interpolation = c(0.3, 0.2, 0.1, 0.01),
            stmv_lowpass_nu = nu, # exp
            stmv_lowpass_phi = stmv::matern_distance2phi( distance=p$pres/2, nu=nu, cor=ac_local )
          )
        }
      }

      if (p$stmv_local_modelengine =="gam") {
        p = parameters_add_without_overwriting( p,
          stmv_gam_optimizer=c("outer", "bfgs"),
          stmv_local_modelformula = formula( paste(
            p$variabletomodel, ' ~ s(yr, k=25, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
              '+ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") ',
              '+ s(log(z), plon, plat, k=25, bs="ts") + s( cos.w, sin.w, yr, k=25, bs="ts")') )
            # more than 100 knots and it takes a very long time, 50 seems sufficient, given the large-scaled pattern outside of the prediction box
            # other possibilities:
            #     seasonal.basic = ' s(yr) + s(dyear, bs="cc") ',
            #     seasonal.smoothed = ' s(yr, dyear) + s(yr) + s(dyear, bs="cc")  ',
            #     seasonal.smoothed.depth.lonlat = ' s(yr, dyear) + s(yr, k=3) + s(dyear, bs="cc") +s(z) +s(plon) +s(plat) + s(plon, plat, by=yr), s(plon, plat, k=10, by=dyear ) ',
        )
      }


      if (p$stmv_local_modelengine == "fft"   ) {
        nu = 0.5  # exponential smoothing
        ac_local = 0.1  # ac at which to designate "effective range"
        p = parameters_add_without_overwriting( p,
          stmv_fft_filter = "matern tapered lowpass modelled exhaustive_predictions",  # exhaustive_predictions required as data density is variable and low
          # options for fft method: also matern, krige (very slow), lowpass, lowpass_matern, stmv_variogram_resolve_time,
          #stmv_fft_filter = "matern tapered lowpass modelled fast_predictions", #  act as a low pass filter first before matern with taper
          stmv_autocorrelation_fft_taper = 0.75,  # benchmark from which to taper
          stmv_autocorrelation_localrange = ac_local,  # for output to stats
          stmv_autocorrelation_interpolation = c(0.3, 0.2, 0.1, 0.01),
          stmv_lowpass_nu = nu, # exp
          stmv_lowpass_phi = stmv::matern_distance2phi( distance=p$pres/2, nu=nu, cor=ac_local )
        )
      }


      if (p$stmv_local_modelengine == "gaussianprocess2Dt") {
        message( "NOTE:: The gaussianprocess2Dt method is really slow .. " )
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


    # default to serial mode
    p = parameters_add_without_overwriting( p,
      stmv_runmode = list(
        globalmodel = FALSE,
        scale = rep("localhost", 1),
        interpolate_correlation_basis = list(
          cor_0.25 = rep("localhost", 1),
          cor_0.1  = rep("localhost", 1),
          cor_0.05 = rep("localhost", 1),
          cor_0.01 = rep("localhost", 1)
        ),
        interpolate_predictions = list(
          c1 = rep("localhost", 1),
          c2 = rep("localhost", 1),
          c3 = rep("localhost", 1),
          c4 = rep("localhost", 1),
          c5 = rep("localhost", 1),
          c6 = rep("localhost", 1),
          c7 = rep("localhost", 1)
        ),
        save_intermediate_results = TRUE,
        save_completed_data = TRUE # just a dummy variable with the correct name
      )
    )


    p = aegis_parameters( p=p, DS="stmv" )


    if ( p$inputdata_spatial_discretization_planar_km >= p$pres ) {
      warning( "p$inputdata_spatial_discretization_planar_km >= p$pres " )
    }
    message ("p$stmv_distance_statsgrid: ", p$stmv_distance_statsgrid)

    return(p)

  }


  # ---------------------


  if (project_class %in% c("hybrid") ) {

    p$project_class = "hybrid"

    p = parameters_add_without_overwriting( p,
      DATA = 'temperature_db( p=p, DS="stmv_inputs" )',
      stmv_model_label="default",
      stmv_variables = list(
        Y="t",
        LOCS=c("plon", "plat"),
        TIME="tiyr",
        COV="z"
      ),  # required as fft has no formulae
      stmv_global_modelengine = "none",  # only marginally useful .. consider removing it and use "none",
      stmv_local_modelengine="carstm",
      stmv_local_covariates_carstm = "",  # only model covariates
      stmv_local_all_carstm = "",  # ignoring au
      stmv_local_modelcall = paste(
        'inla(
          formula = t ~ 1
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
      stmv_filter_depth_m = 10,
      stmv_rsquared_threshold = 0.01, # lower threshold  .. ignore
      stmv_distance_statsgrid = 5, # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
      stmv_au_distance_reference = "none", # additional filters upon polygons relative to windowsize: "centroid", "inside_or_touches_boundary", completely_inside_boundary"
      stmv_au_buffer_links = 0, # number of additional neighbours to extend beyond initial solution
#      pres = 1  # this governs resolution of lattice predictions
      stmv_nmin = 90, # min number of data points req before attempting to model in a localized space
      stmv_nmax = 1000, # no real upper bound.. just speed /RAM
    )


    p = parameters_add_without_overwriting( p,
      stmv_distance_prediction_limits = p$stmv_distance_statsgrid * c( 1, 2 ), # range of permissible predictions km (i.e  stats grid to upper limit based upon data density)
      stmv_distance_interpolation = p$stmv_distance_statsgrid * c( 1/2, 1, 2 ),  # range of permissible predictions km (i.e 1/2 stats grid to upper limit) .. in this case 5, 10, 20
      stmv_distance_interpolate_predictions = p$stmv_distance_statsgrid * c( 1/2, 1, 2) # finalizing preds using linear interpolation
    )


    p = parameters_add_without_overwriting( p,
      stmv_runmode = list(
        carstm = rep("localhost", 1),
        globalmodel = FALSE,
        save_intermediate_results = TRUE,
        save_completed_data = TRUE
      )
    )

    p = aegis_parameters( p=p, DS="stmv" )  # get defaults

    if ( !exists("bstats", p) )  p$bstats = c("tmean", "tsd", "tmin", "tmax", "tamplitude", "degreedays" )


    if ( p$inputdata_spatial_discretization_planar_km >= p$pres ) {
      warning( "p$inputdata_spatial_discretization_planar_km >= p$pres " )
    }
    message ("p$stmv_distance_statsgrid: ", p$stmv_distance_statsgrid)

    return(p)

  }




}
