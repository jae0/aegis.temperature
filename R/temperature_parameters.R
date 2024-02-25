
temperature_parameters = function( p=list(), project_name="temperature", project_class="core", ...) {

  p = parameters_add(p, list(...)) # add passed args to parameter list, priority to args

  # ---------------------

  # create/update library list
  p$libs = unique( c( p$libs, RLibrary ( "colorspace", "lubridate",  "lattice",
    "parallel", "sf",  "INLA" , "data.table") ) )

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
    dimensionality="space-time-cyclic"
  )

  p$quantile_bounds =c(0, 0.95) # trim upper bounds

  p = spatial_parameters( p=p )  # default grid and resolution

  # define focal years
  if (!exists("year.assessment", p )) {
    if (exists("yr", p)) {
      p$year.assessment = max(p$yr)
    } else if (exists("yrs", p)) {
      p$year.assessment = max(p$yrs)
    } else {
      message("probably want to assign current year.assessment, using current year for now")  
      p$year.assessment = lubridate::year(lubridate::now()) 
    }
  }

  yrs_default = 1950:p$year.assessment
  p = parameters_add_without_overwriting( p, yrs = yrs_default, timezone="America/Halifax" )  # default unless already provided
  p = temporal_parameters(p=p)

  p = parameters_add_without_overwriting( p,
    additional.data=c("groundfish", "snowcrab", "USSurvey_NEFSC", "lobster"),
    inputdata_spatial_discretization_planar_km = p$pres / 10, # controls resolution of data prior to modelling (km )
    inputdata_temporal_discretization_yr = 1/52,  # ie., weekly .. controls resolution of data prior to modelling to reduce data set and speed up modelling;; use 1/12 -- monthly or even 1/4.. if data density is low
    dyear_discretization_rawdata = c( {c(1:365)-1}/365, 1)
  )
  # dyear_discretization_rawdata :: intervals of decimal years... fractional year breaks finer than the default 10 units (taking daily for now..) .. need to close right side for "cut" .. controls resolution of data prior to modelling

  p$discretization = aegis.survey::discretizations(p=p$discretization)


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

    #special cases
    if (!exists("carstm_model_label", p)) p$carstm_model_label = "1970_present"
  
    if (!exists("yrs", p)){
      if (exists("carstm_model_label", p)) {
        if (p$carstm_model_label == "1999_present"){
            p$yrs = 1999:p$year.assessment
        } else if (p$carstm_model_label == "1970_present"){
            p$yrs = 1970:p$year.assessment
        } else if (p$carstm_model_label == "1950_present"){
            p$yrs = 1950:p$year.assessment
        }
      }
    }
    
    # resetss in case of changes above
    p$areal_units_timeperiod = p$carstm_model_label # needed?
    p$ny = length(p$yrs)
    p$nt = p$nw*p$ny # i.e., seasonal with p$nw (default is annual: nt=ny)

    # defaults in case not provided ...
    p = parameters_add_without_overwriting( p,
      areal_units_xydata = "temperature_db(p=p, DS='areal_units_input')",
      areal_units_proj4string_planar_km =  p$aegis_proj4string_planar_km,  # coord system to use for areal estimation and gridding for carstm
      # areal_units_proj4string_planar_km = projection_proj4string("omerc_nova_scotia")  # coord system to use for areal estimation and gridding for carstm
      areal_units_type= "tesselation",
      #areal_units_constraint_ntarget = floor(p$ny /3),  # n time slices req in each au
      areal_units_constraint_ntarget=12, 
      areal_units_constraint_nmin=1,   # granularity options for areal_units
      areal_units_constraint="none",
      #areal_units_constraint_nmin = 5,   # n time slices req in each au
      areal_units_resolution_km = 1,  # starting resolution .. if using tesselation/ otherwise grid size ()
      areal_units_overlay = "none",
      areal_units_timeperiod = "none",  # only relevent for groundfish polys
      tus="yr",
      # sa_threshold_km2 = 5, 
      sa_threshold_km2=16, 
      n_iter_drop=0, 
      fraction_todrop = 0.05,
      fraction_cv = 0.9,  # just under poisson (binomial)
      fraction_good_bad = 0.9,
      spbuffer=9, 
      lenprob=0.95,   # these are domain boundary options for areal_units
      nAU_min = 30,
      carstm_modelengine = "inla",  # {model engine}.{label to use to store}
      carstm_model_label = "1970_present",
      carstm_inputs_prefilter = "aggregated",
      carstm_inputs_prefilter_n = 100  # only used for "sampled"
    )

    if ( !exists("carstm_prediction_surface_parameters", p))  {
        # generics using "default" carstm models and stmv solutions for spatial effects
        p$carstm_prediction_surface_parameters = list()
        p$carstm_prediction_surface_parameters = parameters_add_without_overwriting( p$carstm_prediction_surface_parameters,
          bathymetry = aegis.bathymetry::bathymetry_parameters( project_class="stmv"  )
        )
    }
 
    if ( grepl("inla", p$carstm_modelengine) ) {
      if ( !exists("formula", p)  ) {
        p$formula = as.formula( paste(
         p$variabletomodel, ' ~ 1',
          ' + f( time, model="ar1", hyper=H$ar1 ) ',   
          ' + f( cyclic, model="rw2", scale.model=TRUE, cyclic=TRUE, values=1:10, hyper=H$rw2 )',
          ' + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, hyper=H$bym2  ) ',
          ' + f( inla.group( z, method="quantile", n=11 ), model="rw2", scale.model=TRUE, hyper=H$rw2)',
          ' + f( space_cyclic, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, group=cyclic_space, hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group, cyclic=TRUE) ) ',
          ' + f( space_time, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE,  hyper=H$bym2, group=time_space, control.group=list(model="ar1", hyper=H$ar1_group) ) '
          ) )
      }
      if ( !exists("family", p)  )  p$family = "gaussian"
    }

    return(p)
  }


  # ---------------------

  if (project_class %in% c("stmv", "default") ) {

    p$libs = c( p$libs, project.library ( "stmv" ) )
    p$project_class = "stmv"

    nyrs = diff(range( p$yrs )) 

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
      stmv_distance_scale = p$stmv_distance_statsgrid * c( 1/2, 1, 2, 3, 4, 8, 16, 20), # km ... approx guesses of 95% AC range
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
        scale = list(
          c1 = rep("localhost", 1),
          c2 = rep("localhost", 1),
          c3 = rep("localhost", 1),
          c4 = rep("localhost", 1),
          c5 = rep("localhost", 1),
          c6 = rep("localhost", 1),
          c7 = rep("localhost", 1),
          c8 = rep("localhost", 1)
        ),

        interpolate_correlation_basis = list(
          c1 = rep("localhost", 1),
          c2 = rep("localhost", 1),
          c3 = rep("localhost", 1),
          c4 = rep("localhost", 1),
          c5 = rep("localhost", 1),
          c6 = rep("localhost", 1),
          c7 = rep("localhost", 1),
          c8 = rep("localhost", 1)
        ),

        interpolate_predictions = list(
          c1 = rep("localhost", 1),
          c2 = rep("localhost", 1),
          c3 = rep("localhost", 1),
          c4 = rep("localhost", 1),
          c5 = rep("localhost", 1),
          c6 = rep("localhost", 1),
          c7 = rep("localhost", 1),
          c8 = rep("localhost", 1)
        ),
        save_intermediate_results = TRUE,
        save_completed_data = TRUE # just a dummy variable with the correct name
      )
    )


    p = aegis_parameters( p=p, DS="stmv" )


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
            + f( space, model="bym2", graph=slot(sppoly, "nb"), scale.model=TRUE, constr=TRUE, hyper=H$bym2),
          family = "gaussian",
          data= dat,
          inla.mode="compact",
          control.compute=list(dic=TRUE, waic=TRUE, cpo=FALSE, config=FALSE, return.marginals.predictor=TRUE),  # config=TRUE if doing posterior simulations
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


    return(p)

  }




}
