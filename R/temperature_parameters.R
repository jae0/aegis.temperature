

temperature_parameters = function( p=NULL, project_name=NULL, project_class="default", ... ) {

  # ---------------------
  # deal with additional passed parameters
  if ( is.null(p) ) p=list()
  p_add = list(...)
  if (length(p_add) > 0 ) p = c(p, p_add)
  i = which(duplicated(names(p), fromLast = TRUE ))
  if ( length(i) > 0 ) p = p[-i] # give any passed parameters a higher priority, overwriting pre-existing variable


  # ---------------------

  if (project_class =="carstm_auid") {
    # translate param values from one project to a unified representation
    # must be first to catch p
    P = temperature_parameters(
      project_class = "carstm", # defines which parameter class / set to load
      project_name = "temperature",
      yrs = p$yrs,
      spatial_domain = p$spatial_domain,  # defines spatial area, currenty: "snowcrab" or "SSE"
      areal_units_overlay = p$areal_units_overlay, # currently: "snowcrab_managementareas",  "groundfish_strata" .. additional polygon layers for subsequent analysis for now ..
      areal_units_resolution_km = p$areal_units_resolution_km, # km dim of lattice ~ 1 hr
      areal_units_proj4string_planar_km = p$areal_units_proj4string_planar_km,  # coord system to use for areal estimation and gridding for carstm
      inputdata_spatial_discretization_planar_km = p$inputdata_spatial_discretization_planar_km,  # 1 km .. some thinning .. requires 32 GB RAM and limit of speed -- controls resolution of data prior to modelling to reduce data set and speed up modelling
      inputdata_temporal_discretization_yr = p$inputdata_temporal_discretization_yr,  # ie., weekly .. controls resolution of data prior to modelling to reduce data set and speed up modelling
      auid = p$auid
    )
    return(P)
  }


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

  if ( !exists("spatial_domain", p) ) p$spatial_domain = "canada.east" # canada.east.highres and canada.east.superhighres result in memory overflow
  if ( !exists("spatial_domain_subareas", p) )  p$spatial_domain_subareas = c( "SSE.mpa", "SSE", "snowcrab" ) # target domains and resolution for additional data subsets .. add here your are of interest

  p = spatial_parameters( p=p )  # default grid and resolution

  # define focal years
  if (!exists( "yrs", p)) p$yrs = 1950:lubridate::year(lubridate::now())  # default

  p = temporal_parameters(p=p, aegis_dimensionality="space-year-season")

  if ( !exists("additional.data", p) )  p$additional.data=c("groundfish", "snowcrab", "USSurvey_NEFSC", "lobster")



  if (project_class=="default") {
    if ( !exists("inputdata_spatial_discretization_planar_km", p) )  p$inputdata_spatial_discretization_planar_km = p$pres/3  # =3x3=9 units possible per location .. km controls resolution of data prior to modelling to reduce data set and speed up modelling
    if ( !exists("inputdata_temporal_discretization_yr", p) )  p$inputdata_temporal_discretization_yr = 1/12  # ie., monthly .. controls resolution of data prior to modelling to reduce data set and speed up modelling
    return(p)
  }

  if (project_class=="stmv") {
    p$libs = unique( c( p$libs, project.library ( "stmv" ) ) )

    if (!exists("DATA", p) ) p$DATA = 'temperature.db( p=p, DS="stmv_inputs" )'

    if (!exists("variables", p)) p$variables = list()
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$variables)) p$variables$TIME="tiyr"
    if (!exists("Y", p$variables)) p$variables$Y="t"

    # increase resolution from defaults as we can with stmv
    p$inputdata_spatial_discretization_planar_km = p$pres / 10 # controls resolution of data prior to modelling (km .. ie 100 linear units smaller than the final discretization pres)
    p$inputdata_temporal_discretization_yr = 1/52  # ie., weekly .. controls resolution of data prior to modelling to reduce data set and speed up modelling

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
          't ~ s(yr, k=25, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
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
        #   't ~ s(yr, k=5, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
        #     '+ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") ',
        #     '+ s(log(z), plon, plat, cos.w, sin.w, yr, k=100, bs="ts")') )

        if (!exists("stmv_twostep_time", p)) p$stmv_twostep_time = "gam"
        if (!exists("stmv_twostep_space", p))  p$stmv_twostep_space = "fft" #  matern, krige (very slow), lowpass, lowpass_matern

        if (p$stmv_twostep_time == "gam")  {
          if (!exists("stmv_local_modelformula_time", p))  p$stmv_local_modelformula_time = formula( paste(
            't',  '~ s(yr, k=10, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts")  ',
            '+ s( log(z), k=3, bs="ts") + s( plon, k=3, bs="ts") + s( plat, k=3, bs="ts")  ',
            '+ s( cos.w, sin.w, yr, k=15, bs="ts") + s( log(z), plon, plat, k=15, bs="ts")  '
            ) )
        }

        if (p$stmv_twostep_space == "gam")  {
          # very resource intensive ..
          if (!exists("stmv_local_modelformula_space", p))  p$stmv_local_modelformula_space = formula( paste(
          't ~ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s( log(z), plon, plat, k=27, bs="ts")  ') )
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
          't ~ sx(yr,   bs="ps") + sx(cos.w, bs="ps") + s(sin.w, bs="ps")',
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



  if (project_class=="carstm") {
    p$libs = unique( c( p$libs, project.library ( "carstm" ) ) )

    if ( !exists("project_name", p)) p$project_name = "temperature"

    p = aegis_parameters( p=p, DS="carstm" )

    if ( !exists("areal_units_strata_type", p)) p$areal_units_strata_type = "lattice" # "stmv_lattice" to use ageis fields instead of carstm fields ... note variables are not the same

    if ( p$spatial_domain == "SSE" ) {
      if ( !exists("areal_units_overlay", p)) p$areal_units_overlay = "groundfish_strata" #.. additional polygon layers for subsequent analysis for now ..
      if ( !exists("areal_units_resolution_km", p)) p$areal_units_resolution_km = 25 # km dim of lattice ~ 1 hr
      if ( !exists("areal_units_proj4string_planar_km", p)) p$areal_units_proj4string_planar_km = projection_proj4string("utm20")  # coord system to use for areal estimation and gridding for carstm
      p$inputdata_spatial_discretization_planar_km = 1  # 1 km .. requires 32 GB RAM and limit of speed -- controls resolution of data prior to modelling to reduce data set and speed up modelling
      p$inputdata_temporal_discretization_yr = 1/12  #  24/365,  # ie., monthly .. controls resolution of data prior to modelling to reduce data set and speed up modelling
    }

    if ( p$spatial_domain == "snowcrab" ) {
      if ( !exists("areal_units_overlay", p)) p$areal_units_overlay = "snowcrab_managementareas" # currently: "snowcrab_managementareas",  "groundfish_strata" .. additional polygon layers for subsequent analysis for now ..
      if ( !exists("areal_units_resolution_km", p)) p$areal_units_resolution_km = 25 # km dim of lattice ~ 1 hr
      if ( !exists("areal_units_proj4string_planar_km", p)) p$areal_units_proj4string_planar_km = projection_proj4string("utm20")  # coord system to use for areal estimation and gridding for carstm
      # if ( !exists("areal_units_proj4string_planar_km", p)) p$areal_units_proj4string_planar_km = projection_proj4string("omerc_nova_scotia")  # coord system to use for areal estimation and gridding for carstm
      p$inputdata_spatial_discretization_planar_km = 1  # 1 km .. requires 32 GB RAM and limit of speed -- controls resolution of data prior to modelling to reduce data set and speed up modelling
      p$inputdata_temporal_discretization_yr = 1/52  # ie., every 1 weeks .. controls resolution of data prior to modelling to reduce data set and speed up modelling }
    }

    if ( !exists("carstm_modelengine", p)) p$carstm_modelengine = "inla.default"  # {model engine}.{label to use to store}

    if ( !exists("carstm_modelcall", p)) {
      if ( grepl("inla", p$carstm_modelengine) ) {
        p$libs = unique( c( p$libs, project.library ( "INLA" ) ) )

        p$carstm_modelcall = paste('
          inla(
            formula = ', p$variabletomodel, ' ~ 1
              + f(tiyr, model="ar1", hyper=H$ar1 )
              + f(year, model="ar1", hyper=H$ar1 )
              + f(zi, model="rw2", scale.model=TRUE, diagonal=1e-6, hyper=H$rw2)
              + f(strata, model="bym2", graph=sppoly@nb, scale.model=TRUE, constr=TRUE, hyper=H$bym2)
              + f(iid_error, model="iid", hyper=H$iid),
            family = "normal",
            data= M,
            control.compute=list(dic=TRUE, config=TRUE),
            control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
            control.predictor=list(compute=FALSE, link=1 ),
            control.fixed=H$fixed,  # priors for fixed effects, generic is ok
            control.inla=list(strategy="gaussian", int.strategy="eb") ,# to get empirical Bayes results much faster.
            # control.inla=list(int.strategy="eb") ,# to get empirical Bayes results much faster.
            # control.inla=list( strategy="laplace", cutoff=1e-6, correct=TRUE, correct.verbose=FALSE ),
            num.threads=4,
            blas.num.threads=4,
            verbose=TRUE
          ) ' )
      }
         #     + f(tiyr2, model="seasonal", season.length=10 )
        # + f(dyear, model="ar1", hyper=H$ar1 )

      if ( grepl("glm", p$carstm_modelengine) ) {
        p$carstm_modelcall = paste( 'glm( formula = ', p$variabletomodel, ' ~ 1 + StrataID,  family = gaussian(link="log"), data= M[ which(M$tag=="observations"), ], family=gaussian(link="identity")  ) ' )  # for modelengine='glm'
      }
      if ( grepl("gam", p$carstm_modelengine) ) {
        p$libs = unique( c( p$libs, project.library ( "mgcv"  ) ) )
        p$carstm_modelcall = paste( 'gam( formula = ', p$variabletomodel, ' ~ 1 + StrataID,  family = gaussian(link="log"), data= M[ which(M$tag=="observations"), ], family=gaussian(link="identity")  ) ' ) # for modelengine='gam'
      }
    }
    return(p)
  }

}
