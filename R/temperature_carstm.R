
temperature_carstm = function ( p=NULL, DS, varnames=NULL, yr=NULL, ret="mean", dyear_index=NULL, redo=FALSE, ... ) {

  # over-ride default dependent variable name if it exists

  if (is.null(p)) p = temperature_parameters()

  voi = NULL
  if (exists("variables", p)) if(exists("Y", p$variables)) voi=p$variables$Y  # used in stmv
  if (is.null(voi)) voi="t"  # default


  # manipulate temperature databases from osd, groundfish and snow crab and grid them
  # OSD data source is
  # http://www.meds-sdmm.dfo-mpo.gc.ca/zmp/climate/climate_e.htm
  # http://www.mar.dfo-mpo.gc.ca/science/ocean/database/data_query.html
  ## must download manually to this directory and run gzip
  ## use choijae/Jc#00390
  ## depths: 500,500, "complete profile"   .. raw data  for the SS
  # (USER Defined -- region: jc.ss")

  # no time records, just day/mon/year .. assume utc

  basedir = project.datadirectory("aegis", "temperature" )
  dir.create( basedir, recursive=T, showWarnings=F )

  loc.archive = file.path( basedir, "archive", "profiles")
  dir.create( loc.archive, recursive=T, showWarnings=F )

  loc.basedata = file.path( basedir, "basedata", "rawdata" )
  dir.create( loc.basedata, recursive=T, showWarnings=F )

  loc.profile = file.path( basedir, "basedata", "profiles" )
  dir.create( loc.profile, recursive=T, showWarnings=F )

  loc.bottom = file.path( basedir, "basedata", "bottom"  )
  dir.create( loc.bottom, recursive=T, showWarnings=F )


  # OSD data series variables of interest

  # ----------------------

  if (project_class =="carstm_auid") {
    # translate param values from one project to a unified representation
    # must be first to catch p
    pc = temperature_carstm(
      DS = parameters,
      project_class = "carstm", # defines which parameter class / set to load
      project_name = "temperature",
      variabletomodel = "t",
      yrs = p$yrs,
      spatial_domain = p$spatial_domain,  # defines spatial area, currenty: "snowcrab" or "SSE"
      areal_units_overlay = p$areal_units_overlay, # currently: "snowcrab_managementareas",  "groundfish_strata" .. additional polygon layers for subsequent analysis for now ..
      areal_units_resolution_km = p$areal_units_resolution_km, # km dim of lattice ~ 1 hr
      areal_units_proj4string_planar_km = p$areal_units_proj4string_planar_km,  # coord system to use for areal estimation and gridding for carstm
      inputdata_spatial_discretization_planar_km = p$inputdata_spatial_discretization_planar_km,  # 1 km .. some thinning .. requires 32 GB RAM and limit of speed -- controls resolution of data prior to modelling to reduce data set and speed up modelling
      inputdata_temporal_discretization_yr = p$inputdata_temporal_discretization_yr,  # ie., weekly .. controls resolution of data prior to modelling to reduce data set and speed up modelling
      auid = p$auid
    )
    return(pc)
  }

  # ---------------------


  if (DS="parameters") {

    p$libs = unique( c( p$libs, project.library ( "carstm" ) ) )

    if ( !exists("project_name", p)) p$project_name = "temperature"

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

    p = carstm_parameters( p=p, DS="basic" )  # fill in anything missing and some checks

    return(p)
  }


  # ---------------------------------


  if ( DS=="aggregated_data") {

    # param list needs to be reset but keeping the relevent parts;
    p = temperature_carstm(
      DS ="parameters",
      p=p,
      variabletomodel = p$variabletomodel,
      yrs=p$yrs,
      inputdata_spatial_discretization_planar_km=p$inputdata_spatial_discretization_planar_km,
      inputdata_temporal_discretization_yr=p$inputdata_temporal_discretization_yr )

    fn = file.path( loc.bottom, paste( "temperature", "aggregated_data", p$inputdata_spatial_discretization_planar_km, round(p$inputdata_temporal_discretization_yr,6), "rdata", sep=".") )
    if (!redo)  {
      if (file.exists(fn)) {
        load( fn)
        return( M )
      }
    }
    warning( "Generating aggregated data ... ")

    M = temperature.db( p=p, DS="bottom.all"  )
    M = M[ which(M$yr %in% p$yrs), ]
    M$tiyr = lubridate::decimal_date ( M$date )

    # globally remove all unrealistic data
    keep = which( M[,p$variabletomodel] >= -3 & M[,p$variabletomodel] <= 25 ) # hard limits
    if (length(keep) > 0 ) M = M[ keep, ]
    TR = quantile(M[,p$variabletomodel], probs=c(0.0005, 0.9995), na.rm=TRUE ) # this was -1.7, 21.8 in 2015
    keep = which( M[,p$variabletomodel] >=  TR[1] & M[,p$variabletomodel] <=  TR[2] )
    if (length(keep) > 0 ) M = M[ keep, ]
    keep = which( M$z >=  2 ) # ignore very shallow areas ..
    if (length(keep) > 0 ) M = M[ keep, ]

    M$plon = round(M$plon / p$inputdata_spatial_discretization_planar_km + 1 ) * p$inputdata_spatial_discretization_planar_km
    M$plat = round(M$plat / p$inputdata_spatial_discretization_planar_km + 1 ) * p$inputdata_spatial_discretization_planar_km

    M$dyear = M$tiyr - M$yr
    M$dyear = discretize_data( M$dyear, seq(0, 1, by=p$inputdata_temporal_discretization_yr), digits=6 )

    bb = as.data.frame( t( simplify2array(
      tapply( X=M[,p$variabletomodel], INDEX=list(paste( M$plon, M$plat, M$yr, M$dyear, sep="_") ),
        FUN = function(w) { c(
          mean(w, na.rm=TRUE),
          sd(w, na.rm=TRUE),
          length( which(is.finite(w)) )
        ) }, simplify=TRUE )
    )))
    colnames(bb) = paste( p$variabletomodel, c("mean", "sd", "n"), sep=".")
    bb$id = rownames(bb)
    out = bb

    # keep depth at collection .. potentially the biochem data as well (at least until biochem is usable)
    bb = as.data.frame( t( simplify2array(
      tapply( X=M$z, INDEX=list(paste( M$plon, M$plat, M$yr, M$dyear, sep="_") ),
        FUN = function(w) { c(
          mean(w, na.rm=TRUE),
          sd(w, na.rm=TRUE),
          length( which(is.finite(w)) )
        ) }, simplify=TRUE )
    )))
    colnames(bb) = paste( "z", c("mean", "sd", "n"), sep=".")
    bb$id = rownames(bb)

    ii = match( out$id, bb$id )
    out$z = bb$z.mean[ii]

    bb = NULL
    labs = matrix( as.numeric( unlist(strsplit( rownames(out), "_", fixed=TRUE))), ncol=4, byrow=TRUE)

    out$plon = labs[,1]
    out$plat = labs[,2]
    out$yr = labs[,3]
    out$dyear = labs[,4]
    labs = NULL

    M = out[ which( is.finite( out$temperature.mean )) ,]
    out =NULL
    gc()
    M = planar2lonlat( M, p$aegis_proj4string_planar_km)

    save( M, file=fn, compress=TRUE )
    return( M )
  }



  # -----------------------


  if ( DS=="carstm_inputs") {

    fn = file.path( p$modeldir, paste( "temperature", "carstm_inputs", p$auid,
      p$inputdata_spatial_discretization_planar_km,
      round(p$inputdata_temporal_discretization_yr, 6),
      "rdata", sep=".") )

    if (!redo)  {
      if (file.exists(fn)) {
        load( fn)
        return( M )
      }
    }
    message( "Generating carstm_inputs ... ")

    # prediction surface
    sppoly = areal_units( p=p )  # will redo if not found

    crs_lonlat = sp::CRS(projection_proj4string("lonlat_wgs84"))

    # do this immediately to reduce storage for sppoly (before adding other variables)
    M = temperature_carstm( p=p, DS="aggregated_data"  )  # will redo if not found .. not used here but used for data matching/lookup in other aegis projects that use bathymetry

    # reduce size
    M = M[ which( M$lon > p$corners$lon[1] & M$lon < p$corners$lon[2]  & M$lat > p$corners$lat[1] & M$lat < p$corners$lat[2] ), ]
    # levelplot(z.mean~plon+plat, data=M, aspect="iso")

    M$StrataID = over( SpatialPoints( M[, c("lon", "lat")], crs_lonlat ), spTransform(sppoly, crs_lonlat ) )$StrataID # match each datum to an area
    M$lon = NULL
    M$lat = NULL
    M$plon = NULL
    M$plat = NULL
    M = M[ which(is.finite(M$StrataID)),]
    M$StrataID = as.character( M$StrataID )  # match each datum to an area
    M$tiyr = M$yr + M$dyear
    # M[, p$variabletomodel] = M$temperature.mean
    names(M)[which(names(M)==paste(p$variabletomodel, "mean", sep=".") )] = p$variabletomodel
    M$tag = "observations"

    pB = aegis.bathymetry::bathymetry_carstm( p=p, DS="carstm_auid" ) # transcribes relevant parts of p to load bathymetry
    # already has depth .. no need to match
    # BI = bathymetry_carstm ( p=pB, DS="carstm_inputs" )  # unmodeled!
    # jj = match( as.character( M$StrataID), as.character( BI$StrataID) )
    # M$z = BI$z[jj]
    # jj =NULL


    APS = as.data.frame(sppoly)
    APS$StrataID = as.character( APS$StrataID )
    APS$tag ="predictions"
    APS[, p$variabletomodel] = NA

    BM = carstm_model ( p=pB, DS="carstm_modelled" )  # unmodeled!
    jj = match( as.character( APS$StrataID), as.character( BM$StrataID) )
    APS[, pB$variabletomodel] = BM$z.predicted[jj]
    jj =NULL
    BM = NULL

    vn = c( p$variabletomodel, pB$variabletomodel, "tag", "StrataID", "z" )
    APS = APS[, vn]

    # expand APS to all time slices
    n_aps = nrow(APS)
    APS = cbind( APS[ rep.int(1:n_aps, p$nt), ], rep.int( p$prediction_ts, rep(n_aps, p$nt )) )
    names(APS) = c(vn, "tiyr")

    M = rbind( M[, names(APS)], APS )
    APS = NULL

    M$StrataID  = factor( as.character(M$StrataID), levels=levels( sppoly$StrataID ) ) # revert to factors
    M$strata  = as.numeric( M$StrataID)
    M$year = trunc( M$tiyr)
    M$year_factor = as.numeric( factor( M$year, levels=p$yrs))

    M$zi = discretize_data( M[, pB$variabletomodel], p$discretization[[pB$variabletomodel]] )

    M$iid_error = 1:nrow(M) # for inla indexing for set level variation

    save( M, file=fn, compress=TRUE )
    return( M )
  }


  # -----------------------


}
