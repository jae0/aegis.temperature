
temperature_carstm = function ( p=NULL, DS="parameters", redo=FALSE, year.assessment=NULL, ... ) {

  if (DS=="parameters_production") {

    p = temperature_carstm(
      DS = "parameters",
      project_name = "temperature",
      variabletomodel = "t",
      carstm_model_label = "production",
      inputdata_spatial_discretization_planar_km = 1,  # km controls resolution of data prior to modelling to reduce data set and speed up modelling
      inputdata_temporal_discretization_yr = 1/12,  # ie., every 2 weeks .. controls resolution of data prior to modelling to reduce data set and speed up modelling
      yrs = 1999:year.assessment,
      spatial_domain = "SSE",  # defines spatial area, currenty: "snowcrab" or "SSE"
      areal_units_resolution_km = 25, # km dim of lattice ~ 1 hr
      areal_units_proj4string_planar_km = aegis::projection_proj4string("utm20"),  # coord system to use for areal estimation and gridding for carstm
      areal_units_source = "lattice", # "stmv_fields" to use ageis fields instead of carstm fields ... note variables are not the same
      areal_units_overlay = "none"
    )
    return(p)
  }


  if (DS=="parameters") {

    if ( is.null(p)) {
      p = aegis.temperature::temperature_parameters(...)
    } else {

      # if p is passed, assume it is a secondary call ... overwrite nonrelevent params to force use of project defaults
      p$data_root = NULL
      p$datadir  = NULL
      p$data_transformation= NULL
      p$carstm_modelcall = NULL  # defaults to generic
      p$carstm_model_tag = NULL

      p = aegis.temperature::temperature_parameters(p=p, ...)
    }

    p$libs = unique( c( p$libs, project.library ( "aegis", "aegis.bathymetry", "aegis.coastline","aegis.polygons", "aegis.temperature",  "aegis.survey", "carstm"  ) ) )



    if ( !exists("areal_units_source", p)) p$areal_units_source = "lattice" # "stmv_lattice" to use ageis fields instead of carstm fields ... note variables are not the same

    if ( p$spatial_domain == "SSE" ) {
      if ( !exists("areal_units_overlay", p)) p$areal_units_overlay = "groundfish_strata" #.. additional polygon layers for subsequent analysis for now ..
      if ( !exists("areal_units_resolution_km", p)) p$areal_units_resolution_km = 25 # km dim of lattice ~ 1 hr
      if ( !exists("areal_units_proj4string_planar_km", p)) p$areal_units_proj4string_planar_km = projection_proj4string("utm20")  # coord system to use for areal estimation and gridding for carstm
      if ( !exists("inputdata_spatial_discretization_planar_km", p)) p$inputdata_spatial_discretization_planar_km = 1  # 1 km .. requires 32 GB RAM and limit of speed -- controls resolution of data prior to modelling to reduce data set and speed up modelling
      if ( !exists("inputdata_temporal_discretization_yr", p)) p$inputdata_temporal_discretization_yr = 1/12  #  24/365,  # ie., monthly .. controls resolution of data prior to modelling to reduce data set and speed up modelling
    }

    if ( p$spatial_domain == "snowcrab" ) {
      if ( !exists("areal_units_overlay", p)) p$areal_units_overlay = "snowcrab_managementareas" # currently: "snowcrab_managementareas",  "groundfish_strata" .. additional polygon layers for subsequent analysis for now ..
      if ( !exists("areal_units_resolution_km", p)) p$areal_units_resolution_km = 25 # km dim of lattice ~ 1 hr
      if ( !exists("areal_units_proj4string_planar_km", p)) p$areal_units_proj4string_planar_km = projection_proj4string("utm20")  # coord system to use for areal estimation and gridding for carstm
      # if ( !exists("areal_units_proj4string_planar_km", p)) p$areal_units_proj4string_planar_km = projection_proj4string("omerc_nova_scotia")  # coord system to use for areal estimation and gridding for carstm
      if ( !exists("inputdata_spatial_discretization_planar_km", p)) p$inputdata_spatial_discretization_planar_km = 1  # 1 km .. requires 32 GB RAM and limit of speed -- controls resolution of data prior to modelling to reduce data set and speed up modelling
      if ( !exists("inputdata_temporal_discretization_yr", p)) p$inputdata_temporal_discretization_yr = 1/52  # ie., every 1 weeks .. controls resolution of data prior to modelling to reduce data set and speed up modelling }
    }

    if ( !exists("carstm_modelengine", p)) p$carstm_modelengine = "inla"  # {model engine}.{label to use to store}

    if ( !exists("carstm_modelcall", p)) {
      if ( grepl("inla", p$carstm_modelengine) ) {
        p$libs = unique( c( p$libs, project.library ( "INLA" ) ) )

        if ( !exists("carstm_model_label", p))  p$carstm_model_label = "production"
         p$carstm_modelcall = paste('
          inla(
            formula = ', p$variabletomodel, ' ~ 1
              + f( dyri, model="ar1", hyper=H$ar1 )
              + f( inla.group( z, method="quantile", n=9 ), model="rw2", scale.model=TRUE, hyper=H$rw2)
              + f( auid, model="bym2", graph=slot(sppoly, "nb"), group=year_factor, scale.model=TRUE, constr=TRUE, hyper=H$bym2, control.group=list(model="ar1", hyper=H$ar1_group)),
            family = "normal",
            data= M,
            control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE, config=TRUE),
            control.results=list(return.marginals.random=TRUE, return.marginals.predictor=TRUE ),
            control.predictor=list(compute=FALSE, link=1 ),
            control.fixed=H$fixed,  # priors for fixed effects, generic is ok
            # control.inla=list( strategy="laplace", cutoff=1e-6, correct=TRUE, correct.verbose=FALSE ),
            # control.inla = list( h=1e-6, tolerance=1e-12), # increase in case values are too close to zero
            # control.mode = list( restart=TRUE, result=RES ), # restart from previous estimates
            # control.inla = list(cmin = 0 ),
            # control.inla=list( strategy="laplace", cutoff=1e-6, correct=TRUE, correct.verbose=FALSE ),
            # control.inla = list( h=1e-6, tolerance=1e-12), # increase in case values are too close to zero
            # control.inla = list(h=1e-1, tolerance=1e-9, optimise.strategy="smart", stupid.search=FALSE, strategy="adaptive"), # # restart=3), # restart a few times in case posteriors are poorly defined
            # control.inla = list(h=1e-1, tolerance=1e-9, optimise.strategy="smart", stupid.search=FALSE, strategy="adaptive"), # restart=3), # restart a few times in case posteriors are poorly defined
            # control.mode = list( restart=TRUE, result=RES ), # restart from previous estimates
            verbose=TRUE
          ) ' )
      }
        #  + f(tiyr2, model="seasonal", season.length=10 )
        #  + f(dyear, model="ar1", hyper=H$ar1 )
        #  + f(seasonal, model="seasonal", season.length=', pT$n.season, ', scale.model=TRUE )  # using seasonal effect is not recommended as it is not smoothed well .. rw2 is better

      if ( grepl("glm", p$carstm_modelengine) ) {
        if ( !exists("carstm_model_label", p))  p$carstm_model_label = "default_glm"
        p$carstm_modelcall = paste( 'glm( formula = ', p$variabletomodel, ' ~ 1 + AUID,  family = gaussian(link="log"), data= M[ which(M$tag=="observations"), ], family=gaussian(link="identity")  ) ' )  # for modelengine='glm'
      }
      if ( grepl("gam", p$carstm_modelengine) ) {
        p$libs = unique( c( p$libs, project.library ( "mgcv"  ) ) )
        if ( !exists("carstm_model_label", p))  p$carstm_model_label = "default_gam"
        p$carstm_modelcall = paste( 'gam( formula = ', p$variabletomodel, ' ~ 1 + AUID,  family = gaussian(link="log"), data= M[ which(M$tag=="observations"), ], family=gaussian(link="identity")  ) ' ) # for modelengine='gam'
      }
    }

    p = carstm_parameters( p=p )  # fill in anything missing and some checks

    p$carstm_inputs_aggregated = FALSE

    return(p)
  }


  # ---------------------------------


  if ( DS=="carstm_inputs") {

    # prediction surface
    crs_lonlat = sp::CRS(projection_proj4string("lonlat_wgs84"))
    sppoly = areal_units( p=p )  # will redo if not found
    areal_units_fn = attributes(sppoly)[["areal_units_fn"]]


    if (p$carstm_inputs_aggregated) {
      fn = carstm_filenames( p=p, projectname="temperature", projecttype="carstm_inputs", areal_units_fn=areal_units_fn )

    } else {
      fn = paste( "temperature", "carstm_inputs", areal_units_fn, "rawdata", "rdata", sep=".")
    }

    fn = file.path( p$modeldir, fn)

    if (!redo)  {
      if (file.exists(fn)) {
        load( fn)
        return( M )
      }
    }


    # do this immediately to reduce storage for sppoly (before adding other variables)

    if (p$carstm_inputs_aggregated) {

      M = temperature_db( p=p, DS="aggregated_data"  )
      names(M)[which(names(M)==paste(p$variabletomodel, "mean", sep=".") )] = p$variabletomodel

    } else {
      M = temperature_db( p=p, DS="bottom.all"  )
      names(M)[which(names(M)=="t")] = p$variabletomodel
      attr( M, "proj4string_planar" ) =  p$aegis_proj4string_planar_km
      attr( M, "proj4string_lonlat" ) =  projection_proj4string("lonlat_wgs84")

      names(M)[which(names(M)=="yr") ] = "year"
      M = M[ which(M$year %in% p$yrs), ]
      M$tiyr = lubridate::decimal_date ( M$date )

      # globally remove all unrealistic data
      keep = which( M[,p$variabletomodel] >= -3 & M[,p$variabletomodel] <= 25 ) # hard limits
      if (length(keep) > 0 ) M = M[ keep, ]

      # p$quantile_bounds_data = c(0.0005, 0.9995)
      if (exists("quantile_bounds_data", p)) {
        TR = quantile(M[,p$variabletomodel], probs=p$quantile_bounds_data, na.rm=TRUE ) # this was -1.7, 21.8 in 2015
        keep = which( M[,p$variabletomodel] >=  TR[1] & M[,p$variabletomodel] <=  TR[2] )
        if (length(keep) > 0 ) M = M[ keep, ]
        # this was -1.7, 21.8 in 2015
      }

      keep = which( M$z >=  2 ) # ignore very shallow areas ..
      if (length(keep) > 0 ) M = M[ keep, ]

      M = lonlat2planar( M, p$aegis_proj4string_planar_km) # in case plon/plats are from an alternate projection  .. as there are multiple data sources

      M$dyear = M$tiyr - M$year
    }

    # reduce size
    M = M[ which( M$lon > p$corners$lon[1] & M$lon < p$corners$lon[2]  & M$lat > p$corners$lat[1] & M$lat < p$corners$lat[2] ), ]
    # levelplot(z.mean~plon+plat, data=M, aspect="iso")

    M$AUID = over( SpatialPoints( M[, c("lon", "lat")], crs_lonlat ), spTransform(sppoly, crs_lonlat ) )$AUID # match each datum to an area

    # M[, pS$variabletomodel] = substrate_lookup( p=p, locs=M[, c("lon", "lat")], source_data_class="aggregated_rawdata" )

    M = M[ which(!is.na(M$AUID)),]

    M$tiyr = M$year + M$dyear
    M$tag = "observations"

    # already has depth .. but in case some are missing data
    pB = bathymetry_carstm( p=p, DS="parameters", variabletomodel="z" )


    if (!(exists(pB$variabletomodel, M ))) M[,pB$variabletomodel] = NA

    kk = which(!is.finite( M[, pB$variabletomodel] ))
    if (length(kk > 0)) {
      M[kk, pB$variabletomodel] = bathymetry_lookup( p=p, locs=M[kk, c("lon", "lat")], source_data_class="aggregated_rawdata" )
    }

    # if any still missing then use a mean depth by AUID
    kk =  which( !is.finite(M[, pB$variabletomodel]))
    if (length(kk) > 0) {
      AD = bathymetry_db ( p=pB, DS="aggregated_data"   )  # 16 GB in RAM just to store!
      AD = AD[ which( AD$lon > p$corners$lon[1] & AD$lon < p$corners$lon[2]  & AD$lat > p$corners$lat[1] & AD$lat < p$corners$lat[2] ), ]
      # levelplot( eval(paste(p$variabletomodel, "mean", sep="."))~plon+plat, data=M, aspect="iso")
      AD$AUID = over( SpatialPoints( AD[, c("lon", "lat")], crs_lonlat ), spTransform(sppoly, crs_lonlat ) )$AUID # match each datum to an area
      oo = tapply( AD[, paste(pB$variabletomodel, "mean", sep="." )], AD$AUID, FUN=median, na.rm=TRUE )
      jj = match( as.character( M$AUID[kk]), as.character( names(oo )) )
      M[kk, pB$variabletomodel] = oo[jj ]
    }

    if( exists("spatial_domain", p)) M = geo_subset( spatial_domain=p$spatial_domain, Z=M ) # need to be careful with extrapolation ...  filter depths

    M$lon = NULL
    M$lat = NULL
    M$plon = NULL
    M$plat = NULL

    APS = as.data.frame(sppoly)
    APS$AUID = as.character( APS$AUID )
    APS$tag ="predictions"
    APS[, p$variabletomodel] = NA

    BM = carstm_summary( p=pB ) # to load currently saved sppoly

    jj = match( as.character( APS$AUID), as.character( BM$AUID) )
    APS[, pB$variabletomodel] = BM[[ paste(pB$variabletomodel, "predicted", sep=".") ]] [jj]
    jj =NULL
    BM = NULL

    vn = c( p$variabletomodel, pB$variabletomodel, "tag", "AUID"  )
    APS = APS[, vn]

    # expand APS to all time slices
    n_aps = nrow(APS)
    APS = cbind( APS[ rep.int(1:n_aps, p$nt), ], rep.int( p$prediction_ts, rep(n_aps, p$nt )) )
    names(APS) = c(vn, "tiyr")

    M = rbind( M[, names(APS)], APS )
    APS = NULL

    M$auid  = as.numeric( factor( M$AUID) )

    M$year = floor( M$tiyr)
    M$year_factor = as.numeric( factor( M$year, levels=p$yrs))
    M$dyear =  M$tiyr - M$year  # reset in case it has been discretized
    # M$tiyri  = floor( M$tiyr / p$tres )*p$tres    # discretize for inla

    M$dyri = discretize_data( M[, "dyear"], p$discretization[["dyear"]] )

    # M$seasonal = (as.numeric(M$year_factor) - 1) * length(p$dyears)  + as.numeric(M$dyear)

    M$zi = discretize_data( M[, pB$variabletomodel], p$discretization[[pB$variabletomodel]] )

    save( M, file=fn, compress=TRUE )
    return( M )
  }


  # -----------------------


}
