

  require(aegis.temperature)

  year.assessment = 2020

  # construct basic parameter list defining the main characteristics of the study
  # and some plotting parameters (bounding box, projection, bathymetry layout, coastline)
  # p = temperature_parameters( project_class="carstm", yrs=1950:year.assessment )
  

  p = temperature_parameters( project_class="carstm", yrs=1999:year.assessment )


    if (0) { 
        require(INLA)
        inla.setOption(num.threads=2  )  # note, you want 1 here unless you have a lot of RAM and swap 
        inla.setOption(blas.num.threads= 1 )

        # to recreate the underlying data
        xydata=temperature_db(p=p, DS="areal_units_input", redo=TRUE)  # redo if inpute data has changed
        # sppoly = areal_units( p=p, xydata=xydata, redo=TRUE )  # to force create

          p$fraction_cv = 1.0
          p$fraction_todrop = 1/5
#          p$areal_units_constraint_nmin = 30  # n time slices req in each au >> nyears as we resolve season
          p$areal_units_constraint_ntarget = 40  # n time slices req in each au
          p$areal_units_constraint_nmin = 10   # n time slices req in each au
 
        sppoly = areal_units( p=p , redo=TRUE, verbose=TRUE )  # same
        plot( sppoly[ "AUID" ] ) 

        M = temperature_db( p=p, DS="aggregated_data", redo=TRUE )  # redo if input data has changes

        M = temperature_db( p=p, DS="carstm_inputs", redo=TRUE )  # must  redo if sppoly has changed

      M = NULL
      gc()
    }
    

  # !!! WARNING: this uses a lot of RAM !!! 400 + GB with 4 cpus on default settings (1950-2020).. reduce 
  # !!! WARNING: adjust based upon RAM requirements and ncores
  # !!! WARNING: current default with 21 years of data, discretized to 10 seasonal slices and areal_units_constraint_nmin=30 with 2830 areal units 
  # !!! WARNING: took 500 GB RAM/SWAP and 8 hours to process ... 2 cpus were used with 1 blas thread only 

  fit = carstm_model( p=p, M="temperature_db( p=p, DS='carstm_inputs' ) ", file_compress_method=FALSE )   

# Call:
#    c("inla(formula = p$carstm_model_formula, family = p$carstm_model_family, ", " data = M, verbose = TRUE, 
#    control.compute = list(dic = TRUE, ", " waic = TRUE, cpo = FALSE, config = TRUE), control.predictor = 
#    list(compute = TRUE, ", " link = 1), control.family = p$options.control.family, ", " control.inla = 
#    p$options.control.inla[[civ]], control.results = list(return.marginals.random = TRUE, ", " 
#    return.marginals.predictor = TRUE))") 
# Time used:
#     Pre = 52.9, Running = 19283, Post = 88.3, Total = 19424 
# Fixed effects:
#              mean    sd 0.025quant 0.5quant 0.975quant  mode kld
# (Intercept) 6.079 0.114      5.855    6.079      6.303 6.079   0

# Random effects:
#   Name	  Model
#     dyri AR1 model
#    year AR1 model
#    auid_main BYM2 model
#    inla.group(z, method = "quantile", n = 9) RW2 model
#    auid BYM2 model

# Model hyperparameters:
#                                                             mean       sd 0.025quant 0.5quant 0.975quant     mode
# Precision for the Gaussian observations                      Inf      NaN   0.00e+00 0.00e+00        Inf      NaN
# Precision for dyri                                      5.45e+01    0.076   5.44e+01 5.45e+01   5.47e+01 5.45e+01
# Rho for dyri                                            7.61e-01    0.000   7.60e-01 7.61e-01   7.61e-01 7.61e-01
# Precision for year                                      5.73e+01    0.029   5.73e+01 5.73e+01   5.74e+01 5.73e+01
# Rho for year                                            7.57e-01    0.000   7.57e-01 7.57e-01   7.57e-01 7.57e-01
# Precision for auid_main                                 2.16e+07 7146.359   2.16e+07 2.16e+07   2.16e+07 2.16e+07
# Phi for auid_main                                       4.90e-02    0.000   4.90e-02 4.90e-02   4.90e-02 4.90e-02
# Precision for inla.group(z, method = "quantile", n = 9) 4.78e+01    0.021   4.77e+01 4.78e+01   4.78e+01 4.78e+01
# Precision for auid                                      5.13e+00    0.003   5.12e+00 5.13e+00   5.13e+00 5.13e+00
# Phi for auid                                            5.20e-02    0.000   5.20e-02 5.20e-02   5.30e-02 5.20e-02
# GroupRho for auid                                       1.86e-01    0.000   1.86e-01 1.86e-01   1.87e-01 1.86e-01

# Expected number of effective parameters(stdev): 3253.15(0.008)
# Number of equivalent replicates : 30.38 

# Deviance Information Criterion (DIC) ...............: 444473.28
# Deviance Information Criterion (DIC, saturated) ....: 1662775.24
# Effective number of parameters .....................: 3252.97

# Watanabe-Akaike information criterion (WAIC) ...: 445858.87
# Effective number of parameters .................: 4468.11

# Marginal log-Likelihood:  -229235.22 
# Posterior marginals for the linear predictor and
#  the fitted values are computed




    # extract results
    if (0) {
      # very large files .. slow 
      fit = carstm_model( p=p, DS="carstm_modelled_fit" )  # extract currently saved model fit
      plot(fit)
      plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
    }


  res = carstm_model( p=p, DS="carstm_modelled_summary"  ) # to load currently saved results
  res$summary$dic$dic
  res$summary$dic$p.eff
  res$dyear

  
  # maps of some of the results


  plot_crs = p$aegis_proj4string_planar_km
  coastline=aegis.coastline::coastline_db( DS="eastcoast_gadm", project_to=plot_crs )
  isobaths=aegis.bathymetry::isobath_db( depths=c(50, 100, 200, 400 ), project_to=plot_crs )


  time_match = list(year="2019", dyear="0.85")
  time_match = list(year="2020", dyear="0.85")

  vn = paste(p$variabletomodel, "predicted", sep=".")
  carstm_map(  res=res, vn=vn, time_match=time_match, 
          breaks=seq(-1, 9), 
          palette="viridis",
          coastline=coastline,
          isobaths=isobaths,
  main=paste("Bottom temperature", paste0(time_match, collapse="-") )  )

  # vn = paste(p$variabletomodel, "random_sample_iid", sep=".")
  # carstm_map(  res=res, vn=vn, time_match=time_match, 
  #   breaks=seq(-1, 9), 
  #   coastline=coastline,
  #   isobaths=isobaths,
  #   main=paste("Bottom temperature random effects", paste0(time_match, collapse="-") )  
  # )

  vn = paste(p$variabletomodel, "random_auid_nonspatial", sep=".")
  carstm_map(  res=res, vn=vn, time_match=time_match , 
    breaks=seq(-1, 1, by=0.25), 
    palette="-RdYlBu",
    coastline=coastline,
    isobaths=isobaths,
    main=paste("Bottom temperature nonspatial effects", paste0(time_match, collapse="-") ) 
  )

  vn = paste(p$variabletomodel, "random_auid_spatial", sep=".")
  carstm_map(  res=res, vn=vn, time_match=time_match , 
    breaks=seq(-1, 1, by=0.25), 
    coastline=coastline,
    isobaths=isobaths,
    main=paste("Bottom temperature spatial effects", paste0(time_match, collapse="-") )  
  )


  # map all bottom temps:
  vn = paste(p$variabletomodel, "predicted", sep=".")
  outputdir = file.path( gsub( ".rdata", "", res$fn_res ), "figures", vn )
  if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )

  graphics.off()

  for (y in res$year ){
    for ( s in res$dyear ){
      time_match = list( year=as.character(y), dyear=as.character(s) )
      fn_root = paste( "Bottom temperature",  paste0(time_match, collapse=" - ") )
      fn = file.path( outputdir, paste(fn_root, "png", sep=".") )

      carstm_map(  res=res, vn=vn, time_match=time_match, 
        breaks=seq( 1, 9), 
        coastline=coastline,
        isobaths=isobaths,
        palette="-RdYlBu",
        main=fn_root,  
        outfilename=fn
      )
    }
  }
  
# end


  
