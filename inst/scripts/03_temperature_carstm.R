

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

  res = carstm_model( p=p, M="temperature_db( p=p, DS='carstm_inputs' ) ", compression_level=5, redo_fit = TRUE )   

 
# Time used:
#     Pre = 76, Running = 9403, Post = 90, Total = 9569 
# Fixed effects:
#              mean    sd 0.025quant 0.5quant 0.975quant  mode kld
# (Intercept) 6.255 0.165      5.932    6.255      6.579 6.255   0

# Random effects:
#   Name	  Model
#     dyri AR1 model
#    year AR1 model
#    space Besags ICAR model
#    inla.group(z, method = "quantile", n = 9) RW2 model
#    space BYM2 model

# Model hyperparameters:
#                                                           mean    sd 0.025quant 0.5quant 0.975quant   mode
# Precision for the Gaussian observations                  0.358 0.001      0.357    0.358      0.359  0.359
# Precision for dyri                                       6.004 0.016      5.973    6.003      6.036  6.002
# Rho for dyri                                             0.115 0.001      0.113    0.115      0.118  0.115
# Precision for year                                      11.642 0.038     11.561   11.645     11.710 11.656
# Rho for year                                             0.267 0.001      0.265    0.267      0.269  0.267
# Precision for space                                  2.612 0.005      2.601    2.613      2.620  2.615
# Precision for inla.group(z, method = "quantile", n = 9)  0.063 0.000      0.063    0.063      0.063  0.063
# Precision for space                                       0.004 0.000      0.004    0.004      0.004  0.004
# Phi for space                                             0.265 0.001      0.264    0.265      0.266  0.265
# GroupRho for space                                       -1.000 0.000     -1.000   -1.000     -1.000 -1.000

# Expected number of effective parameters(stdev): 7341.23(0.00)
# Number of equivalent replicates : 13.46 

# Deviance Information Criterion (DIC) ...............: 403891.81
# Deviance Information Criterion (DIC, saturated) ....: 1622194.95
# Effective number of parameters .....................: 7314.75

# Watanabe-Akaike information criterion (WAIC) ...: 404937.97
# Effective number of parameters .................: 7557.23

# Marginal log-Likelihood:  -202605.62 
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

  vn = paste(p$variabletomodel, "random_space_nonspatial", sep=".")
  carstm_map(  res=res, vn=vn, time_match=time_match , 
    breaks=seq(-1, 1, by=0.25), 
    palette="-RdYlBu",
    coastline=coastline,
    isobaths=isobaths,
    main=paste("Bottom temperature nonspatial effects", paste0(time_match, collapse="-") ) 
  )

  vn = paste(p$variabletomodel, "random_space_spatial", sep=".")
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


  
