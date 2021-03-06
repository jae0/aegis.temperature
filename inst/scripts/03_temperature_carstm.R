

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
    

  # !!! WARNING: this uses a lot of RAM  
  # Time used: 13841.799 sec (to fit) ~ 4 hrd
  
  res = carstm_model( p=p, M="temperature_db( p=p, DS='carstm_inputs' ) ", compression_level=1, redo_fit = TRUE )   


 

    # extract results
    if (0) {
      # very large files .. slow 
      fit = carstm_model( p=p, DS="carstm_modelled_fit" )  # extract currently saved model fit
      fit$summary$dic$dic
      fit$summary$dic$p.eff
      fit$dyear

      plot(fit)
      plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
    }


  res = carstm_model( p=p, DS="carstm_modelled_summary"  ) # to load currently saved results

  
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


  
