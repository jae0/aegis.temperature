

  require(aegis.temperature)

  year.assessment = 2020

  # construct basic parameter list defining the main characteristics of the study
  # and some plotting parameters (bounding box, projection, bathymetry layout, coastline)
  p = temperature_parameters( project_class="carstm", yrs=1950:year.assessment )


    if (0) { 
        inla.setOption(num.threads=1  )  # note, you want 1 here unless you have a lot of RAM and swap 
        inla.setOption(blas.num.threads= 1 )

        # to recreate the underlying data
        xydata=temperature_db(p=p, DS="areal_units_input", redo=TRUE)  # redo if inpute data has changed
        # sppoly = areal_units( p=p, xydata=xydata, redo=TRUE )  # to force create
        
        sppoly = areal_units( p=p , redo=TRUE, verbose=TRUE )  # same
        plot( sppoly[ "AUID" ] ) 

        M = temperature_db( p=p, DS="aggregated_data", redo=TRUE )  # redo if input data has changes

        M = temperature_db( p=p, DS="carstm_inputs", redo=TRUE )  # must  redo if sppoly has changed

      M = NULL
      gc()
    }
    

  # !!! WARNING, this uses a lot of RAM !!! 400 + GB with 4 cpus on default settings (1950-2020).. reduce 
  # adjust based upon RAM requirements and ncores

  fit = carstm_model( p=p, M="temperature_db( p=p, DS='carstm_inputs' ) ", file_compress_method=FALSE )   

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
  
  time_match = list(year="2019", dyear="0.85")
  time_match = list(year="2020", dyear="0.85")

  vn = paste(p$variabletomodel, "predicted", sep=".")
  carstm_map(  res=res, vn=vn, time_match=time_match, 
    at=seq(-2, 10, by=2),          
    sp.layout = p$coastLayout, 
    col.regions = p$mypalette, 
    main=paste("Bottom temperature", paste0(time_match, collapse="-") )  )

  vn = paste(p$variabletomodel, "random_sample_iid", sep=".")
  carstm_map(  res=res, vn=vn, time_match=time_match, 
    at=seq(-10, 10, by=4),          
    sp.layout = p$coastLayout, 
    col.regions = p$mypalette, 
    main=paste("Bottom temperature random effects", paste0(time_match, collapse="-") )  )

  vn = paste(p$variabletomodel, "random_auid_nonspatial", sep=".")
  carstm_map(  res=res, vn=vn, time_match=time_match , 
    at=seq(-10, 10, by=4),          
    sp.layout = p$coastLayout, 
    col.regions = p$mypalette, 
    main=paste("Bottom temperature nonspatial effects", paste0(time_match, collapse="-") )  )

  vn = paste(p$variabletomodel, "random_auid_spatial", sep=".")
  carstm_map(  res=res, vn=vn, time_match=time_match , 
    at=seq(-10, 10, by=4),          
    sp.layout = p$coastLayout, 
    col.regions = p$mypalette, 
    main=paste("Bottom temperature spatial effects", paste0(time_match, collapse="-") )  )



  # map all bottom temps:
  vn = paste(p$variabletomodel, "predicted", sep=".")
  outputdir = file.path( gsub( ".rdata", "", dirname(res$fn_res) ), "figures", vn )
  if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )


  for (y in res$yrs ){
    for ( s in dyear ){
      time_match = list( year=as.character(y), dyear=as.character(s) )
      fn_root = paste( "Bottom temperature",  paste0(time_match, collapse=" - ") )
      fn = file.path( outdir, paste(fn_root, "png", sep=".") )
      png( filename=fn, width=3072, height=2304, pointsize=40, res=300  )
        o = carstm_map(  res=res, vn=vn, time_match=time_match, 
          at=seq(-2, 10, by=2), 
          sp.layout = p$coastLayout, 
          col.regions = p$mypalette, 
          main=fn_root  )
      print(o); dev.off()
    }
  }
  
# end


  
