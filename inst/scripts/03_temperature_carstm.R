

  require(aegis.temperature)

  year.assessment = 2021

  # construct basic parameter list defining the main characteristics of the study
  # and some plotting parameters (bounding box, projection, bathymetry layout, coastline)
  # p = temperature_parameters( project_class="carstm", yrs=1950:year.assessment )
  
  # about 24 hrs

  p = temperature_parameters( project_class="carstm", yrs=1999:year.assessment )

    if (0) { 
        require(INLA)
        inla.setOption(num.threads=2  )  # note, you want 1 here unless you have a lot of RAM and swap 

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
        
        # or using carstm_map: 
        carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="view" )  # interactive
        carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="plot" )  # regular plot
        
   #     M = temperature_db( p=p, DS="aggregated_data", redo=TRUE )  # redo if input data has changes
        M = temperature_db( p=p, DS="carstm_inputs", redo=TRUE )  # must  redo if sppoly has changed

      M = NULL
      gc()
    }


  # !!! WARNING: this uses a lot of RAM  
  # Time used: 13841.799 sec (to fit) ~ 4 hrd

  # override defaults to try to reduce RAM requirements
  
  fit = carstm_model( 
    p=p, 
    data = "temperature_db( p=p, DS='carstm_inputs' ) ", 
    redo_fit = TRUE, 
    num.threads="4:2",
    control.inla = list( strategy='laplace' ), # "adaptive" strategy seems to run into problems with sparse data (in current year) 
    # control.inla = list( strategy='adaptive', int.strategy="eb" , optimise.strategy="plain"),
    verbose=TRUE 
  )   

    # extract results
    if (0) {
      # very large files .. slow 
      fit = carstm_model( p=p, DS="carstm_modelled_fit" )  # extract currently saved model fit
      fit$summary$dic$dic
      fit$summary$dic$p.eff
    
      plot(fit)
      plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
    }


  res = carstm_model( p=p, DS="carstm_modelled_summary"  ) # to load currently saved results

  
  map_centre = c( (p$lon0+p$lon1)/2 - 0.5, (p$lat0+p$lat1)/2   )
  map_zoom = 7

  # maps of some of the results
  tmatch="2015"
  umatch="0.15"

  tmout = carstm_map(  res=res, vn="predictions", tmatch=tmatch, umatch=umatch, 
    breaks=seq(-1, 9, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths", "coastline", "compass", "scale_bar", "legend" ),
    tmap_zoom= c(map_centre, map_zoom),
    title=paste( "Bottom temperature predictions", tmatch, umatch)  
  )
  tmout

  tmout = carstm_map(  res=res, vn=c( "random", "space", "combined" ), 
    breaks=seq(-5, 5, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths", "coastline", "compass", "scale_bar", "legend" ),
    tmap_zoom= c(map_centre, map_zoom),
    title="Bottom temperature spatial effects (Celcius)"
  )
  tmout

  tmout = carstm_map(  res=res, vn=c( "random", "space_time", "combined" ), tmatch=tmatch, umatch=umatch, 
    breaks=seq(-2, 2, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths", "coastline", "compass", "scale_bar", "legend" ),
    tmap_zoom= c(map_centre, map_zoom),
    title=paste( "Bottom temperature spatiotemporal effects", tmatch, umatch)  
  )
  tmout


  tmatch="2020"
  umatch="0.15"

  tmout = carstm_map(  res=res, vn=c( "random", "space_time", "combined" ), tmatch=tmatch, umatch=umatch, 
    breaks=seq(-2, 2, by=0.25), 
    palette="-RdYlBu",
    plot_elements=c( "isobaths", "coastline", "compass", "scale_bar", "legend" ),
    tmap_zoom= c(map_centre, map_zoom),
    title=paste( "Bottom temperature spatiotemporal effects", tmatch, umatch)  
  )
  tmout

  # map all bottom temps:
  outputdir = file.path( gsub( ".rdata", "", carstm_filenames(p, returntype="carstm_modelled_fit") ), "figures" )
  if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )

  graphics.off()

  # slow due to use of webshot to save html to png (partial solution until tmap view mode saves directly)
  for (y in res$time ){
    for ( u in res$cyclic ){
      fn_root = paste( "Bottom temperature",  as.character(y), as.character(u), sep="-" )
      fn = file.path( outputdir, paste( gsub(" ", "-", fn_root), "png", sep=".") )
      carstm_map(  res=res, vn="predictions", tmatch=as.character(y), umatch=as.character(u),
        breaks=seq( 1, 9), 
        palette="-RdYlBu",
        title=fn_root,  
        outfilename=fn,
        plot_elements=c( "isobaths", "coastline", "compass", "scale_bar", "legend" ),
        vwidth = 1600,
        vheight=1000,
        tmap_zoom= c(map_centre, map_zoom)
      )
    }
  }
  
# end


   

