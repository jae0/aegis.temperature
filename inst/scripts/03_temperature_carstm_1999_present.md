# Spatiotemporal model of bottom temperatures

## Purpose

Data collected (marine ocean bottom temperatures) from miscellaneous sources, opportunistically and from surveys with different rationales result in spatial and temporal aliasing/bias. The reduce some of this bias, the data are subjected to a spatiotemporal model using [carstm](https://github.com/jae0/carstm), a simple front-end to [INLA](https://www.r-inla.org/), used to perform Conditional autocorrelation (BYM) models and temporal (AR1). 

## Prepare parameter settings

Here we prepare the parameter setting to control this modelling and prediction.

```r
require(aegis.temperature)
# loadfunctions("aegis.temperature")

year.assessment = 2023
# about 24 hrs for 1999:2021
# used by bio.snowcrab  --- about 24 hrs ( ~8 hrs +  )... 
# the theta are starting points for the hyper params on link scale
p = temperature_parameters( 
  project_class="carstm", 
  carstm_model_label="1999_present",
  yrs=1999:year.assessment #,
  # spbuffer=9, lenprob=0.95,   # these are domain boundary options for areal_units
  # n_iter_drop=0, sa_threshold_km2=16, 
  # areal_units_constraint_ntarget=12, areal_units_constraint_nmin=1   # granularity options for areal_units
)

current_model = "space_cyclic"
  
if (current_model == "simple_cyclic") {

  p$theta =  c(-0.505, 0.460, 0.599, 1.551, -1.621, 1.406, -3.401, -0.618, 12.828, 0.679 ) 

  # maxld= -269327.874 fn=138 theta= -0.505 0.460 0.599 1.551 -1.621 1.406 -3.401 -0.618 12.828 0.679 [7.66, 16.287]

  # theta[0] = [Log precision for the Gaussian observations]
  # theta[1] = [Log precision for time]
  # theta[2] = [Rho_intern for time]
  # theta[3] = [Log precision for cyclic]
  # theta[4] = [Log precision for space]
  # theta[5] = [Logit phi for space]
  # theta[6] = [Log precision for inla.group(z, method = "quantile", n = 11)]
  # theta[7] = [Log precision for space_time]
  # theta[8] = [Logit phi for space_time]
  # theta[9] = [Group rho_intern for space_time]

              
}

  
if (current_model == "space_cyclic") {
  # params from last run to speed up convergence
  p$theta = c(0.078, 0.442, 0.357, 2.107, 0.479, -2.409, -1.313, -1.568, -0.013, 2.419, 0.375, -0.575, 0.439)
 
  # theta[0] = [Log precision for the Gaussian observations]
  # theta[1] = [Log precision for time]
  # theta[2] = [Rho_intern for time]
  # theta[3] = [Log precision for cyclic]
  # theta[4] = [Log precision for space_cyclic]
  # theta[5] = [Logit phi for space_cyclic]
  # theta[6] = [Group rho_intern for space_cyclic]
  # theta[7] = [Log precision for space]
  # theta[8] = [Logit phi for space]
  # theta[9] = [Log precision for inla.group(z, method = "quantile", n = 11)]
  # theta[10] = [Log precision for space_time]
  # theta[11] = [Logit phi for space_time]
  # theta[12] = [Group rho_intern for space_time]
}  
              

```



# Prepare data
  
```r    
  # to recreate the underlying data
  xydata=temperature_db(p=p, DS="areal_units_input", redo=TRUE)  # redo if inpute data has changed

  xydata=temperature_db(p=p, DS="areal_units_input")  # redo if inpute data has changed
  xydata = xydata[ which(xydata$yr %in% p$yrs), ]
  

  # if sppoly options need to change, do so at parameter-level such that they are consistent  (though, not necessary if sppoly is passed directly to carstm)
  sppoly = areal_units( p=p, xydata=xydata,  redo=TRUE, verbose=TRUE )  # to force create
  # sppoly = areal_units( p=p  )  # reload


  plot( sppoly[ "AUID" ] ) 
  
  # or using carstm_map: 
  # carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="view" )  # interactive
  # carstm_map( sppoly=sppoly, vn="au_sa_km2", map_mode="plot" )  # regular plot

  
  M = temperature_db( p=p, DS="carstm_inputs", sppoly=sppoly, redo=TRUE )  # must  redo if sppoly has changed or new data
  # M = temperature_db( p=p, DS="carstm_inputs", sppoly=sppoly  ) 
  M = NULL
 

```


# Model via carstm


```r
# dimensionality and labels:
p$space_name = sppoly$AUID 
p$space_id = 1:nrow(sppoly)

p$time_name = as.character(p$yrs)
p$time_id =  1:p$ny

p$cyclic_name = as.character(p$cyclic_levels)
p$cyclic_id = 1:p$nw

# !!! WARNING: this uses a lot of RAM  
res = NULL

res = carstm_model( 
    p=p, 
    data ='temperature_db( p=p, DS="carstm_inputs", sppoly=sppoly)',  
    sppoly=sppoly,
    nposteriors=1000,
    posterior_simulations_to_retain=c("predictions", "random_spatial"), 
    theta=p$theta,
    # if problems, try any of: 
    # control.inla = list( strategy='adaptive', int.strategy="eb" , optimise.strategy="plain", strategy='laplace', fast=FALSE),
    # control.inla = list( strategy='adaptive', int.strategy="eb" ),
    # control.inla = list( strategy='laplace'  ),
    # redo_fit=FALSE,
    # debug="extract",
    # debug = "random_spatiotemporal", 
    verbose=TRUE, 
    compress=FALSE,  ## large file size makes compression/decompression too slow
    num.threads="3:2"  # adjust for your machine
  )    


  # extract results
  if (0) {
    # very large files .. slow 
    fit = carstm_model( p=p, DS="carstm_modelled_fit", sppoly=sppoly )  # extract currently saved model fit
    fit$summary$dic$dic
    fit$summary$dic$p.eff
  
    plot(fit)
    plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
  }


( res$summary) 
```

## Some results and maps

```r

# to reload .. can be slow due to large file sizes
# res = carstm_model( p=p, DS="carstm_modelled_summary", sppoly=sppoly  ) # to load currently saved results



  res$summary$fixed_effects["(Intercept)", "mean"]

  ts =  res$random$time 
  IDx = as.numeric(res$time_name)
  plot( mean ~ IDx, ts, type="b", ylim=c(-2,2), lwd=1.5, xlab="year")
  lines( quant0.025 ~ IDx, ts, col="gray", lty="dashed")
  lines( quant0.975 ~ IDx, ts, col="gray", lty="dashed")


  ts =  res$random$cyclic
  IDx = as.numeric(res$cyclic_name)
  plot( mean ~IDx, ts, type="b", ylim=c(-1.5, 1.5), lwd=1.5, xlab="fractional year")
  lines( quant0.025 ~IDx, ts, col="gray", lty="dashed")
  lines( quant0.975 ~IDx, ts, col="gray", lty="dashed")


  # maps of some of the results

  additional_features = features_to_add( 
      p=p, 
#      area_lines="cfa.regions",
      isobaths=c( 100, 200, 300, 400, 500  ), 
      coastline =  c("canada", "united states of america"), 
      xlim=c(-80,-40), 
      ylim=c(38, 60) # ,redo=TRUE 
  )

  # map all bottom temps:
  outputdir = file.path(p$data_root, "maps", p$carstm_model_label )
  if ( !file.exists(outputdir)) dir.create( outputdir, recursive=TRUE, showWarnings=FALSE )


  graphics.off()

  # e.g. management lines, etc
  
  fn_root = paste("Predicted_habitat_probability_persistent_spatial_effect", sep="_")
  outfilename = file.path( outputdir, paste(fn_root, "png", sep=".") )
  
  vn = c( "random", "space", "combined" ) 
  
  # toplot = carstm_results_unpack( res, vn )
  # brks = pretty(  quantile(toplot[,"mean"], probs=c(0,0.975), na.rm=TRUE )  )
  
  brks = pretty(c(-0.5, 0.5))
  plt = carstm_map(  res=res, vn=vn, 
    breaks = brks,
    colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")),
    additional_features=additional_features,
    title= "Bottom temperature -- persistent spatial effect" ,
    outfilename=outfilename
  )  
 
  brks = pretty(c(1, 9))
  for (y in res$time_name ){
    for ( u in res$cyclic_name  ){
      fn_root = paste( "Bottom temperature",  as.character(y), as.character(u), sep="-" )
      outfilename = file.path( outputdir, paste( gsub(" ", "-", fn_root), "png", sep=".") )
      plt = carstm_map(  res=res, vn="predictions", tmatch=as.character(y), umatch=as.character(u),
        breaks=brks, 
        colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")),
        additional_features=additional_features,
        title=fn_root,  
        outfilename=outfilename
      )
    }
  }

'''

# end


