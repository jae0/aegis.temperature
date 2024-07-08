# Spatiotemporal model of bottom temperatures

## Purpose

Data collected (marine ocean bottom temperatures) from miscellaneous sources, opportunistically and from surveys with different rationales result in spatial and temporal aliasing/bias. The reduce some of this bias, the data are subjected to a spatiotemporal model using [carstm](https://github.com/jae0/carstm), a simple front-end to [INLA](https://www.r-inla.org/), used to perform Conditional autocorrelation (BYM) models and temporal (AR1). 

## Prepare parameter settings

Here we prepare the parameter setting to control this modelling and prediction.

```r
require(aegis.temperature)
# loadfunctions("aegis.temperature")

year.assessment = 2023
year.start = 1999

p = temperature_parameters( 
  project_class="carstm", 
  carstm_model_label="1999_present",
  yrs=year.start:year.assessment #,
  # spbuffer=9, lenprob=0.95,   # these are domain boundary options for areal_units
  # n_iter_drop=0, sa_threshold_km2=16, 
  # areal_units_constraint_ntarget=12, areal_units_constraint_nmin=1   # granularity options for areal_units
)

  
# params from last run to speed up convergence
# the theta are starting points for the hyper params on link scale
if (year.start == "1999") {
  p$theta =  c(-0.505, 0.460, 0.599, 1.551, -1.621, 1.406, -3.401, -0.618, 12.828, 0.679 ) 
}
  
if (year.start == "1970") {
  p$theta = c(-0.79, 0.83, 1.35, 0.88, -2.53, 6.89, -4.01, -0.84, 22.22, 1.33 )
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

sppoly = areal_units( p=p  )  # reload polygons

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
    data ='temperature_db( p=p, DS="carstm_inputs", sppoly=sppoly )',  
    sppoly=sppoly,
    nposteriors=1000,
    toget = c("summary", "random_spatial", "predictions"),
    posterior_simulations_to_retain = c("predictions"),
    theta=p$theta,
    # if problems, try any of: 
    # control.inla = list( strategy='adaptive', int.strategy="eb" , optimise.strategy="plain", strategy='laplace', fast=FALSE),
    # control.inla = list( strategy='adaptive', int.strategy="eb" ),
    # control.inla = list( strategy='laplace'  ),
    # redo_fit=FALSE,
    # debug="extract",
    # debug = "random_spatiotemporal", 
    verbose=TRUE, 
    # compress=FALSE,  ## large file size makes compression/decompression too slow
    num.threads="3:2"  # adjust for your machine
  )    

  ( res$summary ) 
       # extract results
      if (0) {
        fit = carstm_model( p=p, DS="modelled_fit"  )  # extract currently saved model fit
        summary(fit)  # inla object
          
        plot(fit)
        plot(fit, plot.prior=TRUE, plot.hyperparameters=TRUE, plot.fixed.effects=FALSE )
 
        # EXAMINE POSTERIORS AND PRIORS
        all.hypers = INLA:::inla.all.hyper.postprocess(fit$all.hyper)
        hypers = fit$marginals.hyperpar
        names(hypers)

        carstm_prior_posterior_compare( hypers=hypers, all.hypers=all.hypers, i=2, xrange=c(0.02, 10) )  # note xrange is for precision .. this gets converted to SD   
        carstm_prior_posterior_compare( hypers=hypers, all.hypers=all.hypers, vn="Precision for space", transf=FALSE )  # no conversion to SD 

        carstm_prior_posterior_compare( hypers=hypers, all.hypers=all.hypers, vn="Rho for time" )  
        carstm_prior_posterior_compare( hypers=hypers, all.hypers=all.hypers, vn="Phi for space" )  
        
        # posterior predictive check
        carstm_posterior_predictive_check(p=p, M=temperature_db( p=p, DS="carstm_inputs" ) )

      }



```

## Some results and maps

```r

    oeffdir = file.path(p$modeldir, p$carstm_model_label, "figures")
    fn_root_prefix = "Temperature bottom"
    carstm_plot_marginaleffects( p=p, outputdir=oeffdir, fn_root_prefix=fn_root_prefix ) 

 
  # maps of some of the results

  additional_features = features_to_add( 
      p=p, 
#      area_lines="cfa.regions",
      isobaths=c( 100, 200, 300, 400, 500  ), 
      xlim=c(-80,-40), 
      ylim=c(38, 60) # ,redo=TRUE 
  )

  # map all bottom temps: 
  
    outputdir = file.path(p$modeldir, p$carstm_model_label, "maps" )

    fn_root_prefix = "Substrate grainsize (mm)"
     
    carstm_plot_map( p=p, outputdir=outputdir, 
      additional_features=additional_features, 
      toplot="random_spatial", probs=c(0.025, 0.975),  transf=log10, 
      colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")) ) 

    carstm_plot_map( p=p, outputdir=outputdir, additional_features=additional_features, 
      toplot="predictions", colors=rev(RColorBrewer::brewer.pal(5, "RdYlBu")) )


 

'''

# end


