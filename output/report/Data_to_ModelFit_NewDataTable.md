Fit the biofilm colonisation area with a stationary solution from the model
================
Andres Diaz,Tatyana Pichugina, Paul Rainey
7/02/2019

Total integral area
-------------------

Integral area represent sum of the colonized pixel for each frame. Each frame represent one hour of experiment. Here we are interested in 15 first hours.

![Total\_Integral\_Area](Data_to_ModelFit_NewDataTable_files/figure-markdown_github/Total%20Integral%20Area-1.png) \#\# Total integral area was fitted by Log(Area)~A+B\*time

![Integral\_Area\_Fit\_results](Data_to_ModelFit_NewDataTable_files/figure-markdown_github/unnamed-chunk-1-1.png)![Integral\_Area\_Fit\_results](Data_to_ModelFit_NewDataTable_files/figure-markdown_github/unnamed-chunk-1-2.png)![Integral\_Area\_Fit\_results](Data_to_ModelFit_NewDataTable_files/figure-markdown_github/unnamed-chunk-1-3.png)![Integral\_Area\_Fit\_results](Data_to_ModelFit_NewDataTable_files/figure-markdown_github/unnamed-chunk-1-4.png)![Integral\_Area\_Fit\_results](Data_to_ModelFit_NewDataTable_files/figure-markdown_github/unnamed-chunk-1-5.png)![Integral\_Area\_Fit\_results](Data_to_ModelFit_NewDataTable_files/figure-markdown_github/unnamed-chunk-1-6.png)

Colonisation area per Layer
---------------------------

LayerSize=64 mkm (400px)

![Area\_per\_Layer](Data_to_ModelFit_NewDataTable_files/figure-markdown_github/unnamed-chunk-3-1.png) \#\# Linear regression fit separetely for each layer

![Area\_per\_Layer\_Fit\_Results](Data_to_ModelFit_NewDataTable_files/figure-markdown_github/unnamed-chunk-5-1.png) \#\# Quality of the fit per Layer: R^2 per experiment ![Area\_per\_Layer\_Fit\_Results\_R2](Data_to_ModelFit_NewDataTable_files/figure-markdown_github/unnamed-chunk-6-1.png) \#\# Quality of the fit per Layer: Intersept per experiment ![](Data_to_ModelFit_NewDataTable_files/figure-markdown_github/unnamed-chunk-7-1.png) \#\# Occupation proportion per experiment ![Occupancy\_percent\_Layer](Data_to_ModelFit_NewDataTable_files/figure-markdown_github/unnamed-chunk-8-1.png)![Occupancy\_percent\_Layer](Data_to_ModelFit_NewDataTable_files/figure-markdown_github/unnamed-chunk-8-2.png)
