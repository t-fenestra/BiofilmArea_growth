Biofilm colonisation area calculation
================
Tatyana Pichugina
5/13/2019

Integral area
-------------

Integral area represent sum of the colonized pixel for each frame. Each frame represent one hour of experiment. Here we are interested in 15 first hours.

![](Documentation_files/figure-markdown_github/introduce%20Integral%20area-1.png)

#### Integlal area ALI and GALI

We cutted each image to the two parts: ALI part include 200px (32mkm) layer counted from the ALI, and GALI part include rest part.

![](Documentation_files/figure-markdown_github/unnamed-chunk-1-1.png)

#### Gain-Lost-Same summary per experiment

1.  GainTotal represents proportion of pixels that appear new between consecutive frames to the total number occupied pixels.
2.  LostTotal is a proportion of pixels that lost between two consecutive frames to the total number occupied pixels.
3.  SameTotal is a proportion of pixels that stays at the same place between two consecutive frames to the total number occupied pixels.
4.  IntAreaTotal is a total occupied area.

#### dwss sum of colonisation area

![](Documentation_files/figure-markdown_github/unnamed-chunk-2-1.png)

#### SM sum of colonisation area

![](Documentation_files/figure-markdown_github/unnamed-chunk-3-1.png) \#\# Linear regression fit

Colonisation area per Layer
---------------------------

LayerSize=32 mkm (200px) ![](Documentation_files/figure-markdown_github/unnamed-chunk-5-1.png) \#\#\#\# Plot shows ratio between Area per Layer to total colonization area for different time frames ![](Documentation_files/figure-markdown_github/unnamed-chunk-6-1.png)![](Documentation_files/figure-markdown_github/unnamed-chunk-6-2.png) \#\#\#\# Gain-Lost-Same
