12Apr:
Stopping work on this model.  Latest run does not seem to be useful.
maxdepth  mfinal  LogLoss   LogLoss SD 
  1          30     1.755964  0.041330686
  1          60     1.769921  0.013420491
  1          90     1.803525  0.009403540
  1         120     1.824269  0.008670391
  1         150     1.841598  0.008343338
  1         180     1.854444  0.006103469
  1         210     1.862842  0.002669450
  1         240     1.872394  0.002217684
  1         270     1.879718  0.002656165
  1         300     1.886980  0.003616614
  5          30     1.477662  0.050442573
  5          60     1.448663  0.030043706
  5          90     1.446988  0.007081562
  5         120     1.460568  0.006327717
  5         150     1.469765  0.004643108
  5         180     1.473048  0.005917317
  5         210     1.479411  0.005096808
  5         240     1.482672  0.006653429
  5         270     1.485723  0.005736844
  5         300     1.488281  0.006471446
  9          30     1.369362  0.045068960
  9          60     1.238786  0.046293943
  9          90     1.231474  0.019551594
  9         120     1.237221  0.020892066
  9         150     1.243578  0.016840014
  9         180     1.246268  0.015622185
  9         210     1.241292  0.013213442
  9         240     1.244244  0.014298888
  9         270     1.244797  0.011799139
  9         300     1.245402  0.011704314

Tuning parameter 'coeflearn' was held constant at a value of Zhu
LogLoss was used to select the optimal model using  the smallest value.
The final values used for the model were mfinal = 90, maxdepth = 9 and coeflearn = Zhu. 

               date.time       model user.cpu.time sys.cpu.time elapsed.time num.observations
user1 2015-04-12 18:32:22 AdaBoost.M1        24.037        1.394       423.18             3713
      num.features    score mfinal maxdepth coeflearn
user1           93 1.235253     90        9       Zhu