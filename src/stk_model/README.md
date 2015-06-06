Model Stacking
==============

## Overivew
This model was built post-competition to test out model stacking concept.

## Folder Naming convention
Folders named as follows: \[model\]\[model id\]\_\[stack level\]

where 
* \[model\] is a character string identifying the model, e.g., "rf"
* \[model id\] is numeri digit 
* \[stack level\] designates model stack level, valid values 1, 2 or 3


Example:  
* rf1_1:  random forest model 1 at stack level 1
* rf2_1:  random forest model 2 at stack level 1
* gbm1_1:  gbm model 1 at stack level 1
* rf1_2:  random forest model 1 at stack level 2