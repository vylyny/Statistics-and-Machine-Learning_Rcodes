# T1: Classification of Leukemia Samples based on 20 genes selected from the training data
### Team members: Vylyny, Nagashree and Yue
### Model used: Lasso penalized logistic regression
This Rscript outputs the predicted cell types for the held-out dataset using LASSO-LR and 20 most predictive genes.

### Inputs of the script:
Example call:
```
Rscript Team1_20genesClassifier.R Problem1_expressionMatrix.txt test_data.txt output_subtypesPrediction.txt
```
**please note this script uses the given "Problem1_expressionMatrix.txt" to train the model, so this file is the train set and needs to be loaded along your test_data.txt to run the script.

Both input files (train and test) have the same formats as shown in this image below (the six sample subtypes are the column names, while the rows of the input matrix are the genes.

![Image of gene matrix](https://raw.githubusercontent.com/satijamlnyu/T1-1/master/Input_geneExpression_image.PNG?token=AJ2LGW26DMN2FHXJZVJCGIC5ZCHDW)

### Output of the script:

This R script generates predicted subtypes for each sample in the test data  and output these predictions in a column as shown below:

![Image of predicted cell types](https://raw.githubusercontent.com/satijamlnyu/T1-1/master/output_example.PNG?token=AJ2LGW5FAJERDYXY5XE547S5ZCHS4)
