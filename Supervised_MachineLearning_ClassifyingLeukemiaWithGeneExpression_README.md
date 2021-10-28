# T1: Classification of Leukemia Samples based on 20 genes selected from the training data

### Model used: Lasso penalized logistic regression
This Rscript outputs the predicted cell types for the held-out dataset using LASSO-LR and 20 most predictive genes.

### Inputs of the script:
Example call:
```
Rscript Supervised_MachineLearning_ClassifyingLeukemiaWithGeneExpression.R Problem1_expressionMatrix.txt test_data.txt output_subtypesPrediction.txt
```
**please note this script uses the given "Problem1_expressionMatrix.txt" to train the model, so this file is the train set and needs to be loaded along your test_data.txt to run the script.

Both input files (train and test) have the same formats as shown in this image below (the six sample subtypes are the column names, while the rows of the input matrix are the genes.

![Input_geneExpression_image](https://user-images.githubusercontent.com/41202523/139178587-002ea265-fbf7-412b-847a-7ba1650898e3.PNG)

### Output of the script:

This R script generates predicted subtypes for each sample in the test data  and output these predictions in a column as shown below:

![output_example](https://user-images.githubusercontent.com/41202523/139178690-00c7362a-60cf-4396-9ae4-dbeb1e6f0171.PNG)
