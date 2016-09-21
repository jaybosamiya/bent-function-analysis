# Datasets

## Description

Each of the following files contains data with each row consisting of one sample.

Each row consists of 2 columns
+ First column has the binary string (consisting of `0`s and `1`s) which represents the boolean function
+ Second column has the Hadamard-coefficient-based-entropy corresponding to the boolean function

## Files

+ [training_set.txt](training_set.txt) contains three thousand 4-variable boolean functions, which must be used for training the model
+ [validation_set.txt](validation_set.txt) contains three thousand 4-variable boolean functions, which must be used for validating the model
+ [ground_truth.txt](ground_truth.txt) contains _all_ 4-variable boolean functions. This file must **only** be used for testing and **no model must train/learn from this**.