#### Overview & Example Datasets

This app performs supervised learning on training dataset. Below are some models (with definition) that user can apply on their datasets using the app

1. **<u>Logistic Regression</u>** (or **logit model**) is used to model the probability of a certain class or event existing such as pass/fail, win/lose, alive/dead or healthy/sick.[[1]](https://en.wikipedia.org/wiki/Logistic_regression)

2. **<u>Naïve Bayes</u>** are a family of simple "[probabilistic classifiers](https://en.wikipedia.org/wiki/Probabilistic_classification)" based on applying [Bayes' theorem](https://en.wikipedia.org/wiki/Bayes'_theorem) with strong (naïve) [independence](https://en.wikipedia.org/wiki/Statistical_independence) assumptions between the features (see [Bayes classifier](https://en.wikipedia.org/wiki/Bayes_classifier)).[[2]](https://en.wikipedia.org/wiki/Naive_Bayes_classifier)

3. **<u>Support Vector Machines</u>** are a [supervised learning](https://brilliant.org/wiki/supervised-learning/) method used to perform binary classification on data. They are motivated by the principle of *optimal separation*, the idea that a good classifier finds the largest gap possible between data points of different classes.[[3]](https://brilliant.org/wiki/support-vector-machines/)

4. <u>**Neural Networks** (**NNs**)</u>, are computing systems vaguely inspired by the [biological neural networks](https://en.wikipedia.org/wiki/Biological_neural_network)[[4]](https://en.wikipedia.org/wiki/Artificial_neural_network)

   

------

#### How to use this App

1. Upload training data from sidebar panel
2. Go to Model Results tab and select Y and X variables from sidebar panel and then select algorithm to train
3. Select percentage of data required for training model
4. Tune model parameters
5. Click on Train model 



Once your model is trained, You may find training report & plots in Model Results and Plot Tabs respectively. 

------

#### Predict new dataset

After training the model you can predict new dataset by uploading it from sidebar panel.

Once your data is uploaded, you can download predicted data from Prediction Output tab.

***Note: Prediction data should contains all the features used while training the model.***

