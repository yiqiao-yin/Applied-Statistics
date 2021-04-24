# README

[![AppliedStats](https://cdn.rawgit.com/sindresorhus/awesome/d7305f38d29fed78fa85652e3a63e154dd8e8829/media/badge.svg)](https://yinscapital.com/research/)
[![Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](http://makeapullrequest.com)

<p align="left">
  <img width="700" src="https://github.com/yiqiao-yin/Applied-Statistics/blob/main/Project/figures/main.gif">
</p>
<p align="left">
	<img src="https://img.shields.io/badge/stars-30+-blue.svg"/>
	<img src="https://img.shields.io/badge/license-CC0-blue.svg"/>
</p>

This is the folder for project. I apologize upfront because the current project is related to a recent submission which is currently under review. However, in order to serve as part of the supplement, we made some simulated code available and open source here. Thank you for your understanding.

## Simulation

In this small simulation, let us examine the following model. Let $y = X_1 + X_2 \text{ with probabiltiy } 1/2 \text{ and (mod 2)}$ and $y = X_3 + X_4 + X_5 \text{ with probabiltiy } 1/2 \text{ and (mod 2)}$

## Shiny App

To enable a user-friendly environment, we post this app that was initially developed by Jon Auerbach. Dr. Auerbach has graduated from the Department of Statistics. He designed the feature selection portion of the app. Later on, I upgraded the app by introducing the separation of training and testing set. We report test set porformance and the difference of the proposed methodology and tree-based algorithms. We use this app to illustrate the usage of the proposed methodology I-score.

### User Manual

The link for shiny app is [here](https://y-yin.shinyapps.io/Iscore-App/).

#### Input

<p align="center">
  <img width="200" src="https://github.com/yiqiao-yin/Applied-Statistics/blob/main/Project/figures/app_input.PNG">
</p>

First, let us introduce the input parameters:
- sample size: this is the number of observations generated to run the model and experiment
- number of times to run backward dropping algorithm: this is the number of rounds of backward dropping algorithm used
- number of modules to report: after running all rounds of backward dropping algorithm, we sort the rows according to I-score and this is the number of top rows reported 
- number of bootstrap samples for bias: this is the number of samples used to run bootstrap
- seed: for reproducbility, we allow user to set seed 
- variables for module 1: the default model has two modules, this is the first module, a segment of 1-3 would mean we use $X_1$, $X_2$, and $X_3$
- variables for module 2:: the default model has two modules, this is the second module, a segment of 2-4 would mean we use $X_2$, $X_3$, and $X_4$

#### Output

*Model* This section uses \LaTex embedded in Shiny app and it presents the formal model used in the experiment. This app assumes model to have two modules. Each module is a summation of certain variables selected in input (see variables for module 1 and 2). Each module is also comnputed using modulo 2.

<p align="center">
  <img width="600" src="https://github.com/yiqiao-yin/Applied-Statistics/blob/main/Project/figures/app_output_model.PNG">
</p>

*Top Sets* This section presents a table of top I-score modules along with their I-score values. We also present I-score over sample size, lower bound, upper bound, and bootstrap bias as summary information.

<p align="center">
  <img width="500" src="https://github.com/yiqiao-yin/Applied-Statistics/blob/main/Project/figures/app_output_top_sets.PNG">
</p>

*Performance* The section follows standard machine learning framework. We create training data using sample size initiated in input parameters by user. We use three tree-based algorithms: random forest (RF), iterative random forest (iRF), and bayesian additive regression tree (BART). These three algorithms are considered as peers. We then use I-score to select the top modules and then use logistic model to make predictions using variables selected by I-score instead of all variables. For example, suppose the table in *Top Sets* presents *1, 2* (in the table it is connected with underscore symbol). This means we use $X_1$ and $X_2$ to build a model and make predictions instead of using all 50 variables like other algorithms.

<p align="center">
  <img width="500" src="https://github.com/yiqiao-yin/Applied-Statistics/blob/main/Project/figures/app_output_performance.PNG">
</p>

*Interpretation* Based on the experience of user interface with the app, there are many different interpretations to be drawn. For example, user can simply stick with the default values for all input parameters. User simply need to change sample size to 50, 100, 150, 200 and run the scripts 4 times individually. Observations can be made that in the beginning with 50 samples the experiment performance should be poor. As sample size increases to 200 or even larger, the testing performance should be picking up fairly fast for I-score. However, it is much more challenging for other algorithms to make the same improvement as sample size increases a little. At an extreme case when sample size is 1000 or even larger, we should see good amount of improvement for all algorithms, yet it is still quite challenging for other algorithm to pick up the signal. On average, we expect with over 500 samples in training set, BART should be able to pick up some signal while iRF would be in the 60's percentage range. Random Forest (RF) is quite poor around 50% even with 500 sample size.
