# Stock-Market-Prediction

## Abstract

## Introduction
Stock prices are rather difficult to forecast. Proponents of efficient market hypotheses arguet hat stock prices cannot be predicted since the market prices have already reflected all known and expected fundamentals. However, precisely because the current market price reflect all relevant information, and therefore the current and historical prices are extremely useful for predicting the future prices since some relevant information is entering the stock markets continuously. It is well documented evidence that stock prices demonstrate strong momentum, short-term continuation and long-term reversal, and other identifiable patterns. 

## Time seris Analysis: ARIMA, SARIMA and Spectral Analysis 

A time series is a sequence of vectors, x(t), t = 0,1,… , where t represents elapsed time.
For simplicity we will consider here only sequences of scalars, although the techniques considered generalise readily to vector series. Theoretically, x may be a value which varies continuously with t, such as a Closing price. In practice, for any given physical system, x will be sampled to give a series of discrete data points, equally spaced in time. The rate at which samples are taken dictates the maximum resolution of the model; however, it is not always the case that the model with the highest resolution has the best predictive power, so that superior results may be obtained by employing only every nth point in the series. 

Time series are generally sequences of measurements of one or more visible variables of an
underlying dynamic system, whose state changes with time as a function of its current state vector$u(t+1) = F(u(t))$. Such dynamic systems may evolve over time to an attracting set of points that is regular and of simple shape; any time series derived from such a system would also have a smooth and regular appearance. However another result is possible: the system may evolve to a chaotic attractor. Here, the path of the state vector through the attractor is non-periodic and because of this any time series derived from it will have a complex appearance and behaviour. In a real-world system such as a stock market, the nature and structure of the state space is obscure; so that the actual variables that contribute to the state vector are unknown or debatable. The task for a time series predictor can therefore be rephrased: given measurements of one component of the state vector of a dynamic system is it possible to reconstruct the (possibly) chaotic dynamics of the phase space and thereby predict the evolution of the measured variable? 


Example: Amazon
For our interested stocks, we have conducted the time series analysis with similar steps to construct a single variable ARIMA model. A detailed exploration process will be shown as below. 

Firstly, we check the ACF and PACF plots for the first-order or second-order difference of the original data. By observing the number of bars outside of the blue lines, which give the values beyond which the autocorrelations are significantly different from zero. 

```{r,echo=FALSE}
pmts=ts(amazon$Close,frequency=1)
par(mfrow=c(1,2))
# acf(pmts,100)
acf(diff(diff(pmts)),100,main="ACF")
pacf(diff(diff(pmts)),100,main="PACF")
```

With this routine step, though there are very much unlikely for the financial data to show a pattern of seasonality, we have still tried to use spectral analysis to detect periodicity of the data by transferring the data from time-domain to frequency domain with the help of FFT(Fast Fourier Transform) and visualize the Raw Periodogram. 

```{r,echo=FALSE,fig.cap='Raw Periodogram',fig.width=6, fig.height=4}
par(mfrow=c(1,1))
a=spec.pgram(diff(pmts),taper=0,log="no",main="Raw Periodogram")
```

In this specific example, there is an obvious peak when the frequency is about 0.11. Thus a possible seasonality worth testing is about 8 or 9. It is worth noting that the rest of the stocks do not show such pattern. However, taking this into consideration would be an essential step to dig potential information. 

After comparing several SARIMA(Seasonal Autoregressive Integrated Moving Average) models based on their AIC, a best model is selected for making forecast.  Here the best for Amazon is :
$Arima(8,0,2)\times(0,1,0)_{(9)}$

The prediction plot can be seen as below with a shaded area of confidence interval of the forecast. 

```{r,echo=FALSE,fig.cap='Amazon Time Series Forecast'}
spm0=Arima(pmts,order=c(8,0,2),seasonal=list(order=c(0,1,0),period=9))
#spm0$aic
pmdarmaforecast1=forecast.Arima(spm0,h=21)
par(mfrow=c(1,2))
plot.forecast(pmdarmaforecast1,ylim=c(250,700),main="Amazon Forecast")

amazon_real = as.data.frame(get.hist.quote(instrument='AMZN',start='2014-09-19',end='2016-4-29'))

plot.ts(amazon_real$Close,ylim=c(250,700),main="Amazon real data",ylab="",xlab="")

mse_amazon = round(sum((pmdarmaforecast1$mean[1:20]-amazon_real$Close[380:399])^2)/20,3)

```

From the graph we can see that the SARIMA model shows a similar trend of the data. However, the MSE is `r mse_amazon`.

The similar analysis procedure is conducted for the rest of the five stocks, the forecasts plots are in the attachment and their MSEs are as below:

```{r,echo=FALSE}
pmts=ts(facebook$Close,frequency=1)
# acf(diff(diff(pmts)),100,main="ACF")
# pacf(diff(diff(pmts)),100,main="PACF")
# spectrum(diff(diff(pmts)))
# a=spec.pgram(diff(diff(pmts)),taper=0,log="no",main="Raw Periodogram")
testarma=Arima(pmts,order=c(9,2,2))
pmdarmaforecast=forecast.Arima(testarma,h=27)
par(mfrow=c(2,2))
#plot.forecast(pmdarmaforecast,ylim=c(70,130),main="Facebook Forecast")

facebook_real = as.data.frame(get.hist.quote(instrument='FB',start='2014-09-19',end='2016-4-29'))
#plot.ts(facebook_real$Close,ylim=c(70,130),main="Facebook real data")

mse_facebook = sum((pmdarmaforecast$mean[1:20]-facebook_real$Close[380:399])^2)/20

pmts_l=ts(linkedin$Close,frequency=1)
# par(mfrow=c(2,2))
# # acf(pmts_l,100)
# acf(diff(diff(pmts_l)),100,main="ACF")
# pacf(diff(diff(pmts_l)),100,main="PACF")
# spectrum(pmts_l)
# a=spec.pgram(diff(pmts_l),taper=0,log="no",main="Raw Periodogram")

testarma_l=Arima(pmts_l,order=c(7,1,2))
pmdarmaforecast_l=forecast.Arima(testarma_l,h=25)
# par(mfrow=c(2,1))
#plot.forecast(pmdarmaforecast_l,main="Linkedin Forecast")

linkedin_real = as.data.frame(get.hist.quote(instrument='LNKD',start='2014-09-19',end='2016-4-29'))

#plot.ts(linkedin_real$Close,main="Linkedin real data")

mse_linkedin = sum((pmdarmaforecast_l$mean[1:20]-linkedin_real$Close[380:399])^2)/20
```




```{r,echo=FALSE}
pmts=ts(google$Close,frequency=1)
par(mfrow=c(2,2))
# # acf(pmts,100)
# acf(diff(diff(pmts)),100)
# pacf(diff(diff(pmts)),100)
# spectrum(pmts)
# a=spec.pgram(diff(pmts),taper=0,log="no",main="Raw Periodogram")
testarma=Arima(pmts,order=c(8,0,1))
pmdarmaforecast=forecast.Arima(testarma,h=25)
# par(mfrow=c(2,2))
#plot.forecast(pmdarmaforecast,main="Google Forecast")
google_real = as.data.frame(get.hist.quote(instrument='GOOG',start='2014-09-19',end='2016-4-29'))
#plot.ts(google_real$Close,main="Google real data")
mse_google = sum((pmdarmaforecast$mean[1:20]-google_real$Close[380:399])^2)/20

pmts=ts(apple$Close,frequency=1)
# par(mfrow=c(2,2))
# # acf(pmts,100)
# acf(diff(diff(pmts)),100)
# pacf(diff(diff(pmts)),100)
# spectrum(pmts)
# a=spec.pgram(diff(pmts),taper=0,log="no",main="Raw Periodogram")
testarma=Arima(pmts,order=c(8,0,2),seasonal=list(order=c(0,1,0),period=3))
pmdarmaforecast=forecast.Arima(testarma,h=25)

#plot.forecast(pmdarmaforecast,main="Apple Forecast")
apple_real = as.data.frame(get.hist.quote(instrument='AAPL',start='2014-09-19',end='2016-4-29'))
#plot.ts(apple_real$Close,main="Apple real data")
mse_apple = sum((pmdarmaforecast$mean[1:20]-apple_real$Close[380:399])^2)/20
```

```{r, echo=FALSE}

pmts=ts(microsoft$Close,frequency=1)
par(mfrow=c(2,2))
# # acf(pmts,100)
# acf(diff(diff(pmts)),100)
# pacf(diff(diff(pmts)),100)
# spectrum(pmts)
# a=spec.pgram(diff(pmts),taper=0,log="no",main="Raw Periodogram")
testarma=Arima(pmts,order=c(10,0,1))
pmdarmaforecast=forecast.Arima(testarma,h=25)
par(mfrow=c(2,2))
#plot.forecast(pmdarmaforecast,main="Microsoft Forecast")
microsoft_real = as.data.frame(get.hist.quote(instrument='MSFT',start='2014-09-19',end='2016-4-29'))
#plot.ts(microsoft_real$Close,main="Microsoft real data")
mse_microsoft = sum((pmdarmaforecast$mean[1:20]-microsoft_real$Close[380:399])^2)/20

pmts=ts(Alibaba$Close,frequency=1)
# par(mfrow=c(2,2))
# # acf(pmts,100)
# acf(diff(diff(pmts)),100)
# pacf(diff(diff(pmts)),100)
# spectrum(pmts)
# a=spec.pgram(diff(pmts),taper=0,log="no",main="Raw Periodogram")

testarma=Arima(pmts,order=c(8,0,2),seasonal=list(order=c(0,1,0),period=3))
pmdarmaforecast=forecast.Arima(testarma,h=25)

#plot.forecast(pmdarmaforecast,main="Alibaba Forecast")
Alibaba_real = as.data.frame(get.hist.quote(instrument='BABA',start='2014-09-19',end='2016-4-29'))
#plot.ts(Alibaba_real$Close,main="Alibaba real data")
mse_Alibaba = sum((pmdarmaforecast$mean[1:20]-Alibaba_real$Close[380:399])^2)/20
```


```{r,echo=FALSE, table1, results="asis",warning=F}
suppressPackageStartupMessages(library(xtable))
options(xtable.comment = FALSE)

table = data.frame('MSE'=c(round(mse_facebook,3),round(mse_linkedin,3),round(mse_google,3),round(mse_apple,3),round(mse_microsoft,3),round(mse_Alibaba,3)))

rownames(table) = c("Facebook","LinkedIn","Google","Apple","Microsoft","Alibaba")
print(xtable(table,caption='MSE for Time Series Forecasting'))
```


## Gaussian Hidden Markov Model

A Hidden Markov Model deals with the Markov process with hidden(unobserved states). Markov chain demonstrates visible states directly. Transition probabilities are the only parameters. In a Hidden Markov Model, the states are not visible, but there are visible observations of output which depend on the hidden states. Each observation has a probability given a specific state, which is called the emission probability.

The Hidden Markov Models are broadly used in various areas like "temporal pattern recognition such as speech, handwriting, gesture recognition, part-of-speech tagging, musical score following, partial discharges and bioinformatics".[2]

In this report, Hidden Markov Model is applied to make forecasts. Firstly, from past data, we fit Gaussian Hidden Markov Model to locate some patterns. Then, we forecast tomorrow’s stock price of the variable of interest.

### Hidden Markov Model:

We build a hidden markov model as the following:  

```{r fig.width=4, fig.height=40, fig.cap='Hidden Markov Model',echo=FALSE}
library(png)
library(grid)
img <- readPNG("1.png")
 grid.raster(img)
```


Denote  
T as the number of observations  
$diff_{t}$ as the difference between closing prices of consecutive two days
$open_{t}$ as the the opening price  
$close_{t}$ as the the closing price  
$h_t$ as the highest price  
$l_t$ as the highest price  
$X_{t}, t=1,\cdots,T$ as the observation  
$Z_{t}\in \{1,2,3,4\}$ as the hidden states 

### Algorithm

Firstly, we use the Bawm-Welch algorithm to estimate the parameters for HMM (the initial
distribution $\pi$, the transition matrix T, and the emission distributions $\epsilon_i$).

Since the observations are continuous series, we set the emission distributions as multinormal ones:
\begin{eqnarray*}
\epsilon_t=p(X_t|Z_t=j)=N(X_t|\mu_{j},V_{j}), j=1,2,3,4
\end{eqnarray*}

where $\mu_j$ and $V_{j}$ are the mean vector and covariance matrix for the observations at state j.

Secondly, plugging the estimated parameters into forward algorithm, we can get $s_j(z_j)=p(x_{1:j},z_j)$ and then we can calculate:
\begin{align*}
p(x_{j+1}|x_{1:j}) &= \frac{p(x_{1:j},x_{j+1})}{p(x_{1:j})} = \sum_{z_j,z_{j+1}} \frac{p(x_{1:j},x_{j+1},z_j,z_{j+1})}{p(x_{1:j})}\\
& = \sum_{z_j,z_{j+1}}p(z_j|x_{1:j})p(z_{j+1}|z_j)p(x_{j+1}|z_{j+1})\\
& = \sum_{z_j,z_{j+1}}p(z_{j+1},z{j}|x_{1:j})p(x_{j+1}|z_{j+1})\\
& = \sum_{z_{j+1}}p(z_{j+1}|x_{1:j})p(x_{j+1}|z_{j+1})
\end{align*}

After obtaining the distribution of $x_{j+1}|x_{1:j}$, we sample 100 points and take the mean (posterior mean) as the prediction of $\hat{x_{j+1}}$.


### Implementation
#### Data
We pick seven high-tech companies: Apple, Facebook, LinkedIn, Amazon, Google, Microsoft and Alibaba, including their price related statistics for nearly seven months. 


#### Forecast and Performance
Firstly, based on previous research[1], we take opening prices, closing prices, highest prices and lowest prices as the obseravations. $X_{t}=(open_t,close_t,h_t,l_t)$. We make one day forecast based on the previous 210 days' data. 

From Viterbi Algorithm, we get the the most likely sequence of hidden states and plot them as below. x label is the date and y label is the real closing price.

```{r fig.cap='Hidden States of Amazon without Closing Price Difference',echo=FALSE}
library(png)
library(grid)
img <- readPNG("amzhs.png")
 grid.raster(img)
```

As we can see, the four price levels are caught well by the models. The forecasting is shown in the plot below. In terms of MSE(as the table shows), the performance is not that satisfying. From the plot, we can obviously found that the model failed to catch the trend of the stock. Since the model is fit on the previous data, the four hidden states correspond to the previous price level. For a given hidden state, the multinormal emission distribution $p(x_{i}|z_{i})$ has a fixed mean and variance. It's not surprising that the model can not catch the ups and downs of prices.

```{r,echo=FALSE, table2, results="asis",warning=F}
suppressPackageStartupMessages(library(xtable))
options(xtable.comment = FALSE)

table = data.frame('MSE'=c(17.911,32.219,81.718,1066.490,475.794,4.547,7.993))

rownames(table) = c("Apple","Facebook","LinkedIn","Amazon","Google","Microsoft","Alibaba")
print(xtable(table,caption='MSE for HMM without Closing Price Difference'))
```

```{r fig.width=4, fig.height=40, fig.cap='Forcasting of Amazon Stock Price without Closing Price Difference',echo=FALSE}
library(png)
library(grid)
img <- readPNG("amzf.png")
 grid.raster(img)
```


After notice that the prediction can't catch the trend, we add closing price difference $diff_{t}=clost_{t}-clost_{t-1}$ to observations $X_{t}=(diff_t, open_t,close_t,h_t,l_t)$. 

Under this model setting, $X_{t}$ is correalted with $X_{t-1}$ since the relationship: $diff_{t}=clost_{t}-clost_{t-1}$

```{r fig.width=4, fig.height=40, fig.cap='Hidden Markov Model with Correlaetd Observations',echo=FALSE}
library(png)
library(grid)
img <- readPNG("2.png")
 grid.raster(img)
```

```{r fig.cap='Hidden States of Amazon with Closing Price Difference',echo=FALSE}
library(png)
library(grid)
img <- readPNG("amzhsd.png")
 grid.raster(img)
```

The MSE and forecasting plot is shown below. In terms of MSE, the performance is better and the trend can be caught to some degree.

```{r,echo=FALSE, table3, results="asis",warning=F}
suppressPackageStartupMessages(library(xtable))
options(xtable.comment = FALSE)

table = data.frame('MSE'=c(11.177,17.369,36.956,671.679,397.122,3.190,3.345))

rownames(table) = c("Apple","Facebook","LinkedIn","Amazon","Google","Microsoft","Alibaba")
print(xtable(table,caption='MSE for HMM with Closing Price Difference'))
```

```{r fig.width=4, fig.height=40, fig.cap='Forcasting of Amazon Stock Price without Closing Price Difference',echo=FALSE}
library(png)
library(grid)
img <- readPNG("amzfd.png")
 grid.raster(img)
```


## Investment Strategy
Using MSE as the evaluation of the forecasting performance might not be that reasonable. The most important goal is to transfer the results of forecasting to profits.

Assume a naive investment strategy used to evaluate whether the prediction is accurate and meaningful. Suppose the current day’s closed price of one stock and the prediction of its tomorrow’s closed price are known. For example, if one stock’s price is projected to increase 3% on the next day, 3 shares of this stock will be purchased. If it is projected to decrease 2%, 2 shares will be sold out. Calculate the daily return with the real closed price. Finally, calculate the annualized returns for these 20 days. The result is shown in the table.


```{r,echo=FALSE}
apple1 = as.data.frame(get.hist.quote(instrument='AAPL',start='2016-03-31',end='2016-04-29'))
apple1$Date = as.Date(rownames(apple1))
facebook1 = as.data.frame(get.hist.quote(instrument='FB',start='2016-03-31',end='2016-04-29'))
facebook1$Date = as.Date(rownames(facebook1))
linkedin1 = as.data.frame(get.hist.quote(instrument='LNKD',start='2016-03-31',end='2016-04-29'))
linkedin1$Date = as.Date(rownames(linkedin1))
linkedin1$PriceDiff = linkedin1$Close - linkedin1$Open
amazon1 = as.data.frame(get.hist.quote(instrument='AMZN',start='2016-03-31',end='2016-04-29'))
amazon1$Date = as.Date(rownames(amazon1))
google1 = as.data.frame(get.hist.quote(instrument='GOOG',start='2016-03-31',end='2016-04-29'))
google1$Date = as.Date(rownames(google1))
microsoft1 = as.data.frame(get.hist.quote(instrument='MSFT',start='2016-03-31',end='2016-04-29'))
microsoft1$Date = as.Date(rownames(microsoft1))

pred_new_data <- read.csv("predict_hmm_5.csv")

prof = NULL

pred_new <- as.numeric(rev(pred_new_data$MSFT))
profit <- 0
for (i in 1:21){
  percent <- (pred_new[i]-microsoft1$Close[i])/microsoft1$Close[i]*100
  profit <- profit + percent*(microsoft1$Close[i+1]-microsoft1$Close[i])/microsoft1$Close[i]*100/20*300
}
prof = c(prof,profit)

pred_new <- as.numeric(rev(pred_new_data$AMZN))
profit <- 0
for (i in 1:21){
  percent <- (pred_new[i]-amazon1$Close[i])/amazon1$Close[i]*100
  profit <- profit +percent*(amazon1$Close[i+1]-amazon1$Close[i])/amazon1$Close[i]*100/20*300
}
prof = c(prof,profit)

pred_new <- rev(pred_new_data$AAPL)
profit <- 0
for (i in 1:21){
  percent <- (pred_new[i]-apple1$Close[i])/apple1$Close[i]*100
  profit <- profit + 
    percent*(apple1$Close[i+1]-apple1$Close[i])/apple1$Close[i]*100/20*300
}
prof = c(prof,profit)
#(pred_new-apple1$Close[1:21])/apple1$Close[1:21]*100

pred_new <- rev(pred_new_data$FB)
profit <- 0
for (i in 1:21){
  percent <- (pred_new[i]-facebook1$Close[i])/facebook1$Close[i]*100
  profit <- profit +percent*(facebook1$Close[i+1]-facebook1$Close[i])/facebook1$Close[i]*100/20*300
}
prof = c(prof,profit)
#(pred_new-facebook1$Close[1:21])/facebook1$Close[1:21]*100


pred_new <- rev(pred_new_data$GOOG)
profit <- 0
for (i in 1:21){
  percent <- (pred_new[i]-google1$Close[i])/google1$Close[i]*100
  profit <- profit + percent*(google1$Close[i+1]-google1$Close[i])/google1$Close[i]*100/20*300
}
prof = c(prof,profit)


pred_new <- rev(pred_new_data$LNKD)
profit <- 0
for (i in 1:21){
  percent <- (pred_new[i]-linkedin1$Close[i])/linkedin1$Close[i]*100
  profit <- profit + percent*(linkedin1$Close[i+1]-linkedin1$Close[i])*100/20*360
}
prof = c(prof,profit)
```

```{r,echo=FALSE, table4, results="asis",warning=F}
suppressPackageStartupMessages(library(xtable))
options(xtable.comment = FALSE)

table = data.frame('Revenue'= prof)

rownames(table) = c("Microsoft","Amazon","Apple","Facebook","Google","LinkedIn")
print(xtable(table,caption='The Invesment Revenue'))
```
For HMM model, the investment will turn into positive returns on 3 stocks and negative returns on 2 stocks. The profits from these 3 stocks are notably higher than the loss from those 2 stocks. Therefore, if a good portfolio is designed, profits can be ensured from the prediction, which indicates that the prediction is informative and useful.

For time series model, sometimes the investment will earn a large profit and sometimes may make a great loss. This prediction is not so satisfying because of great risk and the expected returns are less than that of HMM model.

The reason may be because HMM model takes more variables into account than time series, such as open price, high price, low price and price difference. It has been proved by Neural Network that closed price is indeed influenced by these 4 factors. Moreover, HMM model predicts one day in one prediction while time series predicts 20 days in one prediction. Long time prediction may lead to more discrepancy.

## Neural Network

Before traning the models, the data is normalized before being input to the ANN. We have guaranteed the input vectors of the training data to have zero-mean and unit variance, while considering the activation function may be Unipolar sigmoidm the target values are normalized between 0 and 1. We could also normalized them to between -1 and 1 and 0 and
$\frac{1}{\sqrt{2\pi}\sigma}$ when the activation function is Bipolar sigmoid, Tan hyperbolic or RBF, but these goes beyond the scope of this report and we stick to the Unipolar sigmoidm activation function for "simplicity". Similarly, the test data vector is also scaled by the same factors with which the training data was normalized. The output values from the ANN for this test vector are also scaled back with the same factor as the target values for the training data and plotted together with the real data for comparison.\newline

In spite of all the features mentioned for neural networks, building a neural network for prediction is somehow complicated. In order to have a satisfactory performance, we must consider some crucial factors in designing of such a prediction model. One of the main factors is the network structure including number of layers, neurons, and the connections. 
In the two proposed models, we followed the empirical rules when choosing the number of layer and neurons and test the model based on error.\newline

In the first model, the inputs to the neural networks are the lowest, the highest and the open values in the d previous days. In the second model, the same variables are taken from the past few d days such that the data can be seen as longitudinal.\newline



1. The first Neural Network model takes the data from a fixed period of time(January and Feburary). The estimated network is shown as below.\newline

```{r fig.width=4, fig.height=40, fig.cap='The First Neural Network Model',echo=FALSE}
library(png)
library(grid)
img <- readPNG("NN.png")
 grid.raster(img)
```


With this training model, we put in the test data with the same variables for prediction(March and April). The true data is labeled as the black line and the red line is the predicted values. The pattern can be catched very well, but there may be more improvement.

```{r,echo=FALSE,fig.width=6, fig.height=4, fig.cap='Prediction based on Test data(fixed period)'}
close1 = tail(amazon$Close,60)
open1 = tail(amazon$Open,60)
high1 = tail(amazon$High,60)
low1 = tail(amazon$Low,60)

close2 = amazon$Close[275:334]
open2 = amazon$Open[275:334]
high2 = amazon$High[275:334]
low2 = amazon$Low[275:334]


test_data <- data.frame(cbind(close1,open1,high1,low1))
colnames(test_data) <- c("close","open","high","low")
train_data <- data.frame(cbind(close2,open2,high2,low2))
colnames(train_data) <- c("close","open","high","low")

#You can choose different methods to scale the data (z-normalization, min-max scale, etc…).
#I chose to use the min-max method and scale the data in the interval [0,1].
#Usually scaling in the intervals [0,1] or [-1,1] tends to give better results.
maxs <- apply(train_data, 2, max)
mins <- apply(train_data, 2, min)
scaled_train <- as.data.frame(scale(train_data, center = mins, scale = maxs - mins))
maxs <- apply(test_data, 2, max)
mins <- apply(test_data, 2, min)
scaled_test <- as.data.frame(scale(test_data, center = mins, scale = maxs - mins))

#Train nn with scaled training data
set.seed(12)
net_amazon_last <- neuralnet(close ~(open+high+low),
                               scaled_train, err.fct = "sse", hidden = c(5,2),
                            threshold=0.001,linear.output = TRUE)
#plot(net_amazon_last)

net.results_last <- compute(net_amazon_last, scaled_train[,2:4])
pr.nn_last <- net.results_last$net.result
#plot.ts(train_data$lastclose1,ylab="lastclose1")
# par(new=TRUE)
#plot.ts(pr.nn_last,col='red',yaxt="n",ylab="")

net.results_last <- compute(net_amazon_last, scaled_test[,2:4])
pr.nn_last <- net.results_last$net.result
plot.ts(test_data$close,ylab="Amazon Close Price", main="Prediction based on Test data(fixed period)")
# plot.ts(train_data$lastclose1)
par(new=TRUE)
plot.ts(pr.nn_last,col='red',yaxt="n",ylab="")
```

Next, we try to analyze in terms of longitudinal data to improve prediction. The input will be all the variables in the past 3 periods of time (60 days). The best trained network is show as below. 

```{r fig.width=4, fig.height=40, fig.cap='The Second Neural Network Model',echo=FALSE}
library(png)
library(grid)
img <- readPNG("NN1.png")
 grid.raster(img)
```

This time the red and black lines are almost overlapping. Thus by taking into account the data across a relatively long period of time, the prediction accuracy has been improved to a great extent.


```{r,echo=FALSE,fig.width=6, fig.height=4, fig.cap='Prediction based on past data(longitudinal)'}
lastclose1 = tail(amazon$Close,60)
lastopen1 = tail(amazon$Open,60)
lasthigh1 = tail(amazon$High,60)
lastlow1 = tail(amazon$Low,60)

lastclose2 = amazon$Close[275:334]
lastopen2 = amazon$Open[275:334]
lasthigh2 = amazon$High[275:334]
lastlow2 = amazon$Low[275:334]

lastclose3 = amazon$Close[215:274]
lastopen3 = amazon$Open[215:274]
lasthigh3 = amazon$High[215:274]
lastlow3 = amazon$Low[215:274]

lastclose4 = amazon$Close[155:214]
lastopen4 = amazon$Open[155:214]
lasthigh4 = amazon$High[155:214]
lastlow4 = amazon$Low[155:214]

lastclose5 = amazon$Close[95:154]
lastopen5 = amazon$Open[95:154]
lasthigh5 = amazon$High[95:154]
lastlow5 = amazon$Low[95:154]


test_data <- data.frame(cbind(lastclose1,lastopen1,lasthigh1,lastlow1,
                               lastclose2,lastopen2,lasthigh2,lastlow2,
                               lastclose3,lastopen3,lasthigh3,lastlow3,
                               lastclose4,lastopen4,lasthigh4,lastlow4
                               ))

train_data <- data.frame(cbind(lastclose2,lastopen2,lasthigh2,lastlow2,
                         lastclose3,lastopen3,lasthigh3,lastlow3,
                         lastclose4,lastopen4,lasthigh4,lastlow4,
                         lastclose5,lastopen5,lasthigh5,lastlow5))
colnames(train_data) <- c("lastclose1","lastopen1","lasthigh1","lastlow1",
                          "lastclose2","lastopen2","lasthigh2","lastlow2",
                          "lastclose3","lastopen3","lasthigh3","lastlow3",
                          "lastclose4","lastopen4","lasthigh4","lastlow4")
#You can choose different methods to scale the data (z-normalization, min-max scale, etc…).
#I chose to use the min-max method and scale the data in the interval [0,1].
#Usually scaling in the intervals [0,1] or [-1,1] tends to give better results.
maxs <- apply(train_data, 2, max)
mins <- apply(train_data, 2, min)
scaled_train <- as.data.frame(scale(train_data, center = mins, scale = maxs - mins))
maxs <- apply(test_data, 2, max)
mins <- apply(test_data, 2, min)
scaled_test <- as.data.frame(scale(test_data, center = mins, scale = maxs - mins))

#Train nn with scaled training data
set.seed(777)
net_amazon_last <- neuralnet(lastclose1 ~(lastclose2+lastopen2+lasthigh2+lastlow2+
                                        lastclose3+lastopen3+lasthigh3+lastlow3+
                                         lastclose4+lastopen4+lasthigh4+lastlow4),
                               scaled_test, err.fct = "sse", hidden = c(14,3),
                            threshold=0.001,likelihood = TRUE,linear.output = TRUE)

net.results_last <- compute(net_amazon_last, scaled_test[,5:16])
pr.nn_last <- net.results_last$net.result
plot.ts(test_data$lastclose1,ylab="Amazon Close Price", main="Prediction based on past data(longitudinal)")
par(new=TRUE)
plot.ts(pr.nn_last,col='red',yaxt="n",ylab="")
```

```{r,echo=FALSE,eval=FALSE}
net.results_last <- compute(net_amazon_last, scaled_test[,5:16])
pr.nn_last <- net.results_last$net.result
plot.ts(test_data$lastclose1,ylab="lastclose1")
# plot.ts(train_data$lastclose1)
par(new=TRUE)
plot.ts(pr.nn_last,col='red',yaxt="n",ylab="")

# plot(scaled_test$lastclose1,pr.nn_last,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
# abline(0,1,lwd=2)
# legend('bottomright',legend='NN',pch=18,col='red', bty='n')
```


Neural networks (NN) can assist in finding a relationship, or mapping, between inputs (Open, Low and High price) and an output (Closing price). Neural Networks have the advantage that can approximate any nonlinear functions without any apriori information about the properties of the data series. Comparing with traditional methods such as linear regression and logistic regression which are model based, Neural Networks are self-adjusting methods based on training data, so they have the ability to solve the problem with a little knowledge about its model and without constraining the prediction model by adding any extra assumptions. Besides, neural networks can find the relationship between the input and output of the system even if this relationship might be very complicated because they are general function approximators. Consequently, neural networks are well applied to the problems in which extracting the relationships among data is really difficult but on the other hand there exists a large enough training data sets. It should be mentioned that, although sometimes the rules or patterns that we are looking for might not be easily found or the data could be corrupted due to the process or measurement noise of the system, it is still believed that the inductive learning or data driven methods are the best way to deal with real world prediction problems. Furthermore, Neural Networks have generalization ability. After training, they can recognize the new patterns even if they haven’t been in training set. Since in most of the pattern recognition problems predicting future events (unseen data) is based on previous data (training set), the application of neural networks would be very beneficial. 

The application of neural networks in prediction problems is very promising due to some of their special characteristics. But we still need to be clear that the model is mostly strong in explaining about the prediction rather than forecasting. By  implementing neural networks model in this report, it is further proved that including more variables other than merely the Closing price can be helpful. Also, it also shows a strong potential of making forecasting if we are able to get the required variables. Though this may require a complete reconstruction of the data frames and complexity of computation, it can be a promising model worth optimizing. 

## Conlusion

## Reference
[1] Hassan, Md Rafiul, and Baikunth Nath. "Stock market forecasting using hidden Markov model: a new approach." Intelligent Systems Design and Applications, 2005. ISDA'05. Proceedings. 5th International Conference on. IEEE, 2005.

[2] https://en.wikipedia.org/wiki/Hidden_Markov_model

\newpage

## Attachment

### Time series forecast plots: 

```{r,echo=FALSE}
pmts=ts(facebook$Close,frequency=1)
# acf(diff(diff(pmts)),100,main="ACF")
# pacf(diff(diff(pmts)),100,main="PACF")
# spectrum(diff(diff(pmts)))
# a=spec.pgram(diff(diff(pmts)),taper=0,log="no",main="Raw Periodogram")
testarma=Arima(pmts,order=c(9,2,2))
pmdarmaforecast=forecast.Arima(testarma,h=27)
par(mfrow=c(2,2))
plot.forecast(pmdarmaforecast,ylim=c(70,130),main="Facebook Forecast")

facebook_real = as.data.frame(get.hist.quote(instrument='FB',start='2014-09-19',end='2016-4-29'))
plot.ts(facebook_real$Close,ylim=c(70,130),main="Facebook real data")

mse_facebook = sum((pmdarmaforecast$mean[1:20]-facebook_real$Close[380:399])^2)/20

pmts_l=ts(linkedin$Close,frequency=1)
# par(mfrow=c(2,2))
# # acf(pmts_l,100)
# acf(diff(diff(pmts_l)),100,main="ACF")
# pacf(diff(diff(pmts_l)),100,main="PACF")
# spectrum(pmts_l)
# a=spec.pgram(diff(pmts_l),taper=0,log="no",main="Raw Periodogram")

testarma_l=Arima(pmts_l,order=c(7,1,2))
pmdarmaforecast_l=forecast.Arima(testarma_l,h=25)
# par(mfrow=c(2,1))
plot.forecast(pmdarmaforecast_l,main="Linkedin Forecast")

linkedin_real = as.data.frame(get.hist.quote(instrument='LNKD',start='2014-09-19',end='2016-4-29'))

plot.ts(linkedin_real$Close,main="Linkedin real data")

mse_linkedin = sum((pmdarmaforecast_l$mean[1:20]-linkedin_real$Close[380:399])^2)/20
```

```{r,echo=FALSE}
pmts=ts(google$Close,frequency=1)
par(mfrow=c(2,2))
# # acf(pmts,100)
# acf(diff(diff(pmts)),100)
# pacf(diff(diff(pmts)),100)
# spectrum(pmts)
# a=spec.pgram(diff(pmts),taper=0,log="no",main="Raw Periodogram")
testarma=Arima(pmts,order=c(8,0,1))
pmdarmaforecast=forecast.Arima(testarma,h=25)
# par(mfrow=c(2,2))
plot.forecast(pmdarmaforecast,main="Google Forecast")
google_real = as.data.frame(get.hist.quote(instrument='GOOG',start='2014-09-19',end='2016-4-29'))
plot.ts(google_real$Close,main="Google real data")
mse_google = sum((pmdarmaforecast$mean[1:20]-google_real$Close[380:399])^2)/20

pmts=ts(apple$Close,frequency=1)
# par(mfrow=c(2,2))
# # acf(pmts,100)
# acf(diff(diff(pmts)),100)
# pacf(diff(diff(pmts)),100)
# spectrum(pmts)
# a=spec.pgram(diff(pmts),taper=0,log="no",main="Raw Periodogram")
testarma=Arima(pmts,order=c(8,0,2),seasonal=list(order=c(0,1,0),period=3))
pmdarmaforecast=forecast.Arima(testarma,h=25)

plot.forecast(pmdarmaforecast,main="Apple Forecast")
apple_real = as.data.frame(get.hist.quote(instrument='AAPL',start='2014-09-19',end='2016-4-29'))
plot.ts(apple_real$Close,main="Apple real data")
mse_apple = sum((pmdarmaforecast$mean[1:20]-apple_real$Close[380:399])^2)/20
```

```{r, echo=FALSE}
pmts=ts(microsoft$Close,frequency=1)
par(mfrow=c(2,2))
# # acf(pmts,100)
# acf(diff(diff(pmts)),100)
# pacf(diff(diff(pmts)),100)
# spectrum(pmts)
# a=spec.pgram(diff(pmts),taper=0,log="no",main="Raw Periodogram")
testarma=Arima(pmts,order=c(10,0,1))
pmdarmaforecast=forecast.Arima(testarma,h=25)
par(mfrow=c(2,2))
plot.forecast(pmdarmaforecast,main="Microsoft Forecast")
microsoft_real = as.data.frame(get.hist.quote(instrument='MSFT',start='2014-09-19',end='2016-4-29'))
plot.ts(microsoft_real$Close,main="Microsoft real data")
mse_microsoft = sum((pmdarmaforecast$mean[1:20]-microsoft_real$Close[380:399])^2)/20

pmts=ts(Alibaba$Close,frequency=1)
# par(mfrow=c(2,2))
# # acf(pmts,100)
# acf(diff(diff(pmts)),100)
# pacf(diff(diff(pmts)),100)
# spectrum(pmts)
# a=spec.pgram(diff(pmts),taper=0,log="no",main="Raw Periodogram")

testarma=Arima(pmts,order=c(8,0,2),seasonal=list(order=c(0,1,0),period=3))
pmdarmaforecast=forecast.Arima(testarma,h=25)

plot.forecast(pmdarmaforecast,main="Alibaba Forecast")
Alibaba_real = as.data.frame(get.hist.quote(instrument='BABA',start='2014-09-19',end='2016-4-29'))
plot.ts(Alibaba_real$Close,main="Alibaba real data")
mse_Alibaba = sum((pmdarmaforecast$mean[1:20]-Alibaba_real$Close[380:399])^2)/20
```
