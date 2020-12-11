# Causal-Inference
The continuous trade war between the U.S and China dates back to the early 2000s when American leaders and economic scholars questioned the US trade deficit with China. The trade war’s impact remains largely controversial considering the level of media bias. A suitable hypothesis statement for the study can be that the effect of the trade war on U.S Corporations profitability is significant.
Therefore, the objective of this research paper is to identify the effect of the trade war on the overall profitability of U.S corporations and the impact at the industry level. 
## The China-US trade war
The main problem behind the Sino-US trade war is not just trade imbalances, but China's status as a non-market economy preceived as a "threat" by the United States. There is no doubt that the trade war has led to a decline in bilateral trade, which has adversely affected the export-oriented enterprises of both sides. 
#### Traditional Approach
In terms of exploring the effects of a certain policy, the difference-in-difference regression model is widely used in the empirical analysis of policy mechanisms.

Limitations to DiD:
1. The model assumes independent and identical data distribution despite the fact that the design has a temporal component, so the correlated data causes narrow uncertainty levels. 
2. The DID model only considers two time points: before and after the intervention. The time effect after intervention, especially its onset and decay structure, is an unanswered key question. 

#### A Modern Approach
Bayesian Time series method generalises the DID approach to the time-series setting by modelling the impact on dependent variables observed both before and post intervention. This method is an improvement on the widely used DID approach because of two important points:
1. Provides a fully Bayesian time-series estimate for the effect
2. Averages model to construct the most appropriate synthetic control for modelling the unobserved.

## Data
Three profitability ratios are used as the impact measurement variable,
* Return on assets(ROA)
* Return on equity(ROE)
* Gross profit margin(GPM)

Data from Compustat-Capital IQ Global is downloaded for listed companies in the United States, Australia, and Taiwan on a quarterly basis from 2016 Q1 to 2019 Q4.

## Method

#### Difference in Difference
A descriptive analysis is conducted to view the general trend of  U.S Companies profitability and to examine the parallel trend of the treatment group and control group in the pre-period.
```R
parallel_trend = function(Data){
Date <- as.character(`Data Date`)
agg_data <- aggregate(Performance_Metric~ Date +USA,Data,mean)
agg_data$treatment <- as.factor(agg_data$USA)
agg_data$date1 <- ymd(agg_data$Date )
ggplot(data=agg_data, aes(x=date1, y=ROE, group=treatment)) + geom_line(aes(linetype=treatment, 
`color=treatment))+geom_point(aes(color=treatment))+theme(legend.position="top")+
 geom_vline(xintercept = as.numeric(as.Date("Intervention Date")))}
```
In terms of the parallel trend examination, Australia was identified as the best control group since it has the lowest variation in % difference of ROA and shows the most parallel trend relative to the U.S before the intervention period.
Fit a Differnece in Difference Model and on Industry level identified by GIC codes.
```R
model=lm(ROA~USA*Post,ausdata)
summary(model)
stargazer(model, type='text')

industry.ROA=function(a){
  data1=subset(ausdata,`GIC Sectors`==a)
  model=lm(ROA~USA*Post,data1)
  return(stargazer(model, type='text'))
}
```
#### Bayesian Time Series
A Bayesian model is fit using the Causal Impact R package. It designed such as it is as easy to use as regression, yet powerful, given the assumptions are met. 
To fit the model, the package is called into action by function CausalImpact().

```R
#define intervention period here
pre.period <-as.Date(c("2016-03-31","2018-07-31"))
post.period <-as.Date(c("2018-08-01","2019-12-31"))
#fit the causal impact model based on bayesian time series approach
impact_overall <-CausalImpact(data, pre.period, post.period)
```

```R
#function to wrangle data to measure impact by industries.
industry.ROA_CI=function(a){
  data1 = filter(usadata, usadata$'GIC Sectors' == a)
  data_ctrl = filter(ausonly, ausonly$'GIC Sectors' == a)
  US_GIC_ROA <-ts(data1$ROA)
  US_Control_ROA <-ts(data_ctrl$ROA)
  data_sector <-na.trim(zoo(cbind(US_GIC_ROA,US_Control_ROA),time.points))
  impact <-CausalImpact(data_sector, pre.period, post.period)
  summary(impact)
  return(plot(impact))
}
```
## Results
The absolute effect is the difference in ROA between the actual ROA after the treatment and the predicted ROA. 
#### Difference in Difference
The estimate of the interaction variable indicates that the ROA of U.S listed companies increased by 0.5 units because of the U.S-China trade war.
#### Causal_Impact
An absolute effect of 0.061 was observed which means that the ROA increased by 0.061 units, i.e., 65% because of the trade war. 
In conclusion, both models show the positive impact of trade war on ROA.

## References
[1] CausalImpact version 1.0.3, Brodersen et al., Annals of Applied Statistics (2015).

[2] Difference-in-differences methods in public finance. Clair, T. S., & Cook, T. D. (2015).

