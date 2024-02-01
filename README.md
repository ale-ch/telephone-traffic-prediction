# Telephone traffic prediction

The marketing department of a telecommunications company is interested in analyzing each customer's behavior regarding their telephone traffic.
We want to address the problem of predicting telephone traffic for the next month using the available data up to today. The response variable chosen is the total number of seconds of outgoing calls made in a given month.

The training set refers to 15,310 customers for whom information on a total of 99 variables is recorded. Some of these are customer characteristics (e.g., gender and age) or specific relationship between the customer and the company (e.g., activation date or presence of any additional services), and some relate to traffic information recorded for each of the previous 9 consecutive months before the month of interest. Finally, there is the variable related to the total duration of outgoing calls in the tenth month, which we consider as the response variable.

Predictions on the test set will be evaluated with the following command:

sum( ( log1p(test$y) - log1p(yhat) )^2 )

where test$y is the response variable of the test set and yhat is the provided prediction.

Variable          Description 

tariff.plan       customer's tariff plan (factor, 5 levels)
payment.method    payment method (factor, 3 levels: postal account, credit card, bank direct debit)  
gender            customer's gender (factor, 3 levels: M-male, F-female, B-legal entity)
age               age (years)
activ.zone        activation geographic zone (factor, 4 levels)  
activ.chan        sales channel for activation (factor, 8 levels) 
vas1              presence of a first value-added service 
vas2              presence of a second value-added service  

Variables related to traffic in the 9 available months. For each month, indicated by the first part of the variable name (q01, q02, ..., q9), the following variables are available:

Variable              Description 

q_nn_.out.ch.peak     total monthly number of outgoing calls 
                      during peak hours
q_nn_.out.dur.peak    total monthly duration of outgoing calls 
                      during peak hours
q_nn_.out.val.peak    total monthly value of outgoing calls 
                      during peak hours
q_nn_.out.ch.offpeak  total monthly number of outgoing calls 
                      during off-peak hours
q_nn_.out.dur.offpeak total monthly duration of outgoing calls 
                      during off-peak hours
q_nn_.out.val.offpeak total monthly value of outgoing calls 
                      during off-peak hours
q_nn_.in.ch.tot       total monthly number of incoming calls 
q_nn_.in.dur.tot      total monthly duration of incoming calls 
q_nn_.ch.sms          total monthly number of SMS sent
q_nn_.ch.cc           monthly number of calls to Customer 
                      Service

Response variable y = q10.out.dur.peak + q10.out.dur.offpeak

Status variable which is the indicator variable 
of possible deactivation in the thirteenth month, i.e., 
three months after the last month for which traffic is available 
(factor, 2 levels: 1--deactivated, 0--active).