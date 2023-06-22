# EVO_TEST
PROBLEM AND DATA
The aim is to make a prediction and return predicted values in numeric and graphic format.
The instructions are detailed and comprehensive, without "traps" of sorts; for any question, please ask. Your target prediction variable is "qty", total quantities sold at the ITEM/DAY level. You can assume the price as known for each day.

The attached zip contains:
1) sales_data: sales (sanitized for confidentiality) per day of certain products at the ITEM/DAY/STORE level. "qty" is the number of pieces sold, "unit price" is the price paid per unit.
2) store_master: coordinates of each store

Weather data is not given, but if needed it is possible to get it from open data libraries like Global Historical Climatology Network Daily Weather Data available at https://cloud.google.com/bigquery/public-data/

DETAILED WORK STEPS - INSTRUCTIONS
1) Load data from CSVs

2) Before data analysis
Look at the numbers, and think, "top down":
- Would you use all the data?
- Would you use all the available variables or not? (eg store, category)
- Would you add other variables in the analysis?
Take a quick decision to produce your final result, so to simulate real conditions; reasoning on the approach that you would use to respond to the points above, even without doing all the calculations. Please document your choices and prepare to talk about them later.
Choose 10 items to forecast and motivate your choice properly.

3) Model data
Apply an appropriate forecasting model, for example a multi-variate regression, time series or another model of R (there are many available modules), as long as the results are a good prediction for the variable Qty. The model can account for special factors e.g. holiday effect.

4) Estimated future values
Once the model is built, make an estimate for 30 subsequent days for the 10 items you chose.
Take values ​​for future input variables (if any) that are - in your opinion - reasonable.
Obviously you miss a lot of context, so you have to make some independent assumptions (as it happens in reality); perhaps this test has a 'more extreme' lack of context, but substantially you will face a situation not much unlike in reality. Let yourself be guided by common sense and ease of implementation.

5) Error rate and self-feedback
What error measure would you choose to use to assess your own results? How well do you think your code performs? What other future adjustments or work could further improve this?

RESULTS AND OUTPUT WANTED
The forecasting results can be produced in R, Excel, Word, PDF, etc, in a simple table structured as follows:
Date - Item - Quantity (ie the result of the forecast)
