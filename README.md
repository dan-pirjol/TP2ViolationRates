# TP2ViolationRates
Code for computing TP2 violation rates

The code *loadOptionChainsOptionMetricFormat.R* loads all option chains on a given ticker on the current date from Yahoo Finance and saves them to a csv file in OptionMetrics format.

<img width="808" height="126" alt="allDataFormat" src="https://github.com/user-attachments/assets/446cbbe8-512a-4fdb-be06-8f54e2b65d8c" />

This data can be read in using the functions in *dataFunctionsYFOMFixedTimpVol.R*. A sample usage of these functions is in the file *ListOptionPrices.R*

The function **dataCall(dataOneDay,dateExpiry)** loads all calls with a given expiration date and formats them as a data.frame.

<img width="342" height="210" alt="DataCall" src="https://github.com/user-attachments/assets/1587bb59-88e3-4c23-868a-483933a1161e" />

The function **dataPut(dataOneDay,dateExpiry)** loads all puts and formats them in a similar format.

<img width="303" height="214" alt="DataPut" src="https://github.com/user-attachments/assets/702361f6-4c37-4945-8470-263a380866a1" />
