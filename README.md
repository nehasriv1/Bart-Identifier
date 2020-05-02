# Bart Identifier
An algorithm designed to identify barts in high frequency trading data.

The bart_loop.R file is an algorithim which will run through the cleaned data provided and identify starting and stopping points for any potential barts.
The file will produce one csv file for every day of data passed through the loop. The original data can be found here: https://public.bitmex.com/?prefix=data/trade/

The bart_plot.R file reads in a single file produced from the bart_loop.R file and plots the start and end of each identified bart over the day's Bitcoin futures contract price.