# Swing_Decision_Function
This takes a dataframe with statcast data in it and determines how good of a swing decision the batter made. 

The code will add a handful of columns to the end of the dataframe.

Make sure to download all rds files and put them in your working directory so you have the models to predict events. 


You can find visualizations of the outputs at: https://phildepase.shinyapps.io/Test_app/


Limitations of the function:

1.) Does not take into a ccount each individual batters strengths and weaknesses. It assesses how well the league as a whole performs against a pitch as opposed to the individual player. 

2.) Run Values based on 2023 run values of events

3.) Understand that a C Grade on a swing decision is still a good grade. Largely C's are correct swing decisions but they are easy decisions, so takes on pitches not close to the zone that the league rarely swings at anyways.
