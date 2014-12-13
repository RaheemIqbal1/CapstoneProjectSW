
## Predict Next Word - Language Modeling Capstone Project    

The purpose of this project is to build a natural language model that suggests an appropriate next unseen word in the user specified words sequence. Three types of data including twitter, news and blogs were used to train the model. Appropriate data cleaning and sub-setting techniques were applied to finalize the training data. Various word combinations (N-Grams) were then created using clean data sets and a predictive algorithm (Katz Back-off) was developed to predict next word. The final predictive model was optimized appropriately to fit as a Shiny application.   

Files Included:
ProduceNGrams.R: Manage the input data and creates clean data sets. 
PredNextTerm.R: Includes Katz's back-off algorithm.   
CleanInputString.R: Filter user entered sentence.   
Test_PredNextTerm.R: Test program.   
server.R and ui.R: The Shiny app.   
fDF1.RData ...: Four compressed R input data files.



