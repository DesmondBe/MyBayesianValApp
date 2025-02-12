# MyBayesianValApp
Shiny app for analyzing bioassay validation data in Bayesian

The aim of this app are:
                
                  1. Analyze validation data of bioassay using a Bayesian approach, 
                  2. Implement what I think is a good way to do so,
                  3. A toy project to help me learn various technical tools.
                  
It is a personal project, a personal vision. You do not like it: do not use it. I am however open to any constructive criticism: please shoot: <a href='mailto:rozeteric@gmail.com'> send here ! </a>

This app propose a way to analyze bioassay validation data which is nonetheless coherent with most regulatory guidances (ICH Q2, ICH M10, USP 1033, and FDA related guidances). It is a Bayesian approach that uses Stan via brms R package: so it may take few seconds to process.

A pdf report is also available to download which includes more output than the app:
                   
                 - Precision of the bioassay over the whole range tested
                 - Linearity of the results
                 - Accuracy of the bioassay by target potency
                 - Total analytical error
For decision making about the validity of the assay a <em><b>Probability profile</em></b> that will allow to define the valid range.
                   
Data should be uploaded as csv file (comma separated) with 5 columns labelled exactly as:
                   
                 - Target : the true or target potencies;
                 - Run : the bioassay run;
                 - Res : the measured potencies;
                 - LRes : the log 10 transformed measured potencies;
                 - LTarget : the log 10 transformed true or target potencies;
                   
                   
                   
