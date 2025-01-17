File descriptions:

qualitative_diagrams.Rmd: (Deprecated)
Initially the first script written to generated the diagrams for the
qualitative sub-study. The qualitative diagrams represent the same 5 diagrams
that are generated at baseline through HSQ Network Diagram.Rmd file. 
qualitative_diagrams.Rmd generates the same files, except it uploads them to
separate fields in the RedCap project.

qualitative_diagrams.R: An R script version instead of the 
qualitative_diagrams.Rmd script. The R version was created to execute on an 
automatic basis using task scheduler. 

run_qualitative_diagrams.bat: A .bat script that is used to automatically run
the qualitative_diagrams.R script every Monday at 8:00AM. Task scheduler was
set up on HPC18.

HSQ qualitative dyads.qmd: After all of the qualitative diagrams are generated
and uploaded. This script will produce an excel table listing patient contact
information and the condition they were randomized to. These excel files are 
then sent to the team for contacting patients to see if they want to 
participate in the qualitative substudy.
