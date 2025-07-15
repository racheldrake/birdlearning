## Effects of Merlin on the eBird dataset

### Repository layout
Processing scripts are labelled in sequential order and outputs are uploaded to run the analysis scripts.
Numerical labels on processing scripts are arranged as follows:
00 - large data proessing and API pulls
01 - eBird dataset processing
02 - Supplementary dataset processing
03 - Calulating observer species accumualaton curves and checklist calibration indices
04 - Calcualting observer reporting rates
05 - Anonymising analysis data

### Data downloads
Data for processing scripts should be downloaded centrally from eBird. 
Supplementary data can be accessed as follows:
- Landcover covariates: these are downloaded in script from the Luna package
- MSID information: all requests for access should be sent through https://support.ebird.org/en/support/tickets/new
- Audio Visual Index: the overall summary data has been uploaded here. For more detailed audio-visual indices (e.g. subset spatiotemporally) please contact Rachel (rld3@st-andrews.ac.uk)


### Data anonymisation
Information about the use of Merlin sound identification within checklists is anonymised at the point of upload such that no individual could be identified from the data. De-anonymised data for the processing scripts is not available, neither is unaggregated checklist information as this contains potentially identifying information. For any further queries please contact Rachel (rld3@st-andrews.ac.uk)

### Running the code
Code is formatted such that all variables that need changed are at the top of the script, I don't recommend changing any of this because I'm not 100% certain
that the folder tagging is the same throughout though I've tried to comment them as helpfully as possible.

I highly recommend downloading the repository and running the analysis sccripts without changing anything in the headers for the smoothest experience.


### Acknowledgements
Analyses are completed by Rachel Drake, with supervision from Alison Johnston and David Borchers.
