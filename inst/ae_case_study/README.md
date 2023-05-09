# AE Case Study

This folder contains two `.R` scripts that simulate an Adverse Event assessment (for kri0001) that shows sites as being flagged High and Low. 

The purpose of this case study is to be able to use in documentation, presentations, and other materials to help to explain the breadth of RBQM efforts at Gilead. This includes architecture and analytics packages such as Gismo, `{gsm}`, `{rbm-viz}`, and `{grail}`. 


## Setup

  - The file `01-simulate-data.R` simulates an Adverse Event dataset which is saved as `rawplus_ae_demo.Rda`.
  - The file `02-make-snapshots.R` simulates longitudinal data. The longitudinal data is used in `{gsm}` to create timeseries charts, which shows AE rates over time.
  
The start and end dates for a snapshot can be modified in the `02-make-snapshots.R` file:

```
start_date <- '2010-06-01'
end_date <- '2010-12-01'
```

To modify the AE rates, or to add more assessments to the case study, you will need to modify `01-simulate-data.R`.

