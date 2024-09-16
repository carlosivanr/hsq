# Helper Stay Quit ID (hsqid) - unique project specific identifier
# Capture the hsq ids of those in arm 1 where HSQ training phase is beginning
# of participation
# patients are either at beginning of participation or at end of participation
# *** As of 10/23/2023 there are 59 patients at the beginning of participation
# *** How is it determined that they are at the beginning of participation?
patient_ids <-
  patsurveydta %>%
  filter(arm == "1-  HSQ training - beginning of participation") %>%
  select(hsqid)
