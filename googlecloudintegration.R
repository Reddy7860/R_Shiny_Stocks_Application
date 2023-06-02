library("googleComputeEngineR")

project <- "reddy000-c898c"
zone <- "asia-south1-a"
account_key <- "GoogleCloudReddy_Client_Secret.json"

Sys.setenv(GCE_AUTH_FILE = account_key,
           GCE_DEFAULT_PROJECT = project,
           GCE_DEFAULT_ZONE = zone)

gce_auth()

GCE_AUTH_FILE = "~/Desktop/Reddy_Stocks_Application/GoogleCloudReddy_Client_Secret.json"
GCE_DEFAULT_PROJECT_ID = "reddy000-c898c"
GCE_DEFAULT_ZONE = "asia-south1-a"

gce_get_project()

Sys.setenv("GCE_AUTH_FILE" = "~/Desktop/Reddy_Stocks_Application/GoogleCloudReddy_Client_Secret.json")

library(googleComputeEngineR)

gce_get_project("reddy000-c898c")

library(googlesheets)
gs_auth()

