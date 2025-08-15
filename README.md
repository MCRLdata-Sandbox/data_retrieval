# data_retrieval

Pull available datasets on-demand. Currently, all data from 05/2021 - 05/2025 for available data types can be found in the [L1 folder](). L1 removes points automatically flagged by the MCRLdata pipeline, and may contain additional QC steps, detailed in cleaning scripts located [here](https://github.com/MCRLdata-Sandbox/data_prep/tree/main/scripts). 

🏖️ Note: If this is <ins>your first time here</ins>, or you have questions about the whys, whats, and hows, please start with the **[sandbox_intro](https://github.com/MCRLdata-Sandbox/.github/blob/main/sandbox_intro.md)**

## How do you want to retrieve data?

 - If you want to directly download L1 datasets as csv files, please visit the **[data_prep L1 folder](https://github.com/MCRLdata-Sandbox/data_prep/tree/main/data/outputs/L1)**
 - If you want to download data from Github into R, see the **[L1 data tutorial](https://github.com/MCRLdata-Sandbox/tutorials/blob/main/scripts/1_L1_data_basics.R)**
 - If you want to download data directly from AWS, check out the **[S3 retrieval code example](https://github.com/MCRLdata-Sandbox/data_retrieval/blob/main/scripts/250716_S3_retrieval_script.R)**. Note that this method requires additional steps to gain authorization to interact with AWS via R, including receiving access to MCRLdata (which incurs costs), setting up CLI access, and establishing (then re-establishing) single sign-on access.

**Disclaimer: Data QC is a relative term, all users are responsible for understanding the data quality needed for their use-case and using provided data accordingly**
