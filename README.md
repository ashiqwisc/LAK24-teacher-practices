# Teacher Practices in AI-Supported Classrooms

Supplementary repository for LAK24 full paper "Revealing Networks: Understanding Effective Teacher Practices in AI-Supported Classrooms using Transmodal Ordered Network Analysis." Contains code for data pre-preprocessing, analytical and statistical exploration, analysis, and code that exploration and analysis is dependent upon.  

## Citation

Borchers C., Wang Y., Karumbaiah S., Ashiq M., Shaffer D. W., & Aleven V. (2024). Revealing Networks: Understanding Effective Teacher Practices in AI-Supported Classrooms using Transmodal Ordered Network Analysis. In *Proceedings of the 14th International Learning Analytics and Knowledge Conference* (LAK). ACM. 
```
@inproceedings{borchers2024revealing,
  title = {Revealing Networks: Understanding Effective Teacher Practices in AI-Supported Classrooms. Using Transmodal Ordered Network Analysis},
  author = {Borchers, Conrad, Wang, Yeyu, Karumbaiah, Shamya, Ashiq, Muhammad, Shaffer, David Williamson, Aleven, Vincent},
  booktitle = {LAK24: 14th International Learning Analytics and Knowledge Conference},
  doi={10.1145/3636555.3636892},
  url={https://doi.org/10.1145/3636555.3636892},
  year = {2024}
}
```

## Folder Structure
- `code`: This folder contains all the code in our analysis. Throughout this code, mentions of data in `datasets` are redacted. 
  - `data_preprocessing_1...5`: These five files contain all data wrangling and feature engineering done prior to exploration and analysis. Original file is collapsed into a format beneficial for T/ONA analyses. Student distances from teacher, teacher screen alignment, student prior knowledge and conceptual knowledge scores, student learning rates, new codes used in analysis like HINT REQUEST and FIRST CORRECT ATTEMPT, and whether students were visited or not are all calculated and appended to processed dataframe in these files. To reproduce, please run these five files in order; the below explorations and analyses depend on the generated files. 
  - `base_rates.R`: Code to calculate category counts and in-category base rates 
  - `ck_exploration.Rmd`: Exploratory T/ONA analysis using student conceptual knowledge scores.
  - `pk_exploration.Rmd`: Exploratory T/ONA analysis using student prior knowledge scores.
  - `lr_exploration.Rmd`: Exploratory T/ONA analysis using student learning rates.
  - `tona_exploration.Rmd`: Exploratory T/ONA analysis to understand effect of window size on analysis.
  - `ona_plotter.R`: File to plot ONA networks; used in `tona_exploration.Rmd`.
  - `visit_analysis_exploration.Rmd`: Exploratory T/ONA analysis for students before and after teacher visit.
  - `visit_analysis_exploration_2.Rmd`: Exploratory T/ONA analysis for students before and after teacher visit, continued.
  - `four_modalities.Rmd`: Exploratory T/ONA analysis of various model parameters, as well as paper analysis for low vs. high learning rates.
  - `bootstrap_aic.R`: File to bootstrap AIC for logistic regressions in order to understand AIC significance in `four_modalities.Rmd`. 
  - `visit_analysis.Rmd`: Paper analysis for before vs. after for low learning rates and before vs. after for high learning rates.
- `datasets`: See "Data Availability" section below. 

## Data Availability 
Data to reproduce all analyses conducted for this study can be requested via [CMU DataShop](https://pslcdatashop.web.cmu.edu/DatasetInfo?datasetId=5833). 


