## Final Project 
### *Yuhao Zhuang and Tong Ju*
Thank you very much for your interest in our project.

Our purpose of this project is to understand the transformation of legitimacy of the Chinese state from the ideological-based to performance-based through content analysis of the *People's Daily* during 1986-1990.

**Access Final Project Online**

Please click [here](https://uc-cfss.github.io/fp-jtbeyond/index.html) for the project website.


**Reproduce Final Project Locally**

To run our website locally, please simply run `00_runfile.R` for the reproduction of all contents in our project. 

Please also note that `01_collect_data.R`, `02_translation.R`, and `03_Chinese_segmentation.R` were all used to scrape and tidy the raw data from online sources, but it may take five to eight hours to run these files and collect the dataset for analysis. For your convenience, we have already included our tidied data in the `dataframe` folder. It would make your script running much more smoother. If you'd like to examine the effectiveness of our scripts for scraping data, please refer to `01_collect_data.R` and `03_Chinese_segmentation.R` and use collecting() and create_dataframe() functions respectively. 

In addition, to successfully execute our project locally, please make sure the following packages have been installed: `RYandexTranslate`, `jiebaR`, `tm`, `NLP`, `stringr`, `shinyjs`, `topicmodels`, and `tidytext`. As an API for online translation, `RYandexTranslate` needs an API key. Again, for your convenience, we have included the key in our R script. 

