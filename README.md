# Final project 
## *by Yuhao and Tong*
Thank you for your interests in our project - *Authoritarian Discourses of China during Social Movements in the 1980s*

Our puopose of this project is to understand the transformation of legitimacy of the Chinese state from the ideological-based to performance-based through the content analysis of the People`s Daily during 1986-1989.

**1. To access the final report online**  
Please go [here](https://uc-cfss.github.io/fp-jtbeyond/index.html) for the project website.

**2. To reproduce analysis locally**

To run our website locally, please run `00_runfile.R` for the reproduction of all contents in our project. Please beware that `01_collect_data.R`, `02_translation.R`, and `03_Chinese_segmentation.R` were all used to scrape the raw data from online resources, but it may take five to eight hours to run these files and collect the dataset for analysis. For your convenience, we have included our tidied data in the `dataframe` folder for further analysis. If you'd like to examine the effectiveness of our scripts for scraping data, please refer to `01_collect_data.R` and `03_Chinese_segmentation.R` and use collecting() and create_dataframe() functions respectively. 

To successfully execute our project locally, please make sure the following packages have been installed: `RYandexTranslate`, `jiebaR`, `tm`, `NLP`, `stringr`, and `tidytext`. Since the `RYandexTranslate` is an API for online translation, which needs an API key. Again, for your convenience, we have included the key in our R script. 


