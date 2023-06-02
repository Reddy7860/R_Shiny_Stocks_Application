import pandas
from urllib.request import urlopen, Request
from bs4 import BeautifulSoup
import os
import pandas as pd
import matplotlib.pyplot as plt
import unicodedata
import re
import nltk
from nltk.corpus import stopwords
import numpy as np
from textblob import TextBlob

# NLTK VADER for sentiment analysis
from nltk.sentiment.vader import SentimentIntensityAnalyzer

from lxml import html
import requests
# from gensim.models import doc2vecp


def basic_clean(text):
#     print(text)
    wnl = nltk.stem.WordNetLemmatizer()
    filtered_tokens = []
    stopwords = nltk.corpus.stopwords.words('english')
    
    text = (unicodedata.normalize('NFKD', text)
        .encode('ascii', 'ignore')
        .decode('utf-8', 'ignore')
        .lower())
    words = re.sub(r'[^\w\s]', '', text).split()

    filtered_tokens = [wnl.lemmatize(word) for word in words if word not in stopwords]
    return filtered_tokens

page = requests.get('https://economictimes.indiatimes.com/markets/stocks/news/metal-fmcg-stocks-lift-sensex-as-dalal-street-breaks-two-day-losing-streak/articleshow/81705687.cms')
tree = html.fromstring(page.content)
# #This will create a list of buyers:

news_lst = tree.xpath('.//div[@class="artText"]/text()')
# #This will create a list of prices
# prices = tree.xpath('//span[@class="item-price"]/text()')
temp = pd.DataFrame(news_lst)

words = basic_clean(''.join(str(temp[0].tolist())))

temp['polarity'] = temp[0].apply(lambda x: TextBlob(x).polarity)
temp['subjective'] = temp[0].apply(lambda x: TextBlob(x).subjectivity)

temp = temp.rename(columns = {0: 'News', 'polarity': 'polarity','subjective':'subjective'}, inplace = False)

# Instantiate the sentiment intensity analyzer
vader = SentimentIntensityAnalyzer()

# Iterate through the headlines and get the polarity scores using vader
scores = temp['News'].apply(vader.polarity_scores).tolist()


# Convert the 'scores' list of dicts into a DataFrame
scores_df = pd.DataFrame(scores)


# # Join the DataFrames of the news and the list of dicts
temp = temp.join(scores_df, rsuffix='_right')

result_df = temp[temp['News'] != '\n']

result_df = result_df.reset_index(inplace = False)

result_df = result_df.drop(['index'], axis = 1)

print(result_df)



