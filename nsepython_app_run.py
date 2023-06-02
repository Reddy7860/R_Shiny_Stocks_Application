# from nsepython import *
# 
# print(nse_quote_ltp("BANKNIFTY","15-Jul-2021","PE",35200))

import requests
from bs4 import BeautifulSoup
import numpy as np
import pandas as pd

url = "https://www.nseindia.com/api/option-chain-indices?symbol=NIFTY"

headers = {
    'Connection': 'keep-alive',
    'Cache-Control': 'max-age=0',
    'DNT': '1',
    'Upgrade-Insecure-Requests': '1',
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.79 Safari/537.36',
    'Sec-Fetch-User': '?1',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9',
    'Sec-Fetch-Site': 'none',
    'Sec-Fetch-Mode': 'navigate',
    'Accept-Encoding': 'gzip, deflate, br',
    'Accept-Language': 'en-US,en;q=0.9,hi;q=0.8',
}

# payload = nse_quote(symbol)
output = requests.get("https://www.nseindia.com/api/option-chain-indices?symbol=NIFTY",headers=headers).json()

