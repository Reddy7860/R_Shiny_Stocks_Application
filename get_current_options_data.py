import requests
import pandas as pd

def fetch_current_details(index_name):
  # index_name = "OPTIDXNIFTY05-08-2021PE15800.00"
  url = "".join(["https://www.nseindia.com/api/chart-databyindex?index=", str(index_name)])
  
  print(url)
  
  # baseurl = "https://www.nseindia.com/"
  baseurl = 'https://www.nseindia.com/market-data/pre-open-market-cm-and-emerge-market'
  
  # f"https://www.nseindia.com/api/chart-databyindex?index="+index_name
  headers = {'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, '
                         'like Gecko) '
                         'Chrome/80.0.3987.149 Safari/537.36',
           'accept-language': 'en,gu;q=0.9,hi;q=0.8', 'accept-encoding': 'gzip, deflate, br'}
  session = requests.Session()
  request = session.get(baseurl, headers=headers, timeout=10)
  print(request)
  cookies = dict(request.cookies)
  print(cookies)
  response = session.get(url, headers=headers, timeout=10, cookies=cookies)
  # print(response.json())
  
  response_json = response.json()
  response_data = response_json['grapthData']
  
  df = pd.DataFrame(response_data, columns = ['Timestamp', 'Price'])
  
  print(df.tail())
  
  df.to_csv("~/Downloads/Reddy_Stocks_Application/data/current_option_price.csv")
  
  # df.to_csv("~/Downloads/Reddy_Stocks_Application/data/current_put_option_price.csv")

fetch_current_details("OPTIDXNIFTY02-12-2021CE17300.00")

