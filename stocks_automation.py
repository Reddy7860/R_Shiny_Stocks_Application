import requests
import pandas as pd
import datetime
from smartapi.smartConnect import SmartConnect

def gap_up_strategy(df):
	increment = 0
	Signal_df = pd.DataFrame(columns=["Strategy", "Stock","Signal","Datetime","Value"])
	for i in range(0,len(df)):
		print(df.iloc[i,1])
		final_url = "https://query1.finance.yahoo.com/v8/finance/chart/"+df.iloc[i,1]+"?region=IN&lang=en-IN&includePrePost=false&interval=5m&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet"
		resp = requests.get(url = final_url)
		json_resp = resp.json()
		stock_timestamp = json_resp['chart']['result'][0]['timestamp']
		Close = json_resp['chart']['result'][0]['indicators']['quote'][0]['close']
		High = json_resp['chart']['result'][0]['indicators']['quote'][0]['high']
		Low = json_resp['chart']['result'][0]['indicators']['quote'][0]['low']
		Open = json_resp['chart']['result'][0]['indicators']['quote'][0]['open']
		Volume = json_resp['chart']['result'][0]['indicators']['quote'][0]['volume']
		# print(df)
		new_columns = ['S_No', 'Stock','Previous_Open','Previous_High','Previous_Low','Previous_Close']

		df = pd.DataFrame(df, columns = new_columns)
		# print(df)
		# print(df.loc[i,"Previous_High"])
		stock = df.loc[i,"Stock"]
		high_price = df.loc[i,"Previous_High"]
		close_price = df.loc[i,"Previous_Close"]

		data = {'timestamp':stock_timestamp,
		        'open':Open,
		        'high':High,
		        'low':Low,
		        'close':Close,
		        'volume':Volume}

		final_df = pd.DataFrame(data)

		final_df['timestamp']=final_df['timestamp'].apply(lambda d: datetime.datetime.fromtimestamp(int(d)).strftime('%Y-%m-%d %H:%M:%S'))

		# print(final_df)

		satisfied_df = pd.DataFrame(columns = ['timestamp', 'open', 'high','low','close','volume','call'])

		open_price = final_df.iloc[0,3]

		# print(open_price)
		# print(close_price)
		if(open_price > close_price):
			print("Gap up")
			for j in range(4,len(final_df)):
				current_date = final_df.loc[j,"timestamp"]
				# print(final_df.loc[1:j,]['close'])
				day_high = max(final_df.loc[1:j,]['close'].max(),final_df.loc[1:j,]['open'].max())
				day_low = min(final_df.loc[1:j,]['close'].min(),final_df.loc[1:j,]['open'].min())
				low_range = min(final_df.iloc[j-1,3],final_df.iloc[j-2,3],final_df.iloc[j-3,3],final_df.iloc[j-4,3])
				high_range = max(final_df.iloc[j-1,3],final_df.iloc[j-2,3],final_df.iloc[j-3,3],final_df.iloc[j-4,3])

				current_close = final_df.loc[j,"close"]
				if((abs(high_range - low_range)/low_range*100 < 0.4) and (current_close >= high_price) and (current_close >= day_high)):
					satisfied_df = satisfied_df.append(final_df.loc[j,], ignore_index = True)
					satisfied_df = satisfied_df.reset_index(drop=True)
					satisfied_df.loc[len(satisfied_df)-1,"call"] = "Buy"
				elif((abs(high_range - low_range)/low_range*100 < 0.4) and (current_close <= close_price) and (current_close <= day_low)):
					# print("Hello")
					satisfied_df = satisfied_df.append(final_df.loc[j,], ignore_index = True)
					satisfied_df = satisfied_df.reset_index(drop=True)
					satisfied_df.loc[len(satisfied_df)-1,"call"] = "Sell"

		if(satisfied_df.empty):
			continue
		else:
			satisfied_df = satisfied_df.head(1)
			# print(satisfied_df)

			Signal_df.loc[increment,"Strategy"] = "Gap_up"
			Signal_df.loc[increment,"Stock"]=stock
			Signal_df.loc[increment,"Signal"]=satisfied_df.loc[0,"call"]
			Signal_df.loc[increment,"Datetime"]=satisfied_df.loc[0,"timestamp"]
			Signal_df.loc[increment,"Value"]=satisfied_df.loc[0,"close"]

			increment = increment + 1

		# else:
		# 	print("Gap down")
	return Signal_df

def execute_orders():
	print("Hello World")
          
def __init__():
	df = pd.read_csv('~/Desktop/Reddy_Stocks_Application/data/gaps_strategy.csv')

	Signal_df = gap_up_strategy(df)

	print(Signal_df)

__init__()
