import pandas as pd
import time
import random
from pytrends.request import TrendReq

# Initialize pytrends for English with Israel timezone (UTC+3)
pytrends_en = TrendReq(hl='en-US', tz=180)  # For English queries
# Initialize pytrends for Hebrew with Israel timezone (UTC+3)
pytrends_he = TrendReq(hl='he-IL', tz=180)  # For Hebrew queries

# List of keywords
keywords_english = [
    "netanyahu", "terrorism", "siren", "war", "peace",
    "ceasefire",
    "elections"
]
keywords_hebrew = [
    "נתניהו",
      "טרור",
    "אזעקה",
    "מלחמה", "שלום", "הפסקת אש",
    "בחירות"
]

# Mapping of district codes to district names
districts = {
    "IL-M": "Center District",
    "IL-HA": "Haifa District",
    "IL-JM": "Jerusalem District",
    "IL-Z": "North District",
    "IL-D": "South District",
    "IL-TA": "Tel Aviv District"
}

# Create an empty DataFrame to store all results
all_data = pd.DataFrame()

# Function to fetch data and append to DataFrame
def fetch_data(pytrends, keywords, districts):
    temp_data = pd.DataFrame()
    
    # Loop through each district
    for code, name in districts.items():
        print(f"Fetching data for {name} ({code})...")

        # Loop through each keyword
        for keyword in keywords:
            print(f"   Searching for: {keyword}")

            # Set up the yearly timeframes
            timeframes = [
                ("2014-01-01 2016-12-31", "2014-2016"),
                ("2017-01-01 2019-12-31", "2017-2019"),
                ("2020-01-01 2022-12-31", "2020-2022")
            ]
            
            for timeframe, label in timeframes:
                print(f"   Fetching data for {label}...")
                # Set up the Google Trends request
                pytrends.build_payload(
                    kw_list=[keyword],  # Current keyword
                    geo=code,           # District code
                    timeframe=timeframe  # Weekly data for the respective timeframe
                )
                
                # Fetch interest over time
                while True:
                    try:
                        district_data = pytrends.interest_over_time()
                        
                        # If the result is not empty, process it
                        if not district_data.empty:
                            # Flatten the data and rename the interest column to 'value'
                            district_data = district_data[[keyword]].reset_index()
                            print(f"Successfully fetched data for {label}")
                            district_data['District'] = name
                            district_data['Keyword'] = keyword
                            district_data['Timeframe'] = label  # Add timeframe column
                            district_data.rename(columns={keyword: 'Value'}, inplace=True)
                            
                            # Concatenate the current result to the temporary DataFrame
                            temp_data = pd.concat([temp_data, district_data], ignore_index=True)
                        break  # Exit the loop if successful
                    except Exception as e:
                        print(f"Error fetching data for {keyword} in {name} ({label}): {e}")
                        print("Retrying")
                        # Pause before retrying
                        time.sleep(random.uniform(1, 2))  # Pause for a random time between 60 and 65 seconds
    
    return temp_data

# Fetch data for English keywords
english_data = fetch_data(pytrends_en, keywords_english, districts)
# Fetch data for Hebrew keywords
hebrew_data = fetch_data(pytrends_he, keywords_hebrew, districts)

# Combine both DataFrames
all_data = pd.concat([english_data, hebrew_data], ignore_index=True)

# Save the results to a CSV file
all_data.to_csv("trends_by_district_keywords_weekly.csv", index=False)

# Print the final DataFrame
print(all_data)
