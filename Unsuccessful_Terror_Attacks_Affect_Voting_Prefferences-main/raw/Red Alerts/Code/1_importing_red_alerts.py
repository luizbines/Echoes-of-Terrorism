# IMPORTS ALL DATA AVAILABLE OF TERRORIST ATTACKS IN ISRAEL (2014-2023) FROM ISRAEL'S HOME FRONT COMMAND
# LUIZ BINES -  luizbines@gmail.com
# 2023 

from selenium import webdriver
import time
import os
import pandas as pd
from datetime import datetime, timedelta
from bs4 import BeautifulSoup
import json

# Working directory
wd = 'C:/Users/luizb/Desktop/Dissertation'
os.chdir(wd)

# Start and end dates
start_date = datetime(2014, 7, 1)
end_date = datetime(2023, 12, 18)

# Generate dates from start_date to end_date
dates = pd.date_range(start=start_date, end=end_date, freq='D').strftime('%d.%m.%Y')

# Create a dictionary to store links
links = {}

# Generate links for each date
for date in dates:
    link_key = f'link_{date}'
    link_value = f'https://www.oref.org.il//Shared/Ajax/GetAlarmsHistory.aspx?lang=he&fromDate={date}&toDate={date}&mode=0'
    links[link_key] = link_value

# Chrome driver
driver = webdriver.Chrome()

# Dictionary to store each link's visible text content
content_text = {}

# Loop for web scraping
for link_key, link_value in links.items():
    # Opening each link
    driver.get(link_value)
    
    # Waiting time
    time.sleep(0.1)
    
    # Extracting only the text
    soup = BeautifulSoup(driver.page_source, 'html.parser')
    visible_text = soup.get_text(separator='\n', strip=True)
    
    # Storing
    content_text[link_key] = visible_text

# Closing driver
driver.quit()

# Individual dataframes list
dfs = []

# Loop converting and adding dataframe to list
for link_key, json_text in content_text.items():
    json_data = json.loads(json_text)
    df = pd.DataFrame(json_data)
    dfs.append(df)

# Concatenating dataframes vertically
merged_df = pd.concat(dfs, ignore_index=True)


# Define the new directory where you want to save the CSV
new_directory = 'C:/Users/luizb/Desktop/Dissertation/raw/Red Alerts/Output'

# Ensure the new directory exists, if not, create it
if not os.path.exists(new_directory):
    os.makedirs(new_directory)

# Save the CSV in the new directory
new_csv_path = os.path.join(new_directory, 'red_alerts.csv')
merged_df.to_csv(new_csv_path, index=False, encoding='utf-8-sig')