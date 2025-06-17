# IMPORTS A TABLE FROM A ISRAELI NEWS WEBSITE THAT TRANSLATES ALL AREA CODES INTO CITIES 
# LUIZ BINES - luizbines@gmail.com 
# 2023

import requests
from bs4 import BeautifulSoup
import csv
import os
from selenium import webdriver
import time
import pandas as pd
import json
from selenium.webdriver.common.keys import Keys
import time


# Working directory
wd = 'C:/Users/luizb/Desktop/Dissertation'
os.chdir(wd)

# Specify the URL of the website
url = "https://www.mivzaklive.co.il/%D7%94%D7%AA%D7%A8%D7%90%D7%AA-%D7%A6%D7%91%D7%A2-%D7%90%D7%93%D7%95%D7%9D-%D7%9E%D7%A1%D7%A4%D7%A8%D7%99-%D7%A4%D7%95%D7%9C%D7%99%D7%92%D7%95%D7%A0%D7%99%D7%9D-%D7%95%D7%96%D7%9E%D7%A0%D7%99-%D7%94"

# Make an HTTP request to the URL and parse the content with BeautifulSoup
response = requests.get(url)
soup = BeautifulSoup(response.text, "html.parser")

# Find the table using BeautifulSoup (you may need to inspect the page to get the table's class or id)
table = soup.find("table")

# Initialize lists to store table data
data = []

# Iterate over the table rows and extract the data
for row in table.find_all("tr"):
    # Initialize a list for each table row
    row_data = []
    # Iterate over the cells in the row and extract the text
    for cell in row.find_all(["th", "td"]):
        row_data.append(cell.text.strip())  # strip() removes whitespace and newline characters
    # Add the row data to the data list
    data.append(row_data)

# Specify the path to save the CSV file
csv_file_path = "raw/Red Alerts/Output/area_codes.csv"

# Write the table data to a CSV file
with open(csv_file_path, mode='w', newline='', encoding='utf-8-sig') as csv_file:
    csv_writer = csv.writer(csv_file)
    csv_writer.writerows(data)
