import requests
from bs4 import BeautifulSoup
import csv
import os
from selenium import webdriver
import time
import pandas as pd
import json
from selenium.webdriver.common.keys import Keys

# Working directory
wd = 'C:/Users/luizb/Desktop/Dissertation/Attacks/Cities'
os.chdir(wd)

# Specify the path to save the CSV file
csv_file_path = "cities.csv"

# Using webdriver
browser = webdriver.Edge()

# Reading csv
with open(csv_file_path, mode='r', newline='', encoding='utf-8-sig') as csv_file:
    reader = csv.reader(csv_file)
    data = list(reader)

# Getting links
for i, row in enumerate(data[1:]):  
    city = row[1]  

    try:
        # Search city in Google Maps
        browser.get(f'https://www.google.com/maps/place/{city}')

        # Wait until loaded
        time.sleep(6)

        # Getting link
        link = browser.current_url
        link = link.split('@', 1)[-1]
        link = link.split(',', 2)[:2]

        # Adding link to table
        if len(row) < 4:
            row.extend([''] * (4 - len(row)))  
        row[2] = link[0]  # latitude
        row[3] = link[1]  # longitude

        # Write data after each iteration
        with open(csv_file_path, mode='w', newline='', encoding='utf-8-sig') as csv_file:
            writer = csv.writer(csv_file)
            writer.writerows(data)

    except Exception as e:
        print(f"An error occurred at index {i} with city {city}. Error: {e}")

browser.quit()  
