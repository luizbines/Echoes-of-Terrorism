import pandas as pd
import time
import random
import os
from pytrends.request import TrendReq
import sys


def resolve_project_root(start_dir=None):
    current_dir = os.path.abspath(start_dir or os.getcwd())
    while True:
        if os.path.isdir(os.path.join(current_dir, 'Voting')) and os.path.isdir(os.path.join(current_dir, 'Trends')):
            return current_dir
        parent_dir = os.path.dirname(current_dir)
        if parent_dir == current_dir:
            break
        current_dir = parent_dir
    raise RuntimeError(f"Could not determine project root from start_dir={start_dir or os.getcwd()}")


# 1. Set the Working Directory - dynamic from environment or fallback
base_wd = os.environ.get('R_PROJECT_ROOT')
if base_wd and os.path.isdir(base_wd):
    base_wd = os.path.abspath(base_wd)
    if not (os.path.isdir(os.path.join(base_wd, 'Voting')) and os.path.isdir(os.path.join(base_wd, 'Trends'))):
        base_wd = resolve_project_root()
else:
    base_wd = resolve_project_root()

path = os.path.join(base_wd, "Trends", "raw", "Google Trends", "Output")
try:
    os.chdir(path)
    print(f"Working directory set to: {os.getcwd()}")
except FileNotFoundError:
    print(f"Error: Path {path} not found.")

# Initialize pytrends
pytrends = TrendReq(hl='he-IL', tz=180)

# Full list of keywords (22 total)
keywords_hebrew = [

    'מקלט',           # shelter
    'חירום',          # emergency
    'בתי חולים',      # hospitals
    'אזעקה',          # siren
    'מרחב מוגן',      # protected space
    'פיקוד העורף',    # Home Front Command
    'צבע אדום',       # Code Red
    'התקף חרדה',      # panic attack
    'מד"א',           # MDA (Magen David Adom)
    'שלום',           # peace
    'מלחמה',          # war
    'בחירות',         # elections
    'טרור',           # terrorism
    'הפסקת אש',       # ceasefire
    'נתניהו',         # Netanyahu
    'חמאס',           # Hamas
    'ליכוד',          # Likud
    'ממשלה',          # government
    'סיפוח',          # annexation
    'ריבונות',        # sovereignty
    'התנחלויות',      # settlements
    'מדינה פלסטינית'  # Palestinian state
]

districts = {
    "IL-M": "Center District",
    "IL-HA": "Haifa District",
    "IL-JM": "Jerusalem District",
    "IL-Z": "North District",
    "IL-D": "South District",
    "IL-TA": "Tel Aviv District"
}

timeframe = "2014-01-01 2022-12-31"
output_filename = "trends_israel.csv"

# Remove existing file to start fresh
if os.path.exists(output_filename):
    os.remove(output_filename)

print("\nStarting INDIVIDUAL data collection (Each word scaled 0-100)...")

for code, name in districts.items():
    print(f"\n--- Processing District: {name} ---")
    
    for kw in keywords_hebrew:
        print(f"  Fetching: {kw}")
        
        success = False
        retries = 0
        
        while not success and retries < 5:
            try:
                # Build payload for only ONE keyword
                pytrends.build_payload(kw_list=[kw], geo=code, timeframe=timeframe)
                data = pytrends.interest_over_time()
                
                if not data.empty:
                    data = data.reset_index()
                    # Keep only 'date' and the keyword column
                    data = data.rename(columns={kw: 'value'})
                    data['district'] = name
                    data['keyword'] = kw
                    
                    # Ensure requested column order
                    data = data[['date', 'value', 'district', 'keyword']]
                    data['date'] = data['date'].dt.tz_localize(None)
                    
                    # Save to CSV immediately
                    is_new_file = not os.path.exists(output_filename)
                    data.to_csv(output_filename, mode='a', index=False, header=is_new_file, encoding='utf-8-sig')
                    
                    print(f"    [OK] {kw} collected.")
                    success = True
                    
                    # IMPORTANT: Wait between 15-25 seconds to avoid 429
                    time.sleep(random.uniform(15, 25))
                else:
                    print(f"    [EMPTY] No data for {kw}.")
                    success = True
                
            except Exception as e:
                retries += 1
                # If 429 block occurs, wait 10-15 MINUTES
                if "429" in str(e):
                    wait_time = random.uniform(600, 900)
                    print(f"    [BLOCK 429] Waiting {wait_time/60:.1f} minutes to reset IP...")
                else:
                    wait_time = 60
                    print(f"    [ERROR] {e}. Retrying in 1 min...")
                
                time.sleep(wait_time)

print("\nFinished! Each word is now independently scaled from 0 to 100.")