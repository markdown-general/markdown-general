# -*- coding: utf-8 -*-
import requests
import json
import time
from datetime import datetime
import sys

# Configuration
API_URL = "https://api.coinbase.com/v2/prices/BTC-USD/spot"
OUTPUT_FILE = sys.argv[1] if len(sys.argv) > 1 else "crypto_data.json"
POLL_INTERVAL = 5  # seconds

def fetch_and_append():
    try:
        response = requests.get(API_URL)
        response.raise_for_status()

        data = response.json()

        entry = {
            "timestamp": datetime.now().isoformat(),
            "price": data['data']['amount']
        }

        # Append to file
        with open(OUTPUT_FILE, 'a') as f:
            f.write(json.dumps(entry) + '\n')

        print("[OK] {} - ${}".format(entry['timestamp'], entry['price']))

    except Exception as e:
        print("[ERROR] {}".format(e))

if __name__ == "__main__":
    print("Polling {} every {}s...".format(API_URL, POLL_INTERVAL))
    print("Appending to {}".format(OUTPUT_FILE))

    while True:
        fetch_and_append()
        time.sleep(POLL_INTERVAL)
