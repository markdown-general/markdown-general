# Crypto Price Poller

A simple script that polls cryptocurrency prices and appends them to a file for testing chart updates.

## Usage
```bash
python crypto_poller.py [output_file]
```

**Examples:**
```bash
# Use custom filename
python crypto_poller.py my_data.json

# Use default filename (crypto_data.json)
python crypto_poller.py
```

## Script
```python
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
        
        print(f"✓ {entry['timestamp']} - ${entry['price']}")
        
    except Exception as e:
        print(f"✗ Error: {e}")

if __name__ == "__main__":
    print(f"Polling {API_URL} every {POLL_INTERVAL}s...")
    print(f"Appending to {OUTPUT_FILE}")
    
    while True:
        fetch_and_append()
        time.sleep(POLL_INTERVAL)
```

## Output Format

Each line is a JSON object:
```json
{"timestamp": "2026-01-03T10:30:00.123456", "price": "42150.50"}
{"timestamp": "2026-01-03T10:30:05.234567", "price": "42151.20"}
{"timestamp": "2026-01-03T10:30:10.345678", "price": "42149.80"}
```

## Stop the Script

Press `Ctrl+C` to stop polling.
