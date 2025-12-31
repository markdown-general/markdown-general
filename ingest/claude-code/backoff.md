# Claude API with Exponential Backoff Retry

This guide shows you how to call the Claude API with exponential backoff retry logic to handle rate limits gracefully.

## Prerequisites

Before running the code, make sure you have:

1. **Python installed** (Python 3.7 or higher)
   ```bash
   python --version
   ```

2. **Anthropic Python SDK installed**
   ```bash
   pip install anthropic
   ```

3. **An Anthropic API key**
   - Get one from: https://console.anthropic.com
   - You can either:
     - Pass it directly in the code, or
     - Set it as an environment variable:
       ```bash
       export ANTHROPIC_API_KEY="your-api-key-here"
       ```

## The Code

Create a file called `claude_retry.py`:

```python
import time
import random
from anthropic import Anthropic, RateLimitError

def call_claude_with_retry(client, max_retries=5):
    """
    Call Claude API with exponential backoff retry logic
    """
    base_delay = 1  # Start with 1 second
    max_delay = 60  # Cap at 60 seconds
    
    for attempt in range(max_retries):
        try:
            response = client.messages.create(
                model="claude-sonnet-4-20250514",
                max_tokens=1000,
                messages=[
                    {"role": "user", "content": "Hello! Please respond with a short greeting."}
                ]
            )
            return response
            
        except RateLimitError as e:
            if attempt == max_retries - 1:
                raise
            
            delay = min(base_delay * (2 ** attempt), max_delay)
            jitter = random.uniform(0, delay * 0.1)
            total_delay = delay + jitter
            
            print(f"Rate limited. Retrying in {total_delay:.2f} seconds...")
            time.sleep(total_delay)

# Main execution
if __name__ == "__main__":
    # Option 1: Pass API key directly
    # client = Anthropic(api_key="your-api-key-here")
    
    # Option 2: Use environment variable (recommended)
    client = Anthropic()
    
    try:
        response = call_claude_with_retry(client)
        print("Success!")
        print(response.content[0].text)
    except Exception as e:
        print(f"Error: {e}")
```

## How to Run

### Basic execution:
```bash
python claude_retry.py
```

### With environment variable:
```bash
export ANTHROPIC_API_KEY="your-api-key-here"
python claude_retry.py
```

## Making It Executable

To run the script without typing `python` every time:

### On Linux/Mac:

1. **Add a shebang line** at the very top of your file:
   ```python
   #!/usr/bin/env python3
   ```

2. **Make the file executable:**
   ```bash
   chmod +x claude_retry.py
   ```

3. **Run it directly:**
   ```bash
   ./claude_retry.py
   ```

### On Windows:

Windows will automatically recognize `.py` files if Python is installed, so you can just:
```cmd
claude_retry.py
```

Or create a batch file called `claude_retry.bat`:
```batch
@echo off
python claude_retry.py
```

Then run:
```cmd
claude_retry.bat
```

## Complete Executable Version

Here's the complete file with shebang:

```python
#!/usr/bin/env python3

import time
import random
from anthropic import Anthropic, RateLimitError

def call_claude_with_retry(client, max_retries=5):
    """
    Call Claude API with exponential backoff retry logic
    """
    base_delay = 1  # Start with 1 second
    max_delay = 60  # Cap at 60 seconds
    
    for attempt in range(max_retries):
        try:
            response = client.messages.create(
                model="claude-sonnet-4-20250514",
                max_tokens=1000,
                messages=[
                    {"role": "user", "content": "Hello! Please respond with a short greeting."}
                ]
            )
            return response
            
        except RateLimitError as e:
            if attempt == max_retries - 1:
                raise
            
            delay = min(base_delay * (2 ** attempt), max_delay)
            jitter = random.uniform(0, delay * 0.1)
            total_delay = delay + jitter
            
            print(f"Rate limited. Retrying in {total_delay:.2f} seconds...")
            time.sleep(total_delay)

if __name__ == "__main__":
    client = Anthropic()
    
    try:
        response = call_claude_with_retry(client)
        print("Success!")
        print(response.content[0].text)
    except Exception as e:
        print(f"Error: {e}")
```

## How the Retry Logic Works

1. **Exponential growth**: Each retry waits 2× longer (1s, 2s, 4s, 8s, 16s...)
2. **Jitter**: Adds randomness to prevent thundering herd problems
3. **Max delay cap**: Prevents waiting too long (capped at 60 seconds)
4. **Max retries**: After 5 attempts, it raises the error

## Troubleshooting

- **"ModuleNotFoundError: No module named 'anthropic'"**: Run `pip install anthropic`
- **"AuthenticationError"**: Check your API key is correct
- **"RateLimitError" persists**: You may need to wait until your quota resets (check the error message for the reset time)
