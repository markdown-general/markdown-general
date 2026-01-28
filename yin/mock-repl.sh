#!/bin/bash
# mock-repl: Simulate GHCi by reading queries from /tmp/ghci-in.txt
# and writing responses to /tmp/ghci-out.txt
# Based on: card-mock-repl.md

INPUT="/tmp/ghci-in.txt"
OUTPUT="/tmp/ghci-out.txt"

# Initialize files
> "$INPUT"
> "$OUTPUT"

echo "mock-repl starting"
echo "  reading from: $INPUT"
echo "  writing to:  $OUTPUT"

# Track which line numbers we've processed
LAST_SEEN=0

while true; do
  if [[ -f "$INPUT" ]]; then
    CURRENT_LINES=$(wc -l < "$INPUT" 2>/dev/null || echo 0)

    # If there are new lines beyond what we've seen
    if [[ $CURRENT_LINES -gt $LAST_SEEN ]]; then
      # Read only the new lines
      QUERIES=$(tail -n +$((LAST_SEEN + 1)) "$INPUT")

      while IFS= read -r QUERY; do
        [[ -z "$QUERY" ]] && continue

        # Respond based on query pattern
        RESPONSE=""
        case "$QUERY" in
          ":t fmap")
            RESPONSE="fmap :: Functor f => (a -> b) -> f a -> f b"
            ;;
          ":k Maybe")
            RESPONSE="Maybe :: * -> *"
            ;;
          ":t run")
            RESPONSE="run :: Hyp a a -> a"
            ;;
          ":t base")
            RESPONSE="base :: a -> Hyp a a"
            ;;
          ":t pipe")
            RESPONSE="pipe :: Hyp b a -> b"
            ;;
          ":k Hyp")
            RESPONSE="Hyp :: * -> * -> *"
            ;;
          "import"*)
            RESPONSE="Loaded."
            ;;
          *)
            RESPONSE="error: not in scope: $QUERY"
            ;;
        esac

        # Write response with prompt and query echo
        {
          echo "Prelude> $QUERY"
          echo "$RESPONSE"
          echo ""
        } >> "$OUTPUT"

        echo "  [processed] $QUERY"
      done <<< "$QUERIES"

      LAST_SEEN=$CURRENT_LINES
    fi
  fi

  sleep 0.05
done
