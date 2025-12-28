#!/bin/bash

# File Checker for Thaura Uploads
# Checks common issues that cause "application octet stream" errors

check_file() {
    local file_path="$1"
    
    if [[ ! -f "$file_path" ]]; then
        echo "❌ Error: File not found at '$file_path'"
        return 1
    fi
    
    echo "📁 File: $file_path"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    
    # Basic file info
    echo "📊 File Information:"
    file "$file_path"
    echo
    
    # File size
    local file_size=$(stat -f%z "$file_path" 2>/dev/null || stat -c%s "$file_path" 2>/dev/null)
    local size_kb=$((file_size / 1024))
    local size_mb=$((size_kb / 1024))
    
    echo "📏 File Size: ${file_size} bytes (${size_kb} KB / ${size_mb} MB)"
    echo
    
    # Check for common issues
    echo "🔍 Checking for potential issues:"
    
    # Size check (common limits are 10MB-25MB for many systems)
    if [[ $file_size -gt 25*1024*1024 ]]; then
        echo "⚠️  Warning: File size > 25MB (might be too large)"
    elif [[ $file_size -gt 10*1024*1024 ]]; then
        echo "⚠️  Warning: File size > 10MB (might be large)"
    fi
    
    # Check if it's a binary file
    if file "$file_path" | grep -q "binary"; then
        echo "⚠️  Warning: File appears to be binary"
    else
        echo "✅ File appears to be text"
    fi
    
    # Check file extension vs content type
    echo
    echo "🔗 File Extension Check:"
    local extension="${file_path##*.}"
    local extension_lower=$(echo "$extension" | tr '[:upper:]' '[:lower:]')
    
    case "$extension_lower" in
        txt|md|org|json|yaml|yml|csv|tsv|xml|html|htm|css|js|py|hs|cpp|c|java|rs|go|rb|php|sql|sh|bat|ps1|dockerfile|makefile|gitignore|license|readme|todo|log)
            echo "✅ Extension '$extension' is commonly supported"
            ;;
        pdf|docx|doc|xlsx|xls|pptx|ppt|odt|ods|odp)
            echo "⚠️  Extension '$extension' - document format (might need conversion)"
            ;;
        jpg|jpeg|png|gif|bmp|tiff|svg|ico|webp)
            echo "⚠️  Extension '$extension' - image format (might be supported)"
            ;;
        mp3|wav|flac|aac|ogg|m4a|wma)
            echo "⚠️  Extension '$extension' - audio format (might be supported)"
            ;;
        mp4|avi|mov|wmv|flv|webm|m4v)
            echo "⚠️  Extension '$extension' - video format (might be supported)"
            ;;
        zip|rar|7z|tar|gz|bz2|xz)
            echo "⚠️  Extension '$extension' - archive format (might need extraction)"
            ;;
        exe|dll|so|dylib|bin|dat|db|sqlite|sqlite3)
            echo "❌ Extension '$extension' - likely binary/executable (might be rejected)"
            ;;
        *)
            echo "❓ Extension '$extension' - unknown format"
            ;;
    esac
    
    # Try to read first few lines if not binary
    echo
    echo "📖 Preview (first 3 lines):"
    if file "$file_path" | grep -q "text"; then
        head -n 3 "$file_path" | sed 's/^/   /' || echo "   (Could not read file)"
    else
        echo "   (Binary file - no text preview)"
    fi
    
    echo
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "💡 Recommendations:"
    echo "   - If text file: Try converting to .txt or .md format"
    echo "   - If document: Try exporting as plain text or markdown"
    echo "   - If binary: Check if there's a text export option"
    echo "   - If unknown: Try renaming with a standard extension"
    echo "   - If large: Consider splitting the file or compressing"
}

# Usage instructions
if [[ $# -eq 0 ]]; then
    echo "Usage: $0 <file_path>"
    echo
    echo "This script checks your file for common issues that might cause"
    echo "'application octet stream' errors when uploading to Thaura."
    echo
    echo "Example: $0 my_document.docx"
    exit 1
fi

# Check the file
check_file "$1"
