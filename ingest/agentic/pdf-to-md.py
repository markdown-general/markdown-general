#!/usr/bin/env python3
"""
PDF to Markdown Converter using marker-pdf
Converts PDF documents to clean, structured markdown format
"""

import os
import sys
import argparse
from pathlib import Path
from typing import Optional, List
import json

try:
    from marker.convert import convert_single_pdf
    from marker.models import load_all_models
    from marker.settings import settings
except ImportError:
    print("Error: marker-pdf not installed. Install with: pip install marker-pdf")
    sys.exit(1)

def setup_models() -> List:
    """Load all required models for PDF processing"""
    print("Loading models...")
    return load_all_models()

def convert_pdf_to_markdown(pdf_path: str, output_dir: str, 
                          models: List, start_page: Optional[int] = None,
                          max_pages: Optional[int] = None,
                          langs: Optional[List[str]] = None) -> dict:
    """
    Convert a single PDF to markdown
    
    Args:
        pdf_path: Path to the PDF file
        output_dir: Directory to save the markdown output
        models: Loaded models from setup_models()
        start_page: Page to start conversion from (1-based)
        max_pages: Maximum number of pages to convert
        langs: List of languages in the document
    
    Returns:
        Dictionary with conversion metadata
    """
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    # Convert PDF to markdown
    full_text, images, out_meta = convert_single_pdf(
        pdf_path, 
        models,
        start_page=start_page,
        max_pages=max_pages,
        langs=langs
    )
    
    # Generate output filename
    pdf_name = Path(pdf_path).stem
    md_path = os.path.join(output_dir, f"{pdf_name}.md")
    meta_path = os.path.join(output_dir, f"{pdf_name}_meta.json")
    
    # Save markdown content
    with open(md_path, 'w', encoding='utf-8') as f:
        f.write(full_text)
    
    # Save metadata
    with open(meta_path, 'w', encoding='utf-8') as f:
        json.dump(out_meta, f, indent=2)
    
    # Save images if any were extracted
    if images:
        img_dir = os.path.join(output_dir, f"{pdf_name}_images")
        os.makedirs(img_dir, exist_ok=True)
        
        for img_name, img_data in images.items():
            img_path = os.path.join(img_dir, img_name)
            with open(img_path, 'wb') as f:
                f.write(img_data)
    
    print(f"✓ Converted: {pdf_path}")
    print(f"✓ Output: {md_path}")
    print(f"✓ Metadata: {meta_path}")
    
    if images:
        print(f"✓ Images: {len(images)} images saved to {img_dir}")
    
    return out_meta

def batch_convert_pdfs(input_dir: str, output_dir: str, 
                      models: List, workers: int = 1,
                      max_files: Optional[int] = None,
                      min_length: int = 1000,
                      langs: Optional[List[str]] = None) -> None:
    """
    Convert multiple PDFs in a directory
    
    Args:
        input_dir: Directory containing PDF files
        output_dir: Directory to save markdown outputs
        models: Loaded models
        workers: Number of parallel workers
        max_files: Maximum number of files to convert
        min_length: Minimum characters before processing a PDF
        langs: List of languages for OCR
    """
    pdf_files = list(Path(input_dir).glob("*.pdf"))
    
    if max_files:
        pdf_files = pdf_files[:max_files]
    
    print(f"Found {len(pdf_files)} PDF files to convert")
    
    # Simple sequential processing (can be parallelized)
    for i, pdf_file in enumerate(pdf_files, 1):
        print(f"\nProcessing {i}/{len(pdf_files)}: {pdf_file.name}")
        
        try:
            # Quick check for minimum length to avoid processing mostly image PDFs
            # This is a rough estimate - marker will do more thorough analysis
            if pdf_file.stat().st_size < min_length:
                print(f"⚠ Skipping {pdf_file.name} (too small)")
                continue
            
            convert_pdf_to_markdown(
                str(pdf_file), 
                output_dir, 
                models,
                langs=langs
            )
            
        except Exception as e:
            print(f"✗ Error processing {pdf_file.name}: {e}")
            continue

def main():
    parser = argparse.ArgumentParser(description="Convert PDF to Markdown using marker-pdf")
    parser.add_argument("input", help="PDF file or directory to convert")
    parser.add_argument("-o", "--output", default="output", 
                       help="Output directory (default: output)")
    parser.add_argument("--start-page", type=int, 
                       help="Page to start conversion from (1-based)")
    parser.add_argument("--max-pages", type=int, 
                       help="Maximum number of pages to convert")
    parser.add_argument("--workers", type=int, default=1,
                       help="Number of parallel workers (default: 1)")
    parser.add_argument("--max-files", type=int,
                       help="Maximum number of files to convert in batch mode")
    parser.add_argument("--min-length", type=int, default=1000,
                       help="Minimum characters before processing a PDF (default: 1000)")
    parser.add_argument("--langs", nargs="+",
                       help="Languages in the document for OCR")
    parser.add_argument("--ocr-all", action="store_true",
                       help="Force OCR on all pages")
    parser.add_argument("--paginate", action="store_true",
                       help="Add page breaks between pages in output")
    
    args = parser.parse_args()
    
    # Setup OCR engine and other settings
    if args.ocr_all:
        settings.ocr_all_pages = True
    
    if args.paginate:
        settings.paginate_output = True
    
    # Load models (this will take some time initially)
    models = setup_models()
    
    # Convert single file or batch
    if os.path.isfile(args.input):
        convert_pdf_to_markdown(
            args.input, 
            args.output, 
            models,
            start_page=args.start_page,
            max_pages=args.max_pages,
            langs=args.langs
        )
    elif os.path.isdir(args.input):
        batch_convert_pdfs(
            args.input,
            args.output,
            models,
            workers=args.workers,
            max_files=args.max_files,
            min_length=args.min_length,
            langs=args.langs
        )
    else:
        print(f"Error: Input '{args.input}' is not a valid file or directory")
        sys.exit(1)

if __name__ == "__main__":
    main()
