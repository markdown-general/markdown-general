#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

function ensureDirectoryExists(filePath) {
    const dirname = path.dirname(filePath);
    if (!fs.existsSync(dirname)) {
        fs.mkdirSync(dirname, { recursive: true });
    }
}

function unmashMarkdown(mashedFile, outputDir) {
    const content = fs.readFileSync(mashedFile, 'utf-8');
    const lines = content.split('\n');
    
    let currentFile = null;
    let currentContent = [];
    let filesCreated = 0;
    let artifactZip = null;
    let emptyDirs = [];
    let inMetadata = false;
    
    function saveCurrentFile() {
        if (currentFile) {
            const outputPath = path.join(outputDir, currentFile);
            ensureDirectoryExists(outputPath);
            
            // Join content exactly as it was mashed
            let fileContent = currentContent.join('\n');
            
            fs.writeFileSync(outputPath, fileContent, 'utf-8');
            filesCreated++;
            currentContent = [];
        }
    }
    
    for (const line of lines) {
        // Check for metadata start
        if (line === '<!-- METADATA') {
            inMetadata = true;
            continue;
        }
        
        // Check for metadata end
        if (inMetadata && line === '-->') {
            inMetadata = false;
            continue;
        }
        
        // Parse metadata
        if (inMetadata) {
            const artifactMatch = line.match(/^artifacts:\s*(.+)$/);
            if (artifactMatch) {
                artifactZip = artifactMatch[1];
            }
            
            const emptyDirMatch = line.match(/^empty_dirs:\s*(.+)$/);
            if (emptyDirMatch) {
                emptyDirs = emptyDirMatch[1].split(',');
            }
            continue;
        }
        
        // Check for file start marker
        const startMatch = line.match(/^<!-- FILE: (.+) -->$/);
        if (startMatch) {
            saveCurrentFile();
            currentFile = startMatch[1];
            continue;
        }
        
        // Check for file end marker
        const endMatch = line.match(/^<!-- END FILE: (.+) -->$/);
        if (endMatch) {
            saveCurrentFile();
            currentFile = null;
            continue;
        }
        
        // Add line to current file
        if (currentFile !== null) {
            currentContent.push(line);
        }
    }
    
    // Save last file if any
    saveCurrentFile();
    
    console.log(`✅ Extracted ${filesCreated} text file(s) to '${outputDir}'`);
    
    // Extract artifacts if present
    if (artifactZip) {
        const zipPath = path.join(path.dirname(mashedFile), artifactZip);
        if (fs.existsSync(zipPath)) {
            try {
                execSync(`unzip -q -o "${zipPath}" -d "${outputDir}"`, { stdio: 'pipe' });
                console.log(`   Artifacts: Extracted from ${artifactZip}`);
            } catch (err) {
                console.error(`   Error extracting ${artifactZip}: ${err.message}`);
            }
        }
    }
    
    // Restore empty directories
    if (emptyDirs.length > 0) {
        for (const dir of emptyDirs) {
            const dirPath = path.join(outputDir, dir);
            if (!fs.existsSync(dirPath)) {
                fs.mkdirSync(dirPath, { recursive: true });
            }
        }
        console.log(`   Empty dirs: Restored ${emptyDirs.length}`);
    }
}

// Main
const mashedFile = process.argv[2];
const outputDir = process.argv[3] || './output';

if (!mashedFile) {
    console.error('Usage: ./dir-unmash-v2.md <mashed_file> [output_dir]');
    process.exit(1);
}

unmashMarkdown(mashedFile, outputDir);
