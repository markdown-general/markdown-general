#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

function isBinary(filePath) {
    // Check by extension first
    const binaryExts = ['.png', '.jpg', '.jpeg', '.gif', '.pdf', '.zip', '.ico', '.svg', '.webmanifest'];
    if (binaryExts.some(ext => filePath.endsWith(ext))) {
        return true;
    }
    
    // Check content
    try {
        const buffer = fs.readFileSync(filePath);
        const checkSize = Math.min(8000, buffer.length);
        
        // Check for null bytes
        for (let i = 0; i < checkSize; i++) {
            if (buffer[i] === 0) return true;
        }
        
        return false;
    } catch {
        return true;
    }
}

function scanDirectory(dir) {
    const textFiles = [];
    const binaryFiles = [];
    const emptyDirs = [];
    
    function walk(directory, relativePath = '') {
        const entries = fs.readdirSync(directory, { withFileTypes: true });
        
        if (entries.length === 0) {
            emptyDirs.push(relativePath || '.');
            return;
        }
        
        for (const entry of entries) {
            // Skip hidden files/directories
            if (entry.name.startsWith('.')) continue;
            
            const fullPath = path.join(directory, entry.name);
            const relPath = relativePath ? path.join(relativePath, entry.name) : entry.name;
            
            if (entry.isDirectory()) {
                walk(fullPath, relPath);
            } else if (entry.isFile()) {
                if (entry.name.endsWith('.md') && !isBinary(fullPath)) {
                    textFiles.push({ path: fullPath, relativePath: relPath });
                } else {
                    binaryFiles.push({ path: fullPath, relativePath: relPath });
                }
            }
        }
    }
    
    walk(dir);
    textFiles.sort((a, b) => a.relativePath.localeCompare(b.relativePath));
    binaryFiles.sort((a, b) => a.relativePath.localeCompare(b.relativePath));
    emptyDirs.sort();
    
    return { textFiles, binaryFiles, emptyDirs };
}

function createArtifactZip(binaryFiles, outputDir) {
    if (binaryFiles.length === 0) return null;
    
    const zipPath = path.join(outputDir, 'artifacts.zip');
    const tempDir = fs.mkdtempSync('/tmp/mash-');
    
    try {
        // Copy files to temp dir preserving structure
        for (const file of binaryFiles) {
            const targetPath = path.join(tempDir, file.relativePath);
            const targetDir = path.dirname(targetPath);
            
            if (!fs.existsSync(targetDir)) {
                fs.mkdirSync(targetDir, { recursive: true });
            }
            
            fs.copyFileSync(file.path, targetPath);
        }
        
        // Create zip
        execSync(`cd "${tempDir}" && zip -q -r "${zipPath}" .`, { stdio: 'pipe' });
        
        // Cleanup
        fs.rmSync(tempDir, { recursive: true });
        
        return 'artifacts.zip';
    } catch (err) {
        console.error(`Error creating artifacts.zip: ${err.message}`);
        process.exit(1);
    }
}

function mashDirectory(directory, outputFile) {
    const { textFiles, binaryFiles, emptyDirs } = scanDirectory(directory);
    
    if (textFiles.length === 0) {
        console.error('No .md files found');
        process.exit(1);
    }
    
    // Create artifact zip if needed
    const outputDir = path.dirname(path.resolve(outputFile));
    const artifactZip = createArtifactZip(binaryFiles, outputDir);
    
    const stream = fs.createWriteStream(outputFile);
    
    // Write metadata as HTML comment if we have artifacts or empty dirs
    if (artifactZip || emptyDirs.length > 0) {
        stream.write(`<!-- METADATA\n`);
        if (artifactZip) {
            stream.write(`artifacts: ${artifactZip}\n`);
        }
        if (emptyDirs.length > 0) {
            stream.write(`empty_dirs: ${emptyDirs.join(',')}\n`);
        }
        stream.write(`-->\n\n`);
    }
    
    for (let i = 0; i < textFiles.length; i++) {
        const file = textFiles[i];
        
        // Write file marker
        stream.write(`<!-- FILE: ${file.relativePath} -->\n`);
        
        // Write content
        const content = fs.readFileSync(file.path, 'utf-8');
        stream.write(content);
        
        // Ensure newline before END marker (normalize: all markdown files must end with newline)
        if (!content.endsWith('\n')) {
            stream.write('\n');
        }
        
        // Add blank line before END marker to preserve trailing newline in split/join
        stream.write('\n');
        
        // Write end marker
        stream.write(`<!-- END FILE: ${file.relativePath} -->\n`);
        
        // Add blank line after each file
        stream.write('\n');
    }
    
    stream.end();
    
    console.log(`✅ Mashed ${textFiles.length} text file(s) into '${outputFile}'`);
    if (artifactZip) {
        console.log(`   Artifacts: ${binaryFiles.length} file(s) in ${artifactZip}`);
    }
    if (emptyDirs.length > 0) {
        console.log(`   Empty dirs: ${emptyDirs.length}`);
    }
}

// Main
const dir = process.argv[2] || '.';
const output = process.argv[3] || 'mashed.md';

mashDirectory(dir, output);
