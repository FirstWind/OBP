#!/bin/bash

# Configuration
OUT_ROOT="out"
FB_VERSION="3.0.13"
FB_ZIP_URL="https://github.com/FirebirdSQL/firebird/releases/download/v3.0.13/Firebird-3.0.13.33818-0-x64.zip"
VC_URL="https://aka.ms/vs/17/release/vc_redist.x64.exe"

REPO_DIR=$(pwd)
TOOLS_DIR="$REPO_DIR/tools/portable"
CACHE_DIR="$TOOLS_DIR/cache"

STAMP=$(date +"%Y%m%d_%H%M")
DIST_NAME="OBP_Portable_${STAMP}_dev"
OUT_DIR="$REPO_DIR/$OUT_ROOT"
DIST_DIR="$OUT_DIR/$DIST_NAME"

mkdir -p "$CACHE_DIR"

# 1. Build App
echo "== build"
bash tools/build_app.sh
if [ $? -ne 0 ]; then exit 1; fi

# 2. Prepare Dist Dir
echo "== prepare $DIST_DIR"
rm -rf "$DIST_DIR"
mkdir -p "$DIST_DIR"

# 3. Copy Portable Tools
cp -r "$TOOLS_DIR/"* "$DIST_DIR/"
# Remove cache from dist (it's source only)
rm -rf "$DIST_DIR/cache"

# 4. Copy App
mkdir -p "$DIST_DIR/app/config"
cp "$REPO_DIR/src/app/OBP.exe" "$DIST_DIR/app/OBP.exe"
cp "$REPO_DIR/src/app/config/app.ini" "$DIST_DIR/app/config/app.ini"

# Copy Database (assuming ImportData created it at C:\data\obp.fdb or configured path)
# We need to copy it to app/db/obp.fdb and update config
DB_SRC="/mnt/c/data/obp.fdb"
DB_DEST_DIR="$DIST_DIR/app/db"
mkdir -p "$DB_DEST_DIR"

# Fallback to local import if global path missing
if [ ! -f "$DB_SRC" ]; then
    DB_SRC="$REPO_DIR/tools/test_extract/obp.fdb"
fi

if [ -f "$DB_SRC" ]; then
    echo "Copying Database form $DB_SRC..."
    cp "$DB_SRC" "$DB_DEST_DIR/obp.fdb"
    
    # Update app.ini to use relative path
    # Assuming app.ini has database=...
    # We replace it with database=db\obp.fdb
    # Using sed to replace line starting with database=
    sed -i 's|^database=.*|database=db\\obp.fdb|' "$DIST_DIR/app/config/app.ini"
    # Also fix host to embedded/local?
    # host=127.0.0.1 implies server. For offline portable, we might want embedded or local server.
    # If we ship Firebird Embedded, we should set host to empty or localhost?
    # Usually for embedded: host is empty or skipped, and Client Library matches embedded.
    # But let's leave host=127.0.0.1 if we run local server from portable.
else
    echo "WARNING: Database $DB_SRC not found. Distribution will be empty."
fi

# Copy Docs
mkdir -p "$DIST_DIR/docs"
cp -r "$REPO_DIR/docs/"* "$DIST_DIR/docs/" 2>/dev/null

# 5. Fetch Firebird
echo "== fetch Firebird $FB_VERSION"
PKG_DIR="$DIST_DIR/packages"
mkdir -p "$PKG_DIR"
FB_ZIP_NAME="Firebird-${FB_VERSION}-x64.zip"
ZIP_PATH="$PKG_DIR/$FB_ZIP_NAME"
CACHE_ZIP="$CACHE_DIR/$FB_ZIP_NAME"

# Check Downloads first
if [ ! -f "$CACHE_ZIP" ]; then
    if [ -f ~/Downloads/$FB_ZIP_NAME ]; then
        echo "Found in Downloads"
        cp ~/Downloads/$FB_ZIP_NAME "$CACHE_ZIP"
    elif [ -f ~/Downloads/Firebird*${FB_VERSION}*x64.zip ]; then
         # Fuzzy match
         cp ~/Downloads/Firebird*${FB_VERSION}*x64.zip "$CACHE_ZIP"
    fi
fi

if [ -f "$CACHE_ZIP" ]; then
    echo "Using cached Firebird"
    cp "$CACHE_ZIP" "$ZIP_PATH"
else
    echo "Downloading Firebird..."
    curl -L -o "$ZIP_PATH" "$FB_ZIP_URL"
    # Update cache
    cp "$ZIP_PATH" "$CACHE_ZIP"
fi

# 6. Fetch VC++
VC_NAME="vc_redist.x64.exe"
VC_PATH="$DIST_DIR/$VC_NAME"
CACHE_VC="$CACHE_DIR/$VC_NAME"

if [ ! -f "$CACHE_VC" ]; then
    if [ -f ~/Downloads/$VC_NAME ]; then
         cp ~/Downloads/$VC_NAME "$CACHE_VC"
    fi
fi

if [ -f "$CACHE_VC" ]; then
    echo "Using cached VC++"
    cp "$CACHE_VC" "$VC_PATH"
else
    echo "Downloading VC++..."
    curl -L -o "$VC_PATH" "$VC_URL"
    cp "$VC_PATH" "$CACHE_VC"
fi

# 7. Create setup.bat
SETUP_BAT="$DIST_DIR/setup.bat"
echo "@echo off" > "$SETUP_BAT"
echo "cd /d \"%~dp0\"" >> "$SETUP_BAT"
echo "powershell -NoProfile -ExecutionPolicy Bypass -File \"%~dp0install.ps1\"" >> "$SETUP_BAT"

# 8. Unpack Firebird
echo "== unpack Firebird"
FB_DIR="$DIST_DIR/firebird"
rm -rf "$FB_DIR"
mkdir -p "$FB_DIR"

# Helper to check content
check_and_flatten() {
    # If files are in a subdir "Firebird*", move them up
    SUBDIR=$(find "$FB_DIR" -maxdepth 1 -name "Firebird*x64" -type d | head -n 1)
    if [ -n "$SUBDIR" ]; then
        echo "Found subdir $SUBDIR, moving contents up..."
        mv "$SUBDIR"/* "$FB_DIR/"
        rmdir "$SUBDIR"
    fi
    
    if [ -f "$FB_DIR/fbclient.dll" ]; then
        echo "Firebird unpacked successfully."
    else
        echo "Error: fbclient.dll not found after unpack."
    fi
}

# Unzip using 'unzip' if available
if command -v unzip >/dev/null; then
    unzip -q "$ZIP_PATH" -d "$FB_DIR"
    check_and_flatten
else
    echo "Warning: 'unzip' not found. Trying PowerShell..."
    # Convert paths to Windows format
    WIN_ZIP=$(wslpath -w "$ZIP_PATH")
    WIN_DEST=$(wslpath -w "$FB_DIR")
    
    # Run Expand-Archive
    # Note: Expand-Archive creates the destination folder if it doesn't exist,
    # but we created it. It should put files INSIDE valid destination.
    powershell.exe -NoProfile -ExecutionPolicy Bypass -Command "Expand-Archive -Path '$WIN_ZIP' -DestinationPath '$WIN_DEST' -Force"
    
    check_and_flatten
fi

echo "== done"
echo "$DIST_DIR"
