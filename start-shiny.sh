#!/bin/bash
# Robust Shiny startup script
# Keeps Shiny running even if a session crashes

set -e

echo "Starting RSPrismBB Shiny App..."

# Function to run Shiny
run_shiny() {
    R --no-save --no-restore -q -e "
        options('golem.app.prod' = TRUE)
        app <- RSPrismBB::run_app(options = list(launch.browser = FALSE))
        shiny::runApp(app, host = '0.0.0.0', port = 3838)
    "
}

# Keep trying to run Shiny
# If it exits, wait a bit and restart
while true; do
    echo "[$(date)] Starting Shiny server..."
    
    if run_shiny; then
        echo "[$(date)] Shiny exited normally (exit code 0)"
    else
        exitcode=$?
        echo "[$(date)] Shiny crashed with exit code $exitcode"
    fi
    
    echo "[$(date)] Waiting 2 seconds before restart..."
    sleep 2
done
