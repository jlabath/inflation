#!/bin/bash
NODE_ENV=production tailwindcss -w -c ./tailwind.config.js -o ./public/a/main.css --minify

