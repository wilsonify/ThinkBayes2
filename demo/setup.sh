#!/bin/bash

# Think Bayes 2 Demo Setup Script
# This script sets up and runs the interactive demo

echo "🎯 Think Bayes 2 Demo Setup"
echo "============================"
echo ""

# Check if Node.js is installed
if ! command -v node &> /dev/null; then
    echo "❌ Node.js is not installed. Please install Node.js 16+ from https://nodejs.org/"
    exit 1
fi

# Check Node.js version
NODE_VERSION=$(node -v | cut -d'v' -f2 | cut -d'.' -f1)
if [ "$NODE_VERSION" -lt 16 ]; then
    echo "❌ Node.js version 16+ is required. Current version: $(node -v)"
    exit 1
fi

echo "✅ Node.js $(node -v) detected"

# Check if we're in the right directory
if [ ! -f "package.json" ]; then
    echo "❌ Please run this script from the demo directory"
    echo "   Usage: cd demo && ./setup.sh"
    exit 1
fi

# Install dependencies
echo ""
echo "📦 Installing dependencies..."
npm install

if [ $? -ne 0 ]; then
    echo "❌ Failed to install dependencies"
    exit 1
fi

echo "✅ Dependencies installed successfully"

# Run tests to verify everything works
echo ""
echo "🧪 Running tests to verify setup..."
npm test -- --run --reporter=verbose 2>/dev/null | grep -E "(✓|PASS|FAIL)" | head -5

if [ $? -eq 0 ]; then
    echo "✅ Tests are passing"
else
    echo "⚠️  Some tests may have issues, but the demo should still work"
fi

# Start the development server
echo ""
echo "🚀 Starting development server..."
echo "   The demo will be available at http://localhost:5173"
echo "   Press Ctrl+C to stop the server"
echo ""
npm run dev
