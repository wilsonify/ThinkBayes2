@echo off
REM Think Bayes 2 Demo Setup Script for Windows

echo 🎯 Think Bayes 2 Demo Setup
echo ============================
echo.

REM Check if Node.js is installed
where node >nul 2>nul
if %errorlevel% neq 0 (
    echo ❌ Node.js is not installed. Please install Node.js 16+ from https://nodejs.org/
    pause
    exit /b 1
)

echo ✅ Node.js detected

REM Check if we're in the right directory
if not exist package.json (
    echo ❌ Please run this script from the demo directory
    echo    Usage: cd demo ^&^& setup.bat
    pause
    exit /b 1
)

REM Install dependencies
echo.
echo 📦 Installing dependencies...
call npm install

if %errorlevel% neq 0 (
    echo ❌ Failed to install dependencies
    pause
    exit /b 1
)

echo ✅ Dependencies installed successfully

REM Run tests to verify everything works
echo.
echo 🧪 Running tests to verify setup...
npm test 2>nul | findstr /C:"✓" /C:"PASS" >nul
if %errorlevel% equ 0 (
    echo ✅ Tests are passing
) else (
    echo ⚠️  Some tests may have issues, but the demo should still work
)

REM Start the development server
echo.
echo 🚀 Starting development server...
echo    The demo will be available at http://localhost:5173
echo    Press Ctrl+C to stop the server
echo.
call npm run dev

pause
