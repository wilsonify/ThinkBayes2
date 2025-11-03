# Think Bayes 2 - Interactive Demo

An interactive frontend application demonstrating key concepts from Allen B. Downey's "Think Bayes 2" book. This web app provides hands-on examples of Bayesian statistics with real-time visualizations and interactive calculations.

## 🎯 Available Demos

### 1. **Hockey Goals Analysis**
- **Bayesian Model**: Gamma-Poisson distribution for hockey goal scoring
- **Features**: Interactive goal data input, prior/posterior distribution visualization
- **Learning**: Bayesian inference for count data with hierarchical models

### 2. **Survival Analysis**
- **Bayesian Model**: Weibull distribution for time-to-event data
- **Features**: Kaplan-Meier survival curves, hazard function visualization
- **Learning**: Survival analysis and reliability modeling

### 3. **Changepoint Detection**
- **Bayesian Model**: Poisson processes with structural change detection
- **Features**: Time series event analysis, probability distribution for changepoint location
- **Learning**: Bayesian detection of structural changes in sequential data

### 4. **Hospital Birth Rates**
- **Bayesian Model**: Hierarchical models for hospital performance analysis
- **Features**: Multi-level Bayesian inference, hospital comparison with uncertainty
- **Learning**: Hierarchical Bayesian modeling and partial pooling

### 5. **Radiation Sensor Analysis**
- **Bayesian Model**: Poisson-Gamma model for radiation detection
- **Features**: Sensor calibration, source strength estimation, efficiency modeling
- **Learning**: Bayesian inference for physical measurement systems

### 6. **Typos Estimation (Capture-Recapture)**
- **Bayesian Model**: Lincoln index and Bayesian capture-recapture
- **Features**: Two-reader typo detection, population estimation with uncertainty
- **Learning**: Capture-recapture methods for population estimation

## 🚀 Quick Start

### Prerequisites
- Node.js 16+ installed on your system
- Modern web browser (Chrome, Firefox, Safari, Edge)

### Installation & Running

#### 🚀 Quick Start (Recommended)

**For macOS/Linux:**
```bash
cd demo
./setup.sh
```

**For Windows:**
```cmd
cd demo
setup.bat
```

The setup script will automatically:
- ✅ Check for Node.js 16+
- ✅ Install all dependencies  
- ✅ Run tests to verify setup
- ✅ Start the development server

#### 📋 Manual Setup

If you prefer manual setup:

1. **Navigate to the demo directory**:
   ```bash
   cd demo
   ```

2. **Install dependencies**:
   ```bash
   npm install
   ```

3. **Start the development server**:
   ```bash
   npm run dev
   ```

4. **Open your browser** and navigate to the URL shown (usually `http://localhost:5173` or `http://localhost:3000`)

5. **Explore the demos** using the navigation menu:
   - Click on any demo name in the navigation bar
   - Each demo loads instantly with no 404 errors
   - All interactive elements are fully functional

### Alternative: Production Build

If you prefer to build and serve the demo statically:

1. **Build for production**:
   ```bash
   npm run build
   ```

2. **Serve the built files** (using any static server):
   ```bash
   # Using Python
   python -m http.server 8000 -d dist
   
   # Using Node.js serve (if installed)
   npx serve dist
   
   # Using Vite preview
   npm run preview
   ```

3. **Open your browser** and navigate to `http://localhost:8000` (or appropriate port)

## 🛠️ Development Commands

```bash
# Start development server with hot reload
npm run dev

# Run tests for all demos
npm test

# Run tests for specific demo
npm test -- --run src/test/HockeyGoals.test.jsx

# Build for production
npm run build

# Preview production build
npm run preview

# Run linting (if configured)
npm run lint
```

## 📊 Technologies Used

- **React 18** - Modern component-based UI framework
- **Vite** - Fast development build tool with hot module replacement
- **Tailwind CSS** - Utility-first CSS framework for styling
- **Recharts** - Interactive chart library for data visualization
- **KaTeX** - Mathematical notation rendering
- **Vitest** - Fast unit testing framework

## 🏗️ Project Structure

```
demo/
├── src/
│   ├── components/
│   │   ├── HockeyGoals.jsx           # Hockey goals analysis
│   │   ├── SurvivalAnalysis.jsx      # Survival analysis demo
│   │   ├── ChangepointDetection.jsx  # Changepoint detection
│   │   ├── Hospital.jsx              # Hospital birth rates
│   │   ├── Radiation.jsx             # Radiation sensor analysis
│   │   ├── TyposEstimation.jsx       # Capture-recapture typos
│   │   ├── Header.jsx                # App header and navigation
│   │   └── Navigation.jsx            # Demo navigation menu
│   ├── test/                         # Unit tests for each demo
│   │   ├── HockeyGoals.test.jsx
│   │   ├── SurvivalAnalysis.test.jsx
│   │   ├── ChangepointDetection.test.jsx
│   │   ├── Hospital.test.jsx
│   │   ├── Radiation.test.jsx
│   │   └── TyposEstimation.test.jsx
│   ├── App.jsx                       # Main application router
│   ├── main.jsx                      # Application entry point
│   └── index.css                     # Global styles
├── dist/                             # Production build output
├── index.html                        # HTML template
├── package.json                      # Dependencies and scripts
├── vite.config.js                    # Vite configuration
├── vitest.config.js                  # Test configuration
└── tailwind.config.js                # Tailwind CSS configuration
```

## 🎓 Learning Objectives

Each demo helps users understand specific Bayesian concepts:

### Core Bayesian Concepts
- **Prior Distributions**: Representing initial beliefs
- **Likelihood Functions**: How data relates to parameters
- **Posterior Inference**: Updating beliefs with evidence
- **Predictive Distributions**: Making predictions with uncertainty

### Advanced Topics
- **Hierarchical Models**: Multi-level Bayesian inference
- **Time Series Analysis**: Sequential data and changepoints
- **Survival Analysis**: Time-to-event modeling
- **Capture-Recapture Methods**: Population estimation

### Practical Applications
- **Sports Analytics**: Hockey goal scoring patterns
- **Medical Statistics**: Survival analysis and reliability
- **Quality Control**: Changepoint detection in processes
- **Sensor Calibration**: Physical measurement systems
- **Population Ecology**: Capture-recapture estimation

## 🔧 Troubleshooting

### Common Issues

1. **"404 Error" when clicking demos**
   - **Solution**: All demos are now fixed! If you see 404 errors, ensure you're running the latest version

2. **"Port already in use" error**
   - **Solution**: The dev server will automatically try the next available port (3001, 3002, etc.)
   - **Alternative**: Kill existing Node processes: `pkill -f "node.*vite"`

3. **"Module not found" errors**
   - **Solution**: Run `npm install` to ensure all dependencies are installed
   - **Alternative**: Delete `node_modules` and `package-lock.json`, then run `npm install` again

4. **Build fails with JSX errors**
   - **Solution**: Ensure all files are saved and there are no syntax errors
   - **Check**: Run `npm test` to identify specific issues

### Performance Tips

- **Development**: Use `npm run dev` for fast development with hot reload
- **Testing**: Run `npm test` to verify all demos work correctly
- **Production**: Use `npm run build` for optimized production builds

## 🤝 Contributing

This demo accompanies the Think Bayes 2 book. To contribute:

1. **Fork the repository**
2. **Create a feature branch**: `git checkout -b new-demo-feature`
3. **Add your demo** with corresponding tests
4. **Update documentation** in this README
5. **Submit a pull request**

### Adding New Demos

1. Create component in `src/components/YourDemo.jsx`
2. Add test file in `src/test/YourDemo.test.jsx`
3. Import and add to navigation in `src/App.jsx`
4. Update this README with demo description

## 📚 Resources

- **[Think Bayes 2 Book](http://allendowney.github.io/ThinkBayes2)** - Original textbook
- **[Official Repository](https://github.com/AllenDowney/ThinkBayes2)** - Book source code
- **[React Documentation](https://react.dev)** - React framework docs
- **[Vite Documentation](https://vitejs.dev)** - Build tool docs
- **[Tailwind CSS](https://tailwindcss.com)** - Styling framework
- **[Recharts Documentation](https://recharts.org)** - Chart library

## 📄 License

This demo follows the same license as Think Bayes 2: Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0).

---

**🎉 All demos are fully functional and ready to explore!** Each demo includes interactive controls, real-time visualizations, and mathematical frameworks to help you understand Bayesian concepts through hands-on experimentation.
