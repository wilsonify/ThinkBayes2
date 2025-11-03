import React, { useState } from 'react';
import { Book, Calculator, BarChart3, Lightbulb, Activity, Building2, TrendingUp, Zap, Heart, Edit } from 'lucide-react';
import Header from './components/Header';
import Navigation from './components/Navigation';
import BayesTheorem from './components/BayesTheorem';
import CookieProblem from './components/CookieProblem';
import DiceProblem from './components/DiceProblem';
import PMFDemo from './components/PMFDemo';
import Introduction from './components/Introduction';
import HockeyGoals from './components/HockeyGoals';
import Hospital from './components/Hospital';
import ChangepointDetection from './components/ChangepointDetection';
import Radiation from './components/Radiation';
import SurvivalAnalysis from './components/SurvivalAnalysis';
import TyposEstimation from './components/TyposEstimation';

function App() {
  const [activeSection, setActiveSection] = useState('intro');

  const sections = [
    { id: 'intro', title: 'Introduction', icon: Book },
    { id: 'bayes', title: "Bayes' Theorem", icon: Calculator },
    { id: 'cookie', title: 'Cookie Problem', icon: Lightbulb },
    { id: 'dice', title: 'Dice Problem', icon: Calculator },
    { id: 'pmf', title: 'Probability Distributions', icon: BarChart3 },
    { id: 'hockey', title: 'Hockey Goals', icon: Activity },
    { id: 'hospital', title: 'Hospital Birth Rates', icon: Building2 },
    { id: 'changepoint', title: 'Changepoint Detection', icon: TrendingUp },
    { id: 'radiation', title: 'Radiation Sensor', icon: Zap },
    { id: 'survival', title: 'Survival Analysis', icon: Heart },
    { id: 'typos', title: 'Typos Estimation', icon: Edit },
  ];

  const renderContent = () => {
    switch (activeSection) {
      case 'intro':
        return <Introduction />;
      case 'bayes':
        return <BayesTheorem />;
      case 'cookie':
        return <CookieProblem />;
      case 'dice':
        return <DiceProblem />;
      case 'pmf':
        return <PMFDemo />;
      case 'hockey':
        return <HockeyGoals />;
      case 'hospital':
        return <Hospital />;
      case 'changepoint':
        return <ChangepointDetection />;
      case 'radiation':
        return <Radiation />;
      case 'survival':
        return <SurvivalAnalysis />;
      case 'typos':
        return <TyposEstimation />;
      default:
        return <Introduction />;
    }
  };

  return (
    <div className="min-h-screen bg-gray-50">
      <Header />
      <Navigation 
        sections={sections} 
        activeSection={activeSection} 
        onSectionChange={setActiveSection} 
      />
      <main className="max-w-7xl mx-auto px-4 py-8">
        {renderContent()}
      </main>
    </div>
  );
}

export default App;
