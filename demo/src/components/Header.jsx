import React from 'react';
import { Book, Brain } from 'lucide-react';

const Header = () => {
  return (
    <header className="bg-blue-600 text-white shadow-lg">
      <div className="max-w-7xl mx-auto px-4 py-6">
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-3">
            <Book className="h-8 w-8" />
            <div>
              <h1 className="text-2xl font-bold">Think Bayes 2</h1>
              <p className="text-blue-100 text-sm">Interactive Bayesian Statistics Demo</p>
            </div>
          </div>
          <div className="flex items-center space-x-2">
            <Brain className="h-6 w-6" />
            <span className="text-sm">by Allen B. Downey</span>
          </div>
        </div>
      </div>
    </header>
  );
};

export default Header;
