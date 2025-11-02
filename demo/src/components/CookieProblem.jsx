import React, { useState } from 'react';
import { Cookie, RefreshCw } from 'lucide-react';

const CookieProblem = () => {
  const [bowl1Vanilla, setBowl1Vanilla] = useState(30);
  const [bowl1Chocolate, setBowl1Chocolate] = useState(10);
  const [bowl2Vanilla, setBowl2Vanilla] = useState(20);
  const [bowl2Chocolate, setBowl2Chocolate] = useState(20);
  const [selectedCookie, setSelectedCookie] = useState('vanilla');

  const calculateProbabilities = () => {
    const totalBowl1 = bowl1Vanilla + bowl1Chocolate;
    const totalBowl2 = bowl2Vanilla + bowl2Chocolate;
    const totalCookies = totalBowl1 + totalBowl2;

    const priorBowl1 = 0.5;
    const priorBowl2 = 0.5;

    let likelihoodBowl1, likelihoodBowl2;
    
    if (selectedCookie === 'vanilla') {
      likelihoodBowl1 = bowl1Vanilla / totalBowl1;
      likelihoodBowl2 = bowl2Vanilla / totalBowl2;
    } else {
      likelihoodBowl1 = bowl1Chocolate / totalBowl1;
      likelihoodBowl2 = bowl2Chocolate / totalBowl2;
    }

    const evidence = priorBowl1 * likelihoodBowl1 + priorBowl2 * likelihoodBowl2;
    const posteriorBowl1 = (priorBowl1 * likelihoodBowl1) / evidence;
    const posteriorBowl2 = (priorBowl1 * likelihoodBowl2) / evidence;

    return {
      posteriorBowl1,
      posteriorBowl2,
      likelihoodBowl1,
      likelihoodBowl2,
      evidence
    };
  };

  const probabilities = calculateProbabilities();

  const formatProbability = (prob) => {
    return (prob * 100).toFixed(1) + '%';
  };

  const resetToDefaults = () => {
    setBowl1Vanilla(30);
    setBowl1Chocolate(10);
    setBowl2Vanilla(20);
    setBowl2Chocolate(20);
    setSelectedCookie('vanilla');
  };

  return (
    <div className="space-y-8">
      <section className="bg-white rounded-lg shadow-md p-6">
        <h2 className="text-3xl font-bold text-gray-800 mb-6 flex items-center">
          <Cookie className="h-8 w-8 mr-3 text-amber-600" />
          The Cookie Problem
        </h2>
        
        <div className="prose max-w-none text-gray-600">
          <p className="mb-4">
            This classic problem demonstrates Bayesian reasoning with a simple scenario:
          </p>
          
          <div className="bg-gray-50 rounded-lg p-4 mb-4">
            <p className="font-medium text-gray-800 mb-2">The Setup:</p>
            <ul className="list-disc list-inside space-y-1">
              <li>There are two bowls of cookies</li>
              <li>You randomly choose one bowl (50% chance each)</li>
              <li>You draw a cookie from the chosen bowl</li>
              <li>Given the cookie type, what's the probability it came from each bowl?</li>
            </ul>
          </div>
        </div>
      </section>

      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-2xl font-semibold text-gray-800 mb-6">Configure the Bowls</h3>
        
        <div className="grid md:grid-cols-2 gap-8">
          <div className="bg-blue-50 rounded-lg p-6 border border-blue-200">
            <h4 className="text-lg font-semibold text-blue-800 mb-4">Bowl 1</h4>
            
            <div className="space-y-4">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Vanilla Cookies: <span className="font-semibold text-blue-600">{bowl1Vanilla}</span>
                </label>
                <input
                  type="range"
                  min="0"
                  max="50"
                  value={bowl1Vanilla}
                  onChange={(e) => setBowl1Vanilla(parseInt(e.target.value))}
                  className="w-full h-2 bg-blue-200 rounded-lg appearance-none cursor-pointer"
                />
              </div>
              
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Chocolate Cookies: <span className="font-semibold text-blue-600">{bowl1Chocolate}</span>
                </label>
                <input
                  type="range"
                  min="0"
                  max="50"
                  value={bowl1Chocolate}
                  onChange={(e) => setBowl1Chocolate(parseInt(e.target.value))}
                  className="w-full h-2 bg-blue-200 rounded-lg appearance-none cursor-pointer"
                />
              </div>
              
              <div className="text-center pt-2 border-t border-blue-200">
                <div className="text-sm text-gray-600">Total</div>
                <div className="text-xl font-bold text-blue-800">{bowl1Vanilla + bowl1Chocolate}</div>
              </div>
            </div>
          </div>

          <div className="bg-green-50 rounded-lg p-6 border border-green-200">
            <h4 className="text-lg font-semibold text-green-800 mb-4">Bowl 2</h4>
            
            <div className="space-y-4">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Vanilla Cookies: <span className="font-semibold text-green-600">{bowl2Vanilla}</span>
                </label>
                <input
                  type="range"
                  min="0"
                  max="50"
                  value={bowl2Vanilla}
                  onChange={(e) => setBowl2Vanilla(parseInt(e.target.value))}
                  className="w-full h-2 bg-green-200 rounded-lg appearance-none cursor-pointer"
                />
              </div>
              
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Chocolate Cookies: <span className="font-semibold text-green-600">{bowl2Chocolate}</span>
                </label>
                <input
                  type="range"
                  min="0"
                  max="50"
                  value={bowl2Chocolate}
                  onChange={(e) => setBowl2Chocolate(parseInt(e.target.value))}
                  className="w-full h-2 bg-green-200 rounded-lg appearance-none cursor-pointer"
                />
              </div>
              
              <div className="text-center pt-2 border-t border-green-200">
                <div className="text-sm text-gray-600">Total</div>
                <div className="text-xl font-bold text-green-800">{bowl2Vanilla + bowl2Chocolate}</div>
              </div>
            </div>
          </div>
        </div>

        <div className="mt-6 flex justify-center">
          <button
            onClick={resetToDefaults}
            className="flex items-center space-x-2 px-4 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors"
          >
            <RefreshCw className="h-4 w-4" />
            <span>Reset to Defaults</span>
          </button>
        </div>
      </section>

      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-2xl font-semibold text-gray-800 mb-6">Select Cookie Type</h3>
        
        <div className="flex justify-center space-x-4 mb-8">
          <button
            onClick={() => setSelectedCookie('vanilla')}
            className={`px-6 py-3 rounded-lg font-medium transition-all ${
              selectedCookie === 'vanilla'
                ? 'bg-amber-100 border-2 border-amber-500 text-amber-800'
                : 'bg-gray-100 border-2 border-gray-300 text-gray-600 hover:bg-gray-200'
            }`}
          >
            🍪 Vanilla Cookie
          </button>
          <button
            onClick={() => setSelectedCookie('chocolate')}
            className={`px-6 py-3 rounded-lg font-medium transition-all ${
              selectedCookie === 'chocolate'
                ? 'bg-amber-100 border-2 border-amber-500 text-amber-800'
                : 'bg-gray-100 border-2 border-gray-300 text-gray-600 hover:bg-gray-200'
            }`}
          >
            🍫 Chocolate Cookie
          </button>
        </div>

        <div className="bg-gradient-to-r from-blue-50 to-green-50 rounded-lg p-6 border border-blue-200">
          <h4 className="text-lg font-semibold text-gray-800 mb-4">Bayesian Analysis Results</h4>
          
          <div className="grid md:grid-cols-2 gap-6">
            <div className="text-center">
              <div className="text-sm text-gray-600 mb-2">Probability from Bowl 1</div>
              <div className="text-4xl font-bold text-blue-600 mb-2">
                {formatProbability(probabilities.posteriorBowl1)}
              </div>
              <div className="text-xs text-gray-500">
                Prior: 50% → Posterior: {formatProbability(probabilities.posteriorBowl1)}
              </div>
            </div>
            
            <div className="text-center">
              <div className="text-sm text-gray-600 mb-2">Probability from Bowl 2</div>
              <div className="text-4xl font-bold text-green-600 mb-2">
                {formatProbability(probabilities.posteriorBowl2)}
              </div>
              <div className="text-xs text-gray-500">
                Prior: 50% → Posterior: {formatProbability(probabilities.posteriorBowl2)}
              </div>
            </div>
          </div>

          <div className="mt-6 p-4 bg-white rounded-lg border border-gray-200">
            <h5 className="font-semibold text-gray-800 mb-3">Calculation Details:</h5>
            <div className="space-y-2 text-sm">
              <div className="flex justify-between">
                <span className="text-gray-600">Likelihood (Bowl 1):</span>
                <span className="font-medium">{formatProbability(probabilities.likelihoodBowl1)}</span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-600">Likelihood (Bowl 2):</span>
                <span className="font-medium">{formatProbability(probabilities.likelihoodBowl2)}</span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-600">Total Evidence:</span>
                <span className="font-medium">{formatProbability(probabilities.evidence)}</span>
              </div>
            </div>
          </div>
        </div>
      </section>

      <section className="bg-amber-50 rounded-lg p-6 border border-amber-200">
        <h4 className="text-lg font-semibold text-amber-800 mb-3">Key Insight</h4>
        <p className="text-amber-700">
          The cookie problem shows how evidence updates our beliefs. Even though we started with 
          a 50/50 belief about which bowl was chosen, observing a vanilla cookie makes it much 
          more likely we chose Bowl 1 (which has more vanilla cookies). This is the essence of 
          Bayesian reasoning - updating prior beliefs with new evidence.
        </p>
      </section>
    </div>
  );
};

export default CookieProblem;
