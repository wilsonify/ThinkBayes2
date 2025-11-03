import React, { useState } from 'react';
import { Dices, RotateCcw } from 'lucide-react';

const DiceProblem = () => {
  const [diceType, setDiceType] = useState('4');
  const [observedRoll, setObservedRoll] = useState(1);
  const [rollHistory, setRollHistory] = useState([]);
  
  const diceConfig = {
    '4': { sides: 4, color: 'blue' },
    '6': { sides: 6, color: 'red' },
    '8': { sides: 8, color: 'green' },
    '12': { sides: 12, color: 'purple' },
    '20': { sides: 20, color: 'orange' }
  };

  const [hypotheses, setHypotheses] = useState({
    '4': 0.2,
    '6': 0.2,
    '8': 0.2,
    '12': 0.2,
    '20': 0.2
  });

  const rollDice = () => {
    const newRoll = Math.floor(Math.random() * parseInt(diceType)) + 1;
    setObservedRoll(newRoll);
    updateBeliefs(newRoll);
  };

  const updateBeliefs = (roll) => {
    setRollHistory(prev => [...prev, roll]);
    
    setHypotheses(prev => {
      const newHypotheses = { ...prev };
      let totalEvidence = 0;
      
      Object.keys(newHypotheses).forEach(die => {
        const sides = diceConfig[die].sides;
        const likelihood = roll <= sides ? 1/sides : 0;
        totalEvidence += prev[die] * likelihood;
      });
      
      Object.keys(newHypotheses).forEach(die => {
        const sides = diceConfig[die].sides;
        const likelihood = roll <= sides ? 1/sides : 0;
        newHypotheses[die] = (prev[die] * likelihood) / totalEvidence;
      });
      
      return newHypotheses;
    });
  };

  const resetBeliefs = () => {
    setHypotheses({
      '4': 0.2,
      '6': 0.2,
      '8': 0.2,
      '12': 0.2,
      '20': 0.2
    });
    setRollHistory([]);
    setObservedRoll(1);
  };

  const formatProbability = (prob) => {
    return (prob * 100).toFixed(1) + '%';
  };

  const getMostLikely = () => {
    return Object.entries(hypotheses).reduce((a, b) => 
      hypotheses[a[0]] > hypotheses[b[0]] ? a : b
    )[0];
  };

  return (
    <div className="space-y-8">
      <section className="bg-white rounded-lg shadow-md p-6">
        <h2 className="text-3xl font-bold text-gray-800 mb-6 flex items-center">
          <Dices className="h-8 w-8 mr-3 text-purple-600" />
          The Dice Problem
        </h2>
        
        <div className="prose max-w-none text-gray-600">
          <p className="mb-4">
            Imagine you have a box containing different types of dice (4-sided, 6-sided, 8-sided, 12-sided, and 20-sided). 
            You randomly select one die and roll it. Based on the results, can you determine which die you probably chose?
          </p>
          
          <div className="bg-gray-50 rounded-lg p-4 mb-4">
            <p className="font-medium text-gray-800 mb-2">The Challenge:</p>
            <ul className="list-disc list-inside space-y-1">
              <li>Start with equal belief for each die type (20% each)</li>
              <li>Roll the selected die and observe the result</li>
              <li>Update your beliefs using Bayes' theorem</li>
              <li>Higher rolls are impossible on dice with fewer sides</li>
            </ul>
          </div>
        </div>
      </section>

      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-2xl font-semibold text-gray-800 mb-6">Simulate the Experiment</h3>
        
        <div className="grid md:grid-cols-2 gap-8">
          <div className="bg-purple-50 rounded-lg p-6 border border-purple-200">
            <h4 className="text-lg font-semibold text-purple-800 mb-4">Setup</h4>
            
            <div className="mb-6">
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Secret Die Type (for simulation):
              </label>
              <select
                value={diceType}
                onChange={(e) => setDiceType(e.target.value)}
                className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-purple-500"
              >
                {Object.entries(diceConfig).map(([type, config]) => (
                  <option key={type} value={type}>
                    {config.sides}-sided die
                  </option>
                ))}
              </select>
            </div>

            <div className="text-center">
              <button
                onClick={rollDice}
                className="px-6 py-3 bg-purple-600 text-white rounded-lg hover:bg-purple-700 transition-colors font-medium"
              >
                🎲 Roll the Die
              </button>
            </div>

            {rollHistory.length > 0 && (
              <div className="mt-6 text-center">
                <div className="text-sm text-gray-600 mb-2">Last Roll:</div>
                <div className="text-4xl font-bold text-purple-800">{observedRoll}</div>
                <div className="text-sm text-gray-500 mt-1">
                  Roll History: {rollHistory.join(', ')}
                </div>
              </div>
            )}
          </div>

          <div className="bg-blue-50 rounded-lg p-6 border border-blue-200">
            <h4 className="text-lg font-semibold text-blue-800 mb-4">Current Beliefs</h4>
            
            <div className="space-y-3">
              {Object.entries(hypotheses).map(([die, probability]) => {
                const config = diceConfig[die];
                const isMostLikely = die === getMostLikely();
                
                return (
                  <div key={die} className="flex items-center justify-between">
                    <div className="flex items-center space-x-2">
                      <div className={`w-4 h-4 rounded bg-${config.color}-500`}></div>
                      <span className={`font-medium ${isMostLikely ? 'text-blue-800' : 'text-gray-700'}`}>
                        {config.sides}-sided
                      </span>
                      {isMostLikely && (
                        <span className="text-xs bg-blue-600 text-white px-2 py-1 rounded">Most Likely</span>
                      )}
                    </div>
                    <div className="flex items-center space-x-2">
                      <div className="w-24 bg-gray-200 rounded-full h-2">
                        <div
                          className={`bg-${config.color}-500 h-2 rounded-full transition-all duration-300`}
                          style={{ width: `${probability * 100}%` }}
                        ></div>
                      </div>
                      <span className="text-sm font-medium w-12 text-right">
                        {formatProbability(probability)}
                      </span>
                    </div>
                  </div>
                );
              })}
            </div>

            <button
              onClick={resetBeliefs}
              className="mt-4 w-full flex items-center justify-center space-x-2 px-4 py-2 bg-gray-600 text-white rounded-lg hover:bg-gray-700 transition-colors"
            >
              <RotateCcw className="h-4 w-4" />
              <span>Reset Beliefs</span>
            </button>
          </div>
        </div>
      </section>

      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-2xl font-semibold text-gray-800 mb-6">How It Works</h3>
        
        <div className="space-y-6">
          <div className="bg-gray-50 rounded-lg p-4">
            <h4 className="font-semibold text-gray-800 mb-3">Bayesian Update Process</h4>
            
            <div className="space-y-3 text-sm">
              <div className="flex items-start space-x-3">
                <div className="w-6 h-6 bg-blue-500 text-white rounded-full flex items-center justify-center text-xs font-bold">1</div>
                <div>
                  <strong>Prior:</strong> Start with equal belief (20% each) for all dice types
                </div>
              </div>
              
              <div className="flex items-start space-x-3">
                <div className="w-6 h-6 bg-purple-500 text-white rounded-full flex items-center justify-center text-xs font-bold">2</div>
                <div>
                  <strong>Evidence:</strong> Observe a dice roll
                </div>
              </div>
              
              <div className="flex items-start space-x-3">
                <div className="w-6 h-6 bg-green-500 text-white rounded-full flex items-center justify-center text-xs font-bold">3</div>
                <div>
                  <strong>Likelihood:</strong> Calculate probability of this roll for each die type
                  <div className="text-xs text-gray-600 mt-1">
                    If roll = 7: 4-sided (0%), 6-sided (0%), 8-sided (12.5%), 12-sided (8.3%), 20-sided (5%)
                  </div>
                </div>
              </div>
              
              <div className="flex items-start space-x-3">
                <div className="w-6 h-6 bg-orange-500 text-white rounded-full flex items-center justify-center text-xs font-bold">4</div>
                <div>
                  <strong>Posterior:</strong> Update beliefs using Bayes' theorem
                </div>
              </div>
            </div>
          </div>

          <div className="bg-yellow-50 rounded-lg p-4 border border-yellow-200">
            <h4 className="font-semibold text-yellow-800 mb-2">Key Insights</h4>
            <ul className="text-yellow-700 space-y-1 text-sm">
              <li>• Rolls higher than a die's maximum are impossible (0% likelihood)</li>
              <li>• Higher rolls provide more information by eliminating smaller dice</li>
              <li>• Multiple rolls quickly converge to the correct die type</li>
              <li>• This demonstrates how evidence systematically reduces uncertainty</li>
            </ul>
          </div>
        </div>
      </section>

      {rollHistory.length >= 3 && (
        <section className="bg-green-50 rounded-lg p-6 border border-green-200">
          <h4 className="text-lg font-semibold text-green-800 mb-3">Analysis Complete!</h4>
          <p className="text-green-700">
            After {rollHistory.length} rolls, you're {formatProbability(hypotheses[getMostLikely()])} confident 
            that you're using a {diceConfig[getMostLikely()].sides}-sided die. 
            Bayesian inference has successfully identified the most likely hypothesis based on the evidence.
          </p>
        </section>
      )}
    </div>
  );
};

export default DiceProblem;
