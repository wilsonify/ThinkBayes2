import React, { useState, useMemo } from 'react';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts';

const HockeyGoals = () => {
  const [goalsObserved, setGoalsObserved] = useState([3, 5, 2, 4, 6, 3, 4, 5, 2, 3]);
  const [alpha, setAlpha] = useState(2.0);
  const [beta, setBeta] = useState(1.0);

  // Helper functions
  const factorial = (n) => {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
  };

  // Gamma function approximation
  const gamma = (n) => {
    const g = 7;
    const p = [
      0.99999999999980993, 676.5203681218851, -1259.1392167224028,
      771.32342877765313, -176.61502916214059, 12.507343278686905,
      -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
    ];
    
    if (n < 0.5) {
      return Math.PI / (Math.sin(Math.PI * n) * gamma(1 - n));
    }
    
    n--;
    let x = p[0];
    for (let i = 1; i < g + 2; i++) {
      x += p[i] / (n + i);
    }
    
    let t = n + g + 0.5;
    return Math.sqrt(2 * Math.PI) * Math.pow(t, n + 0.5) * Math.exp(-t) * x;
  };

  // Gamma PDF function
  const gammaPDF = (x, alpha, beta) => {
    if (x <= 0) return 0;
    return Math.pow(beta, alpha) * Math.pow(x, alpha - 1) * Math.exp(-beta * x) / gamma(alpha);
  };

  // Poisson likelihood function
  const poissonLikelihood = (data, mu) => {
    return data.reduce((likelihood, goals) => {
      return likelihood * (Math.pow(mu, goals) * Math.exp(-mu) / factorial(goals));
    }, 1);
  };

  // Calculate posterior using gamma-Poisson model
  const posterior = useMemo(() => {
    const n = goalsObserved.length;
    const sumGoals = goalsObserved.reduce((a, b) => a + b, 0);
    
    // Posterior parameters for gamma distribution
    const posteriorAlpha = alpha + sumGoals;
    const posteriorBeta = beta + n;
    
    // Generate data for visualization
    const data = [];
    for (let mu = 0.1; mu <= 10; mu += 0.1) {
      const prior = gammaPDF(mu, alpha, beta);
      const posterior = gammaPDF(mu, posteriorAlpha, posteriorBeta);
      const likelihood = poissonLikelihood(goalsObserved, mu);
      
      data.push({
        mu: parseFloat(mu.toFixed(2)),
        prior: parseFloat(prior.toFixed(4)),
        posterior: parseFloat(posterior.toFixed(4)),
        likelihood: parseFloat(likelihood.toFixed(4))
      });
    }
    
    return {
      data,
      posteriorMean: posteriorAlpha / posteriorBeta,
      posteriorAlpha,
      posteriorBeta
    };
  }, [goalsObserved, alpha, beta]);

  const handleGoalsChange = (index, value) => {
    const newGoals = [...goalsObserved];
    newGoals[index] = parseInt(value) || 0;
    setGoalsObserved(newGoals);
  };

  const addGame = () => {
    setGoalsObserved([...goalsObserved, 3]);
  };

  const removeGame = (index) => {
    if (goalsObserved.length > 1) {
      setGoalsObserved(goalsObserved.filter((_, i) => i !== index));
    }
  };

  return (
    <div className="space-y-6">
      <div className="text-center">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Hockey Goals Analysis</h1>
        <p className="text-gray-600 max-w-2xl mx-auto">
          Gamma-Poisson hierarchical model for analyzing hockey goal scoring rates. 
          This example demonstrates how to estimate the true scoring rate of a team 
          based on observed goals from multiple games.
        </p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Input Controls */}
        <section className="bg-white rounded-lg shadow-md p-6">
          <h2 className="text-xl font-semibold text-gray-800 mb-4 flex items-center gap-2">
            <span className="px-2 py-1 bg-blue-100 text-blue-800 text-xs rounded">Data Input</span>
            Game Results
          </h2>
          <div className="space-y-4">
            <div>
              <label className="block text-sm font-medium mb-2">Goals Scored per Game</label>
              <div className="space-y-2 max-h-48 overflow-y-auto">
                {goalsObserved.map((goals, index) => (
                  <div key={index} className="flex items-center gap-2">
                    <span className="text-sm w-16">Game {index + 1}:</span>
                    <input
                      type="number"
                      min="0"
                      max="15"
                      value={goals}
                      onChange={(e) => handleGoalsChange(index, e.target.value)}
                      className="flex-1 px-3 py-1 border rounded-md"
                    />
                    <button
                      onClick={() => removeGame(index)}
                      className="px-2 py-1 text-red-600 hover:bg-red-50 rounded"
                      disabled={goalsObserved.length === 1}
                    >
                      Remove
                    </button>
                  </div>
                ))}
              </div>
              <button
                onClick={addGame}
                className="mt-2 px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
              >
                Add Game
              </button>
            </div>

            <div>
              <label className="block text-sm font-medium mb-2">
                Prior Alpha: {alpha.toFixed(1)}
              </label>
              <input
                type="range"
                min="0.1"
                max="10"
                step="0.1"
                value={alpha}
                onChange={(e) => setAlpha(parseFloat(e.target.value))}
                className="w-full"
              />
            </div>

            <div>
              <label className="block text-sm font-medium mb-2">
                Prior Beta: {beta.toFixed(1)}
              </label>
              <input
                type="range"
                min="0.1"
                max="10"
                step="0.1"
                value={beta}
                onChange={(e) => setBeta(parseFloat(e.target.value))}
                className="w-full"
              />
            </div>
          </div>
        </section>

        {/* Results */}
        <section className="bg-white rounded-lg shadow-md p-6">
          <h2 className="text-xl font-semibold text-gray-800 mb-4 flex items-center gap-2">
            <span className="px-2 py-1 bg-green-100 text-green-800 text-xs rounded">Results</span>
            Posterior Analysis
          </h2>
          <div className="space-y-4">
            <div className="grid grid-cols-2 gap-4">
              <div className="p-3 bg-blue-50 rounded">
                <div className="text-sm text-gray-600">Posterior Mean</div>
                <div className="text-xl font-bold text-blue-600">
                  {posterior.posteriorMean.toFixed(3)}
                </div>
                <div className="text-xs text-gray-500">goals per game</div>
              </div>
              <div className="p-3 bg-green-50 rounded">
                <div className="text-sm text-gray-600">Total Games</div>
                <div className="text-xl font-bold text-green-600">
                  {goalsObserved.length}
                </div>
                <div className="text-xs text-gray-500">observed</div>
              </div>
            </div>
            
            <div className="p-3 bg-gray-50 rounded">
              <div className="text-sm text-gray-600 mb-2">Posterior Parameters</div>
              <div className="text-sm">
                <div>Alpha = {posterior.posteriorAlpha.toFixed(2)}</div>
                <div>Beta = {posterior.posteriorBeta.toFixed(2)}</div>
                <div className="mt-1 text-gray-500">
                  Gamma(α={posterior.posteriorAlpha.toFixed(1)}, β={posterior.posteriorBeta.toFixed(1)})
                </div>
              </div>
            </div>

            <div className="p-3 bg-yellow-50 rounded">
              <div className="text-sm text-gray-600 mb-1">Interpretation</div>
              <div className="text-sm">
                Based on {goalsObserved.length} games with {goalsObserved.reduce((a, b) => a + b, 0)} total goals,
                we estimate the team's true scoring rate is {posterior.posteriorMean.toFixed(2)} goals per game.
              </div>
            </div>
          </div>
        </section>
      </div>

      {/* Visualization */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h2 className="text-xl font-semibold text-gray-800 mb-4">Probability Distributions</h2>
        <ResponsiveContainer width="100%" height={400}>
          <LineChart data={posterior.data}>
            <CartesianGrid strokeDasharray="3 3" />
            <XAxis 
              dataKey="mu" 
              label={{ value: 'Goals per Game (μ)', position: 'insideBottom', offset: -5 }}
            />
            <YAxis 
              label={{ value: 'Probability Density', angle: -90, position: 'insideLeft' }}
            />
            <Tooltip />
            <Legend />
            <Line 
              type="monotone" 
              dataKey="prior" 
              stroke="#8884d8" 
              strokeWidth={2}
              dot={false}
              name="Prior"
            />
            <Line 
              type="monotone" 
              dataKey="likelihood" 
              stroke="#82ca9d" 
              strokeWidth={2}
              dot={false}
              name="Likelihood"
            />
            <Line 
              type="monotone" 
              dataKey="posterior" 
              stroke="#ff7300" 
              strokeWidth={3}
              dot={false}
              name="Posterior"
            />
          </LineChart>
        </ResponsiveContainer>
      </section>

      {/* Mathematical Explanation */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h2 className="text-xl font-semibold text-gray-800 mb-4">Mathematical Framework</h2>
        <div className="space-y-4 text-sm">
          <div>
            <h3 className="font-semibold mb-2">Model Structure</h3>
            <div className="bg-gray-50 p-3 rounded font-mono text-xs">
              <div>μ ~ Gamma(α, β)  (scoring rate)</div>
              <div>goals_i ~ Poisson(μ)  (observed goals)</div>
            </div>
          </div>
          
          <div>
            <h3 className="font-semibold mb-2">Posterior Update</h3>
            <div className="bg-gray-50 p-3 rounded font-mono text-xs">
              <div>α_post = α_prior + Σgoals_i</div>
              <div>β_post = β_prior + n_games</div>
              <div>μ_post ~ Gamma(α_post, β_post)</div>
            </div>
          </div>
          
          <div>
            <h3 className="font-semibold mb-2">Key Insights</h3>
            <ul className="list-disc list-inside space-y-1 text-gray-600">
              <li>The gamma distribution is conjugate to the Poisson likelihood</li>
              <li>Posterior mean provides point estimate of true scoring rate</li>
              <li>More games observed → more confident estimate</li>
              <li>Prior parameters represent initial beliefs about scoring rates</li>
            </ul>
          </div>
        </div>
      </section>
    </div>
  );
};

export default HockeyGoals;
