import React, { useState, useMemo } from 'react';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer, BarChart, Bar } from 'recharts';

const TyposEstimation = () => {
  const [observer1Typos, setObserver1Typos] = useState(1);
  const [observer2Typos, setObserver2Typos] = useState(1);
  const [sharedTypos, setSharedTypos] = useState(0);
  const [totalPages, setTotalPages] = useState(200);

  // Helper functions
  const binomialCoefficient = (n, k) => {
    if (k > n || k < 0) return 0;
    if (k === 0 || k === n) return 1;
    if (k > n - k) k = n - k; // Take advantage of symmetry
    
    let result = 1;
    for (let i = 1; i <= k; i++) {
      result = result * (n - k + i) / i;
    }
    return result;
  };

  // Calculate likelihood using hypergeometric distribution
  const calculateLikelihood = (N, n1, n2, k) => {
    if (k > Math.min(n1, n2) || k > N) return 0;
    
    // Simplified likelihood calculation
    const coefficient = binomialCoefficient(n1, k) * binomialCoefficient(N - n1, n2 - k);
    const denominator = binomialCoefficient(N, n2);
    
    return denominator > 0 ? coefficient / denominator : 0;
  };

  // Calculate Lincoln index and Bayesian estimates
  const typoResults = useMemo(() => {
    // Lincoln index estimation
    const lincolnIndex = observer1Typos > 0 && observer2Typos > 0 
      ? (observer1Typos * observer2Typos) / sharedTypos 
      : 0;

    // Bayesian estimation using capture-recapture model
    const maxPopulation = 50;
    const posteriorData = [];
    
    for (let N = sharedTypos + Math.max(observer1Typos, observer2Typos); N <= maxPopulation; N++) {
      // Calculate likelihood using hypergeometric distribution
      const likelihood = calculateLikelihood(N, observer1Typos, observer2Typos, sharedTypos);
      
      // Use uniform prior
      const prior = 1 / maxPopulation;
      
      posteriorData.push({
        population: N,
        likelihood: likelihood,
        posterior: likelihood * prior
      });
    }
    
    // Normalize posterior
    const totalPosterior = posteriorData.reduce((sum, d) => sum + d.posterior, 0);
    const normalizedPosterior = posteriorData.map(d => ({
      ...d,
      normalizedPosterior: d.posterior / totalPosterior
    }));
    
    // Calculate posterior statistics
    const posteriorMean = normalizedPosterior.reduce((sum, d) => sum + d.population * d.normalizedPosterior, 0);
    
    // Find MAP estimate
    const mapEstimate = normalizedPosterior.reduce((best, current) => 
      current.normalizedPosterior > best.normalizedPosterior ? current : best
    );
    
    // Calculate credible interval
    const sortedPosterior = [...normalizedPosterior].sort((a, b) => a.normalizedPosterior - b.normalizedPosterior);
    let cumulative = 0;
    let lowerBound = 0, upperBound = maxPopulation;
    
    for (const point of sortedPosterior) {
      cumulative += point.normalizedPosterior;
      if (cumulative >= 0.025 && lowerBound === 0) lowerBound = point.population;
      if (cumulative >= 0.975) {
        upperBound = point.population;
        break;
      }
    }
    
    // Generate scenarios
    const scenarios = [
      {
        name: "Different typos found",
        observer1: observer1Typos,
        observer2: observer2Typos,
        shared: sharedTypos,
        description: "Current scenario - different typos"
      },
      {
        name: "Same typo found",
        observer1: observer1Typos,
        observer2: observer1Typos,
        shared: observer1Typos,
        description: "If both found the same typo(s)"
      },
      {
        name: "No overlap",
        observer1: observer1Typos,
        observer2: observer2Typos,
        shared: 0,
        description: "If no typos were shared"
      }
    ].map(scenario => ({
      ...scenario,
      estimate: scenario.observer1 > 0 && scenario.observer2 > 0 && scenario.shared > 0
        ? (scenario.observer1 * scenario.observer2) / scenario.shared
        : 0,
      additionalExpected: scenario.observer1 > 0 && scenario.observer2 > 0 && scenario.shared > 0
        ? Math.max(0, (scenario.observer1 * scenario.observer2) / scenario.shared - scenario.observer1 - scenario.observer2 + scenario.shared)
        : 0
    }));

    return {
      lincolnIndex: Math.round(lincolnIndex),
      posteriorMean: Math.round(posteriorMean),
      mapEstimate: mapEstimate.population,
      credibleInterval: [lowerBound, upperBound],
      posteriorData: normalizedPosterior,
      scenarios
    };
  }, [observer1Typos, observer2Typos, sharedTypos, totalPages]);

  return (
    <div className="space-y-6">
      <div className="text-center">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Typos Estimation Problem</h1>
        <p className="text-gray-600 max-w-2xl mx-auto">
          Bayesian capture-recapture analysis for estimating the total number of typos 
          in a document based on two readers' findings. This example demonstrates the 
          Lincoln index and Bayesian inference for population estimation.
        </p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Input Controls */}
        <section className="bg-white rounded-lg shadow-md p-6">
          <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
            <span className="px-2 py-1 text-xs border border-gray-300 rounded">Data Input</span>
            Reader Observations
          </h3>
          <div className="space-y-4">
            <div className="space-y-4">
              <div>
                <label className="block text-sm font-medium mb-2">
                  Reader 1 found: {observer1Typos} typo{observer1Typos !== 1 ? 's' : ''}
                </label>
                <input
                  type="range"
                  min="0"
                  max="10"
                  value={observer1Typos}
                  onChange={(e) => setObserver1Typos(parseInt(e.target.value))}
                  className="w-full"
                />
              </div>
              
              <div>
                <label className="block text-sm font-medium mb-2">
                  Reader 2 found: {observer2Typos} typo{observer2Typos !== 1 ? 's' : ''}
                </label>
                <input
                  type="range"
                  min="0"
                  max="10"
                  value={observer2Typos}
                  onChange={(e) => setObserver2Typos(parseInt(e.target.value))}
                  className="w-full"
                />
              </div>
              
              <div>
                <label className="block text-sm font-medium mb-2">
                  Shared typos: {sharedTypos}
                </label>
                <input
                  type="range"
                  min="0"
                  max={Math.min(observer1Typos, observer2Typos)}
                  value={sharedTypos}
                  onChange={(e) => setSharedTypos(parseInt(e.target.value))}
                  className="w-full"
                />
              </div>
              
              <div>
                <label className="block text-sm font-medium mb-2">
                  Total pages: {totalPages}
                </label>
                <input
                  type="range"
                  min="50"
                  max="500"
                  step="10"
                  value={totalPages}
                  onChange={(e) => setTotalPages(parseInt(e.target.value))}
                  className="w-full"
                />
              </div>
            </div>

            <div className="p-3 bg-blue-50 rounded">
              <div className="text-sm text-gray-600 mb-1">Unique typos found</div>
              <div className="text-lg font-bold text-blue-600">
                {observer1Typos + observer2Typos - sharedTypos}
              </div>
              <div className="text-xs text-gray-500">
                out of {totalPages} pages
              </div>
            </div>
          </div>
        </section>

        {/* Results */}
        <section className="bg-white rounded-lg shadow-md p-6">
          <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
            <span className="px-2 py-1 text-xs border border-gray-300 rounded">Results</span>
            Population Estimates
          </h3>
          <div>
            <div className="space-y-4">
              <div className="grid grid-cols-2 gap-4">
                <div className="p-3 bg-green-50 rounded">
                  <div className="text-sm text-gray-600">Lincoln Index</div>
                  <div className="text-xl font-bold text-green-600">
                    {typoResults.lincolnIndex}
                  </div>
                  <div className="text-xs text-gray-500">total typos</div>
                </div>
                <div className="p-3 bg-purple-50 rounded">
                  <div className="text-sm text-gray-600">Bayesian Mean</div>
                  <div className="text-xl font-bold text-purple-600">
                    {typoResults.posteriorMean}
                  </div>
                  <div className="text-xs text-gray-500">total typos</div>
                </div>
              </div>
              
              <div className="p-3 bg-yellow-50 rounded">
                <div className="text-sm text-gray-600 mb-1">95% Credible Interval</div>
                <div className="text-lg font-bold text-yellow-600">
                  [{typoResults.credibleInterval[0]}, {typoResults.credibleInterval[1]}]
                </div>
                <div className="text-xs text-gray-500">total typos</div>
              </div>

              <div className="p-3 bg-red-50 rounded">
                <div className="text-sm text-gray-600 mb-1">Expected Additional Typos</div>
                <div className="text-lg font-bold text-red-600">
                  {Math.max(0, typoResults.lincolnIndex - observer1Typos - observer2Typos + sharedTypos)}
                </div>
                <div className="text-xs text-gray-500">still undiscovered</div>
              </div>
            </div>
          </div>
        </section>
      </div>

      {/* Scenario Comparison */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-lg font-semibold mb-4">Scenario Analysis</h3>
        <div>
          <ResponsiveContainer width="100%" height={300}>
            <BarChart data={typoResults.scenarios}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="name" />
              <YAxis 
                label={{ value: 'Estimated Total Typos', angle: -90, position: 'insideLeft' }}
              />
              <Tooltip />
              <Legend />
              <Bar 
                dataKey="estimate" 
                fill="#8884d8" 
                name="Estimated Total"
              />
              <Bar 
                dataKey="additionalExpected" 
                fill="#82ca9d" 
                name="Additional Expected"
              />
            </BarChart>
          </ResponsiveContainer>
        </div>
      </section>

      {/* Posterior Distribution */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-lg font-semibold mb-4">Posterior Distribution of Total Typos</h3>
        <div>
          <ResponsiveContainer width="100%" height={300}>
            <LineChart data={typoResults.posteriorData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="population" 
                label={{ value: 'Total Number of Typos', position: 'insideBottom', offset: -5 }}
              />
              <YAxis 
                label={{ value: 'Posterior Probability', angle: -90, position: 'insideLeft' }}
              />
              <Tooltip />
              <Line 
                type="monotone" 
                dataKey="normalizedPosterior" 
                stroke="#ff7300" 
                strokeWidth={2}
                dot={false}
              />
            </LineChart>
          </ResponsiveContainer>
        </div>
      </section>

      {/* Mathematical Framework */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-lg font-semibold mb-4">Mathematical Framework</h3>
        <div className="space-y-4 text-sm">
            <div>
              <h4 className="font-semibold mb-2">Lincoln Index</h4>
              <div className="bg-gray-50 p-3 rounded font-mono text-xs">
                <div>N̂ = (n₁ × n₂) / k</div>
                <div>where: n₁, n₂ = typos found by each reader</div>
                <div>k = shared typos between readers</div>
              </div>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Bayesian Model</h4>
              <div className="bg-gray-50 p-3 rounded font-mono text-xs">
                <div>P(N | data) ∝ P(data | N) × P(N)</div>
                <div>P(data | N) = Hypergeometric(N, n₁, n₂, k)</div>
                <div>P(N) = Uniform(N_min, N_max)</div>
              </div>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Key Assumptions</h4>
              <ul className="list-disc list-inside space-y-1 text-gray-600">
                <li>Each typo has equal probability of being found</li>
                <li>Readers work independently</li>
                <li>Population is closed (no new typos introduced)</li>
                <li>All found typos are correctly identified</li>
              </ul>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Applications</h4>
              <ul className="list-disc list-inside space-y-1 text-gray-600">
                <li>Wildlife population estimation</li>
                <li>Software bug detection</li>
                <li>Quality control in manufacturing</li>
                <li>Epidemiological studies</li>
                <li>Literature review and meta-analysis</li>
              </ul>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Extensions</h4>
              <ul className="list-disc list-inside space-y-1 text-gray-600">
                <li>Multiple observers (more than 2)</li>
                <li>Heterogeneous detection probabilities</li>
                <li>Time-varying detection rates</li>
                <li>Open population models</li>
              </ul>
            </div>
          </div>
      </section>
    </div>
  );
};

export default TyposEstimation;
