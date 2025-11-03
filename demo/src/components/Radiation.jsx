import React, { useState, useMemo } from 'react';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer, ScatterChart, Scatter } from 'recharts';

const Radiation = () => {
  const [sensorData, setSensorData] = useState([
    { time: 1, counts: 12 },
    { time: 2, counts: 15 },
    { time: 3, counts: 8 },
    { time: 4, counts: 18 },
    { time: 5, counts: 11 },
    { time: 6, counts: 14 },
    { time: 7, counts: 9 },
    { time: 8, counts: 16 }
  ]);

  const [sourceRate, setSourceRate] = useState(10.0);
  const [detectorEfficiency, setDetectorEfficiency] = useState(0.7);
  const [backgroundRate, setBackgroundRate] = useState(2.0);

  // Helper functions
  const factorial = (n) => {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
  };

  // Poisson PMF
  const poissonPMF = (k, lambda) => {
    if (k < 0 || lambda <= 0) return 0;
    return Math.pow(lambda, k) * Math.exp(-lambda) / factorial(k);
  };

  // Calculate radiation model results
  const radiationResults = useMemo(() => {
    // Generate posterior distributions for source rate and efficiency
    const posteriorData = [];
    
    // Grid search over source rate and detector efficiency
    const sourceRange = Array.from({length: 50}, (_, i) => 1 + i * 0.4); // 1 to 21
    const efficiencyRange = Array.from({length: 20}, (_, i) => 0.1 + i * 0.05); // 0.1 to 1.0
    
    let maxLikelihood = 0;
    let bestParams = { source: 0, efficiency: 0 };
    
    sourceRange.forEach(source => {
      efficiencyRange.forEach(efficiency => {
        let likelihood = 1;
        
        sensorData.forEach(reading => {
          const expectedCounts = source * efficiency + backgroundRate;
          const poissonLike = poissonPMF(reading.counts, expectedCounts);
          likelihood *= poissonLike;
        });
        
        if (likelihood > maxLikelihood) {
          maxLikelihood = likelihood;
          bestParams = { source, efficiency };
        }
        
        posteriorData.push({
          sourceRate: parseFloat(source.toFixed(2)),
          efficiency: parseFloat(efficiency.toFixed(3)),
          likelihood: likelihood
        });
      });
    });
    
    // Generate marginal posteriors
    const sourceMarginal = sourceRange.map(source => {
      const marginal = posteriorData
        .filter(d => Math.abs(d.sourceRate - source) < 0.01)
        .reduce((sum, d) => sum + d.likelihood, 0);
      return { rate: source, probability: marginal };
    });
    
    const efficiencyMarginal = efficiencyRange.map(efficiency => {
      const marginal = posteriorData
        .filter(d => Math.abs(d.efficiency - efficiency) < 0.001)
        .reduce((sum, d) => sum + d.likelihood, 0);
      return { efficiency: efficiency, probability: marginal };
    });
    
    // Normalize marginal distributions
    const maxSourceProb = Math.max(...sourceMarginal.map(d => d.probability));
    const maxEffProb = Math.max(...efficiencyMarginal.map(d => d.probability));
    
    const normalizedSource = sourceMarginal.map(d => ({
      ...d,
      normalizedProbability: d.probability / maxSourceProb
    }));
    
    const normalizedEfficiency = efficiencyMarginal.map(d => ({
      ...d,
      normalizedProbability: d.probability / maxEffProb
    }));
    
    // Calculate posterior predictive distribution
    const predictiveDistribution = [];
    for (let counts = 0; counts <= 30; counts++) {
      let predictiveProb = 0;
      
      // Integrate over posterior uncertainty
      posteriorData.forEach(params => {
        if (params.likelihood > maxLikelihood * 0.01) { // Only consider significant parameters
          const expectedCounts = params.sourceRate * params.efficiency + backgroundRate;
          predictiveProb += poissonPMF(counts, expectedCounts) * params.likelihood;
        }
      });
      
      predictiveDistribution.push({
        counts,
        probability: predictiveProb / maxLikelihood
      });
    }
    
    return {
      bestParams,
      sourceMarginal: normalizedSource,
      efficiencyMarginal: normalizedEfficiency,
      predictiveDistribution,
      sensorData: sensorData.map(d => ({
        ...d,
        expected: bestParams.source * bestParams.efficiency + backgroundRate
      }))
    };
  }, [sensorData, sourceRate, detectorEfficiency, backgroundRate]);

  const handleDataChange = (index, value) => {
    const newData = [...sensorData];
    newData[index].counts = parseInt(value) || 0;
    setSensorData(newData);
  };

  const addReading = () => {
    const lastTime = sensorData[sensorData.length - 1].time;
    setSensorData([...sensorData, { 
      time: lastTime + 1, 
      counts: Math.floor(Math.random() * 20 + 5) 
    }]);
  };

  const removeReading = (index) => {
    if (sensorData.length > 3) {
      setSensorData(sensorData.filter((_, i) => i !== index));
    }
  };

  const generateSyntheticData = () => {
    const trueSource = 12;
    const trueEfficiency = 0.75;
    const trueBackground = 2;
    const newData = [];
    
    for (let i = 1; i <= 10; i++) {
      const expected = trueSource * trueEfficiency + trueBackground;
      const counts = Math.floor(Math.random() * 15 + 5);
      newData.push({ time: i, counts });
    }
    
    setSensorData(newData);
  };

  return (
    <div className="space-y-6">
      <div className="text-center">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Radiation Sensor Analysis</h1>
        <p className="text-gray-600 max-w-2xl mx-auto">
          Bayesian inference for radiation sensor calibration and source strength estimation.
          This example models the relationship between radioactive source emissions, 
          detector efficiency, and observed counts.
        </p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Sensor Data Input */}
        <section className="bg-white rounded-lg shadow-md p-6">
          <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
            <span className="px-2 py-1 text-xs border border-gray-300 rounded">Data Input</span>
            Sensor Readings
          </h3>
          <div className="space-y-4">
            <div>
              <label className="block text-sm font-medium mb-2">Radiation Counts per Second</label>
              <div className="space-y-2 max-h-64 overflow-y-auto">
                {sensorData.map((reading, index) => (
                  <div key={index} className="flex items-center gap-2">
                    <span className="text-sm w-12">T{reading.time}s:</span>
                    <input
                      type="number"
                      min="0"
                      max="50"
                      value={reading.counts}
                      onChange={(e) => handleDataChange(index, e.target.value)}
                      className="flex-1 px-3 py-1 border rounded-md"
                    />
                    <button
                      onClick={() => removeReading(index)}
                      className="px-2 py-1 text-red-600 hover:bg-red-50 rounded text-sm"
                      disabled={sensorData.length === 3}
                    >
                      Remove
                    </button>
                  </div>
                ))}
              </div>
              <div className="flex gap-2 mt-2">
                <button
                  onClick={addReading}
                  className="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 text-sm"
                >
                  Add Reading
                </button>
                <button
                  onClick={generateSyntheticData}
                  className="px-4 py-2 bg-green-500 text-white rounded hover:bg-green-600 text-sm"
                >
                  Generate Example
                </button>
              </div>
            </div>

            <div className="space-y-3">
              <div>
                <label className="block text-sm font-medium mb-2">
                  Background Rate: {backgroundRate.toFixed(1)} counts/s
                </label>
                <input
                  type="range"
                  min="0"
                  max="10"
                  step="0.5"
                  value={backgroundRate}
                  onChange={(e) => setBackgroundRate(parseFloat(e.target.value))}
                  className="w-full"
                />
              </div>
            </div>
          </div>
        </section>

        {/* Results */}
        <section className="bg-white rounded-lg shadow-md p-6">
          <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
            <span className="px-2 py-1 text-xs border border-gray-300 rounded">Results</span>
            Parameter Estimates
          </h3>
          <div>
            <div className="space-y-4">
              <div className="grid grid-cols-2 gap-4">
                <div className="p-3 bg-blue-50 rounded">
                  <div className="text-sm text-gray-600">Source Rate</div>
                  <div className="text-xl font-bold text-blue-600">
                    {radiationResults.bestParams.source.toFixed(2)}
                  </div>
                  <div className="text-xs text-gray-500">particles/s</div>
                </div>
                <div className="p-3 bg-green-50 rounded">
                  <div className="text-sm text-gray-600">Detector Efficiency</div>
                  <div className="text-xl font-bold text-green-600">
                    {(radiationResults.bestParams.efficiency * 100).toFixed(1)}%
                  </div>
                  <div className="text-xs text-gray-500">detection rate</div>
                </div>
              </div>
              
              <div className="p-3 bg-purple-50 rounded">
                <div className="text-sm text-gray-600 mb-1">Expected Counts</div>
                <div className="text-lg font-bold text-purple-600">
                  {(radiationResults.bestParams.source * radiationResults.bestParams.efficiency + backgroundRate).toFixed(2)}
                </div>
                <div className="text-xs text-gray-500">counts per second</div>
              </div>

              <div className="p-3 bg-yellow-50 rounded">
                <div className="text-sm text-gray-600 mb-1">Model Interpretation</div>
                <div className="text-sm">
                  The radiation source emits approximately {radiationResults.bestParams.source.toFixed(1)} particles per second.
                  With {(radiationResults.bestParams.efficiency * 100).toFixed(0)}% detector efficiency,
                  we expect {(radiationResults.bestParams.source * radiationResults.bestParams.efficiency + backgroundRate).toFixed(1)} counts per second.
                </div>
              </div>
            </div>
          </div>
        </section>
      </div>

      {/* Observed vs Expected */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-lg font-semibold mb-4">Observed vs Expected Counts</h3>
        <div>
          <ResponsiveContainer width="100%" height={300}>
            <ScatterChart data={radiationResults.sensorData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="time" 
                label={{ value: 'Time (seconds)', position: 'insideBottom', offset: -5 }}
              />
              <YAxis 
                label={{ value: 'Counts per Second', angle: -90, position: 'insideLeft' }}
              />
              <Tooltip />
              <Legend />
              <Scatter 
                name="Observed Counts" 
                dataKey="counts" 
                fill="#8884d8"
              />
              <Line 
                type="monotone" 
                dataKey="expected" 
                stroke="#ff7300" 
                strokeWidth={2}
                dot={false}
                name="Expected Counts"
              />
            </ScatterChart>
          </ResponsiveContainer>
        </div>
      </section>

      {/* Posterior Distributions */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <section className="bg-white rounded-lg shadow-md p-6">
          <h3 className="text-lg font-semibold mb-4">Source Rate Posterior</h3>
          <div>
            <ResponsiveContainer width="100%" height={250}>
              <LineChart data={radiationResults.sourceMarginal}>
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis 
                  dataKey="rate" 
                  label={{ value: 'Source Rate (particles/s)', position: 'insideBottom', offset: -5 }}
                />
                <YAxis 
                  label={{ value: 'Probability', angle: -90, position: 'insideLeft' }}
                />
                <Tooltip />
                <Line 
                  type="monotone" 
                  dataKey="normalizedProbability" 
                  stroke="#8884d8" 
                  strokeWidth={2}
                  dot={false}
                />
              </LineChart>
            </ResponsiveContainer>
          </div>
        </section>

        <section className="bg-white rounded-lg shadow-md p-6">
          <h3 className="text-lg font-semibold mb-4">Detector Efficiency Posterior</h3>
          <div>
            <ResponsiveContainer width="100%" height={250}>
              <LineChart data={radiationResults.efficiencyMarginal}>
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis 
                  dataKey="efficiency" 
                  label={{ value: 'Efficiency', position: 'insideBottom', offset: -5 }}
                />
                <YAxis 
                  label={{ value: 'Probability', angle: -90, position: 'insideLeft' }}
                />
                <Tooltip />
                <Line 
                  type="monotone" 
                  dataKey="normalizedProbability" 
                  stroke="#82ca9d" 
                  strokeWidth={2}
                  dot={false}
                />
              </LineChart>
            </ResponsiveContainer>
          </div>
        </section>
      </div>

      {/* Predictive Distribution */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-lg font-semibold mb-4">Posterior Predictive Distribution</h3>
        <div>
          <ResponsiveContainer width="100%" height={300}>
            <LineChart data={radiationResults.predictiveDistribution}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="counts" 
                label={{ value: 'Predicted Counts', position: 'insideBottom', offset: -5 }}
              />
              <YAxis 
                label={{ value: 'Probability', angle: -90, position: 'insideLeft' }}
              />
              <Tooltip />
              <Line 
                type="monotone" 
                dataKey="probability" 
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
              <h4 className="font-semibold mb-2">Physical Model</h4>
              <div className="bg-gray-50 p-3 rounded font-mono text-xs">
                <div>λ_observed = p × θ + b</div>
                <div>counts ~ Poisson(λ_observed)</div>
                <div>where: p = source rate, θ = efficiency, b = background</div>
              </div>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Bayesian Inference</h4>
              <div className="bg-gray-50 p-3 rounded font-mono text-xs">
                <div>P(p, θ | data) ∝ P(data | p, θ) × P(p) × P(θ)</div>
                <div>Posterior ∝ Likelihood × Prior</div>
              </div>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Key Applications</h4>
              <ul className="list-disc list-inside space-y-1 text-gray-600">
                <li>Radiation monitoring and safety</li>
                <li>Medical imaging calibration</li>
                <li>Environmental radiation assessment</li>
                <li>Nuclear facility monitoring</li>
                <li>Scientific instrument calibration</li>
              </ul>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Challenges & Solutions</h4>
              <ul className="list-disc list-inside space-y-1 text-gray-600">
                <li>Low count rates → Poisson modeling essential</li>
                <li>Detector uncertainty → Hierarchical models</li>
                <li>Background radiation → Must be estimated</li>
                <li>Time-varying sources → Extended models needed</li>
              </ul>
            </div>
          </div>
      </section>
    </div>
  );
};

export default Radiation;
