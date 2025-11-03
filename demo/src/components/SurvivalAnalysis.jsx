import React, { useState, useMemo } from 'react';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer, AreaChart, Area } from 'recharts';

const SurvivalAnalysis = () => {
  const [survivalData, setSurvivalData] = useState([
    { time: 0, atRisk: 100, events: 2, censored: 0 },
    { time: 1, atRisk: 98, events: 3, censored: 1 },
    { time: 2, atRisk: 94, events: 4, censored: 2 },
    { time: 3, atRisk: 88, events: 5, censored: 1 },
    { time: 4, atRisk: 82, events: 6, censored: 3 },
    { time: 5, atRisk: 73, events: 8, censored: 2 },
    { time: 6, atRisk: 63, events: 10, censored: 4 },
    { time: 7, atRisk: 49, events: 12, censored: 3 },
    { time: 8, atRisk: 34, events: 15, censored: 5 },
    { time: 9, atRisk: 14, events: 8, censored: 6 }
  ]);

  const [priorAlpha, setPriorAlpha] = useState(1.0);
  const [priorBeta, setPriorBeta] = useState(0.1);

  // Helper functions
  const weibullPDF = (t, shape, scale) => {
    if (t <= 0) return 0;
    return (shape / scale) * Math.pow(t / scale, shape - 1) * Math.exp(-Math.pow(t / scale, shape));
  };

  const weibullSurvival = (t, shape, scale) => {
    if (t <= 0) return 1;
    return Math.exp(-Math.pow(t / scale, shape));
  };

  const weibullHazard = (t, shape, scale) => {
    if (t <= 0) return 0;
    return (shape / scale) * Math.pow(t / scale, shape - 1);
  };

  // Gamma PDF for prior
  const gammaPDF = (x, alpha, beta) => {
    if (x <= 0) return 0;
    return Math.pow(beta, alpha) * Math.pow(x, alpha - 1) * Math.exp(-beta * x) / gamma(alpha);
  };

  // Simplified gamma function
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

  // Calculate survival analysis results
  const survivalResults = useMemo(() => {
    // Calculate Kaplan-Meier survival curve
    let survival = 1.0;
    const kaplanMeierData = survivalData.map((point, index) => {
      if (index === 0) {
        survival = 1.0;
      } else {
        const previousPoint = survivalData[index - 1];
        const hazard = point.events / previousPoint.atRisk;
        survival *= (1 - hazard);
      }
      
      return {
        time: point.time,
        survival: survival,
        atRisk: point.atRisk,
        events: point.events,
        censored: point.censored,
        hazard: point.events / point.atRisk
      };
    });

    // Calculate cumulative incidence
    const cumulativeIncidence = kaplanMeierData.map(point => ({
      time: point.time,
      incidence: 1 - point.survival
    }));

    // Generate Weibull survival model using Bayesian inference
    const weibullPosterior = [];
    const gridSize = 50;
    
    // Grid search over Weibull parameters
    const shapeRange = Array.from({length: gridSize}, (_, i) => 0.5 + i * 0.1); // 0.5 to 5.5
    const scaleRange = Array.from({length: gridSize}, (_, i) => 1 + i * 0.4); // 1 to 21
    
    let maxLikelihood = 0;
    let bestWeibull = { shape: 0, scale: 0 };
    
    shapeRange.forEach(shape => {
      scaleRange.forEach(scale => {
        let likelihood = 1;
        
        survivalData.forEach((point, index) => {
          if (point.events > 0) {
            // Likelihood for events
            const eventLikelihood = Math.pow(
              weibullPDF(point.time, shape, scale), 
              point.events
            );
            likelihood *= eventLikelihood;
          }
          
          if (point.censored > 0) {
            // Likelihood for censored observations
            const survivalLikelihood = Math.pow(
              weibullSurvival(point.time, shape, scale), 
              point.censored
            );
            likelihood *= survivalLikelihood;
          }
        });
        
        // Add prior
        const prior = gammaPDF(shape, priorAlpha, priorBeta);
        likelihood *= prior;
        
        if (likelihood > maxLikelihood) {
          maxLikelihood = likelihood;
          bestWeibull = { shape, scale };
        }
        
        weibullPosterior.push({
          shape: parseFloat(shape.toFixed(2)),
          scale: parseFloat(scale.toFixed(2)),
          likelihood: likelihood
        });
      });
    });

    // Generate smooth survival curves
    const smoothTimePoints = Array.from({length: 100}, (_, i) => i * 0.1);
    const weibullCurve = smoothTimePoints.map(time => ({
      time: parseFloat(time.toFixed(2)),
      survival: weibullSurvival(time, bestWeibull.shape, bestWeibull.scale),
      kaplanMeier: kaplanMeierData.find(km => km.time === time)?.survival || null
    }));

    // Calculate hazard function
    const hazardFunction = smoothTimePoints.map(time => ({
      time: parseFloat(time.toFixed(2)),
      hazard: weibullHazard(time, bestWeibull.shape, bestWeibull.scale)
    }));

    return {
      kaplanMeierData,
      cumulativeIncidence,
      bestWeibull,
      weibullCurve,
      hazardFunction
    };
  }, [survivalData, priorAlpha, priorBeta]);

  const handleDataChange = (index, field, value) => {
    const newData = [...survivalData];
    newData[index][field] = parseInt(value) || 0;
    setSurvivalData(newData);
  };

  const addTimePoint = () => {
    const lastTime = survivalData[survivalData.length - 1].time;
    const lastAtRisk = survivalData[survivalData.length - 1].atRisk;
    setSurvivalData([...survivalData, { 
      time: lastTime + 1, 
      atRisk: Math.max(10, lastAtRisk - 20),
      events: 5,
      censored: 2
    }]);
  };

  const removeTimePoint = (index) => {
    if (survivalData.length > 3) {
      setSurvivalData(survivalData.filter((_, i) => i !== index));
    }
  };

  const generateSyntheticData = () => {
    const newData = [];
    let currentAtRisk = 100;
    
    for (let i = 0; i <= 8; i++) {
      const events = Math.floor(Math.random() * 15 + 2);
      const censored = Math.floor(Math.random() * 8 + 1);
      newData.push({
        time: i,
        atRisk: currentAtRisk,
        events,
        censored
      });
      currentAtRisk -= events + censored;
    }
    
    setSurvivalData(newData);
  };

  return (
    <div className="space-y-6">
      <div className="text-center">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Survival Analysis</h1>
        <p className="text-gray-600 max-w-2xl mx-auto">
          Bayesian survival analysis using Kaplan-Meier estimation and Weibull modeling.
          This example demonstrates time-to-event analysis with censored data, 
          commonly used in medical research and reliability engineering.
        </p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Survival Data Input */}
        <section className="bg-white rounded-lg shadow-md p-6">
          <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
            <span className="px-2 py-1 text-xs border border-gray-300 rounded">Data Input</span>
            Survival Data
          </h3>
          <div className="space-y-4">
            <div>
              <label className="block text-sm font-medium mb-2">Time-to-Event Data</label>
              <div className="space-y-2 max-h-64 overflow-y-auto">
                {survivalData.map((point, index) => (
                  <div key={index} className="border rounded p-3">
                    <div className="flex items-center justify-between mb-2">
                      <span className="font-medium">Time {point.time}</span>
                      <button
                        onClick={() => removeTimePoint(index)}
                        className="px-2 py-1 text-red-600 hover:bg-red-50 rounded text-sm"
                        disabled={survivalData.length === 3}
                      >
                        Remove
                      </button>
                    </div>
                    <div className="grid grid-cols-3 gap-2 text-xs">
                      <div>
                        <label className="text-gray-600">At Risk</label>
                        <input
                          type="number"
                          min="0"
                          value={point.atRisk}
                          onChange={(e) => handleDataChange(index, 'atRisk', e.target.value)}
                          className="w-full px-2 py-1 border rounded"
                        />
                      </div>
                      <div>
                        <label className="text-gray-600">Events</label>
                        <input
                          type="number"
                          min="0"
                          value={point.events}
                          onChange={(e) => handleDataChange(index, 'events', e.target.value)}
                          className="w-full px-2 py-1 border rounded"
                        />
                      </div>
                      <div>
                        <label className="text-gray-600">Censored</label>
                        <input
                          type="number"
                          min="0"
                          value={point.censored}
                          onChange={(e) => handleDataChange(index, 'censored', e.target.value)}
                          className="w-full px-2 py-1 border rounded"
                        />
                      </div>
                    </div>
                  </div>
                ))}
              </div>
              <div className="flex gap-2 mt-2">
                <button
                  onClick={addTimePoint}
                  className="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 text-sm"
                >
                  Add Time Point
                </button>
                <button
                  onClick={generateSyntheticData}
                  className="px-4 py-2 bg-green-500 text-white rounded hover:bg-green-600 text-sm"
                >
                  Generate Example
                </button>
              </div>
            </div>

            <div>
              <label className="block text-sm font-medium mb-2">
                Prior Shape α: {priorAlpha.toFixed(1)}
              </label>
              <input
                type="range"
                min="0.1"
                max="5"
                step="0.1"
                value={priorAlpha}
                onChange={(e) => setPriorAlpha(parseFloat(e.target.value))}
                className="w-full"
              />
            </div>
          </div>
        </section>

        {/* Results */}
        <section className="bg-white rounded-lg shadow-md p-6">
          <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
            <span className="px-2 py-1 text-xs border border-gray-300 rounded">Results</span>
            Survival Estimates
          </h3>
          <div>
            <div className="space-y-4">
              <div className="grid grid-cols-2 gap-4">
                <div className="p-3 bg-blue-50 rounded">
                  <div className="text-sm text-gray-600">Weibull Shape</div>
                  <div className="text-xl font-bold text-blue-600">
                    {survivalResults.bestWeibull.shape.toFixed(2)}
                  </div>
                  <div className="text-xs text-gray-500">
                    {survivalResults.bestWeibull.shape > 1 ? 'Increasing hazard' : 'Decreasing hazard'}
                  </div>
                </div>
                <div className="p-3 bg-green-50 rounded">
                  <div className="text-sm text-gray-600">Weibull Scale</div>
                  <div className="text-xl font-bold text-green-600">
                    {survivalResults.bestWeibull.scale.toFixed(2)}
                  </div>
                  <div className="text-xs text-gray-500">time units</div>
                </div>
              </div>
              
              <div className="p-3 bg-purple-50 rounded">
                <div className="text-sm text-gray-600 mb-1">Final Survival</div>
                <div className="text-lg font-bold text-purple-600">
                  {(survivalResults.kaplanMeierData[survivalResults.kaplanMeierData.length - 1]?.survival * 100).toFixed(1)}%
                </div>
                <div className="text-xs text-gray-500">
                  at time {survivalResults.kaplanMeierData[survivalResults.kaplanMeierData.length - 1]?.time}
                </div>
              </div>

              <div className="p-3 bg-yellow-50 rounded">
                <div className="text-sm text-gray-600 mb-1">Total Events</div>
                <div className="text-sm">
                  {survivalData.reduce((sum, d) => sum + d.events, 0)} events out of {survivalData[0]?.atRisk} initial subjects
                </div>
              </div>
            </div>
          </div>
        </section>
      </div>

      {/* Survival Curves */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-lg font-semibold mb-4">Survival Curves Comparison</h3>
        <div>
          <ResponsiveContainer width="100%" height={400}>
            <LineChart data={survivalResults.weibullCurve}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="time" 
                label={{ value: 'Time', position: 'insideBottom', offset: -5 }}
              />
              <YAxis 
                label={{ value: 'Survival Probability', angle: -90, position: 'insideLeft' }}
                domain={[0, 1]}
              />
              <Tooltip />
              <Legend />
              <Line 
                type="monotone" 
                dataKey="weibull" 
                stroke="#8884d8" 
                strokeWidth={2}
                dot={false}
                name="Weibull Model"
              />
              <Line 
                type="stepAfter" 
                dataKey="kaplanMeier" 
                stroke="#ff7300" 
                strokeWidth={2}
                dot={{ r: 3 }}
                name="Kaplan-Meier"
              />
            </LineChart>
          </ResponsiveContainer>
        </div>
      </section>

      {/* Hazard Function */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-lg font-semibold mb-4">Hazard Function</h3>
        <div>
          <ResponsiveContainer width="100%" height={300}>
            <LineChart data={survivalResults.hazardFunction}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="time" 
                label={{ value: 'Time', position: 'insideBottom', offset: -5 }}
              />
              <YAxis 
                label={{ value: 'Hazard Rate', angle: -90, position: 'insideLeft' }}
              />
              <Tooltip />
              <Line 
                type="monotone" 
                dataKey="hazard" 
                stroke="#82ca9d" 
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
              <h4 className="font-semibold mb-2">Kaplan-Meier Estimator</h4>
              <div className="bg-gray-50 p-3 rounded font-mono text-xs">
                <div>Ŝ(t) = ∏[ti≤t] (1 - di/ni)</div>
                <div>where: di = events at time ti, ni = at risk at time ti</div>
              </div>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Weibull Distribution</h4>
              <div className="bg-gray-50 p-3 rounded font-mono text-xs">
                <div>S(t) = exp(-(t/λ)^k)</div>
                <div>h(t) = (k/λ) × (t/λ)^(k-1)</div>
                <div>where: k = shape, λ = scale</div>
              </div>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Key Concepts</h4>
              <ul className="list-disc list-inside space-y-1 text-gray-600">
                <li>Censoring: Partial information about survival times</li>
                <li>Hazard function: Instantaneous rate of events</li>
                <li>Survival function: Probability of surviving beyond time t</li>
                <li>Non-parametric vs parametric approaches</li>
              </ul>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Applications</h4>
              <ul className="list-disc list-inside space-y-1 text-gray-600">
                <li>Clinical trials and medical research</li>
                <li>Reliability engineering</li>
                <li>Customer churn analysis</li>
                <li>Epidemiological studies</li>
                <li>Equipment failure analysis</li>
              </ul>
            </div>
          </div>
        </section>
    </div>
  );
};

export default SurvivalAnalysis;
