import React, { useState, useMemo } from 'react';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer, AreaChart, Area } from 'recharts';

const ChangepointDetection = () => {
  const [timeSeriesData, setTimeSeriesData] = useState([
    { time: 1, events: 3 },
    { time: 2, events: 2 },
    { time: 3, events: 4 },
    { time: 4, events: 3 },
    { time: 5, events: 5 },
    { time: 6, events: 8 },
    { time: 7, events: 7 },
    { time: 8, events: 12 },
    { time: 9, events: 11 },
    { time: 10, events: 9 }
  ]);

  const [priorLambda1, setPriorLambda1] = useState(1.0);
  const [priorLambda2, setPriorLambda2] = useState(5.0);

  // Helper functions
  const factorial = (n) => {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
  };

  // Poisson likelihood function
  const poissonLikelihood = (data, lambda) => {
    return data.reduce((likelihood, point) => {
      return likelihood * (Math.pow(lambda, point.events) * Math.exp(-lambda) / factorial(point.events));
    }, 1);
  };

  // Calculate changepoint probabilities
  const changepointResults = useMemo(() => {
    const n = timeSeriesData.length;
    const changepointProbabilities = [];
    
    // Calculate likelihood for each possible changepoint
    for (let t = 1; t < n - 1; t++) {
      const before = timeSeriesData.slice(0, t);
      const after = timeSeriesData.slice(t);
      
      const beforeEvents = before.reduce((sum, d) => sum + d.events, 0);
      const afterEvents = after.reduce((sum, d) => sum + d.events, 0);
      
      // Calculate likelihood using Poisson distributions
      const beforeLikelihood = poissonLikelihood(before, priorLambda1);
      const afterLikelihood = poissonLikelihood(after, priorLambda2);
      
      // Posterior probability (assuming uniform prior over changepoints)
      const posterior = beforeLikelihood * afterLikelihood;
      
      changepointProbabilities.push({
        changepoint: t,
        time: timeSeriesData[t].time,
        probability: posterior,
        beforeRate: beforeEvents / before.length,
        afterRate: afterEvents / after.length
      });
    }
    
    // Normalize probabilities
    const maxProb = Math.max(...changepointProbabilities.map(cp => cp.probability));
    const normalizedData = changepointProbabilities.map(cp => ({
      ...cp,
      normalizedProbability: cp.probability / maxProb
    }));
    
    // Find most likely changepoint
    const mostLikely = normalizedData.reduce((best, current) => 
      current.probability > best.probability ? current : best
    );
    
    // Generate posterior distributions for rates
    const posteriorData = normalizedData.map(cp => {
      const before = timeSeriesData.slice(0, cp.changepoint);
      const after = timeSeriesData.slice(cp.changepoint);
      
      const beforeEvents = before.reduce((sum, d) => sum + d.events, 0);
      const afterEvents = after.reduce((sum, d) => sum + d.events, 0);
      
      return {
        ...cp,
        posteriorLambda1: (1 + beforeEvents) / (1 + before.length),
        posteriorLambda2: (1 + afterEvents) / (1 + after.length)
      };
    });
    
    return {
      changepointData: posteriorData,
      mostLikely,
      timeSeriesData: timeSeriesData.map(d => ({
        ...d,
        isBeforeChangepoint: d.time <= mostLikely.time
      }))
    };
  }, [timeSeriesData, priorLambda1, priorLambda2]);

  // Poisson PMF
  const poissonPMF = (k, lambda) => {
    if (k < 0 || lambda <= 0) return 0;
    return Math.pow(lambda, k) * Math.exp(-lambda) / factorial(k);
  };

  const handleDataChange = (index, value) => {
    const newData = [...timeSeriesData];
    newData[index].events = parseInt(value) || 0;
    setTimeSeriesData(newData);
  };

  const addDataPoint = () => {
    const lastTime = timeSeriesData[timeSeriesData.length - 1].time;
    setTimeSeriesData([...timeSeriesData, { 
      time: lastTime + 1, 
      events: 5 
    }]);
  };

  const removeDataPoint = (index) => {
    if (timeSeriesData.length > 4) {
      setTimeSeriesData(timeSeriesData.filter((_, i) => i !== index));
    }
  };

  const generateSyntheticData = () => {
    const changepointAt = 5;
    const lambda1 = 3;
    const lambda2 = 8;
    const newData = [];
    
    for (let i = 1; i <= 12; i++) {
      const lambda = i <= changepointAt ? lambda1 : lambda2;
      const events = Math.floor(Math.random() * 10 + 1);
      newData.push({ time: i, events });
    }
    
    setTimeSeriesData(newData);
  };

  return (
    <div className="space-y-6">
      <div className="text-center">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Changepoint Detection</h1>
        <p className="text-gray-600 max-w-2xl mx-auto">
          Bayesian changepoint detection for identifying when the underlying rate 
          of events changes in time series data. This example uses Poisson processes 
          with different rates before and after the changepoint.
        </p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Time Series Data Input */}
        <section className="bg-white rounded-lg shadow-md p-6">
          <h3 className="text-lg font-semibold mb-4 flex items-center gap-2">
            <span className="px-2 py-1 text-xs border border-gray-300 rounded">Data Input</span>
            Time Series Events
          </h3>
          <div className="space-y-4">
            <div>
              <label className="block text-sm font-medium mb-2">Event Counts Over Time</label>
              <div className="space-y-2 max-h-64 overflow-y-auto">
                {timeSeriesData.map((point, index) => (
                  <div key={index} className="flex items-center gap-2">
                    <span className="text-sm w-12">T{point.time}:</span>
                    <input
                      type="number"
                      min="0"
                      max="20"
                      value={point.events}
                      onChange={(e) => handleDataChange(index, e.target.value)}
                      className="flex-1 px-3 py-1 border rounded-md"
                    />
                    <button
                      onClick={() => removeDataPoint(index)}
                      className="px-2 py-1 text-red-600 hover:bg-red-50 rounded text-sm"
                      disabled={timeSeriesData.length === 4}
                    >
                      Remove
                    </button>
                  </div>
                ))}
              </div>
              <div className="flex gap-2 mt-2">
                <button
                  onClick={addDataPoint}
                  className="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 text-sm"
                >
                  Add Point
                </button>
                <button
                  onClick={generateSyntheticData}
                  className="px-4 py-2 bg-green-500 text-white rounded hover:bg-green-600 text-sm"
                >
                  Generate Example
                </button>
              </div>
            </div>

            <div className="grid grid-cols-2 gap-4">
              <div>
                <label className="block text-sm font-medium mb-2">
                  Prior λ₁: {priorLambda1.toFixed(1)}
                </label>
                <input
                  type="range"
                  min="0.5"
                  max="10"
                  step="0.5"
                  value={priorLambda1}
                  onChange={(e) => setPriorLambda1(parseFloat(e.target.value))}
                  className="w-full"
                />
              </div>
              <div>
                <label className="block text-sm font-medium mb-2">
                  Prior λ₂: {priorLambda2.toFixed(1)}
                </label>
                <input
                  type="range"
                  min="0.5"
                  max="15"
                  step="0.5"
                  value={priorLambda2}
                  onChange={(e) => setPriorLambda2(parseFloat(e.target.value))}
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
            Changepoint Analysis
          </h3>
          <div>
            <div className="space-y-4">
              <div className="p-3 bg-purple-50 rounded">
                <div className="text-sm text-gray-600">Most Likely Changepoint</div>
                <div className="text-xl font-bold text-purple-600">
                  Between T{changepointResults.mostLikely.time} and T{changepointResults.mostLikely.time + 1}
                </div>
                <div className="text-xs text-gray-500">
                  Probability: {changepointResults.mostLikely.normalizedProbability.toFixed(3)}
                </div>
              </div>
              
              <div className="grid grid-cols-2 gap-4">
                <div className="p-3 bg-blue-50 rounded">
                  <div className="text-sm text-gray-600">Before Rate</div>
                  <div className="text-lg font-bold text-blue-600">
                    {changepointResults.mostLikely.beforeRate.toFixed(2)}
                  </div>
                  <div className="text-xs text-gray-500">events per period</div>
                </div>
                <div className="p-3 bg-green-50 rounded">
                  <div className="text-sm text-gray-600">After Rate</div>
                  <div className="text-lg font-bold text-green-600">
                    {changepointResults.mostLikely.afterRate.toFixed(2)}
                  </div>
                  <div className="text-xs text-gray-500">events per period</div>
                </div>
              </div>

              <div className="p-3 bg-yellow-50 rounded">
                <div className="text-sm text-gray-600 mb-1">Interpretation</div>
                <div className="text-sm">
                  The data suggests a structural change occurred around time {changepointResults.mostLikely.time},
                  with the event rate changing from {changepointResults.mostLikely.beforeRate.toFixed(1)} 
                  to {changepointResults.mostLikely.afterRate.toFixed(1)} events per period.
                </div>
              </div>
            </div>
          </div>
        </section>
      </div>

      {/* Time Series Visualization */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-lg font-semibold mb-4">Time Series Data</h3>
        <div>
          <ResponsiveContainer width="100%" height={300}>
            <LineChart data={changepointResults.timeSeriesData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="time" 
                label={{ value: 'Time', position: 'insideBottom', offset: -5 }}
              />
              <YAxis 
                label={{ value: 'Event Count', angle: -90, position: 'insideLeft' }}
              />
              <Tooltip />
              <Legend />
              <Line 
                type="monotone" 
                dataKey="events" 
                stroke="#8884d8" 
                strokeWidth={2}
                dot={{ fill: '#8884d8' }}
                name="Observed Events"
              />
            </LineChart>
          </ResponsiveContainer>
        </div>
      </section>

      {/* Changepoint Probability Distribution */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-lg font-semibold mb-4">Changepoint Probability Distribution</h3>
        <div>
          <ResponsiveContainer width="100%" height={300}>
            <AreaChart data={changepointResults.changepointData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="time" 
                label={{ value: 'Changepoint Location', position: 'insideBottom', offset: -5 }}
              />
              <YAxis 
                label={{ value: 'Normalized Probability', angle: -90, position: 'insideLeft' }}
              />
              <Tooltip />
              <Legend />
              <Area 
                type="monotone" 
                dataKey="normalizedProbability" 
                stroke="#ff7300" 
                fill="#ff7300" 
                fillOpacity={0.6}
                name="Changepoint Probability"
              />
            </AreaChart>
          </ResponsiveContainer>
        </div>
      </section>

      {/* Mathematical Framework */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-lg font-semibold mb-4">Mathematical Framework</h3>
        <div className="space-y-4 text-sm">
            <div>
              <h4 className="font-semibold mb-2">Model Structure</h4>
              <div className="bg-gray-50 p-3 rounded font-mono text-xs">
                <div>events_t ~ Poisson(&lambda;&sub1;)  for t &le; &tau;</div>
                <div>events_t ~ Poisson(&lambda;&sub2;)  for t &gt; &tau;</div>
                <div>&tau; ~ Uniform(1, n-1)  (changepoint location)</div>
                <div>&lambda;&sub1;, &lambda;&sub2; ~ Prior distributions</div>
              </div>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Likelihood Calculation</h4>
              <div className="bg-gray-50 p-3 rounded font-mono text-xs">
                <div>L(&tau;) = &prod;[i&le;&tau;] P(events_i|&lambda;&sub1;) &times; &prod;[i&gt;&tau;] P(events_i|&lambda;&sub2;)</div>
                <div>P(&tau;|data) &prop; L(&tau;) &times; P(&tau;)</div>
              </div>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Applications</h4>
              <ul className="list-disc list-inside space-y-1 text-gray-600">
                <li>Detecting regime changes in financial markets</li>
                <li>Monitoring system performance degradation</li>
                <li>Identifying disease outbreak onset</li>
                <li>Quality control and process monitoring</li>
                <li>Climate change detection</li>
              </ul>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Key Extensions</h4>
              <ul className="list-disc list-inside space-y-1 text-gray-600">
                <li>Multiple changepoints</li>
                <li>Different distribution families</li>
                <li>Online detection algorithms</li>
                <li>Time-varying changepoint probabilities</li>
              </ul>
            </div>
          </div>
        </section>
    </div>
  );
};

export default ChangepointDetection;
