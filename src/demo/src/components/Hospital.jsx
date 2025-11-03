import React, { useState, useMemo } from 'react';
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer, BarChart, Bar } from 'recharts';

const Hospital = () => {
  const [hospitals, setHospitals] = useState([
    { name: 'Hospital A', births: 50, days: 30 },
    { name: 'Hospital B', births: 75, days: 30 },
    { name: 'Hospital C', births: 45, days: 30 },
    { name: 'Hospital D', births: 60, days: 30 },
    { name: 'Hospital E', births: 55, days: 30 }
  ]);
  
  const [hyperAlpha, setHyperAlpha] = useState(2.0);
  const [hyperBeta, setHyperBeta] = useState(0.5);

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

  // Poisson PMF
  const poissonPMF = (k, lambda) => {
    if (k < 0 || lambda <= 0) return 0;
    return Math.pow(lambda, k) * Math.exp(-lambda) / factorial(k);
  };

  // Calculate hierarchical model results
  const modelResults = useMemo(() => {
    // Calculate birth rates for each hospital
    const hospitalData = hospitals.map(hospital => ({
      ...hospital,
      birthRate: hospital.births / hospital.days
    }));

    // Generate pooled estimate (complete pooling)
    const totalBirths = hospitals.reduce((sum, h) => sum + h.births, 0);
    const totalDays = hospitals.reduce((sum, h) => sum + h.days, 0);
    const pooledRate = totalBirths / totalDays;

    // Generate hierarchical model estimates using grid approximation
    const gridData = [];
    const gridSize = 100;
    
    // Create grid for hyperparameters
    for (let alpha = 0.5; alpha <= 10; alpha += 0.2) {
      for (let beta = 0.1; beta <= 2; beta += 0.05) {
        let likelihood = 1;
        
        // Calculate likelihood for each hospital
        hospitals.forEach(hospital => {
          const rate = hospital.births / hospital.days;
          // Gamma prior for each hospital's rate
          const prior = gammaPDF(rate, alpha, beta);
          // Poisson likelihood for observed births
          const poissonLike = poissonPMF(hospital.births, rate * hospital.days);
          likelihood *= prior * poissonLike;
        });
        
        gridData.push({
          alpha: parseFloat(alpha.toFixed(2)),
          beta: parseFloat(beta.toFixed(2)),
          likelihood: likelihood
        });
      }
    }

    // Find best hyperparameters
    const bestHyper = gridData.reduce((best, current) => 
      current.likelihood > best.likelihood ? current : best
    );

    // Generate posterior distributions for each hospital
    const posteriorData = hospitalData.map(hospital => {
      const posteriorAlpha = hyperAlpha + hospital.births;
      const posteriorBeta = hyperBeta + hospital.days;
      
      const distribution = [];
      for (let rate = 0.5; rate <= 5; rate += 0.05) {
        distribution.push({
          rate: parseFloat(rate.toFixed(2)),
          probability: gammaPDF(rate, posteriorAlpha, posteriorBeta)
        });
      }
      
      return {
        ...hospital,
        posteriorAlpha,
        posteriorBeta,
        posteriorMean: posteriorAlpha / posteriorBeta,
        distribution
      };
    });

    return {
      hospitalData,
      pooledRate,
      bestHyper,
      posteriorData,
      gridData
    };
  }, [hospitals, hyperAlpha, hyperBeta]);

  const handleInputChange = (index, field, value) => {
    const newHospitals = [...hospitals];
    newHospitals[index][field] = parseInt(value) || 0;
    setHospitals(newHospitals);
  };

  const handleHospitalChange = (index, field, value) => {
    const newHospitals = [...hospitals];
    newHospitals[index][field] = parseInt(value) || 0;
    setHospitals(newHospitals);
  };

  const addHospital = () => {
    setHospitals([...hospitals, { 
      name: `Hospital ${String.fromCharCode(65 + hospitals.length)}`, 
      births: 50, 
      days: 30 
    }]);
  };

  const removeHospital = (index) => {
    if (hospitals.length > 2) {
      setHospitals(hospitals.filter((_, i) => i !== index));
    }
  };

  return (
    <div className="space-y-6">
      <div className="text-center">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Hospital Birth Rate Analysis</h1>
        <p className="text-gray-600 max-w-2xl mx-auto">
          Hierarchical Bayesian model for comparing birth rates across multiple hospitals.
          This example demonstrates partial pooling - sharing information between hospitals 
          while accounting for hospital-specific variation.
        </p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Hospital Data Input */}
        <section className="bg-white rounded-lg shadow-md p-6">
          <h2 className="text-xl font-semibold text-gray-800 mb-4 flex items-center gap-2">
            <span className="px-2 py-1 bg-blue-100 text-blue-800 text-xs rounded">Data Input</span>
            Hospital Statistics
          </h2>
            <div>
              <label className="block text-sm font-medium mb-2">Hospital Data</label>
              <div className="space-y-2 max-h-64 overflow-y-auto">
                {hospitals.map((hospital, index) => (
                  <div key={index} className="border rounded p-3">
                    <div className="flex items-center justify-between mb-2">
                      <input
                        type="text"
                        value={hospital.name}
                        onChange={(e) => handleHospitalChange(index, 'name', e.target.value)}
                        className="font-medium px-2 py-1 border rounded"
                      />
                      <button
                        onClick={() => removeHospital(index)}
                        className="px-2 py-1 text-red-600 hover:bg-red-50 rounded text-sm"
                        disabled={hospitals.length === 2}
                      >
                        Remove
                      </button>
                    </div>
                    <div className="grid grid-cols-2 gap-2">
                      <div>
                        <label className="text-xs text-gray-600">Births</label>
                        <input
                          type="number"
                          min="0"
                          max="500"
                          value={hospital.births}
                          onChange={(e) => handleHospitalChange(index, 'births', e.target.value)}
                          className="w-full px-2 py-1 border rounded"
                        />
                      </div>
                      <div>
                        <label className="text-xs text-gray-600">Days</label>
                        <input
                          type="number"
                          min="1"
                          max="365"
                          value={hospital.days}
                          onChange={(e) => handleHospitalChange(index, 'days', e.target.value)}
                          className="w-full px-2 py-1 border rounded"
                        />
                      </div>
                    </div>
                    <div className="text-xs text-gray-500 mt-1">
                      Rate: {(hospital.births / hospital.days).toFixed(3)} births/day
                    </div>
                  </div>
                ))}
              </div>
              <button
                onClick={addHospital}
                className="mt-2 px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 text-sm"
              >
                Add Hospital
              </button>
            </div>
        </section>

        {/* Model Results */}
        <section className="bg-white rounded-lg shadow-md p-6">
          <h2 className="text-xl font-semibold text-gray-800 mb-4 flex items-center gap-2">
            <span className="px-2 py-1 bg-green-100 text-green-800 text-xs rounded">Results</span>
            Model Estimates
          </h2>
            <div className="space-y-4">
              <div className="grid grid-cols-2 gap-4">
                <div className="p-3 bg-blue-50 rounded">
                  <div className="text-sm text-gray-600">Pooled Rate</div>
                  <div className="text-xl font-bold text-blue-600">
                    {modelResults.pooledRate.toFixed(3)}
                  </div>
                  <div className="text-xs text-gray-500">births per day</div>
                </div>
                <div className="p-3 bg-green-50 rounded">
                  <div className="text-sm text-gray-600">Total Hospitals</div>
                  <div className="text-xl font-bold text-green-600">
                    {hospitals.length}
                  </div>
                  <div className="text-xs text-gray-500">in analysis</div>
                </div>
              </div>
              
              <div className="p-3 bg-gray-50 rounded">
                <div className="text-sm text-gray-600 mb-2">Hyperparameter Estimates</div>
                <div className="text-sm">
                  <div>α = {modelResults.bestHyper.alpha.toFixed(2)}</div>
                  <div>β = {modelResults.bestHyper.beta.toFixed(2)}</div>
                  <div className="mt-1 text-gray-500">
                    Prior ~ Gamma(α, β)
                  </div>
                </div>
              </div>

              <div className="p-3 bg-yellow-50 rounded">
                <div className="text-sm text-gray-600 mb-1">Model Comparison</div>
                <div className="text-sm space-y-1">
                  <div>• Complete pooling: {modelResults.pooledRate.toFixed(3)} births/day</div>
                  <div>• No pooling: Individual rates vary</div>
                  <div>• Partial pooling: Shrinks toward group mean</div>
                </div>
              </div>
            </div>
        </section>
      </div>

      {/* Birth Rate Comparison Chart */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h2 className="text-xl font-semibold text-gray-800 mb-4">Birth Rate Comparison</h2>
          <ResponsiveContainer width="100%" height={300}>
            <BarChart data={modelResults.hospitalData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="name" />
              <YAxis 
                label={{ value: 'Births per Day', angle: -90, position: 'insideLeft' }}
              />
              <Tooltip />
              <Legend />
              <Bar 
                dataKey="birthRate" 
                fill="#8884d8" 
                name="Observed Rate"
              />
            </BarChart>
          </ResponsiveContainer>
      </section>

      {/* Posterior Distributions */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h2 className="text-xl font-semibold text-gray-800 mb-4">Posterior Distributions</h2>
          <ResponsiveContainer width="100%" height={400}>
            <LineChart data={modelResults.posteriorData[0]?.distribution || []}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis 
                dataKey="rate" 
                label={{ value: 'Birth Rate (births/day)', position: 'insideBottom', offset: -5 }}
              />
              <YAxis 
                label={{ value: 'Probability Density', angle: -90, position: 'insideLeft' }}
              />
              <Tooltip />
              <Legend />
              {modelResults.posteriorData.map((hospital, index) => (
                <Line
                  key={hospital.name}
                  type="monotone"
                  dataKey="probability"
                  data={hospital.distribution}
                  stroke={`hsl(${index * 60}, 70%, 50%)`}
                  strokeWidth={2}
                  dot={false}
                  name={hospital.name}
                />
              ))}
            </LineChart>
          </ResponsiveContainer>
      </section>

      {/* Mathematical Framework */}
      <section className="bg-white rounded-lg shadow-md p-6">
        <h2 className="text-xl font-semibold text-gray-800 mb-4">Mathematical Framework</h2>
          <div className="space-y-4 text-sm">
            <div>
              <h4 className="font-semibold mb-2">Hierarchical Model Structure</h4>
              <div className="bg-gray-50 p-3 rounded font-mono text-xs">
                <div>λ_i ~ Gamma(α, β)  (hospital-specific rates)</div>
                <div>births_i ~ Poisson(λ_i × days_i)  (observed births)</div>
                <div>α, β ~ Hyperpriors  (population parameters)</div>
              </div>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Pooling Strategies</h4>
              <div className="space-y-2">
                <div className="p-2 bg-red-50 rounded">
                  <div className="font-medium text-red-700">Complete Pooling</div>
                  <div className="text-xs text-gray-600">All hospitals share same rate</div>
                </div>
                <div className="p-2 bg-yellow-50 rounded">
                  <div className="font-medium text-yellow-700">No Pooling</div>
                  <div className="text-xs text-gray-600">Each hospital estimated independently</div>
                </div>
                <div className="p-2 bg-green-50 rounded">
                  <div className="font-medium text-green-700">Partial Pooling</div>
                  <div className="text-xs text-gray-600">Shares information, allows variation</div>
                </div>
              </div>
            </div>
            
            <div>
              <h4 className="font-semibold mb-2">Key Benefits</h4>
              <ul className="list-disc list-inside space-y-1 text-gray-600">
                <li>Borrows strength across hospitals</li>
                <li>Reduces overfitting for small hospitals</li>
                <li>Accounts for between-hospital variation</li>
                <li>Provides uncertainty quantification</li>
              </ul>
            </div>
          </div>
      </section>
    </div>
  );
};

export default Hospital;
