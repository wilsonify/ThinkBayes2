import React, { useState } from 'react';
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, Legend, LineChart, Line, Area, AreaChart } from 'recharts';
import { TrendingUp, BarChart3 } from 'lucide-react';

const PMFDemo = () => {
  const [distribution, setDistribution] = useState('binomial');
  const [n, setN] = useState(10);
  const [p, setP] = useState(0.5);
  const [lambda, setLambda] = useState(3);

  const generatePMFData = () => {
    let data = [];
    
    switch (distribution) {
      case 'binomial':
        for (let k = 0; k <= n; k++) {
          const probability = binomialPMF(k, n, p);
          data.push({
            x: k,
            probability: probability,
            cumulative: data.reduce((sum, item) => sum + item.probability, 0) + probability
          });
        }
        break;
        
      case 'poisson':
        for (let k = 0; k <= Math.max(15, lambda * 3); k++) {
          const probability = poissonPMF(k, lambda);
          data.push({
            x: k,
            probability: probability,
            cumulative: data.reduce((sum, item) => sum + item.probability, 0) + probability
          });
        }
        break;
        
      case 'uniform':
        for (let k = 1; k <= 6; k++) {
          const probability = 1/6;
          data.push({
            x: k,
            probability: probability,
            cumulative: data.reduce((sum, item) => sum + item.probability, 0) + probability
          });
        }
        break;
    }
    
    return data;
  };

  const binomialPMF = (k, n, p) => {
    const coefficient = factorial(n) / (factorial(k) * factorial(n - k));
    return coefficient * Math.pow(p, k) * Math.pow(1 - p, n - k);
  };

  const poissonPMF = (k, lambda) => {
    return (Math.pow(lambda, k) * Math.exp(-lambda)) / factorial(k);
  };

  const factorial = (n) => {
    if (n <= 1) return 1;
    let result = 1;
    for (let i = 2; i <= n; i++) {
      result *= i;
    }
    return result;
  };

  const data = generatePMFData();
  const expectedValue = distribution === 'binomial' ? n * p : 
                       distribution === 'poisson' ? lambda : 3.5;
  const variance = distribution === 'binomial' ? n * p * (1 - p) : 
                   distribution === 'poisson' ? lambda : 35/12;

  const formatProbability = (prob) => {
    return (prob * 100).toFixed(2) + '%';
  };

  return (
    <div className="space-y-8">
      <section className="bg-white rounded-lg shadow-md p-6">
        <h2 className="text-3xl font-bold text-gray-800 mb-6 flex items-center">
          <BarChart3 className="h-8 w-8 mr-3 text-indigo-600" />
          Probability Mass Functions
        </h2>
        
        <div className="prose max-w-none text-gray-600">
          <p className="mb-4">
            A Probability Mass Function (PMF) gives the probability that a discrete random variable 
            takes on a specific value. Explore different common distributions and see how their parameters 
            affect the shape and characteristics.
          </p>
          
          <div className="grid md:grid-cols-3 gap-4 mt-4">
            <div className="bg-blue-50 rounded-lg p-3">
              <h4 className="font-semibold text-blue-800 mb-1">PMF</h4>
              <p className="text-sm text-blue-700">
                P(X = x) - Probability of exact value
              </p>
            </div>
            <div className="bg-green-50 rounded-lg p-3">
              <h4 className="font-semibold text-green-800 mb-1">CDF</h4>
              <p className="text-sm text-green-700">
                P(X &le; x) - Cumulative probability
              </p>
            </div>
            <div className="bg-purple-50 rounded-lg p-3">
              <h4 className="font-semibold text-purple-800 mb-1">Expected Value</h4>
              <p className="text-sm text-purple-700">
                E[X] - Mean of the distribution
              </p>
            </div>
          </div>
        </div>
      </section>

      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-2xl font-semibold text-gray-800 mb-6">Distribution Explorer</h3>
        
        <div className="grid md:grid-cols-3 gap-6 mb-8">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Distribution Type
            </label>
            <select
              value={distribution}
              onChange={(e) => setDistribution(e.target.value)}
              className="w-full px-3 py-2 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-indigo-500"
            >
              <option value="binomial">Binomial</option>
              <option value="poisson">Poisson</option>
              <option value="uniform">Uniform (Die Roll)</option>
            </select>
          </div>

          {distribution === 'binomial' && (
            <>
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Trials (n): <span className="font-semibold text-indigo-600">{n}</span>
                </label>
                <input
                  type="range"
                  min="1"
                  max="20"
                  value={n}
                  onChange={(e) => setN(parseInt(e.target.value))}
                  className="w-full h-2 bg-indigo-200 rounded-lg appearance-none cursor-pointer"
                />
              </div>
              
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Success Probability (p): <span className="font-semibold text-indigo-600">{p.toFixed(2)}</span>
                </label>
                <input
                  type="range"
                  min="0"
                  max="1"
                  step="0.05"
                  value={p}
                  onChange={(e) => setP(parseFloat(e.target.value))}
                  className="w-full h-2 bg-indigo-200 rounded-lg appearance-none cursor-pointer"
                />
              </div>
            </>
          )}

          {distribution === 'poisson' && (
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Rate (λ): <span className="font-semibold text-indigo-600">{lambda}</span>
              </label>
              <input
                type="range"
                min="1"
                max="10"
                step="0.5"
                value={lambda}
                onChange={(e) => setLambda(parseFloat(e.target.value))}
                className="w-full h-2 bg-indigo-200 rounded-lg appearance-none cursor-pointer"
              />
            </div>
          )}
        </div>

        <div className="grid md:grid-cols-2 gap-8">
          <div className="bg-gray-50 rounded-lg p-6">
            <h4 className="text-lg font-semibold text-gray-800 mb-4 flex items-center">
              <BarChart className="h-5 w-5 mr-2 text-blue-600" />
              Probability Mass Function
            </h4>
            <BarChart width={400} height={300} data={data}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="x" />
              <YAxis />
              <Tooltip formatter={(value) => formatProbability(value)} />
              <Bar dataKey="probability" fill="#4F46E5" />
            </BarChart>
          </div>

          <div className="bg-gray-50 rounded-lg p-6">
            <h4 className="text-lg font-semibold text-gray-800 mb-4 flex items-center">
              <TrendingUp className="h-5 w-5 mr-2 text-green-600" />
              Cumulative Distribution Function
            </h4>
            <LineChart width={400} height={300} data={data}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="x" />
              <YAxis />
              <Tooltip formatter={(value) => formatProbability(value)} />
              <Line type="monotone" dataKey="cumulative" stroke="#10B981" strokeWidth={2} />
            </LineChart>
          </div>
        </div>
      </section>

      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-2xl font-semibold text-gray-800 mb-6">Distribution Properties</h3>
        
        <div className="grid md:grid-cols-2 gap-8">
          <div className="bg-indigo-50 rounded-lg p-6 border border-indigo-200">
            <h4 className="text-lg font-semibold text-indigo-800 mb-4">Statistical Properties</h4>
            
            <div className="space-y-3">
              <div className="flex justify-between">
                <span className="text-gray-700">Expected Value (E[X]):</span>
                <span className="font-semibold text-indigo-600">{expectedValue.toFixed(2)}</span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-700">Variance (Var[X]):</span>
                <span className="font-semibold text-indigo-600">{variance.toFixed(2)}</span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-700">Standard Deviation:</span>
                <span className="font-semibold text-indigo-600">{Math.sqrt(variance).toFixed(2)}</span>
              </div>
            </div>
          </div>

          <div className="bg-purple-50 rounded-lg p-6 border border-purple-200">
            <h4 className="text-lg font-semibold text-purple-800 mb-4">Key Insights</h4>
            
            <div className="text-sm text-purple-700 space-y-2">
              {distribution === 'binomial' && (
                <>
                  <p>• Models number of successes in <strong>{n}</strong> independent trials</p>
                  <p>• Each trial has success probability <strong>{p}</strong></p>
                  <p>• Symmetric when p = 0.5, skewed otherwise</p>
                  <p>• Used for: quality control, medical trials, polling</p>
                </>
              )}
              
              {distribution === 'poisson' && (
                <>
                  <p>• Models events occurring at rate <strong>{lambda}</strong> per interval</p>
                  <p>• Events are independent and rare</p>
                  <p>• Mean = Variance = {lambda}</p>
                  <p>• Used for: customer arrivals, defects, rare events</p>
                </>
              )}
              
              {distribution === 'uniform' && (
                <>
                  <p>• All outcomes equally likely (1/6 each)</p>
                  <p>• Models fair die rolls</p>
                  <p>• Maximum uncertainty for discrete outcomes</p>
                  <p>• Used for: games of chance, random selection</p>
                </>
              )}
            </div>
          </div>
        </div>
      </section>

      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-2xl font-semibold text-gray-800 mb-6">Probability Table</h3>
        
        <div className="overflow-x-auto">
          <table className="w-full text-sm">
            <thead>
              <tr className="bg-gray-100">
                <th className="px-4 py-2 text-left">Value (x)</th>
                <th className="px-4 py-2 text-left">P(X = x)</th>
                <th className="px-4 py-2 text-left">P(X &le; x)</th>
                <th className="px-4 py-2 text-left">P(X &gt; x)</th>
              </tr>
            </thead>
            <tbody>
              {data.map((item, index) => (
                <tr key={index} className="border-b hover:bg-gray-50">
                  <td className="px-4 py-2 font-medium">{item.x}</td>
                  <td className="px-4 py-2">{formatProbability(item.probability)}</td>
                  <td className="px-4 py-2">{formatProbability(item.cumulative)}</td>
                  <td className="px-4 py-2">{formatProbability(1 - item.cumulative)}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </section>
    </div>
  );
};

export default PMFDemo;
