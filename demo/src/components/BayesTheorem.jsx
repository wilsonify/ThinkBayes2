import React, { useState } from 'react';
import { InlineMath, BlockMath } from 'react-katex';
import 'katex/dist/katex.min.css';

const BayesTheorem = () => {
  const [priorA, setPriorA] = useState(0.01);
  const [likelihoodBA, setLikelihoodBA] = useState(0.99);
  const [likelihoodNotA, setLikelihoodNotA] = useState(0.05);

  const calculatePosterior = () => {
    const priorNotA = 1 - priorA;
    const evidence = priorA * likelihoodBA + priorNotA * likelihoodNotA;
    const posterior = (priorA * likelihoodBA) / evidence;
    return posterior;
  };

  const posterior = calculatePosterior();
  const evidence = priorA * likelihoodBA + (1 - priorA) * likelihoodNotA;

  const formatProbability = (prob) => {
    return (prob * 100).toFixed(2) + '%';
  };

  return (
    <div className="space-y-8">
      <section className="bg-white rounded-lg shadow-md p-6">
        <h2 className="text-3xl font-bold text-gray-800 mb-6">Bayes' Theorem</h2>
        
        <div className="mb-8">
          <p className="text-gray-600 mb-4">
            Bayes' Theorem describes how to update the probability of a hypothesis based on new evidence. 
            The theorem is expressed as:
          </p>
          
          <div className="bg-gray-50 rounded-lg p-4 text-center">
            <BlockMath math="P(A|B) = \frac{P(A) \cdot P(B|A)}{P(B)}" />
          </div>
          
          <div className="mt-4 grid md:grid-cols-2 gap-4 text-sm">
            <div className="bg-blue-50 rounded p-3">
              <strong className="text-blue-800">P(A|B)</strong>: Posterior probability
              <div className="text-gray-600">Probability of A given B occurred</div>
            </div>
            <div className="bg-green-50 rounded p-3">
              <strong className="text-green-800">P(A)</strong>: Prior probability
              <div className="text-gray-600">Initial probability of A</div>
            </div>
            <div className="bg-purple-50 rounded p-3">
              <strong className="text-purple-800">P(B|A)</strong>: Likelihood
              <div className="text-gray-600">Probability of B given A</div>
            </div>
            <div className="bg-orange-50 rounded p-3">
              <strong className="text-orange-800">P(B)</strong>: Evidence
              <div className="text-gray-600">Total probability of B</div>
            </div>
          </div>
        </div>
      </section>

      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-2xl font-semibold text-gray-800 mb-6">Interactive Calculator</h3>
        
        <div className="space-y-6">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Prior Probability P(A): <span className="text-blue-600 font-semibold">{formatProbability(priorA)}</span>
            </label>
            <input
              type="range"
              min="0"
              max="1"
              step="0.01"
              value={priorA}
              onChange={(e) => setPriorA(parseFloat(e.target.value))}
              className="w-full h-2 bg-blue-200 rounded-lg appearance-none cursor-pointer"
            />
            <div className="flex justify-between text-xs text-gray-500 mt-1">
              <span>0%</span>
              <span>50%</span>
              <span>100%</span>
            </div>
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Likelihood P(B|A): <span className="text-purple-600 font-semibold">{formatProbability(likelihoodBA)}</span>
            </label>
            <input
              type="range"
              min="0"
              max="1"
              step="0.01"
              value={likelihoodBA}
              onChange={(e) => setLikelihoodBA(parseFloat(e.target.value))}
              className="w-full h-2 bg-purple-200 rounded-lg appearance-none cursor-pointer"
            />
            <div className="flex justify-between text-xs text-gray-500 mt-1">
              <span>0%</span>
              <span>50%</span>
              <span>100%</span>
            </div>
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              Likelihood P(B|¬A): <span className="text-purple-600 font-semibold">{formatProbability(likelihoodNotA)}</span>
            </label>
            <input
              type="range"
              min="0"
              max="1"
              step="0.01"
              value={likelihoodNotA}
              onChange={(e) => setLikelihoodNotA(parseFloat(e.target.value))}
              className="w-full h-2 bg-purple-200 rounded-lg appearance-none cursor-pointer"
            />
            <div className="flex justify-between text-xs text-gray-500 mt-1">
              <span>0%</span>
              <span>50%</span>
              <span>100%</span>
            </div>
          </div>
        </div>

        <div className="mt-8 bg-gradient-to-r from-blue-50 to-purple-50 rounded-lg p-6 border border-blue-200">
          <h4 className="text-lg font-semibold text-gray-800 mb-4">Calculation Results</h4>
          
          <div className="grid md:grid-cols-3 gap-4">
            <div className="text-center">
              <div className="text-sm text-gray-600 mb-1">Evidence P(B)</div>
              <div className="text-2xl font-bold text-orange-600">{formatProbability(evidence)}</div>
            </div>
            
            <div className="text-center">
              <div className="text-sm text-gray-600 mb-1">Posterior P(A|B)</div>
              <div className="text-3xl font-bold text-blue-600">{formatProbability(posterior)}</div>
            </div>
            
            <div className="text-center">
              <div className="text-sm text-gray-600 mb-1">Change from Prior</div>
              <div className={`text-2xl font-bold ${posterior > priorA ? 'text-green-600' : 'text-red-600'}`}>
                {posterior > priorA ? '+' : ''}{formatProbability(posterior - priorA)}
              </div>
            </div>
          </div>

          <div className="mt-4 text-center">
            <BlockMath math={`P(A|B) = \\frac{${priorA.toFixed(2)} \\cdot ${likelihoodBA.toFixed(2)}}{${evidence.toFixed(4)}} = ${posterior.toFixed(4)}`} />
          </div>
        </div>
      </section>

      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-2xl font-semibold text-gray-800 mb-4">Example: Medical Testing</h3>
        
        <div className="bg-gray-50 rounded-lg p-4">
          <p className="text-gray-700 mb-3">
            Consider a disease that affects 1% of the population (prior = 0.01). A test has:
          </p>
          <ul className="list-disc list-inside text-gray-700 space-y-1 mb-3">
            <li>99% sensitivity (correctly positive if diseased)</li>
            <li>5% false positive rate (positive if healthy)</li>
          </ul>
          <p className="text-gray-700">
            If you test positive, what's the probability you actually have the disease?
          </p>
        </div>
        
        <button
          onClick={() => {
            setPriorA(0.01);
            setLikelihoodBA(0.99);
            setLikelihoodNotA(0.05);
          }}
          className="mt-4 px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors"
        >
          Load Example Values
        </button>
        
        <div className="mt-4 p-4 bg-yellow-50 rounded-lg border border-yellow-200">
          <p className="text-sm text-yellow-800">
            <strong>Result:</strong> Even with a positive test, there's only about a 16.7% chance 
            you actually have the disease! This counterintuitive result shows why Bayes' theorem 
            is so important in medical decision-making.
          </p>
        </div>
      </section>
    </div>
  );
};

export default BayesTheorem;
