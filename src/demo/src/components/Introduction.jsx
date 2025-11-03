import React from 'react';
import { Book, ExternalLink, Code } from 'lucide-react';

const Introduction = () => {
  return (
    <div className="space-y-8">
      <section className="bg-white rounded-lg shadow-md p-6">
        <h2 className="text-3xl font-bold text-gray-800 mb-4 flex items-center">
          <Book className="h-8 w-8 mr-3 text-blue-600" />
          Welcome to Think Bayes 2
        </h2>
        
        <div className="prose max-w-none text-gray-600">
          <p className="text-lg leading-relaxed mb-4">
            <em>Think Bayes</em> is an introduction to Bayesian statistics using computational methods. 
            This interactive demo demonstrates key concepts from Allen B. Downey's book, allowing you to 
            explore Bayesian thinking through hands-on examples.
          </p>
          
          <p className="mb-4">
            Bayesian statistics provides a powerful framework for updating beliefs based on evidence. 
            Unlike traditional frequentist approaches, Bayesian methods incorporate prior knowledge and 
            update it systematically as new data becomes available.
          </p>
        </div>
      </section>

      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-2xl font-semibold text-gray-800 mb-4">What You'll Explore</h3>
        
        <div className="grid md:grid-cols-2 gap-6">
          <div className="border-l-4 border-blue-500 pl-4">
            <h4 className="font-semibold text-gray-800 mb-2">Bayes' Theorem</h4>
            <p className="text-gray-600">
              The fundamental theorem that underpins Bayesian inference, showing how to update 
              probabilities based on new evidence.
            </p>
          </div>
          
          <div className="border-l-4 border-green-500 pl-4">
            <h4 className="font-semibold text-gray-800 mb-2">The Cookie Problem</h4>
            <p className="text-gray-600">
              A classic example demonstrating how to calculate posterior probabilities when 
              drawing from different sources.
            </p>
          </div>
          
          <div className="border-l-4 border-purple-500 pl-4">
            <h4 className="font-semibold text-gray-800 mb-2">Dice Problems</h4>
            <p className="text-gray-600">
              Interactive examples showing how to reason about uncertain outcomes and update 
              beliefs with each roll.
            </p>
          </div>
          
          <div className="border-l-4 border-orange-500 pl-4">
            <h4 className="font-semibold text-gray-800 mb-2">Probability Distributions</h4>
            <p className="text-gray-600">
              Visual exploration of PMFs, PDFs, and CDFs that form the building blocks of 
              Bayesian analysis.
            </p>
          </div>
        </div>
      </section>

      <section className="bg-white rounded-lg shadow-md p-6">
        <h3 className="text-2xl font-semibold text-gray-800 mb-4">Key Concepts</h3>
        
        <div className="space-y-4">
          <div className="bg-gray-50 rounded-lg p-4">
            <h4 className="font-semibold text-gray-800 mb-2">Prior Probability</h4>
            <p className="text-gray-600">
              What you believe about a hypothesis before seeing evidence.
            </p>
          </div>
          
          <div className="bg-gray-50 rounded-lg p-4">
            <h4 className="font-semibold text-gray-800 mb-2">Likelihood</h4>
            <p className="text-gray-600">
              The probability of observing the evidence given a hypothesis.
            </p>
          </div>
          
          <div className="bg-gray-50 rounded-lg p-4">
            <h4 className="font-semibold text-gray-800 mb-2">Posterior Probability</h4>
            <p className="text-gray-600">
              The updated belief about a hypothesis after considering evidence.
            </p>
          </div>
        </div>
      </section>

      <section className="bg-blue-50 rounded-lg p-6 border border-blue-200">
        <h3 className="text-xl font-semibold text-blue-800 mb-3 flex items-center">
          <Code className="h-5 w-5 mr-2" />
          About the Original Book
        </h3>
        
        <p className="text-blue-700 mb-4">
          This demo is based on <em>Think Bayes 2</em> by Allen B. Downey. The complete book 
          is available for free under Creative Commons license.
        </p>
        
        <div className="flex flex-wrap gap-4">
          <a 
            href="http://allendowney.github.io/ThinkBayes2" 
            target="_blank" 
            rel="noopener noreferrer"
            className="inline-flex items-center text-blue-600 hover:text-blue-800 font-medium"
          >
            <ExternalLink className="h-4 w-4 mr-1" />
            Read the Full Book
          </a>
          
          <a 
            href="https://github.com/AllenDowney/ThinkBayes2" 
            target="_blank" 
            rel="noopener noreferrer"
            className="inline-flex items-center text-blue-600 hover:text-blue-800 font-medium"
          >
            <ExternalLink className="h-4 w-4 mr-1" />
            GitHub Repository
          </a>
        </div>
      </section>
    </div>
  );
};

export default Introduction;
