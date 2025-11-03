import { describe, it, expect, test } from 'vitest'

// Mathematical utility functions for testing
const mathematicalFunctions = {
  // Gamma function approximation
  gamma: (n) => {
    const g = 7
    const p = [
      0.99999999999980993, 676.5203681218851, -1259.1392167224028,
      771.32342877765313, -176.61502916214059, 12.507343278686905,
      -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
    ]
    
    if (n < 0.5) {
      return Math.PI / (Math.sin(Math.PI * n) * mathematicalFunctions.gamma(1 - n))
    }
    
    n--
    let x = p[0]
    for (let i = 1; i < g + 2; i++) {
      x += p[i] / (n + i)
    }
    
    let t = n + g + 0.5
    return Math.sqrt(2 * Math.PI) * Math.pow(t, n + 0.5) * Math.exp(-t) * x
  },

  // Beta function
  beta: (x, y) => {
    return mathematicalFunctions.gamma(x) * mathematicalFunctions.gamma(y) / mathematicalFunctions.gamma(x + y)
  },

  // Binomial coefficient
  binomialCoefficient: (n, k) => {
    if (k > n) return 0
    if (k === 0 || k === n) return 1
    
    let result = 1
    for (let i = 0; i < k; i++) {
      result *= (n - i) / (i + 1)
    }
    return result
  },

  // Poisson PMF
  poissonPMF: (k, lambda) => {
    return Math.pow(lambda, k) * Math.exp(-lambda) / mathematicalFunctions.factorial(k)
  },

  // Factorial
  factorial: (n) => {
    if (n < 0) return 0
    if (n === 0 || n === 1) return 1
    let result = 1
    for (let i = 2; i <= n; i++) {
      result *= i
    }
    return result
  },

  // Gamma PDF
  gammaPDF: (x, alpha, beta) => {
    if (x <= 0) return 0
    return Math.pow(beta, alpha) * Math.pow(x, alpha - 1) * Math.exp(-beta * x) / mathematicalFunctions.gamma(alpha)
  },

  // Normal PDF
  normalPDF: (x, mu, sigma) => {
    return (1 / (sigma * Math.sqrt(2 * Math.PI))) * Math.exp(-0.5 * Math.pow((x - mu) / sigma, 2))
  },

  // Weibull PDF
  weibullPDF: (t, shape, scale) => {
    if (t <= 0) return 0
    return (shape / scale) * Math.pow(t / scale, shape - 1) * Math.exp(-Math.pow(t / scale, shape))
  },

  // Weibull survival function
  weibullSurvival: (t, shape, scale) => {
    if (t <= 0) return 1
    return Math.exp(-Math.pow(t / scale, shape))
  },

  // Weibull hazard function
  weibullHazard: (t, shape, scale) => {
    if (t <= 0) return 0
    return (shape / scale) * Math.pow(t / scale, shape - 1)
  }
}

describe('Mathematical Functions', () => {
  describe('Factorial', () => {
    it('calculates factorial correctly', () => {
      expect(mathematicalFunctions.factorial(0)).toBe(1)
      expect(mathematicalFunctions.factorial(1)).toBe(1)
      expect(mathematicalFunctions.factorial(5)).toBe(120)
      expect(mathematicalFunctions.factorial(10)).toBe(3628800)
    })

    it('handles negative input', () => {
      expect(mathematicalFunctions.factorial(-1)).toBe(0)
    })
  })

  describe('Binomial Coefficient', () => {
    it('calculates binomial coefficients correctly', () => {
      expect(mathematicalFunctions.binomialCoefficient(5, 2)).toBe(10)
      expect(mathematicalFunctions.binomialCoefficient(10, 3)).toBe(120)
      expect(mathematicalFunctions.binomialCoefficient(6, 0)).toBe(1)
      expect(mathematicalFunctions.binomialCoefficient(6, 6)).toBe(1)
    })

    it('handles invalid input', () => {
      expect(mathematicalFunctions.binomialCoefficient(5, 6)).toBe(0)
    })
  })

  describe('Poisson PMF', () => {
    it('calculates Poisson probability correctly', () => {
      const result = mathematicalFunctions.poissonPMF(3, 2)
      expect(result).toBeCloseTo(0.1804, 4)
    })

    it('handles edge cases', () => {
      expect(mathematicalFunctions.poissonPMF(0, 1)).toBeCloseTo(0.3679, 4)
      expect(mathematicalFunctions.poissonPMF(0, 0)).toBe(1) // e^0 = 1
    })
  })

  describe('Gamma PDF', () => {
    it('calculates gamma PDF correctly', () => {
      const result = mathematicalFunctions.gammaPDF(2, 2, 1)
      expect(result).toBeCloseTo(0.2707, 4)
    })

    it('handles zero input', () => {
      expect(mathematicalFunctions.gammaPDF(0, 2, 1)).toBe(0)
    })
  })

  describe('Normal PDF', () => {
    it('calculates normal PDF correctly', () => {
      const result = mathematicalFunctions.normalPDF(0, 0, 1)
      expect(result).toBeCloseTo(0.3989, 4)
    })

    it('calculates normal PDF for different parameters', () => {
      const result = mathematicalFunctions.normalPDF(1, 0, 1)
      expect(result).toBeCloseTo(0.2420, 4)
    })
  })

  describe('Weibull Functions', () => {
    it('calculates Weibull PDF correctly', () => {
      const result = mathematicalFunctions.weibullPDF(1, 2, 1)
      expect(result).toBeCloseTo(0.7358, 4)
    })

    it('calculates Weibull survival function correctly', () => {
      const result = mathematicalFunctions.weibullSurvival(1, 2, 1)
      expect(result).toBeCloseTo(0.3679, 4)
    })

    it('calculates Weibull hazard function correctly', () => {
      const result = mathematicalFunctions.weibullHazard(1, 2, 1)
      expect(result).toBeCloseTo(2, 4)
    })

    it('handles zero time input', () => {
      expect(mathematicalFunctions.weibullPDF(0, 2, 1)).toBe(0)
      expect(mathematicalFunctions.weibullSurvival(0, 2, 1)).toBe(1)
      expect(mathematicalFunctions.weibullHazard(0, 2, 1)).toBe(0)
    })
  })

  describe('Gamma Function', () => {
    it('calculates gamma function correctly', () => {
      expect(mathematicalFunctions.gamma(1)).toBe(1)
      expect(mathematicalFunctions.gamma(2)).toBe(1)
      expect(mathematicalFunctions.gamma(3)).toBe(2)
      expect(mathematicalFunctions.gamma(4)).toBe(6)
    })

    it('handles half-integer values', () => {
      expect(mathematicalFunctions.gamma(0.5)).toBeCloseTo(Math.sqrt(Math.PI), 4)
    })
  })

  describe('Beta Function', () => {
    it('calculates beta function correctly', () => {
      const result = mathematicalFunctions.beta(2, 3)
      expect(result).toBeCloseTo(0.0833, 4)
    })
  })

  test('all functions handle edge cases gracefully', () => {
    // Test zero and negative inputs
    expect(() => mathematicalFunctions.factorial(-5)).not.toThrow()
    expect(() => mathematicalFunctions.binomialCoefficient(5, -1)).not.toThrow()
    expect(() => mathematicalFunctions.poissonPMF(-1, 1)).not.toThrow()
    expect(() => mathematicalFunctions.gammaPDF(-1, 1, 1)).not.toThrow()
  })

  test('mathematical consistency', () => {
    // Test that sum of probabilities equals 1 for discrete distributions
    let poissonSum = 0
    for (let k = 0; k < 20; k++) {
      poissonSum += mathematicalFunctions.poissonPMF(k, 5)
    }
    expect(poissonSum).toBeCloseTo(1, 2)
  })

  test('precision and accuracy', () => {
    // Test that functions return reasonable precision
    const gammaResult = mathematicalFunctions.gammaPDF(1.5, 2, 1)
    expect(gammaResult).toBeGreaterThan(0)
    expect(gammaResult).toBeLessThan(1)
    
    const normalResult = mathematicalFunctions.normalPDF(0, 0, 1)
    expect(normalResult).toBeCloseTo(0.3989, 3)
  })
})

describe('Statistical Calculations', () => {
  test('expected value calculation', () => {
    // Test expected value for discrete distribution
    const values = [1, 2, 3, 4, 5]
    const probabilities = [0.1, 0.2, 0.3, 0.2, 0.2]
    
    let expectedValue = 0
    for (let i = 0; i < values.length; i++) {
      expectedValue += values[i] * probabilities[i]
    }
    
    expect(expectedValue).toBeCloseTo(3.2, 4)
  })

  test('variance calculation', () => {
    const values = [1, 2, 3, 4, 5]
    const probabilities = [0.1, 0.2, 0.3, 0.2, 0.2]
    
    // Calculate expected value
    let expectedValue = 0
    for (let i = 0; i < values.length; i++) {
      expectedValue += values[i] * probabilities[i]
    }
    
    // Calculate variance
    let variance = 0
    for (let i = 0; i < values.length; i++) {
      variance += Math.pow(values[i] - expectedValue, 2) * probabilities[i]
    }
    
    expect(variance).toBeCloseTo(1.36, 4)
  })

  test('cumulative probability calculation', () => {
    const probabilities = [0.1, 0.2, 0.3, 0.2, 0.2]
    
    let cumulative = 0
    const cumulativeProbabilities = []
    
    for (const p of probabilities) {
      cumulative += p
      cumulativeProbabilities.push(cumulative)
    }
    
    expect(cumulativeProbabilities).toEqual([0.1, 0.3, 0.6, 0.8, 1.0])
  })
})
