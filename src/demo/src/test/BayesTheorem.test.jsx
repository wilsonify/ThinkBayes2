import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, test } from 'vitest'
import BayesTheorem from '../components/BayesTheorem'

describe('BayesTheorem Component', () => {
  it('renders the component with initial values', () => {
    render(<BayesTheorem />)
    
    expect(screen.getByText("Bayes' Theorem")).toBeInTheDocument()
    expect(screen.getByDisplayValue('0.01')).toBeInTheDocument() // Prior A
    expect(screen.getByDisplayValue('0.99')).toBeInTheDocument() // Likelihood B|A
    expect(screen.getByDisplayValue('0.05')).toBeInTheDocument() // Likelihood B|not A
  })

  it('calculates posterior correctly with initial values', () => {
    render(<BayesTheorem />)
    
    // With initial values: prior=0.01, likelihoodBA=0.99, likelihoodNotA=0.05
    // Posterior should be: (0.01 * 0.99) / (0.01 * 0.99 + 0.99 * 0.05) = 0.1667
    expect(screen.getByText('16.67%')).toBeInTheDocument()
  })

  it('updates calculation when prior changes', () => {
    render(<BayesTheorem />)
    
    const priorInput = screen.getByLabelText('Prior P(A):')
    fireEvent.change(priorInput, { target: { value: '0.5' } })
    
    // With prior=0.5: (0.5 * 0.99) / (0.5 * 0.99 + 0.5 * 0.05) = 0.9524
    expect(screen.getByText('95.24%')).toBeInTheDocument()
  })

  it('updates calculation when likelihood changes', () => {
    render(<BayesTheorem />)
    
    const likelihoodInput = screen.getByLabelText('Likelihood P(B|A):')
    fireEvent.change(likelihoodInput, { target: { value: '0.8' } })
    
    // With likelihood=0.8: (0.01 * 0.8) / (0.01 * 0.8 + 0.99 * 0.05) = 0.1395
    expect(screen.getByText('13.95%')).toBeInTheDocument()
  })

  it('handles edge case with zero prior', () => {
    render(<BayesTheorem />)
    
    const priorInput = screen.getByLabelText('Prior P(A):')
    fireEvent.change(priorInput, { target: { value: '0' } })
    
    expect(screen.getByText('0.00%')).toBeInTheDocument()
  })

  it('handles edge case with maximum likelihood', () => {
    render(<BayesTheorem />)
    
    const likelihoodInput = screen.getByLabelText('Likelihood P(B|A):')
    fireEvent.change(likelihoodInput, { target: { value: '1' } })
    
    // With likelihood=1: (0.01 * 1) / (0.01 * 1 + 0.99 * 0.05) = 0.1681
    expect(screen.getByText('16.81%')).toBeInTheDocument()
  })

  it('displays mathematical formula correctly', () => {
    render(<BayesTheorem />)
    
    expect(screen.getByTestId('block-math')).toBeInTheDocument()
    expect(screen.getByText(/Posterior =/)).toBeInTheDocument()
  })

  it('shows interpretation text', () => {
    render(<BayesTheorem />)
    
    expect(screen.getByText(/Interpretation/)).toBeInTheDocument()
    expect(screen.getByText(/Given a prior probability/)).toBeInTheDocument()
  })

  test('all interactive elements are accessible', () => {
    render(<BayesTheorem />)
    
    const inputs = screen.getAllByRole('spinbutton')
    inputs.forEach(input => {
      expect(input).toHaveAttribute('aria-label')
      expect(input).toHaveAttribute('min')
      expect(input).toHaveAttribute('max')
      expect(input).toHaveAttribute('step')
    })
  })

  test('probability values are formatted correctly', () => {
    render(<BayesTheorem />)
    
    // Check that probabilities are displayed as percentages with 2 decimal places
    const posteriorElement = screen.getByText(/16\.67%/)
    expect(posteriorElement).toBeInTheDocument()
  })

  test('evidence calculation is displayed', () => {
    render(<BayesTheorem />)
    
    // Evidence should be: 0.01 * 0.99 + 0.99 * 0.05 = 0.0594
    expect(screen.getByText('5.94%')).toBeInTheDocument()
  })
})
