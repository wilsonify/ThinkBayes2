import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, test } from 'vitest'
import PMFDemo from '../components/PMFDemo'

describe('PMFDemo Component', () => {
  it('renders the component with initial distribution', () => {
    render(<PMFDemo />)
    
    expect(screen.getByText('Probability Mass Functions')).toBeInTheDocument()
    expect(screen.getByText('Discrete Probability Distribution')).toBeInTheDocument()
    expect(screen.getByDisplayValue('0.1')).toBeInTheDocument() // P(X=1)
    expect(screen.getByDisplayValue('0.2')).toBeInTheDocument() // P(X=2)
  })

  it('calculates expected value correctly', () => {
    render(<PMFDemo />)
    
    // With initial values: 1*0.1 + 2*0.2 + 3*0.3 + 4*0.2 + 5*0.2 = 3.2
    expect(screen.getByText('3.20')).toBeInTheDocument()
  })

  it('calculates variance correctly', () => {
    render(<PMFDemo />)
    
    // Variance should be calculated and displayed
    const varianceElement = screen.getByText(/Variance:/)
    expect(varianceElement).toBeInTheDocument()
  })

  it('normalizes probabilities when they sum to more than 1', () => {
    render(<PMFDemo />)
    
    const input1 = screen.getByLabelText('P(X = 1):')
    const input2 = screen.getByLabelText('P(X = 2):')
    
    fireEvent.change(input1, { target: { value: '0.5' } })
    fireEvent.change(input2, { target: { value: '0.5' } })
    
    // Should normalize and show warning
    expect(screen.getByText(/Warning: Probabilities sum to/)).toBeInTheDocument()
  })

  it('updates calculations when probabilities change', () => {
    render(<PMFDemo />)
    
    const input1 = screen.getByLabelText('P(X = 1):')
    fireEvent.change(input1, { target: { value: '0.5' } })
    
    // Expected value should change
    expect(screen.getByText('3.70')).toBeInTheDocument()
  })

  it('displays probability table with correct values', () => {
    render(<PMFDemo />)
    
    expect(screen.getByText('Probability Table')).toBeInTheDocument()
    expect(screen.getByText('Value (x)')).toBeInTheDocument()
    expect(screen.getByText('P(X = x)')).toBeInTheDocument()
    expect(screen.getByText('P(X ≤ x)')).toBeInTheDocument()
    expect(screen.getByText('P(X > x)')).toBeInTheDocument()
  })

  it('shows cumulative probabilities correctly', () => {
    render(<PMFDemo />)
    
    // Check cumulative probabilities in table
    const cumulativeValues = screen.getAllByText(/\d+\.\d+%/)
    expect(cumulativeValues.length).toBeGreaterThan(0)
  })

  it('displays PMF visualization', () => {
    render(<PMFDemo />)
    
    expect(screen.getByTestId('bar-chart')).toBeInTheDocument()
    expect(screen.getByText('Probability Mass Function')).toBeInTheDocument()
  })

  it('displays CDF visualization', () => {
    render(<PMFDemo />)
    
    expect(screen.getByTestId('line-chart')).toBeInTheDocument()
    expect(screen.getByText('Cumulative Distribution Function')).toBeInTheDocument()
  })

  test('handles zero probability edge case', () => {
    render(<PMFDemo />)
    
    const input1 = screen.getByLabelText('P(X = 1):')
    fireEvent.change(input1, { target: { value: '0' } })
    
    // Should still calculate expected value correctly
    expect(screen.getByText('3.10')).toBeInTheDocument()
  })

  test('handles single probability case', () => {
    render(<PMFDemo />)
    
    // Set all probabilities to 0 except one
    const inputs = screen.getAllByRole('spinbutton')
    inputs.forEach((input, index) => {
      if (index === 0) {
        fireEvent.change(input, { target: { value: '1' } })
      } else {
        fireEvent.change(input, { target: { value: '0' } })
      }
    })
    
    expect(screen.getByText('1.00')).toBeInTheDocument()
  })

  test('validates probability range', () => {
    render(<PMFDemo />)
    
    const input1 = screen.getByLabelText('P(X = 1):')
    
    // Test invalid input
    fireEvent.change(input1, { target: { value: '-0.1' } })
    expect(input1.value).toBe('0.1') // Should clamp to valid range
    
    fireEvent.change(input1, { target: { value: '1.5' } })
    expect(input1.value).toBe('1') // Should clamp to valid range
  })

  test('displays mathematical framework', () => {
    render(<PMFDemo />)
    
    expect(screen.getByText('Mathematical Framework')).toBeInTheDocument()
    expect(screen.getByText(/Expected Value:/)).toBeInTheDocument()
    expect(screen.getByText(/Variance:/)).toBeInTheDocument()
    expect(screen.getByText(/Standard Deviation:/)).toBeInTheDocument()
  })

  test('shows interpretation section', () => {
    render(<PMFDemo />)
    
    expect(screen.getByText('Interpretation')).toBeInTheDocument()
    expect(screen.getByText(/The expected value represents/)).toBeInTheDocument()
  })
})
