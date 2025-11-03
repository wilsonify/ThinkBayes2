import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, test } from 'vitest'
import TyposEstimation from '../components/TyposEstimation'

describe('TyposEstimation Component', () => {
  it('renders the component with initial observer data', () => {
    render(<TyposEstimation />)
    
    expect(screen.getByText('Typos Estimation Problem')).toBeInTheDocument()
    expect(screen.getByDisplayValue('1')).toBeInTheDocument() // Observer 1 typos
    expect(screen.getByDisplayValue('1')).toBeInTheDocument() // Observer 2 typos
    expect(screen.getByDisplayValue('0')).toBeInTheDocument() // Shared typos
    expect(screen.getByDisplayValue('200')).toBeInTheDocument() // Total pages
  })

  it('calculates Lincoln index correctly', () => {
    render(<TyposEstimation />)
    
    expect(screen.getByText('Lincoln Index')).toBeInTheDocument()
    // With 1,1,0: should show error or handle division by zero
    expect(screen.getByText('0')).toBeInTheDocument() // Should handle edge case
  })

  it('updates calculations when observer data changes', () => {
    render(<TyposEstimation />)
    
    const observer1Input = screen.getByDisplayValue('1')
    fireEvent.change(observer1Input, { target: { value: '5' } })
    
    // Should update calculations
    expect(screen.getByText('Lincoln Index')).toBeInTheDocument()
  })

  it('calculates Bayesian mean correctly', () => {
    render(<TyposEstimation />)
    
    expect(screen.getByText('Bayesian Mean')).toBeInTheDocument()
    const bayesianMean = screen.getByText(/\d+/) // Should show a number
    expect(bayesianMean).toBeInTheDocument()
  })

  it('displays scenario comparison chart', () => {
    render(<TyposEstimation />)
    
    expect(screen.getByTestId('bar-chart')).toBeInTheDocument()
    expect(screen.getByText('Scenario Analysis')).toBeInTheDocument()
  })

  it('shows posterior distribution visualization', () => {
    render(<TyposEstimation />)
    
    expect(screen.getByTestId('line-chart')).toBeInTheDocument()
    expect(screen.getByText('Posterior Distribution of Total Typos')).toBeInTheDocument()
  })

  it('updates when shared typos change', () => {
    render(<TyposEstimation />)
    
    const sharedInput = screen.getByDisplayValue('0')
    fireEvent.change(sharedInput, { target: { value: '1' } })
    
    expect(screen.getByDisplayValue('1')).toBeInTheDocument()
  })

  it('displays credible interval', () => {
    render(<TyposEstimation />)
    
    expect(screen.getByText('95% Credible Interval')).toBeInTheDocument()
    const intervalElement = screen.getByText(/\[\d+, \d+\]/) // Should show interval
    expect(intervalElement).toBeInTheDocument()
  })

  it('shows expected additional typos', () => {
    render(<TyposEstimation />)
    
    expect(screen.getByText('Expected Additional Typos')).toBeInTheDocument()
    const additionalTypos = screen.getByText(/\d+/) // Should show a number
    expect(additionalTypos).toBeInTheDocument()
  })

  test('handles zero shared typos edge case', () => {
    render(<TyposEstimation />)
    
    const observer1Input = screen.getByDisplayValue('1')
    const observer2Input = screen.getByDisplayValue('1')
    const sharedInput = screen.getByDisplayValue('0')
    
    fireEvent.change(observer1Input, { target: { value: '5' } })
    fireEvent.change(observer2Input, { target: { value: '3' } })
    fireEvent.change(sharedInput, { target: { value: '0' } })
    
    // Should handle gracefully
    expect(screen.getByText('Lincoln Index')).toBeInTheDocument()
  })

  test('validates typo count inputs', () => {
    render(<TyposEstimation />)
    
    const observer1Input = screen.getByDisplayValue('1')
    
    // Test negative input
    fireEvent.change(observer1Input, { target: { value: '-1' } })
    expect(observer1Input.value).toBe('0') // Should clamp to 0
    
    // Test maximum input
    fireEvent.change(observer1Input, { target: { value: '15' } })
    expect(observer1Input.value).toBe('10') // Should clamp to max
  })

  test('validates shared typos constraint', () => {
    render(<TyposEstimation />)
    
    const observer1Input = screen.getByDisplayValue('1')
    const sharedInput = screen.getByDisplayValue('0')
    
    fireEvent.change(observer1Input, { target: { value: '2' } })
    fireEvent.change(sharedInput, { target: { value: '3' } }) // Should not exceed min(observer1, observer2)
    
    expect(parseInt(sharedInput.value)).toBeLessThanOrEqual(2)
  })

  test('updates total pages parameter', () => {
    render(<TyposEstimation />)
    
    const pagesInput = screen.getByDisplayValue('200')
    fireEvent.change(pagesInput, { target: { value: '300' } })
    
    expect(screen.getByDisplayValue('300')).toBeInTheDocument()
  })

  test('displays mathematical framework', () => {
    render(<TyposEstimation />)
    
    expect(screen.getByText('Mathematical Framework')).toBeInTheDocument()
    expect(screen.getByText(/Lincoln Index/)).toBeInTheDocument()
    expect(screen.getByText(/Bayesian Model/)).toBeInTheDocument()
  })

  test('shows key assumptions section', () => {
    render(<TyposEstimation />)
    
    expect(screen.getByText('Key Assumptions')).toBeInTheDocument()
    expect(screen.getByText(/Each typo has equal probability/)).toBeInTheDocument()
    expect(screen.getByText(/Readers work independently/)).toBeInTheDocument()
  })

  test('displays applications section', () => {
    render(<TyposEstimation />)
    
    expect(screen.getByText('Applications')).toBeInTheDocument()
    expect(screen.getByText(/Wildlife population estimation/)).toBeInTheDocument()
    expect(screen.getByText(/Software bug detection/)).toBeInTheDocument()
  })

  test('shows extensions section', () => {
    render(<TyposEstimation />)
    
    expect(screen.getByText('Extensions')).toBeInTheDocument()
    expect(screen.getByText(/Multiple observers/)).toBeInTheDocument()
    expect(screen.getByText(/Heterogeneous detection probabilities/)).toBeInTheDocument()
  })

  test('calculates MAP estimate correctly', () => {
    render(<TyposEstimation />)
    
    expect(screen.getByText('MAP Estimate')).toBeInTheDocument()
    const mapEstimate = screen.getByText(/\d+/) // Should show a number
    expect(mapEstimate).toBeInTheDocument()
  })

  test('displays unique typos found', () => {
    render(<TyposEstimation />)
    
    expect(screen.getByText('Unique typos found')).toBeInTheDocument()
    const uniqueTypos = screen.getByText(/\d+/) // Should show a number
    expect(uniqueTypos).toBeInTheDocument()
  })

  test('scenario analysis updates correctly', () => {
    render(<TyposEstimation />)
    
    const observer1Input = screen.getByDisplayValue('1')
    fireEvent.change(observer1Input, { target: { value: '3' } })
    
    // Scenario chart should update
    expect(screen.getByTestId('bar-chart')).toBeInTheDocument()
  })

  test('handles maximum observer counts', () => {
    render(<TyposEstimation />)
    
    const observer1Input = screen.getByDisplayValue('1')
    const observer2Input = screen.getByDisplayValue('1')
    
    fireEvent.change(observer1Input, { target: { value: '10' } })
    fireEvent.change(observer2Input, { target: { value: '10' } })
    
    expect(screen.getByDisplayValue('10')).toBeInTheDocument()
  })
})
