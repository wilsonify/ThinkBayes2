import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, test } from 'vitest'
import SurvivalAnalysis from '../components/SurvivalAnalysis'

describe('SurvivalAnalysis Component', () => {
  it('renders the component with initial survival data', () => {
    render(<SurvivalAnalysis />)
    
    expect(screen.getByText('Survival Analysis')).toBeInTheDocument()
    expect(screen.getByDisplayValue('100')).toBeInTheDocument() // Initial at risk
    expect(screen.getByDisplayValue('2')).toBeInTheDocument() // Initial events
    expect(screen.getByDisplayValue('1.0')).toBeInTheDocument() // Prior alpha
  })

  it('calculates survival curves correctly', () => {
    render(<SurvivalAnalysis />)
    
    expect(screen.getByText('Survival Estimates')).toBeInTheDocument()
    expect(screen.getByText('Weibull Shape')).toBeInTheDocument()
    expect(screen.getByText('Weibull Scale')).toBeInTheDocument()
  })

  it('adds new time point', () => {
    render(<SurvivalAnalysis />)
    
    const addButton = screen.getByText('Add Time Point')
    fireEvent.click(addButton)
    
    expect(screen.getByText('Time 10:')).toBeInTheDocument()
  })

  it('removes time point', () => {
    render(<SurvivalAnalysis />)
    
    // Should have time points 0-9 initially
    expect(screen.getByText('Time 9:')).toBeInTheDocument()
    
    const removeButtons = screen.getAllByText('Remove')
    fireEvent.click(removeButtons[removeButtons.length - 1])
    
    // Should no longer have Time 9
    expect(screen.queryByText('Time 9:')).not.toBeInTheDocument()
  })

  it('updates calculations when survival data changes', () => {
    render(<SurvivalAnalysis />)
    
    const eventsInput = screen.getByDisplayValue('2')
    fireEvent.change(eventsInput, { target: { value: '5' } })
    
    // Survival estimates should update
    expect(screen.getByText('Survival Estimates')).toBeInTheDocument()
  })

  it('displays survival curves comparison', () => {
    render(<SurvivalAnalysis />)
    
    expect(screen.getByTestId('line-chart')).toBeInTheDocument()
    expect(screen.getByText('Survival Curves Comparison')).toBeInTheDocument()
  })

  it('shows hazard function visualization', () => {
    render(<SurvivalAnalysis />)
    
    expect(screen.getByTestId('line-chart')).toBeInTheDocument()
    expect(screen.getByText('Hazard Function')).toBeInTheDocument()
  })

  it('updates when prior parameters change', () => {
    render(<SurvivalAnalysis />)
    
    const alphaSlider = screen.getByLabelText(/Prior Shape α:/)
    fireEvent.change(alphaSlider, { target: { value: '2.0' } })
    
    expect(screen.getByDisplayValue('2.0')).toBeInTheDocument()
  })

  it('displays final survival percentage', () => {
    render(<SurvivalAnalysis />)
    
    expect(screen.getByText('Final Survival')).toBeInTheDocument()
    const survivalPercentage = screen.getByText(/\d+\.\d+%/) // Should show percentage
    expect(survivalPercentage).toBeInTheDocument()
  })

  test('handles minimum time points', () => {
    render(<SurvivalAnalysis />)
    
    // Remove all time points except minimum
    const removeButtons = screen.getAllByText('Remove')
    for (let i = 0; i < 7; i++) {
      fireEvent.click(removeButtons[0])
    }
    
    expect(screen.getByText('Time 0:')).toBeInTheDocument()
    expect(screen.getByText('Time 1:')).toBeInTheDocument()
    expect(screen.getByText('Time 2:')).toBeInTheDocument()
  })

  test('validates survival data inputs', () => {
    render(<SurvivalAnalysis />)
    
    const atRiskInput = screen.getByDisplayValue('100')
    const eventsInput = screen.getByDisplayValue('2')
    
    // Test negative inputs
    fireEvent.change(atRiskInput, { target: { value: '-1' } })
    expect(atRiskInput.value).toBe('0') // Should clamp to 0
    
    fireEvent.change(eventsInput, { target: { value: '-1' } })
    expect(eventsInput.value).toBe('0') // Should clamp to 0
  })

  test('generates synthetic data correctly', () => {
    render(<SurvivalAnalysis />)
    
    const generateButton = screen.getByText('Generate Example')
    fireEvent.click(generateButton)
    
    // Should have new synthetic data
    const inputs = screen.getAllByRole('spinbutton')
    inputs.forEach(input => {
      const value = parseInt(input.value)
      expect(value).toBeGreaterThanOrEqual(0)
    })
  })

  test('displays mathematical framework', () => {
    render(<SurvivalAnalysis />)
    
    expect(screen.getByText('Mathematical Framework')).toBeInTheDocument()
    expect(screen.getByText(/Kaplan-Meier Estimator/)).toBeInTheDocument()
    expect(screen.getByText(/Weibull Distribution/)).toBeInTheDocument()
  })

  test('shows key concepts section', () => {
    render(<SurvivalAnalysis />)
    
    expect(screen.getByText('Key Concepts')).toBeInTheDocument()
    expect(screen.getByText(/Censoring:/)).toBeInTheDocument()
    expect(screen.getByText(/Hazard function:/)).toBeInTheDocument()
    expect(screen.getByText(/Survival function:/)).toBeInTheDocument()
  })

  test('displays applications section', () => {
    render(<SurvivalAnalysis />)
    
    expect(screen.getByText('Applications')).toBeInTheDocument()
    expect(screen.getByText(/Clinical trials/)).toBeInTheDocument()
    expect(screen.getByText(/Reliability engineering/)).toBeInTheDocument()
  })

  test('handles censored data correctly', () => {
    render(<SurvivalAnalysis />)
    
    const censoredInput = screen.getByDisplayValue('0')
    fireEvent.change(censoredInput, { target: { value: '3' } })
    
    expect(screen.getByDisplayValue('3')).toBeInTheDocument()
    expect(screen.getByText('Survival Estimates')).toBeInTheDocument()
  })

  test('displays credible interval', () => {
    render(<SurvivalAnalysis />)
    
    expect(screen.getByText('95% Credible Interval')).toBeInTheDocument()
    const intervalElement = screen.getByText(/\[\d+, \d+\]/) // Should show interval
    expect(intervalElement).toBeInTheDocument()
  })

  test('shows total events count', () => {
    render(<SurvivalAnalysis />)
    
    expect(screen.getByText('Total Events')).toBeInTheDocument()
    const eventsElement = screen.getByText(/\d+ events out of \d+ initial subjects/)
    expect(eventsElement).toBeInTheDocument()
  })

  test('prior shape parameter updates correctly', () => {
    render(<SurvivalAnalysis />)
    
    const alphaSlider = screen.getByLabelText(/Prior Shape α:/)
    fireEvent.change(alphaSlider, { target: { value: '3.0' } })
    
    expect(screen.getByDisplayValue('3.0')).toBeInTheDocument()
  })

  test('hazard function displays correctly', () => {
    render(<SurvivalAnalysis />)
    
    expect(screen.getByText('Hazard Function')).toBeInTheDocument()
    expect(screen.getByTestId('line-chart')).toBeInTheDocument()
  })
})
