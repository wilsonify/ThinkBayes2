import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, test } from 'vitest'
import ChangepointDetection from '../components/ChangepointDetection'

describe('ChangepointDetection Component', () => {
  it('renders the component with initial time series data', () => {
    render(<ChangepointDetection />)
    
    expect(screen.getByText('Changepoint Detection')).toBeInTheDocument()
    expect(screen.getByDisplayValue('3')).toBeInTheDocument() // First time point events
    expect(screen.getByDisplayValue('1.0')).toBeInTheDocument() // Prior rate 1
    expect(screen.getByDisplayValue('5.0')).toBeInTheDocument() // Prior rate 2
  })

  it('calculates changepoint probabilities', () => {
    render(<ChangepointDetection />)
    
    expect(screen.getByText('Most Likely Changepoint')).toBeInTheDocument()
    const changepointResult = screen.getByText(/Time point \d+/)
    expect(changepointResult).toBeInTheDocument()
  })

  it('adds new time point to the series', () => {
    render(<ChangepointDetection />)
    
    const addButton = screen.getByText('Add Time Point')
    fireEvent.click(addButton)
    
    expect(screen.getByText('Time 11:')).toBeInTheDocument()
  })

  it('removes time point from the series', () => {
    render(<ChangepointDetection />)
    
    // Should have time points 1-10 initially
    expect(screen.getByText('Time 10:')).toBeInTheDocument()
    
    const removeButtons = screen.getAllByText('Remove')
    fireEvent.click(removeButtons[removeButtons.length - 1])
    
    // Should no longer have Time 10
    expect(screen.queryByText('Time 10:')).not.toBeInTheDocument()
  })

  it('updates calculations when time series data changes', () => {
    render(<ChangepointDetection />)
    
    const firstEventInput = screen.getByDisplayValue('3')
    fireEvent.change(firstEventInput, { target: { value: '5' } })
    
    // Changepoint analysis should update
    expect(screen.getByText('Most Likely Changepoint')).toBeInTheDocument()
  })

  it('displays changepoint probability chart', () => {
    render(<ChangepointDetection />)
    
    expect(screen.getByTestId('area-chart')).toBeInTheDocument()
    expect(screen.getByText('Changepoint Probability Distribution')).toBeInTheDocument()
  })

  it('shows event rate visualization', () => {
    render(<ChangepointDetection />)
    
    expect(screen.getByTestId('line-chart')).toBeInTheDocument()
    expect(screen.getByText('Event Rates Over Time')).toBeInTheDocument()
  })

  it('displays likelihood calculations', () => {
    render(<ChangepointDetection />)
    
    expect(screen.getByText('Likelihood Analysis')).toBeInTheDocument()
    expect(screen.getByText(/Best Likelihood:/)).toBeInTheDocument()
  })

  it('updates when prior rates change', () => {
    render(<ChangepointDetection />)
    
    const rate1Slider = screen.getByLabelText(/Prior Rate 1:/)
    fireEvent.change(rate1Slider, { target: { value: '2.0' } })
    
    expect(screen.getByDisplayValue('2.0')).toBeInTheDocument()
  })

  test('handles minimum time series length', () => {
    render(<ChangepointDetection />)
    
    // Remove all time points except minimum
    const removeButtons = screen.getAllByText('Remove')
    for (let i = 0; i < 7; i++) {
      fireEvent.click(removeButtons[0])
    }
    
    expect(screen.getByText('Time 1:')).toBeInTheDocument()
    expect(screen.getByText('Time 2:')).toBeInTheDocument()
    expect(screen.getByText('Time 3:')).toBeInTheDocument()
  })

  test('validates event count input', () => {
    render(<ChangepointDetection />)
    
    const firstEventInput = screen.getByDisplayValue('3')
    
    // Test negative input
    fireEvent.change(firstEventInput, { target: { value: '-1' } })
    expect(firstEventInput.value).toBe('0') // Should clamp to 0
    
    // Test maximum input
    fireEvent.change(firstEventInput, { target: { value: '25' } })
    expect(firstEventInput.value).toBe('20') // Should clamp to max
  })

  test('generates synthetic data correctly', () => {
    render(<ChangepointDetection />)
    
    const generateButton = screen.getByText('Generate Synthetic Data')
    fireEvent.click(generateButton)
    
    // Should have new synthetic data
    const inputs = screen.getAllByRole('spinbutton')
    inputs.forEach(input => {
      const value = parseInt(input.value)
      expect(value).toBeGreaterThanOrEqual(0)
      expect(value).toBeLessThanOrEqual(20)
    })
  })

  test('displays mathematical framework', () => {
    render(<ChangepointDetection />)
    
    expect(screen.getByText('Mathematical Framework')).toBeInTheDocument()
    expect(screen.getByText(/Model Structure/)).toBeInTheDocument()
    expect(screen.getByText(/Likelihood Calculation/)).toBeInTheDocument()
  })

  test('shows applications section', () => {
    render(<ChangepointDetection />)
    
    expect(screen.getByText('Applications')).toBeInTheDocument()
    expect(screen.getByText(/Detecting regime changes/)).toBeInTheDocument()
    expect(screen.getByText(/Monitoring system performance/)).toBeInTheDocument()
  })

  test('handles clear changepoint scenario', () => {
    render(<ChangepointDetection />)
    
    // Generate data with clear changepoint
    const generateButton = screen.getByText('Generate Synthetic Data')
    fireEvent.click(generateButton)
    
    // Should still identify most likely changepoint
    expect(screen.getByText('Most Likely Changepoint')).toBeInTheDocument()
  })

  test('displays posterior probability correctly', () => {
    render(<ChangepointDetection />)
    
    expect(screen.getByText('Posterior Probability')).toBeInTheDocument()
    const probabilityElement = screen.getByText(/\d+\.\d+%/) // Should show percentage
    expect(probabilityElement).toBeInTheDocument()
  })

  test('prior rate sliders work correctly', () => {
    render(<ChangepointDetection />)
    
    const rate1Slider = screen.getByLabelText(/Prior Rate 1:/)
    const rate2Slider = screen.getByLabelText(/Prior Rate 2:/)
    
    fireEvent.change(rate1Slider, { target: { value: '3.0' } })
    fireEvent.change(rate2Slider, { target: { value: '7.0' } })
    
    expect(screen.getByDisplayValue('3.0')).toBeInTheDocument()
    expect(screen.getByDisplayValue('7.0')).toBeInTheDocument()
  })
})
